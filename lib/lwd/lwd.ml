type 'a t =
  | Pure of 'a
  | Impure : {
      mutable value : 'a option;
      mutable trace : trace;
      desc: 'a desc;
    } -> 'a t
  | Root : {
      mutable on_invalidate : 'a -> unit;
      mutable value : 'a option;
      child : 'a t;
    } -> 'a t

and _ desc =
  | Map  : 'a t * ('a -> 'b) -> 'b desc
  | Map2 : 'a t * 'b t * ('a -> 'b -> 'c) -> 'c desc
  | Pair : 'a t * 'b t -> ('a * 'b) desc
  | App  : ('a -> 'b) t * 'a t -> 'b desc
  | Bind : { child : 'a t; map : 'a -> 'b t;
             mutable intermediate : 'b t option } -> 'b desc
  | Var  : { mutable binding : 'a } -> 'a desc
  | Prim : { acquire : unit -> 'a;
             release : 'a -> unit;
             mutable acquired : 'a option } -> 'a desc

and trace =
  | Done
  | More : 'a t * trace -> trace

(* Basic combinators *)
let return x = Pure x
let pure x = Pure x

let impure desc =
  Impure { value = None; trace = Done; desc }

let map f x = impure (Map (x, f))
let map2 f x y = impure (Map2 (x, y, f))
let map' x f = impure (Map (x, f))
let map2' x y f = impure (Map2 (x, y, f))
let pair x y = impure (Pair (x, y))
let app f x = impure (App (f, x))
let bind child map = impure (Bind { child; map; intermediate = None })
let id x = x
let join child = impure (Bind { child; map = id; intermediate = None })

(* Propagating invalidation *)
let rec invalidate_node : type a . a t -> unit = function
  | Pure _ -> assert false
  | Root { value = None; _ } | Impure { value = None ; _ } -> ()
  | Root ({ value = Some x; _ } as t) ->
    t.value <- None;
    t.on_invalidate x
  | Impure t ->
    t.value <- None;
    invalidate_trace t.trace

and invalidate_trace = function
  | Done -> ()
  | More (x, xs) ->
    invalidate_trace xs;
    invalidate_node x

(* Variables *)
type 'a var = 'a t
let var x = impure (Var {binding = x})
let get x = x

let set vx x =
  match vx with
  | Impure ({desc = Var v; _}) ->
    invalidate_node vx;
    v.binding <- x
  | _ -> assert false

let peek = function
  | Impure ({desc = Var v; _}) -> v.binding
  | _ -> assert false

(* Primitives *)
type 'a prim = 'a t
let prim ~acquire ~release =
  impure (Prim { acquire; release; acquired = None })
let get_prim x = x
let invalidate = invalidate_node

let observe ?(on_invalidate=ignore) child =
  Root { child; value = None; on_invalidate }

let rec sub_release : type a b . a t -> b t -> unit = fun origin ->
  function
  | Root _ -> assert false
  | Pure _ -> ()
  | Impure t as self ->
    let rec filter_trace origin = function
      | Done -> assert false
      | More (origin', rest) ->
        if Obj.magic origin == Obj.magic origin'
        then rest
        else More (origin', filter_trace origin rest)
    in
    match filter_trace origin t.trace with
    | More _ -> ()
    | Done ->
      t.value <- None;
      match t.desc with
      | Map  (x, _) -> sub_release self x
      | Map2 (x, y, _) ->
        sub_release self x;
        sub_release self y
      | Pair (x, y) ->
        sub_release self x;
        sub_release self y
      | App  (x, y) ->
        sub_release self x;
        sub_release self y
      | Bind { child; intermediate; map = _ } ->
        sub_release self child;
        begin match intermediate with
          | None -> ()
          | Some child' -> sub_release self child'
        end
      | Var  _ -> ()
      | Prim t ->
        let x = match t.acquired with None -> assert false | Some x -> x in
        t.acquired <- None;
        t.release x

let rec sub_sample : type a b . a t -> b t -> b = fun origin ->
  function
  | Root _ -> assert false
  | Pure x -> x
  | Impure t as self ->
    t.trace <- More (origin, t.trace);
    match t.value with
    | Some value -> value
    | None ->
      let value : b = match t.desc with
        | Map  (x, f) -> f (sub_sample self x)
        | Map2 (x, y, f) -> f (sub_sample self x) (sub_sample self y)
        | Pair (x, y) -> (sub_sample self x, sub_sample self y)
        | App  (f, x) -> (sub_sample self f) (sub_sample self x)
        | Bind x ->
          let old_intermediate = x.intermediate in
          let intermediate = x.map (sub_sample self x.child) in
          x.intermediate <- Some intermediate;
          let result = sub_sample self intermediate in
          begin match old_intermediate with
            | Some x' -> sub_release self x'
            | None -> ()
          end;
          result
        | Var  x -> x.binding
        | Prim t ->
          begin match t.acquired with
            | Some x -> x
            | None ->
              let x = t.acquire () in
              t.acquired <- Some x;
              x
          end
      in
      t.value <- Some value;
      value

type 'a root = 'a t

let sample = function
  | Pure _ | Impure _ -> assert false
  | Root t as self ->
    match t.value with
    | Some value -> value
    | None ->
      let value = sub_sample self t.child in
      t.value <- Some value;
      value

let is_damaged = function
  | Pure _ | Impure _ -> assert false
  | Root { value = None ; _ } -> true
  | Root { value = Some _ ; _ } -> false

let release = function
  | Pure _ | Impure _ -> assert false
  | Root t as self ->
    sub_release self t.child

let set_on_invalidate x f =
  match x with
  | Pure _ | Impure _ -> assert false
  | Root t -> t.on_invalidate <- f

module Infix = struct
  let (let$) = bind
  let (and$) = pair
  let ($=) = set
end
