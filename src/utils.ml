let rec interleave sep = function
  | ([] | [ _ ]) as tail -> tail
  | hd :: tail ->
      let tail = interleave sep tail in
      hd :: sep :: tail

let failwithf fmt = Printf.ksprintf failwith fmt

let rec filter_map f = function
  | [] -> []
  | x :: xs -> (
      match f x with
      | None -> filter_map f xs
      | Some x' -> x' :: filter_map f xs)

let update_if_changed v x = if Lwd.peek v <> x then Lwd.set v x
