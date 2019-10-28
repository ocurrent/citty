open Nottui

(* Find the index of next control character in a string *)
let control_character_index str i =
  let len = String.length str in
  let i = ref i in
  while
    let i = !i in
    i < len && str.[i] >= ' '
  do
    incr i
  done;
  if !i = len then raise Not_found;
  !i

(* Typeset the strings stored in a table with a fixed width, wrapping
   characters at the end of the line. *)
let word_wrap_string_table table width =
  if width <= 0 then
    Lwd.pure Ui.empty
  else
    (* Wrap at least around 8 characters *)
    let width = max 8 width in
    (* Split lines around newline characters.
       Control characters will normally be \n or \r, but other might sneak in.
       TODO: allow customization of handling of control characters? *)
    let rec split_lines x acc i =
      match control_character_index x i with
      | exception Not_found -> String.sub x i (String.length x - i) :: acc
      | j -> split_lines x (String.sub x i (j - i) :: acc) (j + 1)
    in
    (* Turn an input line into visual lines.
       Split the line into chunks of the right width,
       surrounding the splits with ↳ and ↲. *)
    let wrap_line str =
      let lines = ref [] in
      let pos = ref 0 in
      let len = String.length str in
      (* Compute visual lines *)
      while len - !pos > if !pos > 0 then width - 1 else width do
        if !pos = 0 then (
          lines := (String.sub str !pos (width - 1) ^ "↲") :: !lines;
          pos := !pos + (width - 1) )
        else (
          lines := ("↳" ^ String.sub str !pos (width - 2) ^ "↲") :: !lines;
          pos := !pos + (width - 2) )
      done;
      (* Produce an image for one visual line *)
      let render_line str = Ui.atom Notty.(I.string A.empty str) in
      match !lines with
      | [] ->
        (* Nothing to split, render the full input *)
        render_line str
      | lines ->
        (* Something was split:
           - append remaining characters
           - render each line
           - concatenate them vertically *)
        ("↳" ^ String.sub str !pos (len - !pos)) :: lines
        |> List.rev_map render_line
        |> Lwd_utils.pure_pack Ui.pack_y
    in
    (* Stack three images vertically *)
    let join3 a b c = Ui.join_y a (Ui.join_y b c) in
    (* Map reduce the table, lifting the strings to an intermediate type
       that is suitable for word wrapping.

       The input is provided as a stream of strings, but they don't represent
       lines, just a continuous stream of characters.
       Therefore we don't know where a line starts or ends until we see a
       control character.

       So individual pieces are represented as values of type
         [string * (ui * string) option]
       as follow:
       - a string "foo" that does not contain any newline character is
         [("foo", None)]
       - a string "foo\nbar" that contains a single newline character is
         [("foo", Some (empty, "bar"))]
       - a string "foo\nbar\nbaz" that contains two newline characters, and
         thus one fully defined line, is
         [("foo", Some (I.string "bar", "baz"))]
       - a string "foo\nbar\nbaz\foo" that contains three newline characters,
         and two fully defined lines, is
         [("foo", Some (I.vcat [string "bar"; string "baz"], "foo"))]

       The informal interpretation is thus [(prefix, Some (body, suffix))]:
       - [prefix] is string of character to append to the preceding line
       - [body] is the image of lines already rendered
       - [suffix] is string of character to prepend to the following line

       This type can be given monoid structure compatible with the string
       monoid. This gives a wrapping algorithm with efficient concatenation,
       suitable for incrementally rendering a stream of characters.
    *)
    Lwd_table.map_reduce
      (fun _ x ->
        match control_character_index x 0 with
        | exception Not_found -> (x, None)
        | i -> (
            let prefix = String.sub x 0 i in
            match split_lines x [] (i + 1) with
            | [] -> assert false
            | suffix :: rest ->
                let ui =
                  rest
                  |> List.rev_map wrap_line
                  |> Lwd_utils.pure_pack Ui.pack_y
                in
                (prefix, Some (ui, suffix)) ))
      ( ("", None),
        fun (pa, ta) (pb, tb) ->
          match ta with
          | None -> (pa ^ pb, tb)
          | Some (ua, sa) ->
              let line = sa ^ pb in
              ( pa,
                Some
                  ( match tb with
                  | None -> (ua, line)
                  | Some (ub, sb) -> (join3 ua (wrap_line line) ub, sb) ) ) )
      table
    |>
    (* After reducing the table, we produce the final UI, interpreting
       unterminated prefix and suffix has line of their own. *)
    Lwd.map (function
        | pa, None -> wrap_line pa
        | pa, Some (ub, sb) -> join3 (wrap_line pa) ub (wrap_line sb))

(* Grab the mouse and repeat an event until button is released *)
let grab_and_repeat f =
  let stop = ref false in
  let rec step delay () =
    if not !stop then
      Lwt.bind (f ()) @@ fun () ->
      Lwt.bind (Lwt_unix.sleep delay) (step 0.025)
    else Lwt.return_unit
  in
  Lwt.async (step 0.4);
  `Grab ((fun ~x:_ ~y:_ -> ()),
         fun ~x:_ ~y:_ -> stop := true)

let on_click f = fun ~x:_ ~y:_ -> function
  | `Left ->
    f ();
    `Handled
  | _ -> `Unhandled

(* Render a vertical scroll representing a [Nottui_widgets.scroll_state].
   The [set_scroll] function is called when the state should be updated to
   reflect a user interaction. *)
let vertical_scrollbar ~set_scroll (st : Nottui_widgets.scroll_state) =
  let bar color h = Notty.(I.char A.(bg color) ' ' 1 h) in
  let gray = Notty.A.gray 1 in
  let lightgray = Notty.A.white in
  if st.visible = 0 then
    Ui.atom Notty.I.empty
  else if st.total > st.visible then
    (* Compute size of the handle inside the bar *)
    let ratio = max 1 (st.visible * st.visible / st.total) in
    let rest = st.visible - ratio in
    let prefix = rest * st.position / st.bound in
    let suffix = rest - prefix in
    (* React to mouse events on the scroll bar *)
    let mouse_handler ~x:_ ~y = function
      | `Left ->
        if y < prefix then
          let position = ref st.position in
          grab_and_repeat (
            fun () ->
              position := max 0 (!position - (st.visible / 2));
              set_scroll { st with position = !position };
              Lwt.return_unit
          )
        else if y > prefix + ratio then
          let position = ref st.position in
          grab_and_repeat (
            fun () ->
              position := min st.bound (!position + (st.visible / 2));
              set_scroll { st with position = !position };
              Lwt.return_unit
          )
        else
          `Grab (
            (fun ~x:_ ~y:y' ->
               let dy = y' - y in
               let position =
                 float st.position +.
                 (float dy /. float st.visible *. float st.total)
               in
               let position =
                 max 0 (min st.bound (int_of_float position))
               in
               set_scroll { st with position }
            ),
            (fun ~x:_ ~y:_ -> () )
          )
      | _ -> `Unhandled
    in
    Notty.I.vcat [
      bar gray prefix;
      bar lightgray ratio;
      bar gray suffix;
    ]
    |> Ui.atom
    |> Ui.mouse_area mouse_handler
  else
    Ui.atom (bar gray st.visible)

let list_box ~items ~render ~select =
  let prev_highlight = ref (Lwd.var false) in
  let select_item (var, item) =
      Lwd.set !prev_highlight false;
      Lwd.set var true;
      prev_highlight := var;
      select item
  in
  let select_next list =
    let rec seek = function
      | [] -> false
      | ((x, _), _) :: (item, _) :: _ when Lwd.peek x ->
        select_item item;
        true
      | _ :: rest -> seek rest
    in
    if seek list then ()
    else match list with (item, _) :: _ -> select_item item | [] -> ()
  and select_prev list =
    let rec seek = function
      | [] -> ()
      | (item, _) :: ((y, _), _) :: _ when Lwd.peek y -> select_item item
      | [ (item, _) ] -> select_item item
      | _ :: rest -> seek rest
    in
    seek list
  in
  let show_item x =
    let item = (Lwd.var false, x) in
    let ui =
      Lwd.map' (Lwd.get (fst item)) @@ fun highlight ->
      Ui.mouse_area
        (on_click @@ fun () -> select_item item)
        (render (snd item) highlight)
    in
    (item, ui)
  in
  let items = List.map show_item items in
  let dispatch = function
    | `Select_prev -> select_prev items
    | `Select_next -> select_next items
  in
  let view =
    Lwd.map'
      (Lwd_utils.pack Ui.pack_y (List.map snd items))
      (Ui.focus_area (Time.next ())
         {
           Ui.action =
             (fun direct key ->
                match (direct, key) with
                | `Direct, (`Arrow `Up, []) ->
                  dispatch `Select_prev;
                  `Handled
                | `Direct, (`Arrow `Down, []) ->
                  dispatch `Select_next;
                  `Handled
                | _ -> `Unhandled);
           Ui.status = (fun _ _ -> ());
         })
  in
  (view, dispatch)

let fit_string str len =
  let len0 = String.length str in
  if len < len0 then String.sub str 0 len
  else if len > len0 then str ^ String.make (len - len0) ' '
  else str


type pane = Pane of ((pane -> unit) -> ui Lwd.t)

let pane_navigator pane =
  let empty = Lwd.pure Ui.empty in
  let left_pane = Lwd.var empty in
  let middle_pane = Lwd.var empty in
  let right_pane = Lwd.var empty in
  let view =
    let place_ui_var ?sw v =
      Lwd.(v |> get |> join |> map (Ui.resize ~w:0 ?sw))
    in
    let spacer =
      Ui.empty |> Ui.resize ~w:1 ~sh:1 ~bg:Notty.A.(bg (gray 1)) |> Lwd.pure
    in
    Lwd_utils.pack Ui.pack_x
      [
        place_ui_var left_pane ~sw:1;
        spacer;
        place_ui_var middle_pane ~sw:2;
        spacer;
        place_ui_var right_pane ~sw:6;
      ]
  in
  let rec render_pane backlinks (Pane pane) =
    let content = Lwd.var empty in
    let value = Lwd.get content |> Lwd.join in
    let backlinks' = value :: backlinks in
    Lwd.set content (pane (render_pane backlinks'));
    begin match backlinks with
      | [] ->
        Lwd.set left_pane empty;
        Lwd.set middle_pane value;
        Lwd.set right_pane empty;
      | [x] ->
        Lwd.set left_pane empty;
        Lwd.set middle_pane x;
        Lwd.set right_pane value;
      | x :: y :: _ ->
        Lwd.set left_pane y;
        Lwd.set middle_pane x;
        Lwd.set right_pane value;
    end;
  in
  render_pane [] pane;
  view
