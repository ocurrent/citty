open Lwt
open Ocaml_ci_api
open Nottui
module C = Capnp_rpc_lwt
module W = Nottui_widgets

let header = Lwd.var (Lwd.pure Ui.empty)

let left_pane = Lwd.var (Lwd.pure Ui.empty)

let middle_pane = Lwd.var (Lwd.pure Ui.empty)

let right_pane = Lwd.var (Lwd.pure Ui.empty)

let footer = Lwd.var (Lwd.pure Ui.empty)

let ui =
  let place_ui_var ?sw v =
    Lwd.(v |> get |> join |> map (Ui.resize ~w:0 ?sw))
  in
  let spacer =
    Ui.empty |> Ui.resize ~w:1 ~bg:Notty.A.(bg (gray 1)) |> Lwd.pure
  in
  Lwd_utils.pack Ui.pack_y
    [
      place_ui_var header;
      Lwd_utils.pack Ui.pack_x
        [
          place_ui_var left_pane ~sw:1;
          spacer;
          place_ui_var middle_pane ~sw:2;
          spacer;
          place_ui_var right_pane ~sw:6;
        ];
      place_ui_var footer;
    ]

let errorf fmt = Printf.ksprintf failwith fmt

let import_ci_ref ~vat = function
  | Some url -> Capnp_rpc_unix.Vat.import vat url
  | None -> (
      match Sys.getenv_opt "HOME" with
      | None -> errorf "$HOME not set! Can't get default cap file location."
      | Some home ->
          let path = Filename.concat home ".ocaml-ci.cap" in
          if Sys.file_exists path then Capnp_rpc_unix.Cap_file.load vat path
          else errorf "Default cap file %S not found!" path )

let with_ref r fn =
  Lwt.finalize
    (fun () -> fn r)
    (fun () ->
      C.Capability.dec_ref r;
      Lwt.return_unit)

let show_log table job =
  let add str = if str <> "" then Lwd_table.append' table str in
  let rec aux start =
    Current_rpc.Job.log ~start job >>= function
    | Error _ as e ->
        add "ERROR";
        Lwt.return e
    | Ok ("", _) ->
        add "DONE";
        Lwt.return_ok ()
    | Ok (data, next) ->
        add data;
        aux next
  in
  aux 0L

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

let reduce_string_table table width =
  if width = 0 then Lwd.pure Ui.empty
  else
    let width = max 8 width in
    let rec split_lines x acc i =
      match control_character_index x i with
      | exception Not_found -> String.sub x i (String.length x - i) :: acc
      | j -> split_lines x (String.sub x i (j - i) :: acc) (j + 1)
    in
    let render_line str = Ui.atom Notty.(I.string A.empty str) in
    let wrap_line str =
      let lines = ref [] in
      let pos = ref 0 in
      let len = String.length str in
      while len - !pos > if !pos > 0 then width - 1 else width do
        if !pos = 0 then (
          lines := (String.sub str !pos (width - 1) ^ "⏎") :: !lines;
          pos := !pos + (width - 1) )
        else (
          lines := ("↳" ^ String.sub str !pos (width - 2) ^ "⏎") :: !lines;
          pos := !pos + (width - 2) )
      done;
      match !lines with
      | [] -> render_line str
      | lines ->
          ("↳" ^ String.sub str !pos (len - !pos)) :: lines
          |> List.rev_map render_line
          |> Lwd_utils.pure_pack Ui.pack_y
    in
    let join3 a b c = Ui.join_y a (Ui.join_y b c) in
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
                  rest |> List.rev_map wrap_line
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
    |> Lwd.map (function
         | pa, None -> wrap_line pa
         | pa, Some (ub, sb) -> join3 (wrap_line pa) ub (wrap_line sb))

let rec show_status var job =
  Current_rpc.Job.status job
  |> Lwt_result.map
     @@ fun { Current_rpc.Job.id; description; can_cancel; can_rebuild } ->
     let text =
       Format.asprintf
         "@[<v2>Job %S:@,\
          Description: @[%a@]@,\
          Can cancel: %b@,\
          Can rebuild: %b@]@."
         id Fmt.lines description can_cancel can_rebuild
     in
     let buttons = Lwd.var Ui.empty in
     let rebuild_button =
       if can_rebuild then
         Ui.mouse_area
           (fun ~x:_ ~y:_ -> function
             | `Left ->
                 Lwd.set buttons Ui.empty;
                 ignore (show_status var (Current_rpc.Job.rebuild job));
                 `Handled | _ -> `Unhandled)
           (W.string ~attr:Notty.A.(bg red) "[Rebuild]")
       else Ui.empty
     in
     let cancel_button =
       if can_cancel then
         Ui.mouse_area
           (fun ~x:_ ~y:_ -> function
             | `Left ->
                 Lwd.set buttons Ui.empty;
                 Lwt.async (fun () ->
                     Current_rpc.Job.cancel job >>= fun _ ->
                     ignore (show_status var job);
                     Lwt.return_unit);
                 `Handled | _ -> `Unhandled)
           (W.string ~attr:Notty.A.(bg blue) "[Cancel]")
       else Ui.empty
     in
     Lwd.set buttons
       (Lwd_utils.pure_pack Ui.pack_x
          [ rebuild_button; Ui.atom (Notty.I.void 1 0); cancel_button ]);
     let log_lines = Lwd_table.make () in
     ignore (show_log log_lines job);
     let width = Lwd.var 0 in
     let update_if_changed v x = if Lwd.peek v <> x then Lwd.set v x in
     let scroll_state = Lwd.var W.default_scroll_state in
     let set_scroll reason st =
       let st =
         if
           reason = `Content
           &&
           let st' = Lwd.peek scroll_state in
           st'.W.position = st'.W.bound
           && st.W.position = st'.W.position
           && st.W.position < st.W.bound
         then { st with position = st.W.bound }
         else st
       in
       Lwd.set scroll_state st
     in
     let scrollbar =
       Lwd.get scroll_state
       |> Lwd.map (fun st ->
              let bar color h = Notty.(I.char A.(bg color) ' ' 1 h) in
              let gray = Notty.A.gray 1 in
              let lightgray = Notty.A.white in
              if st.W.visible = 0 then Ui.atom Notty.I.empty
              else if st.W.total > st.W.visible then
                let ratio = max 1 (st.W.visible * st.W.visible / st.W.total) in
                let rest = st.W.visible - ratio in
                let prefix = rest * st.position / st.bound in
                let suffix = rest - prefix in
                let repeat f =
                  let stop = ref false in
                  let rec step delay () =
                    if not !stop then (
                      f ();
                      Lwt_unix.sleep delay >>= step 0.025 )
                    else Lwt.return_unit
                  in
                  Lwt.async (step 0.4);
                  `Grab ((fun ~x:_ ~y:_ -> ()), fun ~x:_ ~y:_ -> stop := true)
                in
                Ui.mouse_area
                  (fun ~x:_ ~y -> function
                    | `Left ->
                        if y < prefix then
                          let position = ref st.position in
                          repeat (fun () ->
                              position := max 0 (!position - (st.visible / 2));
                              set_scroll `Change
                                { st with position = !position })
                        else if y > prefix + ratio then
                          let position = ref st.position in
                          repeat (fun () ->
                              position :=
                                min st.bound (!position + (st.visible / 2));
                              set_scroll `Change
                                { st with position = !position })
                        else
                          `Grab
                            ( (fun ~x:_ ~y:y' ->
                                let dy = y' - y in
                                let position =
                                  float st.position
                                  +. float dy /. float st.W.visible
                                     *. float st.W.total
                                in
                                let position =
                                  max 0 (min st.bound (int_of_float position))
                                in
                                set_scroll `Change { st with position }),
                              fun ~x:_ ~y:_ -> () ) | _ -> `Unhandled)
                  ( Ui.atom
                  @@ Notty.I.vcat
                       [
                         bar gray prefix;
                         bar lightgray ratio;
                         bar gray suffix;
                       ] )
              else Ui.atom (bar gray st.W.visible))
     in
     let result =
       Lwd_utils.pack Ui.pack_y
         [
           Lwd.pure (W.string text);
           Lwd_utils.pack Ui.pack_x
             [
               Lwd.get buttons;
               Lwd.pure
                 (Ui.resize ~h:1 ~sw:1 ~bg:Notty.A.(bg (gray 1)) Ui.empty);
             ];
           Lwd_utils.pack Ui.pack_x
             [
               Lwd_utils.pack Ui.pack_y
                 [ Lwd.bind (Lwd.get width) (reduce_string_table log_lines) ]
               |> W.vscroll_area ~state:(Lwd.get scroll_state)
                    ~change:set_scroll
               |> Lwd.map (fun ui ->
                      ui |> Ui.resize ~w:0 ~sw:1
                      |> Ui.size_sensor (fun w _ -> update_if_changed width w));
               scrollbar;
             ];
         ]
     in
     Lwd.set var result

let show_status job =
  let result = Lwd.var (Lwd.pure Ui.empty) in
  show_status result job
  |> Lwt_result.map (fun () -> Lwd.join (Lwd.get result))

let fit_string str len =
  let len0 = String.length str in
  if len < len0 then String.sub str 0 len
  else if len > len0 then str ^ String.make (len - len0) ' '
  else str

let render_ref repo hash =
  Lwd.set right_pane (Lwd.pure (W.string "..."));
  Lwt.async (fun () ->
      let commit = Client.Repo.commit_of_hash repo hash in
      Lwt.map (fun ui ->
          let ui =
            match ui with
            | Ok ui -> ui
            | Error (`Capnp e) -> Lwd.pure (W.fmt "%a" Capnp_rpc.Error.pp e)
            | Error `No_job -> Lwd.pure (W.string "No jobs")
          in
          Lwd.set right_pane ui)
      @@
      let open Lwt_result in
      Client.Commit.jobs commit >>= function
      | [] -> Lwt.return_error `No_job
      | job :: _ ->
          (* FIXME: handle other jobs *)
          show_status (Client.Commit.job_of_variant commit job.variant))

let prev_highlight = ref (Lwd.var false)

let do_highlight (var, (repo, hash)) =
  Lwd.set !prev_highlight false;
  Lwd.set var true;
  prev_highlight := var;
  render_ref repo hash

let show_ref repo (gref, hash) =
  let gref = fit_string gref 24 in
  let vhighlight = (Lwd.var false, (repo, hash)) in
  let style hl =
    if hl then Notty.A.(st bold ++ fg lightblue ++ st reverse)
    else Notty.A.(st bold ++ fg lightblue)
  in
  let ui =
    Lwd.map' (Lwd.get (fst vhighlight)) @@ fun highlight ->
    W.printf ~attr:(style highlight) "%10s   #%s" gref (String.sub hash 0 6)
    |> Ui.mouse_area (fun ~x:_ ~y:_ ->
         function
         | `Left ->
             do_highlight vhighlight;
             `Handled
         | _ -> `Unhandled)
  in
  (vhighlight, ui)

let select_next list =
  let rec seek = function
    | [] -> false
    | ((x, _), _) :: (item, _) :: _ when Lwd.peek x ->
        do_highlight item;
        true
    | _ :: rest -> seek rest
  in
  if seek list then ()
  else match list with (item, _) :: _ -> do_highlight item | [] -> ()

let select_prev list =
  let rec seek = function
    | [] -> ()
    | (item, _) :: ((y, _), _) :: _ when Lwd.peek y -> do_highlight item
    | [ (item, _) ] -> do_highlight item
    | _ :: rest -> seek rest
  in
  seek list

let show_repo repo =
  Client.Repo.refs repo >>= function
  | Ok refs ->
      let refs =
        refs |> Client.Ref_map.to_seq |> Seq.map (show_ref repo) |> List.of_seq
      in
      Lwd_utils.pack Ui.pack_y (List.map snd refs)
      |> Lwd.map
           (Ui.focus_area (Time.next ())
              {
                Ui.action =
                  (fun direct key ->
                    match (direct, key) with
                    | `Direct, (`Arrow `Up, []) ->
                        select_prev refs;
                        `Handled
                    | `Direct, (`Arrow `Down, []) ->
                        select_next refs;
                        `Handled
                    | _ -> `Unhandled);
                Ui.status = (fun _ _ -> ());
              })
      |> Lwd.set middle_pane;
      Lwt.return_unit
  | Error (`Capnp e) ->
      Lwd.pure (W.fmt "%a" Capnp_rpc.Error.pp e) |> Lwd.set middle_pane;
      Lwt.return_unit

let show_repos () =
  let vat = Capnp_rpc_unix.client_only_vat () in
  match import_ci_ref ~vat None with
  | Error _ as e -> Lwt.return e
  | Ok sr -> (
      let host = Uri.host_with_default (Capnp_rpc_unix.Vat.export vat sr) in
      Lwd.set header (Lwd.pure (W.string ~attr:Notty.A.(fg green) host));
      C.Sturdy_ref.connect_exn sr >>= fun ci ->
      Client.CI.orgs ci >>= function
      | Error _ as err -> Lwt.return err
      | Ok orgs ->
          Lwt_list.map_s
            (fun x -> x)
            (List.map
               (fun org ->
                 let handle = Client.CI.org ci org in
                 Lwt_result.map
                   (fun repos ->
                     repos
                     |> List.map (fun repo ->
                            ((org, repo), Client.Org.repo handle repo)))
                   (Client.Org.repos handle))
               orgs)
          >>= fun repos ->
          let render = function
            | Ok repos ->
                List.map
                  (fun ((org, repo), handle) ->
                    W.printf "%s/%s" org repo
                    |> Ui.mouse_area (fun ~x:_ ~y:_ ->
                         function
                         | `Left ->
                             Lwt.async (fun () -> show_repo handle);
                             `Handled
                         | _ -> `Unhandled))
                  repos
            | Error (`Capnp e) -> [ W.fmt "%a" Capnp_rpc.Error.pp e ]
          in
          Lwd.set left_pane
            (Lwd.pure
               (Lwd_utils.pure_pack Ui.pack_y
                  (List.concat (List.map render repos))));
          Lwt.return_ok () )

(*| Some (`Ref _target) ->
      (*with_ref (Client.Repo.job_of_ref repo target) @@ fun _ ->*)
      Lwt.return (Lwd.return (W.string "Ref target"))
    | Some (`Commit _hash) ->
      (*with_ref (Client.Repo.job_of_ref repo hash) @@ fun _ ->*)
      Lwt.return (Lwd.return (W.string "Commit hash"))*)

let main () =
  Lwt_main.run
    (show_repos () >>= function
     | Ok () -> Nottui_lwt.run ui
     | Error (`Capnp err) ->
         Format.eprintf "%a" Capnp_rpc.Error.pp err;
         Lwt.return_unit
     | Error (`Msg msg) ->
         Format.eprintf "Error: %S" msg;
         Lwt.return_unit)

let () = main ()
