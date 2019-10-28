open Lwt
open Ocaml_ci_api
open Nottui
module C = Capnp_rpc_lwt
module NW = Nottui_widgets
module W = Widgets

let header = Lwd.var (Lwd.pure Ui.empty)

let body = Lwd.var (Lwd.pure Ui.empty)

let footer = Lwd.var (Lwd.pure Ui.empty)

let ui =
  let place_ui_var ?sw v =
    Lwd.(v |> get |> join |> map (Ui.resize ~w:0 ?sw))
  in
  Lwd_utils.pack Ui.pack_y
    [
      place_ui_var header;
      Lwd.get body |> Lwd.join;
      place_ui_var footer;
    ]

let failwithf fmt = Printf.ksprintf failwith fmt

let import_ci_ref ~vat = function
  | Some url -> Capnp_rpc_unix.Vat.import vat url
  | None -> (
      match Sys.getenv_opt "HOME" with
      | None -> failwithf "$HOME not set! Can't get default cap file location."
      | Some home ->
        let path = Filename.concat home ".ocaml-ci.cap" in
        if Sys.file_exists path then
          Capnp_rpc_unix.Cap_file.load vat path
        else
          failwithf "Default cap file %S not found!" path
    )

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

let rec interleave sep = function
  | [] | [_] as tail -> tail
  | hd :: tail ->
    let tail = interleave sep tail in
    hd :: sep :: tail

let rec show_status var job =
  Current_rpc.Job.status job
  |> Lwt_result.map
     @@ fun { Current_rpc.Job.id; description; can_cancel; can_rebuild } ->
     let text =
       Format.asprintf
         "@[<v2>Job %S:@,\
          Description: @[%a@]@]@."
         id Fmt.lines description
     in
     let buttons = Lwd.var Ui.empty in
     let rebuild_button =
       if can_rebuild then
         Ui.mouse_area
           (W.on_click @@ fun () ->
            Lwd.set buttons Ui.empty;
            ignore (show_status var (Current_rpc.Job.rebuild job));
           )
           (NW.string ~attr:Notty.A.(bg red) "[Rebuild]")
       else Ui.empty
     in
     let cancel_button =
       if can_cancel then
         Ui.mouse_area
           (W.on_click @@ fun () ->
            Lwd.set buttons Ui.empty;
            Lwt.async (fun () ->
                Current_rpc.Job.cancel job >>= fun _ ->
                ignore (show_status var job);
                Lwt.return_unit);
           )
           (NW.string ~attr:Notty.A.(bg blue) "[Cancel]")
       else Ui.empty
     in
     [ rebuild_button; cancel_button ]
     |> interleave (Ui.atom (Notty.I.void 1 0))
     |> Lwd_utils.pure_pack Ui.pack_x
     |> Lwd.set buttons;
     let log_lines = Lwd_table.make () in
     ignore (show_log log_lines job);
     let width = Lwd.var 0 in
     let update_if_changed v x = if Lwd.peek v <> x then Lwd.set v x in
     let scroll_state = Lwd.var NW.default_scroll_state in
     let set_scroll reason st =
       let scroll_on_output =
         reason = `Content
         &&
         let st' = Lwd.peek scroll_state in
         st'.NW.position = st'.NW.bound
         && st.NW.position = st'.NW.position
         && st.NW.position < st.NW.bound
       in
       if scroll_on_output then
         Lwd.set scroll_state { st with position = st.NW.bound }
       else
         Lwd.set scroll_state st
     in
     let result =
       Lwd_utils.pack Ui.pack_y
         [
           Lwd.pure (text |> NW.string |> Ui.resize ~w:0 ~sw:1);
           Lwd_utils.pack Ui.pack_x
             [
               Lwd.get buttons;
               Lwd.pure
                 (Ui.resize ~h:1 ~sw:1 ~bg:Notty.A.(bg (gray 1)) Ui.empty);
             ]
           |> Lwd.map (Ui.resize ~w:0 ~sw:1);
           Lwd_utils.pack Ui.pack_x
             [
               Lwd.bind (Lwd.get width) (W.word_wrap_string_table log_lines)
               |> NW.vscroll_area ~state:(Lwd.get scroll_state)
                    ~change:set_scroll
               |> Lwd.map (fun ui ->
                      ui
                      |> Ui.resize ~w:0 ~sw:1 ~h:0 ~sh:1
                      |> Ui.size_sensor (fun w _ -> update_if_changed width w)
                      |> Ui.mouse_area (fun ~x:_ ~y:y0 -> function
                          | `Left ->
                            let st = Lwd.peek scroll_state in
                            `Grab (
                              (fun ~x:_ ~y:y1 ->
                                 set_scroll `Change
                                   {
                                     st with
                                     position = st.position + y0 - y1;
                                   }
                              ),
                              fun ~x:_ ~y:_ -> ()
                            )
                          | _ -> `Unhandled)
                 );
               Lwd.get scroll_state
               |> Lwd.map (W.vertical_scrollbar ~set_scroll:(set_scroll `Change))
               |> Lwd.map (Ui.resize ~w:1 ~sw:0 ~h:0 ~sh:1);
             ];
         ]
     in
     Lwd.set var result

let show_status job =
  let result = Lwd.var (Lwd.pure Ui.empty) in
  show_status result job
  |> Lwt_result.map (fun () -> Lwd.join (Lwd.get result))

let show_repo repo pane =
  Client.Repo.refs repo >>= function
  | Ok refs ->
    let select next_pane (_, hash) =
      next_pane (W.Pane (fun _ -> Lwd.pure (NW.string "...")));
      Lwt.async @@ fun () ->
      let commit = Client.Repo.commit_of_hash repo hash in
      Lwt.map (fun ui ->
          let ui =
            match ui with
            | Ok ui -> ui
            | Error (`Capnp e) -> Lwd.pure (NW.fmt "%a" Capnp_rpc.Error.pp e)
            | Error `No_job -> Lwd.pure (NW.string "No jobs")
          in
          next_pane (W.Pane (fun _ -> ui)))
      @@
      let open Lwt_result in
      Client.Commit.jobs commit >>= function
      | [] -> Lwt.return_error `No_job
      | job :: _ ->
        (* FIXME: handle other jobs *)
        show_status (Client.Commit.job_of_variant commit job.variant)
    in
    let render (gref, hash) highlight =
      let attr =
        if highlight then
          Notty.A.(st bold ++ fg lightblue ++ st reverse)
        else
          Notty.A.(st bold ++ fg lightblue)
      in
      NW.printf ~attr "%10s   #%s" (W.fit_string gref 24) (String.sub hash 0 6)
    in
    let items = refs |> Client.Ref_map.to_seq |> List.of_seq in
    pane (W.Pane (fun next ->
        let ui, _dispatch = W.list_box ~items ~render ~select:(select next) in
        ui));
    Lwt.return_unit
  | Error (`Capnp e) ->
      pane (W.Pane (fun _ -> Lwd.pure (NW.fmt "%a" Capnp_rpc.Error.pp e)));
      Lwt.return_unit

let show_repos pane =
  let vat = Capnp_rpc_unix.client_only_vat () in
  match import_ci_ref ~vat None with
  | Error _ as e -> Lwt.return e
  | Ok sr -> (
      let host = Uri.host_with_default (Capnp_rpc_unix.Vat.export vat sr) in
      Lwd.set header (Lwd.pure (NW.string ~attr:Notty.A.(fg green) host));
      C.Sturdy_ref.connect_exn sr >>= fun ci ->
      Client.CI.orgs ci >>= function
      | Error _ as err -> Lwt.return err
      | Ok orgs ->
        let render repo hl =
          match repo with
          | Ok ((org, repo), _handle) ->
            let attr =
              if hl then
                Notty.A.(st bold ++ st reverse)
              else
                Notty.A.empty
            in
            NW.printf ~attr "%s/%s" org repo
          | Error (`Capnp e) ->
            NW.fmt ~attr:Notty.A.(fg red) "%a" Capnp_rpc.Error.pp e
        in
        let select next_pane = function
          | Ok (_repo, handle) ->
            Lwt.async (fun () -> show_repo handle next_pane)
          | Error _ ->
            next_pane (W.Pane (fun _ -> Lwd.pure Ui.empty))
        in
        Lwt_list.map_s
          (fun x -> x)
          (List.map
             (fun org ->
                let handle = Client.CI.org ci org in
                Lwt.map
                  (function
                    | Error e -> [Error e]
                    | Ok repos ->
                      let handle_of repo =
                        Ok ((org, repo), Client.Org.repo handle repo)
                      in
                      List.map handle_of repos)
                  (Client.Org.repos handle))
             orgs)
          >>= fun items ->
          let items = List.flatten items in
          pane (W.Pane (fun next_pane -> fst (W.list_box ~items ~render ~select:(select next_pane))));
          Lwt.return_ok () )

(*| Some (`Ref _target) ->
      (*with_ref (Client.Repo.job_of_ref repo target) @@ fun _ ->*)
      Lwt.return (Lwd.return (NW.string "Ref target"))
    | Some (`Commit _hash) ->
      (*with_ref (Client.Repo.job_of_ref repo hash) @@ fun _ ->*)
      Lwt.return (Lwd.return (NW.string "Commit hash"))*)

let main () =
  Lwt_main.run
    (show_repos (fun pane -> Lwd.set body (W.pane_navigator pane))
     >>= function
     | Ok () -> Nottui_lwt.run ui
     | Error (`Capnp err) ->
       Format.eprintf "%a" Capnp_rpc.Error.pp err;
       Lwt.return_unit
     | Error (`Msg msg) ->
       Format.eprintf "Error: %S" msg;
       Lwt.return_unit)

let () = main ()
