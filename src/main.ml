open Lwt
open Ocaml_ci_api
open Nottui
open Utils
module C = Capnp_rpc_lwt
module NW = Nottui_widgets
module W = Widgets
module Pane = W.Pane

let header = Lwd.var W.empty

let body = Lwd.var W.empty

let footer = Lwd.var W.empty

let spinner =
  let rec frames =
    "⠋"
    :: "⠙"
    :: "⠹"
    :: "⠸"
    :: "⠼"
    :: "⠴"
    :: "⠦"
    :: "⠧"
    :: "⠇"
    :: "⠏"
    :: frames
  in
  Lwd.prim
    ~acquire:(fun () ->
      let running = ref true in
      let frame = Lwd.var frames in
      let rec next_frame () =
        Lwt_unix.sleep 0.080 >>= fun () ->
        let frames = Lwd.peek frame in
        Lwd.set frame (List.tl frames);
        if !running then next_frame () else Lwt.return_unit
      in
      Lwt.async next_frame;
      (running, Lwd.get frame))
    ~release:(fun (running, _) -> running := false)
  |> Lwd.get_prim
  |> Lwd.map (fun (_running, var) -> var)
  |> Lwd.join
  |> Lwd.map List.hd

let ui =
  let place_ui_var v = Lwd.(v |> get |> join |> map (Ui.resize ~w:0)) in
  Lwd_utils.pack Ui.pack_y
    [ place_ui_var header; Lwd.get body |> Lwd.join; place_ui_var footer ]

let render_list_item highlight text =
  let attr =
    if highlight then Notty.A.(st bold ++ fg lightblue ++ st reverse)
    else Notty.A.(st bold ++ fg lightblue)
  in
  NW.string ~attr text

let render_state highlight (state : Raw.Reader.JobInfo.State.unnamed_union_t) =
  let icon, color =
    let open Notty.A in
    match state with
    | NotStarted -> ("[ ]", white)
    | Passed -> ("[✓]", green)
    | Failed _ -> ("[X]", red)
    | Active -> ("[.]", yellow)
    | Aborted -> ("[A]", lightred)
    | Undefined _ -> ("[�]", white)
  in
  if highlight then NW.string ~attr:Notty.A.(fg color ++ st reverse) icon
  else NW.string ~attr:Notty.A.(fg color) icon

let import_ci_ref ~vat = function
  | Some url -> Capnp_rpc_unix.Vat.import vat url
  | None -> (
      match Sys.getenv_opt "HOME" with
      | None -> failwithf "$HOME not set! Can't get default cap file location."
      | Some home ->
          let path = Filename.concat home ".ocaml-ci.cap" in
          if Sys.file_exists path then Capnp_rpc_unix.Cap_file.load vat path
          else failwithf "Default cap file %S not found!" path )

let log_file = Filename.temp_file "citty" ".log"

let () = at_exit (fun () -> Sys.remove log_file)

let open_in_editor var log_lines = function
  | `Activate ->
      let oc = open_out_bin log_file in
      Lwd_table.iter (output_string oc) log_lines;
      close_out_noerr oc;
      let safe_name = Filename.quote log_file in
      let candidates =
        match Sys.getenv_opt "VISUAL" with
        | Some x -> [ x ]
        | None -> (
            match Sys.getenv_opt "EDITOR" with
            | Some x -> [ x ]
            | None -> (
                match Sys.getenv_opt "PAGER" with
                | Some x -> [ x ]
                | None -> [] ) )
      in
      let candidates = candidates @ [ "xdg-open"; "open" ] in
      ignore
        (List.exists
           (fun bin ->
             Sys.command (Filename.quote bin ^ " " ^ safe_name) <> 127)
           candidates);

      (* Change var to refresh view *)
      Lwd.set var (Lwd.peek var)
  | _ -> ()

let show_status dispatch var =
  let rec aux job =
    let status = Current_rpc.Job.status job in
    let start_log = Current_rpc.Job.log ~start:0L job in
    status
    |> Lwt_result.map
       @@ fun { Current_rpc.Job.id; description; can_cancel; can_rebuild } ->
       let text =
         Format.asprintf "@[<v2>Job %S:@,Description: @[%a@]@]@." id Fmt.lines
           description
       in
       let buttons = Lwd.var Ui.empty in
       [
         ( if can_rebuild then
           Some
             ( W.button Notty.A.(bg red) "[Rebuild]" @@ fun () ->
               Lwd.set buttons Ui.empty;
               ignore (aux (Current_rpc.Job.rebuild job)) )
         else None );
         ( if can_cancel then
           Some
             ( W.button Notty.A.(bg blue) "[Cancel]" @@ fun () ->
               Lwd.set buttons Ui.empty;
               Lwt.async (fun () ->
                   Current_rpc.Job.cancel job >>= fun _ ->
                   ignore (aux job);
                   Lwt.return_unit) )
         else None );
       ]
       |> filter_map (fun x -> x)
       |> interleave (Ui.atom (Notty.I.void 1 0))
       |> Lwd_utils.pure_pack Ui.pack_x
       |> Lwd.set buttons;
       let log_lines = Lwd_table.make () in
       let show_log table job =
         let add str = if str <> "" then Lwd_table.append' table str in
         let rec aux = function
           | Error _ as e ->
               add "ERROR";
               Lwt.return e
           | Ok ("", _) -> Lwt.return_ok ()
           | Ok (data, next) ->
               add data;
               Current_rpc.Job.log ~start:next job >>= aux
         in
         start_log >>= aux
       in
       Lwt.async (fun () ->
           show_log log_lines job >|= fun _ ->
           Lwt.wakeup dispatch (open_in_editor var log_lines));
       let description =
         Lwd.pure (text |> NW.string |> Ui.resize ~w:0 ~sw:1)
       in
       let buttons =
         Lwd.map2
           (fun x y -> Ui.resize ~w:0 ~sw:1 (Ui.join_x x y))
           (Lwd.get buttons)
           (Lwd.pure (Ui.resize Ui.empty ~h:1 ~sw:1 ~bg:Notty.A.(bg (gray 1))))
       in
       let text_view =
         (* Setup scrolling *)
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
           else Lwd.set scroll_state st
         in
         let text_body =
           let width = Lwd.var 0 in
           Lwd.bind (Lwd.get width) (W.word_wrap_string_table log_lines)
           |> NW.vscroll_area ~state:(Lwd.get scroll_state) ~change:set_scroll
           |> Lwd.map (fun ui ->
                  ui
                  |> (* Fill available space *)
                     Ui.resize ~w:0 ~sw:1 ~h:0 ~sh:1
                  |> (* Adjust wrapping based on actual width *)
                     Ui.size_sensor (fun w _ -> update_if_changed width w)
                  |> (* Scroll when dragging *)
                     Ui.mouse_area (fun ~x:_ ~y:y0 ->
                       function
                       | `Left ->
                           let st = Lwd.peek scroll_state in
                           `Grab
                             ( (fun ~x:_ ~y:y1 ->
                                 set_scroll `Change
                                   { st with position = st.position + y0 - y1 }),
                               fun ~x:_ ~y:_ -> () )
                       | _ -> `Unhandled))
         in
         let scroll_bar =
           Lwd.get scroll_state
           |> Lwd.map (fun x ->
                  x
                  |> W.vertical_scrollbar ~set_scroll:(set_scroll `Change)
                  |> Ui.resize ~w:1 ~sw:0 ~h:0 ~sh:1)
         in
         Lwd_utils.pack Ui.pack_x [ text_body; scroll_bar ]
       in
       Lwd.set var
         (Lwd_utils.pack Ui.pack_y [ description; buttons; text_view ])
  in
  aux

let show_status dispatch_var job =
  let result = Lwd.var W.empty in
  show_status dispatch_var result job
  |> Lwt_result.map (fun () -> Lwd.get result |> Lwd.join)

let show_jobs commit pane =
  Client.Commit.jobs commit >|= function
  | Ok items ->
      let select Client.{ variant; _ } =
        let pane = Pane.open_subview pane in
        let dispatch, dispatch_var = Lwt.wait () in
        let open_editor_asap = ref false in
        let footer_spinner = Lwd.var (Lwd.pure Ui.empty) in
        let dispatch_fun action =
          match Lwt.state dispatch with
          | Return fn -> fn action
          | Fail _ -> ()
          | Sleep ->
              if (not !open_editor_asap) && action = `Activate then (
                open_editor_asap := true;
                let display_spinner frame =
                  NW.string (frame ^ " Opening editor as soon as possible")
                in
                Lwd.set footer_spinner (Lwd.map display_spinner spinner);
                Lwt.async (fun () ->
                    Lwt.map
                      (fun fn ->
                        Lwd.set footer_spinner (Lwd.pure Ui.empty);
                        fn `Activate)
                      dispatch) )
        in
        let footer_ui = Lwd.get footer_spinner |> Lwd.join in
        Pane.set pane (Some dispatch_fun)
          (Lwd.map' footer_ui (fun ui ->
               Ui.join_y (NW.string "...")
                 (Ui.resize ~sh:1
                    ~fill:(Gravity.make ~h:`Negative ~v:`Positive)
                    ui)));
        Lwt.async @@ fun () ->
        Client.Commit.job_of_variant commit variant |> show_status dispatch_var
        >|= function
        | Ok ui ->
            Pane.set pane (Some dispatch_fun)
              (Lwd.map2' ui footer_ui Ui.join_y)
        | Error (`Capnp e) ->
            Pane.set pane None (Lwd.pure (NW.fmt "%a" Capnp_rpc.Error.pp e))
        | Error `No_job -> Pane.set pane None (Lwd.pure (NW.string "No jobs"))
      and render Client.{ variant; outcome } highlight =
        Ui.hcat
          [
            render_state highlight outcome;
            Ui.atom (Notty.I.void 1 1);
            render_list_item highlight variant;
          ]
      in
      let ui, dispatch = W.list_box ~items ~render ~select in
      Pane.set pane (Some dispatch) ui
  | Error (`Capnp e) ->
      Pane.close_subview pane;
      Pane.set pane None (Lwd.pure (NW.fmt "%a" Capnp_rpc.Error.pp e))

let show_repo repo pane =
  Client.Repo.refs repo >>= function
  | Ok refs ->
      let select (_, hash) =
        let pane = Pane.open_subview pane in
        Pane.set pane None (Lwd.pure (NW.string "..."));
        Lwt.async (fun () ->
            let commit = Client.Repo.commit_of_hash repo hash in
            show_jobs commit pane)
      in
      let render (gref, hash) highlight =
        render_list_item highlight
          (Printf.sprintf "%10s   #%s" (W.fit_string gref 24)
             (String.sub hash 0 6))
      in
      let items = refs |> Client.Ref_map.to_seq |> List.of_seq in
      let ui, dispatch = W.list_box ~items ~render ~select in
      Pane.set pane (Some dispatch) ui;
      Lwt.return_unit
  | Error (`Capnp e) ->
      Pane.close_subview pane;
      Pane.set pane None (Lwd.pure (NW.fmt "%a" Capnp_rpc.Error.pp e));
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
                render_list_item hl (Printf.sprintf "%s/%s" org repo)
            | Error (`Capnp e) ->
                NW.fmt ~attr:Notty.A.(fg red) "%a" Capnp_rpc.Error.pp e
          in
          let select = function
            | Ok (_repo, handle) ->
                let pane = Pane.open_subview pane in
                Lwt.async (fun () ->
                    (* C.Capability.with_ref handle (fun handle -> *)
                    show_repo handle pane
                    (* ) *))
            | Error _ -> Pane.close_subview pane
          in
          Lwt_list.map_s
            (fun x -> x)
            (List.map
               (fun org ->
                 let handle = Client.CI.org ci org in
                 Lwt.map
                   (function
                     | Error e -> [ Error e ]
                     | Ok repos ->
                         let handle_of repo =
                           Ok ((org, repo), Client.Org.repo handle repo)
                         in
                         List.map handle_of repos)
                   (Client.Org.repos handle))
               orgs)
          >>= fun items ->
          let items = List.flatten items in
          let ui, dispatch = W.list_box ~items ~render ~select in
          Pane.set pane (Some dispatch) ui;
          Lwt.return_ok () )

let main () =
  let pane = Pane.make () in
  let dispatch pos action =
    match Pane.current_view pane pos with
    | None -> `Unhandled
    | Some view -> (
        match Pane.get view with
        | None -> `Unhandled
        | Some dispatch ->
            dispatch action;
            `Handled )
  in
  Lwd.set body
    ( Pane.render pane
    |> Lwd.map
         (Ui.focus_area (Time.next ())
            {
              action =
                (fun _ -> function
                  | (`Arrow `Up | `ASCII 'k'), [] ->
                      dispatch `Middle `Select_prev
                  | (`Arrow `Down | `ASCII 'j'), [] ->
                      dispatch `Middle `Select_next
                  | (`Arrow `Left | `ASCII 'h'), [] -> dispatch `Left `Activate
                  | (`Arrow `Right | `ASCII 'l'), [] ->
                      dispatch `Right `Activate
                  | (`Escape | `ASCII 'q'), [] -> exit 0
                  | _ -> `Unhandled);
              status = (fun _ _ -> ());
            }) );
  Lwt_main.run
    (show_repos (Pane.open_root pane) >>= function
     | Ok () -> Nottui_lwt.run ui
     | Error (`Capnp err) ->
         Format.eprintf "%a" Capnp_rpc.Error.pp err;
         Lwt.return_unit
     | Error (`Msg msg) ->
         Format.eprintf "Error: %S" msg;
         Lwt.return_unit)

let () = main ()
