open Lwt
open Ocaml_ci_api
open Nottui
open Utils
module C = Capnp_rpc_lwt
module NW = Nottui_widgets
module W = Widgets
module Pane = W.Pane

let clampi mn mx a : int = if a > mx then mx else if a < mn then mn else a
let header = Lwd.var W.empty
let body = Lwd.var W.empty
let footer = Lwd.var W.empty

let spinner =
  let rec frames =
    "⠋" :: "⠙" :: "⠹" :: "⠸" :: "⠼" :: "⠴" :: "⠦" :: "⠧" :: "⠇" :: "⠏" :: frames
  in
  Lwd.prim
    ~acquire:(fun _self ->
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
    ~release:(fun _self (running, _) -> running := false)
  |> Lwd.get_prim
  |> Lwd.map ~f:(fun (_running, var) -> var)
  |> Lwd.join
  |> Lwd.map ~f:List.hd

let ui =
  let place_ui_var v = Lwd.(v |> get |> join |> map ~f:(Ui.resize ~w:0)) in
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
          else failwithf "Default cap file %S not found!" path)

let log_file = Filename.temp_file "citty" ".log"
let () = at_exit (fun () -> Sys.remove log_file)

let open_in_editor refresh log_lines = function
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
                match Sys.getenv_opt "PAGER" with Some x -> [ x ] | None -> []))
      in
      let candidates = candidates @ [ "xdg-open"; "open" ] in
      ignore
        (List.exists
           (fun bin ->
             Sys.command (Filename.quote bin ^ " " ^ safe_name) <> 127)
           candidates);
      refresh ()
  | _ -> ()

let rec show_job pane job =
  let dispatch, dispatch_var = Lwt.wait () in
  let open_editor_asap = ref false in
  let footer, set_footer =
    let display msg = Lwd.map ~f:(fun img -> NW.string (img ^ msg)) spinner in
    let var = Lwd.var (display " Receiving log") in
    let footer_content = function
      | `Opening -> display " Opening editor as soon as possible"
      | `Done -> W.empty
      | `Refresh -> Lwd.peek var
    in
    (Lwd.join (Lwd.get var), fun status -> Lwd.set var (footer_content status))
  in
  Lwt.ignore_result (Lwt.map (fun _ -> set_footer `Done) dispatch);
  let dispatch_fun action =
    match Lwt.state dispatch with
    | Return fn -> fn action
    | Fail _ -> ()
    | Sleep ->
        if (not !open_editor_asap) && action = `Activate then (
          open_editor_asap := true;
          set_footer `Opening;
          Lwt.ignore_result (Lwt.map (fun fn -> fn `Activate) dispatch))
  in
  Pane.set pane (Some dispatch_fun)
    (Lwd.map footer
       ~f:(Ui.resize ~sh:1 ~pad:(Gravity.make ~h:`Negative ~v:`Positive)));
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
       (if can_rebuild then
        Some
          ( W.button Notty.A.(bg red) "[Rebuild]" @@ fun () ->
            Lwd.set buttons Ui.empty;
            ignore (show_job pane (Current_rpc.Job.rebuild job)) )
       else None);
       (if can_cancel then
        Some
          ( W.button Notty.A.(bg blue) "[Cancel]" @@ fun () ->
            Lwd.set buttons Ui.empty;
            Lwt.async (fun () ->
                Current_rpc.Job.cancel job >>= fun _ ->
                ignore (show_job pane job);
                Lwt.return_unit) )
       else None);
     ]
     |> filter_map (fun x -> x)
     |> interleave (Ui.atom (Notty.I.void 1 0))
     |> Lwd_utils.reduce Ui.pack_x
     |> Lwd.set buttons;
     let log_lines = Lwd_table.make () in
     Lwt.async (fun () ->
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
         show_log log_lines job >|= fun _ ->
         let refresh () = set_footer `Refresh in
         Lwt.wakeup dispatch_var (open_in_editor refresh log_lines));
     let description = Lwd.pure (text |> NW.string |> Ui.resize ~w:0 ~sw:1) in
     let buttons =
       Lwd.map2
         ~f:(fun x y -> Ui.resize ~w:0 ~sw:1 (Ui.join_x x y))
         (Lwd.get buttons)
         (Lwd.pure (Ui.resize Ui.empty ~h:1 ~sw:1 ~bg:Notty.A.(bg (gray 1))))
     in
     let text_view =
       (* Setup scrolling *)
       let scroll_state = Lwd.var NW.default_scroll_state in
       let set_scroll reason st =
         let off_screen = reason = `Content && st.NW.position > st.NW.bound in
         let scroll_on_output =
           reason = `Content
           &&
           let st' = Lwd.peek scroll_state in
           st'.NW.position = st'.NW.bound
           && st.NW.position = st'.NW.position
           && st.NW.position < st.NW.bound
         in
         if scroll_on_output || off_screen then
           Lwd.set scroll_state { st with position = st.NW.bound }
         else Lwd.set scroll_state st
       in
       let text_body =
         W.dynamic_width ~w:0 ~sw:1 ~h:0 ~sh:1 (fun width ->
             Lwd.bind width ~f:(W.word_wrap_string_table log_lines)
             |> NW.vscroll_area ~state:(Lwd.get scroll_state) ~change:set_scroll
             |> (* Scroll when dragging *)
             Lwd.map
               ~f:
                 (Ui.mouse_area (fun ~x:_ ~y:y0 -> function
                    | `Left ->
                        let st = Lwd.peek scroll_state in
                        `Grab
                          ( (fun ~x:_ ~y:y1 ->
                              let position = st.position + y0 - y1 in
                              let position = clampi 0 st.bound position in
                              set_scroll `Change { st with position }),
                            fun ~x:_ ~y:_ -> () )
                    | _ -> `Unhandled)))
       in
       let scroll_bar =
         Lwd.get scroll_state
         |> Lwd.map ~f:(fun x ->
                x
                |> W.vertical_scrollbar ~set_scroll:(set_scroll `Change)
                |> Ui.resize ~w:1 ~sw:0 ~h:0 ~sh:1)
       in
       Lwd_utils.pack Ui.pack_x [ text_body; scroll_bar ]
     in
     Lwd_utils.pack Ui.pack_y [ description; buttons; text_view; footer ]
     |> Pane.set pane (Some dispatch_fun)

let show_jobs commit pane =
  Client.Commit.jobs commit >|= function
  | Ok items ->
      let select Client.{ variant; _ } =
        let pane = Pane.open_subview pane in
        Lwt.ignore_result
          (show_job pane (Client.Commit.job_of_variant commit variant)
           >|= function
           | Ok () -> ()
           | Error (`Capnp e) ->
               Pane.set pane None (Lwd.pure (NW.fmt "%a" Capnp_rpc.Error.pp e))
           | Error `No_job ->
               Pane.set pane None (Lwd.pure (NW.string "No jobs")))
      and render Client.{ variant; outcome; _ } highlight =
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
      let select (_, Client.Repo.{ hash; _ }) =
        let pane = Pane.open_subview pane in
        Pane.set pane None (Lwd.pure (NW.string "..."));
        Lwt.async (fun () ->
            let commit = Client.Repo.commit_of_hash repo hash in
            show_jobs commit pane)
      in
      let render (gref, Client.Repo.{ hash; _ }) highlight =
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

let show_repos pane ~ci_uri =
  let vat = Capnp_rpc_unix.client_only_vat () in
  match import_ci_ref ~vat ci_uri with
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
                         let handle_of { Client.Org.name; _ } =
                           Ok ((org, name), Client.Org.repo handle name)
                         in
                         List.map handle_of repos)
                   (Client.Org.repos handle))
               orgs)
          >>= fun items ->
          let items = List.flatten items in
          let ui, dispatch = W.list_box ~items ~render ~select in
          Pane.set pane (Some dispatch) ui;
          Lwt.return_ok ())

let main () ci_uri =
  let pane = Pane.make () in
  let dispatch pos action =
    match Pane.current_view pane pos with
    | None -> `Unhandled
    | Some view -> (
        match Pane.get view with
        | None -> `Unhandled
        | Some dispatch ->
            dispatch action;
            `Handled)
  in
  let focus_handle = Focus.make () in
  Focus.request focus_handle;
  Lwd.set body
    (Pane.render pane
    |> Lwd.map2
         ~f:(fun focus ->
           Ui.keyboard_area ~focus @@ function
           | (`Arrow `Up | `ASCII 'k'), [] -> dispatch `Middle `Select_prev
           | (`Arrow `Down | `ASCII 'j'), [] -> dispatch `Middle `Select_next
           | (`Arrow `Left | `ASCII 'h'), [] -> dispatch `Left `Activate
           | (`Arrow `Right | `ASCII 'l'), [] -> dispatch `Right `Activate
           | (`Escape | `ASCII 'q'), [] -> exit 0
           | _ -> `Unhandled)
         (Focus.status focus_handle));
  Lwt_main.run
    (show_repos (Pane.open_root pane) ~ci_uri >>= function
     | Ok () -> Nottui_lwt.run ui
     | Error (`Capnp err) ->
         Format.eprintf "%a" Capnp_rpc.Error.pp err;
         Lwt.return_unit
     | Error (`Msg msg) ->
         Format.eprintf "Error: %S" msg;
         Lwt.return_unit)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

(* Command line interface *)

open Cmdliner

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let cap =
  Arg.value
  @@ Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info ~doc:"The ocaml-ci.cap file. Defaults to $(i,~/.ocaml-ci.cap)."
       ~docv:"CAP" [ "ci-cap" ]

let main = Term.(const main $ setup_log $ cap)
let () = exit @@ Cmd.(eval (v (info "citty") main))
