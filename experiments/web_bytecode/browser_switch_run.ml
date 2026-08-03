let ensure_browser_toplevel_initialized =
  let initialized = ref false in
  fun () ->
    if not !initialized
    then (
      Js_of_ocaml_toplevel.JsooTop.initialize ();
      initialized := true)

let run_string ~browser ~filename ~source =
  let environment =
    if browser then Browser_switch_common.Browser
    else Browser_switch_common.Native
  in
  let suffix = "_" ^ Filename.basename filename in
  let source_path = Filename.temp_file "browser_switch_run_" suffix in
  let output_prefix = source_path ^ ".build" in
  Browser_switch_common.write_source_file ~source_path ~source;
  Browser_switch_common.with_missing_cmi_detection environment (fun () ->
    Browser_switch_common.capture_diagnostics (fun ppf ->
        Browser_switch_common.prepare_compiler environment ~filename;
        Fun.protect
          (fun () ->
            let cmo_path =
              Browser_switch_common.compile_source_file
                ~filename
                ~source_path
                ~output_prefix
            in
            if browser
            then (
              ensure_browser_toplevel_initialized ();
              Toploop.initialize_toplevel_env ();
              Topdirs.dir_directory Browser_switch_common.browser_cmis_dir;
              List.iter
                Topdirs.dir_directory
                Browser_switch_package_manifest.browser_package_include_dirs)
            else Toploop.initialize_toplevel_env ();
            let baseline_symtable = Symtable.current_state () in
            Fun.protect
              (fun () ->
                Toploop.override_sys_argv [| filename |];
                Toploop.input_name := filename;
                Sys.interactive := false;
                Toploop.load_file ppf cmo_path)
              ~finally:(fun () -> Symtable.restore_state baseline_symtable))
          ~finally:(fun () ->
            Browser_switch_common.cleanup_build_artifacts ~source_path ~output_prefix)))
  |> Browser_switch_common.replace_all ~pattern:source_path ~with_:filename

let initialize_toplevel_for_environment environment =
  match environment with
  | Browser_switch_common.Native -> Toploop.initialize_toplevel_env ()
  | Browser_switch_common.Browser ->
    ensure_browser_toplevel_initialized ();
    Toploop.initialize_toplevel_env ();
    Topdirs.dir_directory Browser_switch_common.browser_cmis_dir;
    List.iter
      Topdirs.dir_directory
      Browser_switch_package_manifest.browser_package_include_dirs

let capture_toplevel_output f =
  let buffer, ppf = Browser_switch_common.make_formatter_buffer () in
  (try ignore (f ppf) with
   | Browser_switch_common.Missing_cmi _ as exn -> raise exn
   | exn -> Location.report_exception ppf exn);
  Browser_switch_common.flush_formatter ppf;
  Buffer.contents buffer

let preprocess_toplevel_phrase ppf phrase =
  let phrase = Toploop.preprocess_phrase ppf phrase in
  match phrase with
  | Parsetree.Ptop_def structure ->
    Parsetree.Ptop_def (Browser_switch_common.expand_structure_with_ppx structure)
  | phrase -> phrase

let utop_string ~browser ~filename ~source =
  let environment =
    if browser then Browser_switch_common.Browser
    else Browser_switch_common.Native
  in
  Browser_switch_common.with_missing_cmi_detection environment (fun () ->
    capture_toplevel_output (fun ppf ->
      Browser_switch_common.prepare_compiler environment ~filename;
      initialize_toplevel_for_environment environment;
      Toploop.override_sys_argv [| filename |];
      Toploop.input_name := filename;
      Sys.interactive := false;
      let lexbuf = Browser_switch_common.prepare_lexbuf ~filename source in
      let phrases = !Toploop.parse_use_file lexbuf in
      List.for_all
        (fun phrase ->
          Warnings.reset_fatal ();
          let phrase = preprocess_toplevel_phrase ppf phrase in
          Env.reset_cache_toplevel ();
          Toploop.execute_phrase true ppf phrase)
        phrases
      |> ignore))

type dox_unit = {
  kind : string;
  filename : string;
  source : string;
}

let dox_unit_of_json json =
  let open Yojson.Safe.Util in
  { kind = json |> member "kind" |> to_string_option |> Option.value ~default:"document";
    filename = json |> member "filename" |> to_string;
    source = json |> member "source" |> to_string
  }

let remove_if_exists path =
  try Sys.remove path with Sys_error _ -> ()

let run_dox_project ~request =
  let open Yojson.Safe.Util in
  let request = Yojson.Safe.from_string request in
  let units = request |> member "units" |> to_list |> List.map dox_unit_of_json in
  let project_cmis =
    List.map
      (fun ({ filename; _ } : dox_unit) ->
        Filename.basename (Browser_switch_common.file_prefix filename ^ ".cmi")
        |> String.lowercase_ascii)
      units
  in
  let is_project_cmi filename =
    List.mem
      (Filename.basename filename |> String.lowercase_ascii)
      project_cmis
  in
  let compiling_alias = ref false in
  let directory = Filename.temp_dir ~temp_dir:"." "dox_browser_" "" in
  let event_path = Filename.concat directory "events" in
  let compiled = ref [] in
  let manifest_paths = ref [] in
  let cleanup () =
    List.iter
      (fun ({ filename; _ } : dox_unit) ->
        let source_path = Filename.concat directory filename in
        let output_prefix = Browser_switch_common.file_prefix source_path in
        List.iter remove_if_exists
          [ source_path;
            source_path ^ ".ppx.ast";
            output_prefix ^ ".cmo";
            output_prefix ^ ".cmi";
            output_prefix ^ ".cmt";
            output_prefix ^ ".cms";
            output_prefix ^ ".annot";
            output_prefix ^ ".dox-constructs"
          ])
      units;
    remove_if_exists event_path;
    (try Sys.rmdir directory with Sys_error _ -> ())
  in
  Fun.protect ~finally:cleanup (fun () ->
    Browser_switch_common.with_missing_cmi_detection
      ~allow_missing:(fun filename -> !compiling_alias && is_project_cmi filename)
      Browser_switch_common.Browser
      (fun () ->
        List.iter
          (fun ({ filename; source } : dox_unit) ->
            Browser_switch_common.write_source_file
              ~source_path:(Filename.concat directory filename) ~source)
          units;
        Dox_browser_runtime.set_env "DOX_TRACE_ALL" "1";
        let compile_unit ({ kind; filename; _ } : dox_unit) =
          let source_path = Filename.concat directory filename in
          let output_prefix = Browser_switch_common.file_prefix source_path in
          let manifest_path = output_prefix ^ ".dox-constructs" in
          if Filename.check_suffix filename ".ml.md"
          then Dox_browser_runtime.set_env "DOX_EXECUTION_MANIFEST" manifest_path
          else Dox_browser_runtime.set_env "DOX_EXECUTION_MANIFEST" "";
          Browser_switch_common.prepare_compiler Browser_switch_common.Browser
            ~project_dir:directory ~filename;
          Clflags.no_alias_deps := true;
          compiling_alias := String.equal kind "alias";
          let cmo_path =
            Fun.protect
              (fun () ->
                try
                  Browser_switch_common.compile_source_file_direct
                    ~source_path ~output_prefix
                with exn ->
                  failwith
                    (Printf.sprintf "compiling %s: %s" filename
                       (Printexc.to_string exn)))
              ~finally:(fun () -> compiling_alias := false)
          in
          if Filename.check_suffix filename ".ml.md"
          then manifest_paths := manifest_path :: !manifest_paths;
          cmo_path
        in
        let rec discover_order ordered pending =
          match pending with
          | [] -> ordered
          | _ ->
            let succeeded, blocked =
              List.fold_left
                (fun (succeeded, blocked) unit ->
                  try (unit, compile_unit unit) :: succeeded, blocked with
                  | Browser_switch_common.Missing_cmi filename as exn ->
                    if is_project_cmi filename
                    then succeeded, (unit, filename) :: blocked
                    else raise exn)
                ([], []) pending
            in
            if succeeded = []
            then
              let ({ filename; _ } : dox_unit) = List.hd pending in
              let missing =
                blocked
                |> List.map snd
                |> List.sort_uniq String.compare
                |> String.concat ", "
              in
              failwith
                (Printf.sprintf
                   "Could not resolve project dependencies for %s (missing %s)"
                   filename missing)
            else
              let newly_ordered = List.rev_map fst succeeded in
              discover_order (ordered @ newly_ordered) (List.rev_map fst blocked)
        in
        let compilation_order = discover_order [] units in
        compiled := [];
        List.iter
          (fun ({ kind; filename; _ } : dox_unit) ->
            if not (String.equal kind "alias")
            then (
              let source_path = Filename.concat directory filename in
              let output_prefix = Browser_switch_common.file_prefix source_path in
              compiled := (output_prefix ^ ".cmo") :: !compiled))
          compilation_order;
        ensure_browser_toplevel_initialized ();
        Toploop.initialize_toplevel_env ();
        Topdirs.dir_directory Browser_switch_common.browser_cmis_dir;
        Topdirs.dir_directory directory;
        List.iter Topdirs.dir_directory
          Browser_switch_package_manifest.browser_package_include_dirs;
        Dox_browser_runtime.set_env "DOCLANG_EVENT_PATH" event_path;
        Dox_browser_runtime.reset_trace ();
        let ppf = Format.err_formatter in
        List.rev !compiled
        |> List.iter (fun cmo_path ->
          try
            if not (Toploop.load_file ppf cmo_path) then
              failwith ("Could not load " ^ Filename.basename cmo_path)
          with exn ->
            failwith
              (Printf.sprintf "loading %s: %s" (Filename.basename cmo_path)
                 (Printexc.to_string exn)));
        let events =
          if Sys.file_exists event_path then Browser_switch_common.read_file event_path
          else ""
        in
        let manifests =
          List.rev !manifest_paths
          |> List.filter_map (fun path ->
            if Sys.file_exists path then Some (Browser_switch_common.read_file path)
            else None)
        in
        `Assoc
          [ "kind", `String "ok";
            "events", `String events;
            "trace", `String (Dox_browser_runtime.read_trace ());
            "manifests", `List (List.map (fun text -> `String text) manifests)
          ]
        |> Yojson.Safe.to_string))
