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

type cached_dox_unit = {
  kind : string;
  source_digest : string;
}

(* The browser worker lives for the lifetime of the page, so its pseudo-filesystem
   can be the incremental build store.  Entries after the first changed unit are
   invalidated conservatively: OCaml units execute front-to-back, and this keeps
   both interfaces and initialization order correct without maintaining a second
   dependency graph in the browser bridge. *)
let dox_cache_directory =
  lazy (Filename.temp_dir ~temp_dir:"." "dox_browser_cache_" "")

let dox_unit_cache : (string, cached_dox_unit) Hashtbl.t = Hashtbl.create 32
let dox_compilation_order = ref []

let dox_unit_of_json json =
  let open Yojson.Safe.Util in
  { kind = json |> member "kind" |> to_string_option |> Option.value ~default:"document";
    filename = json |> member "filename" |> to_string;
    source = json |> member "source" |> to_string
  }

let remove_if_exists path =
  try Sys.remove path with Sys_error _ -> ()

let source_digest source = Digest.string source |> Digest.to_hex

let unit_paths directory ({ filename; _ } : dox_unit) =
  let source_path = Filename.concat directory filename in
  let output_prefix = Browser_switch_common.file_prefix source_path in
  source_path, output_prefix

let remove_unit_artifacts directory unit =
  let source_path, output_prefix = unit_paths directory unit in
  List.iter remove_if_exists
    [ source_path ^ ".ppx.ast";
      output_prefix ^ ".cmo";
      output_prefix ^ ".cmi";
      output_prefix ^ ".cmt";
      output_prefix ^ ".cms";
      output_prefix ^ ".annot";
      output_prefix ^ ".dox-constructs"
    ]

let remove_cached_unit directory filename cached =
  let unit = { kind = cached.kind; filename; source = "" } in
  let source_path, _ = unit_paths directory unit in
  remove_if_exists source_path;
  remove_unit_artifacts directory unit

let cached_unit_is_usable directory (({ kind; filename; source } : dox_unit) as unit) =
  let _, output_prefix = unit_paths directory unit in
  match Hashtbl.find_opt dox_unit_cache filename with
  | Some cached ->
    String.equal cached.kind kind
    && String.equal cached.source_digest (source_digest source)
    && Sys.file_exists (output_prefix ^ ".cmo")
    && Sys.file_exists (output_prefix ^ ".cmi")
    && (not (Filename.check_suffix filename ".ml.md")
       || Sys.file_exists (output_prefix ^ ".dox-constructs"))
  | None -> false

let cache_unit ({ kind; filename; source } : dox_unit) =
  Hashtbl.replace dox_unit_cache filename
    { kind; source_digest = source_digest source }

let run_dox_project ~request =
  let started_at = Sys.time () in
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
  let directory = Lazy.force dox_cache_directory in
  let event_path = Filename.concat directory "events" in
  let compiled = ref [] in
  let manifest_paths = ref [] in
  let cleanup () =
    remove_if_exists event_path
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
        let compile_unit (({ kind; filename; _ } : dox_unit) as unit) =
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
          remove_unit_artifacts directory unit;
          let cmo_path =
            Fun.protect
              (fun () ->
                try
                  Browser_switch_common.compile_source_file_direct
                    ~source_path ~output_prefix
                with
                | Browser_switch_common.Missing_cmi _ as exn -> raise exn
                | exn ->
                  failwith
                    (Printf.sprintf "compiling %s: %s" filename
                       (Printexc.to_string exn)))
              ~finally:(fun () -> compiling_alias := false)
          in
          cache_unit unit;
          cmo_path
        in
        let units_by_filename = Hashtbl.create (List.length units) in
        List.iter
          (fun ({ filename; _ } as unit) ->
            Hashtbl.replace units_by_filename filename unit)
          units;
        Hashtbl.to_seq dox_unit_cache
        |> List.of_seq
        |> List.iter (fun (filename, cached) ->
          if not (Hashtbl.mem units_by_filename filename)
          then (
            Hashtbl.remove dox_unit_cache filename;
            remove_cached_unit directory filename cached));
        let rec reusable_prefix reused = function
          | filename :: remaining ->
            (match Hashtbl.find_opt units_by_filename filename with
            | Some unit when cached_unit_is_usable directory unit ->
              reusable_prefix (unit :: reused) remaining
            | None | Some _ -> List.rev reused)
          | [] -> List.rev reused
        in
        let reused = reusable_prefix [] !dox_compilation_order in
        let reused_filenames = Hashtbl.create (List.length reused) in
        List.iter
          (fun ({ filename; _ } : dox_unit) ->
            Hashtbl.replace reused_filenames filename ())
          reused;
        let pending =
          List.filter
            (fun ({ filename; _ } : dox_unit) ->
              not (Hashtbl.mem reused_filenames filename))
            units
        in
        (* Once one unit changes, no later cached interface or implementation is
           trusted.  Remove it before compiling so a failed compile cannot
           accidentally resolve against an artifact from the previous run. *)
        List.iter
          (fun ({ filename; _ } as unit) ->
            Hashtbl.remove dox_unit_cache filename;
            remove_unit_artifacts directory unit)
          pending;
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
        let compilation_order = reused @ discover_order [] pending in
        let compiled_at = Sys.time () in
        dox_compilation_order :=
          List.map (fun ({ filename; _ } : dox_unit) -> filename) compilation_order;
        manifest_paths :=
          compilation_order
          |> List.filter_map (fun ({ filename; _ } as unit) ->
            if Filename.check_suffix filename ".ml.md"
            then
              let _, output_prefix = unit_paths directory unit in
              Some (output_prefix ^ ".dox-constructs")
            else None)
          |> List.rev;
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
        let loaded_at = Sys.time () in
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
        let trace = Dox_browser_runtime.read_trace () in
        let collected_at = Sys.time () in
        let milliseconds from_ until_ =
          `Int (Float.round ((until_ -. from_) *. 1000.) |> int_of_float)
        in
        `Assoc
          [ "kind", `String "ok";
            "events", `String events;
            "trace", `String trace;
            "manifests", `List (List.map (fun text -> `String text) manifests);
            ( "cache",
              `Assoc
                [ "compiledUnits", `Int (List.length pending);
                  "reusedUnits", `Int (List.length reused) ] );
            ( "timings",
              `Assoc
                [ "compileMs", milliseconds started_at compiled_at;
                  "loadMs", milliseconds compiled_at loaded_at;
                  "collectMs", milliseconds loaded_at collected_at ] )
          ]
        |> Yojson.Safe.to_string))
