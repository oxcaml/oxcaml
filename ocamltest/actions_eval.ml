open Ocamltest_stdlib

let promoted_output (files : Filecompare.files) =
  let ref_dir =
    Filename.concat (Filename.dirname files.reference_filename) "" in
  let name =
    match String.remove_prefix ~prefix:ref_dir files.output_filename with
    | Some s -> s
    | None -> files.output_filename
  in
  let name =
    (* work around dune limitation *)
    String.concat "__"
      (String.split_on_char Filename.dir_sep.[0] name)
  in
  Filename.concat ref_dir name

let run_task_item ~log : Actions.task_item -> Result.t = function
  | Predicate_pass _ ->
     Result.pass
  | Chdir dir  ->
     Builtin_actions.do_chdir dir
  | Check_file_exists { filename; reason } ->
     Builtin_actions.do_check_file_exists ~filename ~reason
  | Check_file_absent { filename } ->
     Builtin_actions.do_check_file_absent ~filename
  | Copy_file { src; dst } ->
     Builtin_actions.do_copy_file log ~src ~dst
  | Setup_build_env { source_dir; build_dir; remove; files; subdirs } ->
     Actions_helpers.do_setup_build_env
       ~source_dir ~build_dir ~remove ~files ~subdirs
  | Run_cmd { settings; message; exits } ->
     Actions_helpers.do_run_cmd ~settings ~message ~exits log
  | Compare_files { kind_of_output; files; tool; reference_exists; promote } ->
     let is_empty file =
       let ch = open_in_bin file in
       Fun.protect ~finally:(fun () -> close_in_noerr ch)
         (fun () -> in_channel_length ch = 0)
     in
     if Options.no_diff then
       (if not (Sys.file_exists files.output_filename) then
         Result.fail_with_reason
           ("Failed to produce output " ^ files.output_filename)
        else if is_empty files.output_filename && not reference_exists then
          (* Special case: empty output and no reference file is OK *)
          Result.pass
        else
          Builtin_actions.do_copy_file log
            ~src:files.output_filename
            ~dst:(promoted_output files))
     else
       Actions_helpers.do_compare_files
         ~kind_of_output ~files ~tool ~promote log

let rec run_tasks ~log tasks =
  match tasks with
  | [] -> Result.pass
  | t :: ts ->
     match run_task_item ~log t with
     | r when ts = [] -> r
     | { status = Pass | Predicate true; _ } -> run_tasks ~log ts
     | { status = Skip | Fail | Predicate false; _ } as r -> r

let run_task ~log = function
  | Actions.Static r -> r
  | Actions.Task tasks -> run_tasks ~log tasks

let run log testenv actions =
  let total = List.length actions in
  let rec run_actions_aux action_number env = function
    | [] -> (Result.pass, env)
    | action::remaining_actions ->
      begin
        Printf.fprintf log "\nRunning action %d/%d (%s)\n%!"
          action_number total (Actions.name action);
        let (task, env') = Actions.run log env action in
        let result =
          match task with
          | Static r -> r
          | Task tasks -> run_tasks ~log tasks
        in
        Printf.fprintf log "Action %d/%d (%s) %s\n%!"
          action_number total (Actions.name action)
          (Result.string_of_result result);
        if Result.is_pass result
        then (
          (* Avoid clobbering [Predicate true] results *)
          match remaining_actions with
          | [] -> result, env'
          | _::_ -> run_actions_aux (action_number+1) env' remaining_actions
        )
        else (result, env')
      end in
  run_actions_aux 1 testenv actions

type dirs =
  { src_dir: string;
    build_dir: string;
    ocaml_dir: string }

let quote ~dirs name =
  let quote_sh s =
    let ok_char = function
      | 'a'..'z' | 'A'..'Z' | '0'..'9'
      | '_' | '-' | '/' | '.' -> true
      | _ -> false
    in
    if String.for_all ok_char s then s
    else Filename.quote s
  in
  [ dirs.src_dir, "$src";
    dirs.build_dir, "$build";
    dirs.ocaml_dir, "$ocaml" ]
  |> List.find_map (fun (prefix,v) ->
    match String.remove_prefix ~prefix name with
    | Some name -> Some (v ^ "/" ^ quote_sh name)
    | None -> None)
  |> Option.value ~default:(quote_sh name)


let print_task_item ~dirs ~env (t : Actions.task_item) : unit =
  let quote t = quote ~dirs t in
  match t with
  | Predicate_pass txt ->
     Printf.printf "# %s\n" txt
  | Chdir dir ->
     Printf.printf "cd %s\n" (quote dir)
  | Check_file_exists {filename; reason=_} ->
     Printf.printf "test -e %s\n" filename
  | Check_file_absent {filename} ->
     Printf.printf "! test -e %s\n" filename
  | Copy_file {src;dst} ->
     Printf.printf "cp %s %s\n" (quote src) (quote dst)
  | Setup_build_env {source_dir;build_dir;remove;files;subdirs} ->
     if remove <> [] then
       Printf.printf "rm -f %s\n" (String.concat " " (List.map quote remove));
     Printf.printf "mkdir -p %s\n" (quote build_dir);
     if files <> [] then
       Printf.printf "cp %s %s\n"
         (String.concat " "
            (List.map (fun f -> quote (Filename.concat source_dir f)) files))
         (quote build_dir);
     if subdirs <> [] then
       subdirs |> List.iter (fun dir ->
         Printf.printf "cp -R %s %s\n"
           (quote (Filename.concat source_dir dir))
           (quote build_dir));
     Printf.printf "cd %s\n" (quote build_dir)
  | Run_cmd cmd ->
     let Run_command.{
       progname = _; argv; envp;
       stdin_filename; stdout_filename; stderr_filename;
       append; timeout=_; log=_ } =
       cmd.settings
     in
     let redirects =
       let op = if append then ">>" else ">" in
       (if stdin_filename <> "" then ["<" ^ quote stdin_filename] else []) @
       (if stdout_filename <> "" then [op ^ quote stdout_filename] else []) @
       (if stderr_filename <> "" then
          if stderr_filename = stdout_filename then ["2>&1"]
          else [op ^ quote stderr_filename] else [])
     in
     let new_env =
       let tbl = Hashtbl.create 10 in
       envp |> Array.iter (fun s ->
         Hashtbl.replace tbl (String.sub s 0 (String.index s '=')) s);
       tbl
     in
     new_env |> Hashtbl.iter (fun k v ->
       if Hashtbl.find_opt env k <> Some v then
         (Hashtbl.replace env k v; Printf.printf "export %s\n" v));
     let cmd =
       String.concat " " ((Array.to_list argv |> List.map quote) @ redirects)
     in
     Printf.printf "%s\n" cmd
  | Compare_files cmp ->
     Printf.printf "diff %s %s\n"
       (quote cmp.files.reference_filename)
       (quote cmp.files.output_filename)

let print_task_commands env = function
  | Actions.Static r ->
     let code =
       match r.status with
       | Pass | Predicate true -> 0
       | Fail | Predicate false -> 1
       | Skip -> 125
     in
     Printf.printf "exit %d # %s\n" code (Result.string_of_result r)
  | Actions.Task its ->
     let dirs =
       let add_slash s = Filename.concat s "" in
       { src_dir =
           Environments.safe_lookup
             Builtin_variables.test_source_directory env
           |> add_slash;
         build_dir =
           Environments.safe_lookup
             Builtin_variables.test_build_directory_prefix env
           |> add_slash;
         ocaml_dir = Ocaml_directories.srcdir_path [] |> add_slash }
     in
     Printf.printf "src=%s\nbuild=%s\nocaml=%s\n"
       dirs.src_dir dirs.build_dir dirs.ocaml_dir;
     let env = Hashtbl.create 10 in
     List.iter (print_task_item ~dirs ~env) its

let ocaml_dependency_paths : (string * string * Sexp.t list option) list=
  [ "ocamltest", "ocamltest/ocamltest.native", None;
    "ocamlc", "main_native.bc",
    Some ([A "%{project_root}/main_native.bc";
           S [A "glob_files";
              A "%{project_root}/stdlib/build/libcamlrun{,d}.a"];
           S [A "glob_files";
              A "%{project_root}/boot_stublibs/*"]]);
    "ocamlopt", "boot_ocamlopt.bc",
    Some ([A "%{project_root}/boot_ocamlopt.bc";
           S [A "glob_files";
              A "%{project_root}/stdlib/build/libasmrun{,d}.a"];
           S [A "glob_files";
              A "%{project_root}/boot_stublibs/*"]]);
    "ocamlc.opt", "main_native.exe",
    Some ([A "%{project_root}/main_native.exe";
           S [A "glob_files";
              A "%{project_root}/stdlib/build/libcamlrun{,d}.a"]]);
    "ocamlopt.opt", "boot_ocamlopt.exe",
    Some ([A "%{project_root}/boot_ocamlopt.exe";
           S [A "glob_files";
              A "%{project_root}/stdlib/build/libasmrun{,d}.a"]]);
    "runtime", "stdlib/build",
    Some ([S [A "glob_files";
              A "%{project_root}/stdlib/build/lib{caml,asm}run{,d}.a"];
           S [A "glob_files";
              A "%{project_root}/stdlib/build/caml/*.{h,tbl}"]]);
    "runtime/ocamlrun", "runtime/ocamlrun", None;
    "runtime/ocamlrund", "runtime/ocamlrund", None;
    "stdlib", "stdlib/build",
    Some ([S [A "glob_files";
              A "%{project_root}/stdlib/build/\
                 {*.{cmi,cmx,cmo},stdlib.{cma,cmxa,a},\
                  std_exit.{cmo,cmx,o},runtime-launch-info}"]]);
    "tools/ocamlfilt", "tools/ocamlfilt.exe", None;
  ]

let ocaml_dependency_table =
  let table = Hashtbl.create 10 in
  ocaml_dependency_paths |> List.iter (fun (k, file, dep) ->
    let file = "%{project_root}/" ^ file in
    let dep = Option.value ~default:[Sexp.A file] dep in
    Hashtbl.replace table k (file, dep));
  table

let generate_dune_dependencies ~dir:relative_dir env its =
  let add_trailing_sep s = Filename.concat s "" in
  let src_dir' =
    Environments.safe_lookup Builtin_variables.test_source_directory env
  in
  let src_dir = add_trailing_sep src_dir' in
  let build_dir' =
    Environments.safe_lookup Builtin_variables.test_build_directory_prefix env
  in
  let build_dir = add_trailing_sep build_dir' in
  let ocaml_dir' = Ocaml_directories.srcdir in
  let ocaml_dir = add_trailing_sep ocaml_dir' in
  let test_file = Environments.safe_lookup Builtin_variables.test_file env in
  let dep_files = ref [test_file] in
  let dep_dirs = ref [] in
  let dep_ocaml = ref [] in
  let cmp = ref [] in
  let check_path path =
    (match String.remove_prefix ~prefix:src_dir path with
     | Some dep when not (String.starts_with ~prefix:build_dir path) ->
        dep_files := dep :: !dep_files
     | _ -> ());
    (match String.remove_prefix ~prefix:ocaml_dir path with
     | Some dep -> dep_ocaml := dep :: !dep_ocaml
     | None -> ())
  in
  let collect_deps : Actions.task_item -> unit = function
    | Predicate_pass _ -> ()
    | Chdir _ -> ()
    | Copy_file {src; dst=_} -> check_path src
    | Check_file_exists p -> check_path p.filename
    | Check_file_absent _ -> ()
    | Setup_build_env { source_dir; files; subdirs;
                        build_dir = _; remove = _ } ->
       if source_dir <> src_dir' then
         failwith (Printf.sprintf
                     "setup_build_env: invalid source_dir %s (expected %s)"
                     source_dir src_dir');
       dep_files := files @ !dep_files;
       dep_dirs := subdirs @ !dep_dirs
    | Run_cmd { settings = { progname; argv; _ }; _ } ->
       List.iter check_path (progname :: Array.to_list argv)
    | Compare_files {files; _} ->
       let maybe_strip_src s =
         Option.value (String.remove_prefix ~prefix:src_dir s) ~default:s in
       let reference =
         Filename.concat relative_dir
           (maybe_strip_src files.reference_filename) in
       let output = maybe_strip_src (promoted_output files) in
       cmp := (reference, output) :: !cmp;
  in
  List.iter collect_deps its;
  let dep_files = List.sort_uniq String.compare !dep_files in
  let dep_dirs = List.sort_uniq String.compare !dep_dirs in
  let dep_ocaml, missing_dep_ocaml =
    List.sort_uniq String.compare !dep_ocaml
    |> List.partition_map (fun cmd ->
      match Hashtbl.find ocaml_dependency_table cmd with
      | (path, deps) -> Left (cmd, path, deps)
      | exception Not_found -> Right cmd)
  in

  match missing_dep_ocaml with
  | [] ->
     let open Sexp in
     let ocamltest_cmd, ocamltest_deps =
       Hashtbl.find ocaml_dependency_table "ocamltest" in
     let deps =
       (dep_files |> List.map (fun s -> A s)) @
       (dep_dirs |> List.map (fun s ->
         S [A "glob_files_rec"; A (Filename.concat s "**")])) @
       ocamltest_deps @
       (dep_ocaml |> List.concat_map (fun (_,_,deps) -> deps))
     in
     let ldpath = "%{project_root}/boot_stublibs" in
     let cmd =
       [A ocamltest_cmd; A "-no-diff"; A "-no-ocamlsrcdir"; A test_file;
        A "-override"; A "ocamlrun_sys=ocamlrun"] @
       (dep_ocaml |> List.concat_map (fun (cmd,path,_) ->
         [A "-override"; A (cmd ^ "=" ^ path)]))
     in
     let action =
       S (A "progn" ::
           S [A "setenv"; A "CAML_LD_LIBRARY_PATH"; A ldpath;
              S (A "run" :: cmd)] ::
          (!cmp |> List.map (fun (r, p) -> S [A "diff?"; A r; A p])))
     in
     Ok (S [A "rule";
            S [A "aliases"; A test_file; A "test"];
            S (A "deps" :: deps);
            S [A "action"; action]])
  | missing ->
     Error missing

let print_task_dune_file ~dir env its =
  let test_file = Environments.safe_lookup Builtin_variables.test_file env in
  match generate_dune_dependencies ~dir env its with
  | Ok dune ->
     Format.printf ";; TEST OK %s@." test_file;
     Format.printf "%a@.@." Sexp.pp dune;
  | Error missing ->
     let missing = String.concat " " missing in
     Format.printf ";; TEST UNAVAILABLE %s@." test_file;
     Format.printf ";; Unavailable dependencies %s@.@." missing
