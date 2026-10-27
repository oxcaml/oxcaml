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
    String.concat "__" (String.split_on_char '/' name) in (* dune limitation *)
  Filename.concat ref_dir name

let run_task_item ~log : Actions.task_item -> Result.t = function
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
