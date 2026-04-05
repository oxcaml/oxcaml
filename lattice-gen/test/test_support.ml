let contains haystack needle =
  let h_len = String.length haystack in
  let n_len = String.length needle in
  let rec loop i =
    if i + n_len > h_len
    then false
    else if String.sub haystack i n_len = needle
    then true
    else loop (i + 1)
  in
  loop 0

let failf fmt = Printf.ksprintf failwith fmt

let ensure cond fmt =
  Printf.ksprintf (fun message -> if not cond then failwith message) fmt

let read_file path =
  In_channel.with_open_bin path In_channel.input_all

let write_file path contents =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc contents)

let write_solver_runtime path =
  write_file path Solver_runtime_source.contents

let ensure_dir_exists path =
  if Sys.file_exists path
  then (
    if not (Sys.is_directory path) then failf "%s exists and is not a directory" path)
  else Unix.mkdir path 0o755

let remove_dir path =
  ignore (Sys.command ("rm -rf " ^ Filename.quote path))

let remove_dir_if_empty path =
  if Sys.file_exists path && Sys.is_directory path && Array.length (Sys.readdir path) = 0
  then Unix.rmdir path

let keep_temp_dirs () =
  match Sys.getenv_opt "LATTICE_GEN_KEEP_TEMP_DIRS" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let prune_stale_temp_dirs base =
  let now = Unix.time () in
  if Sys.file_exists base && Sys.is_directory base
  then
    Sys.readdir base
    |> Array.to_list
    |> List.iter (fun entry ->
         let path = Filename.concat base entry in
         if Sys.is_directory path
         then
           match Unix.stat path with
           | stats when now -. stats.Unix.st_mtime > 3600.0 -> remove_dir path
           | _ -> ())

let rec copy_tree ~src ~dst =
  if Sys.is_directory src
  then (
    ensure_dir_exists dst;
    Sys.readdir src
    |> Array.to_list
    |> List.iter (fun entry ->
         copy_tree
           ~src:(Filename.concat src entry)
           ~dst:(Filename.concat dst entry)))
  else write_file dst (read_file src)

let command_output ~cwd command =
  let ic =
    Unix.open_process_in
      (Printf.sprintf "cd %s && %s" (Filename.quote cwd) command)
  in
  Fun.protect
    ~finally:(fun () -> ignore (Unix.close_process_in ic))
    (fun () -> input_line ic |> String.trim)

let repo_root () = command_output ~cwd:(Sys.getcwd ()) "git rev-parse --show-toplevel"

let command_lines ~cwd command =
  let ic =
    Unix.open_process_in
      (Printf.sprintf "cd %s && %s" (Filename.quote cwd) command)
  in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  Fun.protect
    ~finally:(fun () -> ignore (Unix.close_process_in ic))
    (fun () -> loop [])

let compiler_root () =
  let roots =
    command_lines ~cwd:(Sys.getcwd ()) "git worktree list --porcelain"
    |> List.filter_map (fun line ->
         if String.length line > 9 && String.sub line 0 9 = "worktree "
         then Some (String.sub line 9 (String.length line - 9))
         else None)
  in
  let is_configured root =
    Sys.file_exists (Filename.concat root "typing/solver_intf.mli")
    && Sys.file_exists (Filename.concat root "dune.runtime_selection")
    && Sys.file_exists (Filename.concat root "duneconf/dirs-to-ignore.inc")
    && Sys.file_exists (Filename.concat root "duneconf/ox-extra.inc")
  in
  match List.find_opt is_configured roots with
  | Some root -> root
  | None -> failf "could not find configured compiler worktree from %s" (repo_root ())

let with_temp_dir prefix f =
  let base = Filename.concat (compiler_root ()) "lattice-gen-test-build" in
  if not (Sys.file_exists base) then Unix.mkdir base 0o755;
  prune_stale_temp_dirs base;
  let dir = Filename.temp_file ~temp_dir:base prefix "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  match f dir with
  | result ->
    remove_dir dir;
    remove_dir_if_empty base;
    result
  | exception exn ->
    if keep_temp_dirs ()
    then prerr_endline ("preserving failing temp dir: " ^ dir)
    else (
      remove_dir dir;
      remove_dir_if_empty base);
    raise exn

let run_command ~cwd command =
  let log_path = Filename.concat cwd "command.log" in
  let full_command =
    Printf.sprintf
      "cd %s && (%s) > %s 2>&1"
      (Filename.quote cwd)
      command
      (Filename.quote log_path)
  in
  match Sys.command full_command with
  | 0 -> ()
  | n ->
    let output =
      try read_file log_path with
      | Sys_error _ -> ""
    in
    failf
      "command failed (%d) in %s:\n$ %s\n%s"
      n
      cwd
      command
      output

let expect_error ~name ~source ~needle =
  try
    ignore (Generate.render_string ~input_name:(name ^ ".lattice") source);
    failf "expected an error for %s" name
  with
  | Error.Error _ as exn ->
    let message = Error.describe_exn exn in
    if not (contains message needle)
    then failf "unexpected error for %s: %s" name message

let resolve_model ~name ~source =
  let ast = Parse.from_string ~input_name:(name ^ ".lattice") source in
  Model.resolve ast

let load_fixture name = read_file name

let expect_generated_ml_excludes ~name ~source needles =
  let outputs =
    Generate.render_string ~root_module:"Generated" ~input_name:name source
  in
  List.iter
    (fun needle ->
      ensure (not (contains outputs.ml needle))
        "generated output for %s unexpectedly contains %S"
        name
        needle)
    needles

let generate_case_files ~dir ~source =
  let input_path = Filename.concat dir "input.lattice" in
  let ml_path = Filename.concat dir "generated.ml" in
  let mli_path = Filename.concat dir "generated.mli" in
  let test_ml_path = Filename.concat dir "generated_test.ml" in
  let dune_path = Filename.concat dir "dune" in
  write_file input_path source;
  Generate.generate_to_files
    ~input_path
    ~ml_path
    ~mli_path
    ~test_ml_path
    ();
  write_solver_runtime (Filename.concat dir "solver_runtime.ml");
  write_file
    dune_path
    {|(executable
 (name generated_test)
 (modules generated solver_runtime generated_test)
 (libraries ocamlcommon))
|};
  ml_path, mli_path, test_ml_path

let compile_and_run_generated_test ~dir =
  let root = compiler_root () in
  let rel_dir = Filename.concat "lattice-gen-test-build" (Filename.basename dir) in
  run_command
    ~cwd:root
    (Printf.sprintf
       "RUNTIME_DIR=runtime4 dune build --display=short %s/generated_test.exe"
       rel_dir);
  run_command
    ~cwd:root
    (Printf.sprintf
       "RUNTIME_DIR=runtime4 dune exec --display=short ./%s/generated_test.exe"
       rel_dir)

let compile_generated_case ~name ~source =
  with_temp_dir ("lattice-gen-" ^ name ^ "-") (fun dir ->
    try
      ignore (generate_case_files ~dir ~source);
      let root = compiler_root () in
      let rel_dir =
        Filename.concat "lattice-gen-test-build" (Filename.basename dir)
      in
      run_command
        ~cwd:root
        (Printf.sprintf
           "RUNTIME_DIR=runtime4 dune build --display=short %s/generated_test.exe"
           rel_dir)
    with
    | exn ->
      failf
        "generated compile case %s failed\nsource:\n%s\nerror: %s"
        name
        source
        (Printexc.to_string exn))

let compile_generated_case_with_module ~name ~source ~module_name ~module_source =
  with_temp_dir ("lattice-gen-" ^ name ^ "-") (fun dir ->
    try
      ignore (generate_case_files ~dir ~source);
      write_file (Filename.concat dir (module_name ^ ".ml")) module_source;
      write_file
        (Filename.concat dir "dune")
        (Printf.sprintf
           {|(executable
 (name %s)
 (modules generated solver_runtime %s)
 (flags (:standard -w -53))
 (libraries ocamlcommon))
|}
           module_name
           module_name);
      let root = compiler_root () in
      let rel_dir =
        Filename.concat "lattice-gen-test-build" (Filename.basename dir)
      in
      run_command
        ~cwd:root
        (Printf.sprintf
           "RUNTIME_DIR=runtime4 dune build --display=short %s/%s.exe"
           rel_dir
           module_name)
    with
    | exn ->
      failf
        "generated compile case %s with extra module %s failed\nsource:\n%s\nerror: %s"
        name
        module_name
        source
        (Printexc.to_string exn))

let run_generated_case ~name ~source =
  with_temp_dir ("lattice-gen-" ^ name ^ "-") (fun dir ->
    try
      ignore (generate_case_files ~dir ~source);
      compile_and_run_generated_test ~dir
    with
    | exn ->
      failf
        "generated case %s failed\nsource:\n%s\nerror: %s"
        name
        source
        (Printexc.to_string exn))
