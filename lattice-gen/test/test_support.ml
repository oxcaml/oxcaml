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

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path contents =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc contents)

let ensure_dir_exists path =
  if Sys.file_exists path
  then ensure (Sys.is_directory path) "%s exists and is not a directory" path
  else Unix.mkdir path 0o755

let remove_dir path =
  ignore (Sys.command ("rm -rf " ^ Filename.quote path))

let with_temp_dir prefix f =
  let base = Filename.concat (Filename.get_temp_dir_name ()) "lattice-gen-tests" in
  ensure_dir_exists base;
  let dir = Filename.temp_file ~temp_dir:base prefix "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  match f dir with
  | result ->
    remove_dir dir;
    result
  | exception exn ->
    remove_dir dir;
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
    ensure (contains message needle) "unexpected error for %s: %s" name message

let resolve_model ~name ~source =
  let ast = Parse.from_string ~input_name:(name ^ ".lattice") source in
  Model.resolve ast

let load_fixture name =
  if Sys.file_exists name
  then read_file name
  else read_file (Filename.concat (Filename.concat (Sys.getcwd ()) "test") name)

let render ~name ~source =
  Generate.render_string ~root_module:"Generated" ~input_name:name source

let expect_generated_ml_contains ~name ~source needles =
  let outputs = render ~name ~source in
  List.iter
    (fun needle ->
      ensure
        (contains outputs.ml needle)
        "generated .ml for %s is missing %S"
        name
        needle)
    needles

let expect_generated_mli_contains ~name ~source needles =
  let outputs = render ~name ~source in
  List.iter
    (fun needle ->
      ensure
        (contains outputs.mli needle)
        "generated .mli for %s is missing %S"
        name
        needle)
    needles

let expect_generated_excludes ~name ~source needles =
  let outputs = render ~name ~source in
  List.iter
    (fun needle ->
      ensure
        (not (contains outputs.ml needle) && not (contains outputs.mli needle))
        "generated output for %s unexpectedly contains %S"
        name
        needle)
    needles

let compile_generated_case ~name:_ ~source ~main =
  with_temp_dir "compile-" (fun dir ->
      let input_path = Filename.concat dir "generated.lattice" in
      let ml_path = Filename.concat dir "generated.ml" in
      let mli_path = Filename.concat dir "generated.mli" in
      let main_path = Filename.concat dir "main.ml" in
      let dune_path = Filename.concat dir "dune" in
      let dune_project_path = Filename.concat dir "dune-project" in
      write_file input_path source;
      Generate.generate_to_files ~input_path ~ml_path ~mli_path ();
      write_file main_path main;
      write_file dune_project_path "(lang dune 3.10)\n";
      write_file
        dune_path
        {|(executable
 (name main)
 (modules generated main))|};
      run_command ~cwd:dir "dune build main.exe")

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
