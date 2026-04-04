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

let remove_dir path =
  ignore (Sys.command ("rm -rf " ^ Filename.quote path))

let with_temp_dir prefix f =
  let dir = Filename.temp_file prefix "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  match f dir with
  | result ->
    remove_dir dir;
    result
  | exception exn ->
    prerr_endline ("preserving failing temp dir: " ^ dir);
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
  write_file input_path source;
  Generate.generate_to_files
    ~input_path
    ~ml_path
    ~mli_path
    ~test_ml_path
    ();
  ml_path, mli_path, test_ml_path

let compile_and_run_generated_test ~dir =
  run_command
    ~cwd:dir
    "ocamlc -c generated.mli && \
     ocamlc -c generated.ml && \
     ocamlc -o generated_test generated.cmo generated_test.ml && \
     ./generated_test"

let compile_generated_case ~name ~source =
  with_temp_dir ("lattice-gen-" ^ name ^ "-") (fun dir ->
    try
      ignore (generate_case_files ~dir ~source);
      run_command
        ~cwd:dir
        "ocamlc -c generated.mli && \
         ocamlc -c generated.ml && \
         ocamlc -o generated_test generated.cmo generated_test.ml"
    with
    | exn ->
      failf
        "generated compile case %s failed\nsource:\n%s\nerror: %s"
        name
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
