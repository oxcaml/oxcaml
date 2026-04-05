let usage_generation = "lattice_gen INPUT.lattice --ml OUTPUT.ml --mli OUTPUT.mli"

let usage_observable =
  "lattice_gen observable [--check|--update] [--test] PATH..."

let run_generation args =
  let input_path = ref None in
  let ml_path = ref None in
  let mli_path = ref None in
  let set_input path =
    match !input_path with
    | None -> input_path := Some path
    | Some _ -> raise (Arg.Bad "only one input file is supported")
  in
  let specs =
    [ "--ml", Arg.String (fun path -> ml_path := Some path), "output .ml path";
      "--mli", Arg.String (fun path -> mli_path := Some path), "output .mli path"
    ]
  in
  let argv = Array.of_list ("lattice_gen" :: args) in
  let current = ref 0 in
  Arg.parse_argv ~current argv specs set_input usage_generation;
  let input_path =
    match !input_path with
    | Some path -> path
    | None -> raise (Arg.Bad "missing input .lattice file")
  in
  let ml_path =
    match !ml_path with
    | Some path -> path
    | None -> raise (Arg.Bad "missing --ml output path")
  in
  let mli_path =
    match !mli_path with
    | Some path -> path
    | None -> raise (Arg.Bad "missing --mli output path")
  in
  Generate.generate_to_files ~input_path ~ml_path ~mli_path ()

let run_observable args =
  let mode = ref Observable.Check in
  let test = ref false in
  let paths = ref [] in
  let specs =
    [ "--check", Arg.Unit (fun () -> mode := Observable.Check), "check HTML snapshots";
      "--update", Arg.Unit (fun () -> mode := Observable.Update), "rewrite HTML snapshots";
      "--test", Arg.Unit (fun () -> test := true), "compile and run lattice tests for each <lat> block"
    ]
  in
  let argv = Array.of_list ("lattice_gen observable" :: args) in
  let current = ref 0 in
  Arg.parse_argv ~current argv specs (fun path -> paths := !paths @ [ path ]) usage_observable;
  match !paths with
  | [] -> raise (Arg.Bad "missing HTML path")
  | paths -> Observable.process_paths ~mode:!mode ~test:!test paths

let () =
  try
    let args = Array.to_list Sys.argv |> List.tl in
    match args with
    | "observable" :: rest -> run_observable rest
    | _ -> run_generation args
  with
  | Arg.Bad message ->
    prerr_endline message;
    prerr_endline usage_generation;
    prerr_endline usage_observable;
    exit 2
  | Error.Error _ as exn ->
    prerr_endline (Error.describe_exn exn);
    exit 1
  | Observable.Error message ->
    prerr_endline message;
    exit 1
