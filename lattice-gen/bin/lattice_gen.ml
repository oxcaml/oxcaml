let () =
  let input_path = ref None in
  let ml_path = ref None in
  let mli_path = ref None in
  let test_ml_path = ref None in
  let set_input path =
    match !input_path with
    | None -> input_path := Some path
    | Some _ -> raise (Arg.Bad "only one input file is supported")
  in
  let specs =
    [ "--ml", Arg.String (fun path -> ml_path := Some path), "output .ml path";
      "--mli", Arg.String (fun path -> mli_path := Some path), "output .mli path";
      "--test-ml",
      Arg.String (fun path -> test_ml_path := Some path),
      "optional output generated test .ml path"
    ]
  in
  let usage =
    "lattice_gen INPUT.lattice --ml OUTPUT.ml --mli OUTPUT.mli [--test-ml OUTPUT_test.ml]"
  in
  try
    Arg.parse specs set_input usage;
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
    Generate.generate_to_files
      ~input_path
      ~ml_path
      ~mli_path
      ?test_ml_path:!test_ml_path
      ()
  with
  | Arg.Bad message ->
    prerr_endline message;
    prerr_endline usage;
    exit 2
  | Error.Error _ as exn ->
    prerr_endline (Error.describe_exn exn);
    exit 1
