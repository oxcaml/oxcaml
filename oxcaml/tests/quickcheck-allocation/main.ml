(* Entry point: parse arguments and hand off to [Fuzzer]. Usage:

   main.exe <ocamlopt-path> [-count N] [-seed S] [-mode M] [-out DIR]
   [-max-decls N] *)

let count = ref 200

let seed0 = ref 0

let compiler = ref ""

let mode = ref Gen.Mode.Soundness

let out_dir = ref ""

let max_decls = ref 4

let allow_assume = ref false

let usage_msg =
  "main.exe <ocamlopt-path> [-count N] [-seed S] [-mode \
   soundness|completeness] [-out DIR] [-max-decls N] [-allow-assume]"

let speclist =
  [ ( "-count",
      Arg.Set_int count,
      Printf.sprintf "N  number of programs to try (default %d)" !count );
    ( "-seed",
      Arg.Set_int seed0,
      Printf.sprintf "S  starting PRNG seed (default %d)" !seed0 );
    ( "-mode",
      Arg.Symbol
        ( ["soundness"; "completeness"],
          fun s ->
            match Gen.Mode.of_string s with
            | Some m -> mode := m
            | None -> raise (Arg.Bad (Printf.sprintf "unknown mode %S" s)) ),
      Printf.sprintf "  hunting mode (default %s)" (Gen.Mode.to_string !mode) );
    ( "-out",
      Arg.Set_string out_dir,
      "DIR  save witness .ml files (all suspects; one per rejection cause) to \
       DIR" );
    ( "-max-decls",
      Arg.Int
        (fun n ->
          if n < 1
          then raise (Arg.Bad "-max-decls must be at least 1")
          else max_decls := n),
      Printf.sprintf
        "N  max type declarations per program, min(2, N) to N (default %d)"
        !max_decls );
    ( "-allow-assume",
      Arg.Set allow_assume,
      "  render the assume annotation lane as [@zero_alloc assume] instead of \
       checked [@zero_alloc]; same seed generates the same program modulo the \
       keyword (experimental; suspects from such runs may implicate a wrong \
       assumption rather than the mode axis; default off)" ) ]

let anon_arg s =
  if !compiler = ""
  then compiler := s
  else raise (Arg.Bad (Printf.sprintf "unexpected extra argument %S" s))

let () =
  Arg.parse speclist anon_arg usage_msg;
  if !compiler = ""
  then (
    prerr_endline "error: missing <ocamlopt-path>";
    Arg.usage speclist usage_msg;
    exit 2);
  let stats =
    Fuzzer.run ~compiler:!compiler ~count:!count ~seed0:!seed0 ~mode:!mode
      ~max_decls:!max_decls ~allow_assume:!allow_assume
  in
  Fuzzer.report stats;
  if !out_dir <> "" then Fuzzer.save stats ~dir:!out_dir
