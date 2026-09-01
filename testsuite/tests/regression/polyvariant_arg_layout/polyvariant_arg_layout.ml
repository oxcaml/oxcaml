(* TEST
 reference = "${test_source_directory}/polyvariant_arg_layout.reference";
 flambda2;
 native;
*)

[@@@ocaml.flambda_o3]

let () =
  let b = Sys.opaque_identity true in
  let r = `A (if b then Null else This 42) in
  match r with
  | `A (This _) -> print_endline "This"
  | `A Null -> print_endline "Null"
