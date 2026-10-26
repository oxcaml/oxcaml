(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "gen_empty.ml";
 ocamlopt.opt;
 flags = "-generate-laws-implementation -o gen_empty_laws.ml";
 module = "gen_empty.cmx";
 ocamlopt.opt;
 output = "${test_build_directory}/gen_empty_laws.ml";
 reference = "${test_source_directory}/gen_empty_laws.ml.reference";
 check-program-output;
 flags = "-extension laws";
 module = "gen_empty_laws.ml";
 ocamlopt.opt;
*)

(* The laws file of a unit without laws. *)

let x = 1
