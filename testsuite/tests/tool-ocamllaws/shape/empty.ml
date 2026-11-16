(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "empty.ml";
 ocamlopt.opt;
 script = "${ocamlrun} ${ocamlsrcdir}/tools/ocamllaws extract \
   -format laws-file -o empty_laws.ml empty.cmx";
 script;
 output = "${test_build_directory}/empty_laws.ml";
 reference = "${test_source_directory}/empty_laws.ml.reference";
 check-program-output;
 flags = "-extension laws";
 module = "empty_laws.ml";
 ocamlopt.opt;
*)

(* The laws file of a unit without laws. *)

let x = 1
