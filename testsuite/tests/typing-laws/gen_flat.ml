(* TEST
 readonly_files = "shapes.mli";
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "shapes.mli";
 ocamlopt.opt;
 flags = "-generate-laws-implementation -o shapes_laws.ml";
 module = "shapes.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/shapes_laws.ml";
 reference = "${test_source_directory}/shapes_laws.ml.reference";
 check-program-output;
 flags = "-generate-laws-interface -o shapes_laws.mli";
 module = "shapes.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/shapes_laws.mli";
 reference = "${test_source_directory}/shapes_laws.mli.reference";
 check-program-output;
 flags = "";
 module = "shapes_laws.mli shapes_laws.ml";
 ocamlopt.opt;
*)

(* The laws file of an interface whose laws are all at the top level,
   exercising the constructs of clauses (see shapes.mli). The generated
   files are checked against the references and compiled. *)
