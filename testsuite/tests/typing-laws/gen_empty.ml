(* TEST
 readonly_files = "nolaws.mli";
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "nolaws.mli";
 ocamlopt.opt;
 flags = "-generate-laws-implementation -o nolaws_laws.ml";
 module = "nolaws.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/nolaws_laws.ml";
 reference = "${test_source_directory}/nolaws_laws.ml.reference";
 check-program-output;
 flags = "-generate-laws-interface -o nolaws_laws.mli";
 module = "nolaws.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/nolaws_laws.mli";
 reference = "${test_source_directory}/nolaws_laws.mli.reference";
 check-program-output;
 flags = "";
 module = "nolaws_laws.mli nolaws_laws.ml";
 ocamlopt.opt;
*)

(* The laws file of an interface without laws. *)
