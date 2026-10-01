(* TEST
 readonly_files = "kinded.mli kinded_inputs.ml";
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "kinded.mli";
 ocamlopt.opt;
 flags = "-generate-laws-implementation -o kinded_laws.ml";
 module = "kinded.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/kinded_laws.ml";
 reference = "${test_source_directory}/kinded_laws.ml.reference";
 check-program-output;
 flags = "-generate-laws-interface -o kinded_laws.mli";
 module = "kinded.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/kinded_laws.mli";
 reference = "${test_source_directory}/kinded_laws.mli.reference";
 check-program-output;
 flags = "";
 module = "kinded_laws.mli kinded_laws.ml kinded_inputs.ml";
 ocamlopt.opt;
*)

(* The input types of laws quantify their type variables with their kinds
   (see kinded.mli), so that kinded_inputs.ml constructs the inputs the
   laws admit, nullable ones included. *)
