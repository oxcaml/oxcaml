(* TEST
 readonly_files = "law.mli laws_gen_Law.mli c.mli capture.mli captured.mli capture_parameter.mli";
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "law.mli laws_gen_Law.mli c.mli capture.mli captured.mli capture_parameter.mli";
 ocamlopt.opt;
 flags = "-generate-laws-implementation -o law_laws.ml";
 module = "law.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/law_laws.ml";
 reference = "${test_source_directory}/law_laws.ml.reference";
 check-program-output;
 flags = "-generate-laws-interface -o law_laws.mli";
 module = "law.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/law_laws.mli";
 reference = "${test_source_directory}/law_laws.mli.reference";
 check-program-output;
 flags = "-generate-laws-implementation -o c_laws.ml";
 module = "c.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/c_laws.ml";
 reference = "${test_source_directory}/c_laws.ml.reference";
 check-program-output;
 flags = "-generate-laws-interface -o c_laws.mli";
 module = "c.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/c_laws.mli";
 reference = "${test_source_directory}/c_laws.mli.reference";
 check-program-output;
 flags = "-generate-laws-implementation -o capture_laws.ml";
 module = "capture.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/capture_laws.ml";
 reference = "${test_source_directory}/capture_laws.ml.reference";
 check-program-output;
 flags = "-generate-laws-interface -o capture_laws.mli";
 module = "capture.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/capture_laws.mli";
 reference = "${test_source_directory}/capture_laws.mli.reference";
 check-program-output;
 flags = "-generate-laws-implementation -o capture_parameter_laws.ml";
 module = "capture_parameter.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/capture_parameter_laws.ml";
 reference = "${test_source_directory}/capture_parameter_laws.ml.reference";
 check-program-output;
 flags = "-generate-laws-interface -o capture_parameter_laws.mli";
 module = "capture_parameter.cmi";
 ocamlopt.opt;
 output = "${test_build_directory}/capture_parameter_laws.mli";
 reference = "${test_source_directory}/capture_parameter_laws.mli.reference";
 check-program-output;
 flags = "-extension laws";
 module = "law_laws.mli law_laws.ml c_laws.mli c_laws.ml capture_laws.mli capture_laws.ml capture_parameter_laws.mli capture_parameter_laws.ml";
 ocamlopt.opt;
*)

(* Compilation units are referred to through aliases bound at the top of
   the generated file, whose names are fresh: units named like the
   modules of the generated file ([Law], [C]), like the alias of another
   unit ([Laws_gen_Law]), or like a functor parameter ([Captured]) are
   told apart. *)
