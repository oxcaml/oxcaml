(* TEST
 readonly_files = "has_deps.ml no_deps.ml";
 setup-ocamlc.byte-build-env;
 {
   commandline = "-depend -modules has_deps.ml no_deps.ml -o modules.output";
   ocamlc.byte;
   output = "modules.output";
   reference = "${test_source_directory}/modules.reference";
   check-program-output;
 }
 {
   commandline = "-depend -modules-only has_deps.ml no_deps.ml -o modules_only.output";
   ocamlc.byte;
   output = "modules_only.output";
   reference = "${test_source_directory}/modules_only.reference";
   check-program-output;
 }
*)
