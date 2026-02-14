(* TEST
 readonly_files = "a1.ml a2.ml b.ml";
 setup-ocamlc.byte-build-env;
 commandline = "-depend b.ml -o b.deps";
 ocamlc.byte;
 output = "b.deps";
 reference = "${test_source_directory}/b.deps.reference";
 check-program-output;
*)
