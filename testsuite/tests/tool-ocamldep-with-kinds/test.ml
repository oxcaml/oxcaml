(* TEST
 readonly_files = "dep.mli with_dep.mli";
 setup-ocamlc.byte-build-env;
 commandline = "-depend with_dep.mli -o with_dep.deps";
 ocamlc.byte;
 output = "with_dep.deps";
 reference = "${test_source_directory}/with_dep.deps.reference";
 check-program-output;
*)
