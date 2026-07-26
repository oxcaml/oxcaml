(* TEST
 subdirectories = "lib";
 readonly_files = "consumer.ml";
 compile_only = "true";
 setup-ocamlc.byte-build-env;
 all_modules = "lib/res_lam.ml";
 ocamlc.byte;
 flags = "-I lib";
 all_modules = "consumer.ml";
 ocamlc.byte;
*)
