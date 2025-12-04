(* TEST
 readonly_files = "f64.ml i64.ml";

 setup-ocamlopt.byte-build-env;

 flags = "-Oclassic";
 module = "f64.ml i64.ml";
 ocamlopt.byte;
*)
