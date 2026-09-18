(* TEST
 compile_only = "true";
 modules = "test_cmx_offsets1.ml";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

let n = Test_cmx_offsets1.f 3
