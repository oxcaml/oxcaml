(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* A statically-allocated block in a recursive loop with a closure *)

let rec foo x = `Foo foo
