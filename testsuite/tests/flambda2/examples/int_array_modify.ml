(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

let f x =
  let a = Array.make 42 0 in
  Array.unsafe_set a 0 x;
  a
