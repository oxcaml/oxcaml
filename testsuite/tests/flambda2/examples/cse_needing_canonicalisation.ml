(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

let f x y =
  let a = x + y in
  let p = x, y in
  let x' = fst p in
  let b = x' + y in
  a, b
