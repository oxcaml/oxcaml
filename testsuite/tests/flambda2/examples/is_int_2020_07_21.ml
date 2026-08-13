(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

type src =
  | C1 of int
  | C2

type dst =
  | A
  | B
  | C

let[@inline always] of_src = function C1 _ -> A | C2 -> B

let test src f g =
  let dst = if f () then of_src src else C in
  g dst
