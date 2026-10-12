(* TEST
 flags = "-dlambda -dcanonical-ids";
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

let uniform () =
  let (_x, y) = (1, 2) in
  y

let mixed_nonconstant a =
  let (_x, y) = (a, #2.0) in
  y

(* resulting lambda should be optimized, since this is a constant *)
let mixed_constant () =
  let (_x, y) = (#1.0, 2) in
  y
