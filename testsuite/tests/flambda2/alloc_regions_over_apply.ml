(* TEST
 compile_only = "true";
 ocamlopt_flags = "-O3";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
*)

let[@inline never] make_adder x =
  let x2 = Sys.opaque_identity (x * 2) in
  fun y -> x2 + y

let over1 x = make_adder x 5

let[@zero_alloc assume] over2 x = make_adder x 5
