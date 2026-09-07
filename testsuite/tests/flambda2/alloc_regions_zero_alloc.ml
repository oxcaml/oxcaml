(* TEST
 compile_only = "true";
 ocamlopt_flags = "-O3";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
*)

let[@unboxable] f1 x = x +. 1.

let f2 (x [@unboxable]) = x +. 1.

let g x = raise x

type t = A | B | C

let[@zero_alloc] h t = match t with A -> A | B -> B | C -> C

let[@zero_alloc strict] h_strict t = match t with A -> B | B -> C | C -> A

let z t =
  let y = h t in
  match y with A -> B | B -> C | C -> A

let[@zero_alloc assume] pair x = (x, x)

let[@zero_alloc assume strict] pair_strict x = (x, x)

external exp : float -> float = "caml_exp_float"

let p r = exp (exp r)
