(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags += " -dcmm-invariants";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
*)

let[@inline never] f x = #(x, x)

let g x =
  let #(a, b) = f x in
  (a, b)
