(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

external ( +. ) : float -> float -> float = "%addfloat"

let[@inline never] g x = x +. 42., x +. 1.

let f b x =
  let y, z = g x (* if b then x +. 1. else g x *) in
  y +. z
