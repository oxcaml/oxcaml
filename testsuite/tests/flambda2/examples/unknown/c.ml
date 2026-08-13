(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

external ( ~-. ) : float -> float = "%negfloat"

external ( +. ) : float -> float -> float = "%addfloat"

(* let f x y = x +. y *)

let neg x = ~-.x
