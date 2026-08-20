(* TEST
 compile_only = "true";
 flambda2;
 flags = "-extension layout_poly_alpha -O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Instantiating a fully static template in the same unit. [id] is marked
   [@inline never] so that the compiled artifacts stay visible: the
   specialized code, its statically-allocated closure, and a direct call to
   it from [f]. *)

external to_float : float# -> float = "%box_float"

let[@inline never] poly_ id x = x

let f (v : float#) = to_float (id v)
