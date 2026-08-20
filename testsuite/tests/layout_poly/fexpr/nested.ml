(* TEST
 compile_only = "true";
 flambda2;
 flags = "-extension layout_poly_alpha -O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Nested templates: the inner template is defined inside the outer one and
   instantiated at the outer's layout variable. Both are [@inline never] so
   the dump keeps the two levels of specialized code and their closures. *)

external to_float : float# -> float = "%box_float"

let[@inline never] poly_ outer x =
  let[@inline never] poly_ inner y = y in
  inner x

let f (v : float#) = to_float (outer v)
