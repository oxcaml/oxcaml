(* TEST
 compile_only = "true";
 flambda2;
 flags = "-extension layout_poly_alpha -O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* A local template capturing a dynamic value, instantiated at two layouts in
   the same unit: the environment block, both instantiations and their
   closures should all be optimized away, leaving only the two increments and
   the boxing. *)

external to_float : float# -> float = "%box_float"

let f r =
  let poly_ k x = incr r; x in
  ignore (k 1);
  to_float (k #2.0)
