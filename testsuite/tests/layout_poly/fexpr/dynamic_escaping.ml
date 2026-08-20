(* TEST
 compile_only = "true";
 flambda2;
 flags = "-extension layout_poly_alpha -O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* A template capturing dynamic values (including an unboxed float) whose
   instantiation escapes: the specialized closure must survive, with the
   captures copied out of the environment block into its value slots. *)

external to_float : float# -> float = "%box_float"

let f r (v : float#) =
  let poly_ k x = incr r; ignore (to_float v); x in
  Sys.opaque_identity (k : int -> int)
