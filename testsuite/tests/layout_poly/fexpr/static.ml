(* TEST
 compile_only = "true";
 flambda2;
 flags = "-extension layout_poly_alpha -O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* A completely static template: the environment is a statically-allocated
   set of closures with no value slots (just the deleted function slot). *)

let poly_ id x = x
