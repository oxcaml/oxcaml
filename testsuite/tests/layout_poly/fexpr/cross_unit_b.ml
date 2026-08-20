(* TEST
 compile_only = "true";
 modules = "cross_unit_a.ml";
 flambda2;
 flags = "-extension layout_poly_alpha -O3 -nocwd -Ix .";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Instantiating a template from another compilation unit: the captured [r]
   is copied out of the imported environment block (whose value slots must
   resolve across units) into the closures built for the two
   instantiations. *)

external to_float : float# -> float = "%box_float"

let f () =
  ignore (Cross_unit_a.bump 1);
  to_float (Cross_unit_a.bump #2.0)
