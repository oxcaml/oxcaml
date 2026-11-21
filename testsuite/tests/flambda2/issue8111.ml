(* TEST
   compile_only = "true";
   flambda2;
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-simplify;
   check-fexpr-dump;
 *)

[@@@ocaml.flambda_o3]

(* Issue #8111: the two arms below raise the same exception but come from two
   different source locations, so their debuginfo differs.  That must not stop
   the arms being deduplicated when debugging is not taking precedence over
   code generation.  The simplified fexpr should contain a single [raise
   Not_found], not a switch with two identical raise arms. *)
let[@inline never] two_identical_raises x strict =
  if 0 < x then x else if strict then raise Not_found else raise Not_found
