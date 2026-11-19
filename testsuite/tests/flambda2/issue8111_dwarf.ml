(* TEST
   compile_only = "true";
   flambda2;
   ocamlopt_flags += " -gdwarf-may-alter-codegen";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-simplify;
   check-fexpr-dump;
 *)

[@@@ocaml.flambda_o3]

(* Issue #8111, debug-precedence branch: when [gdwarf_may_alter_codegen] is set,
   debuginfo must be preserved, so the two arms below (identical raises from two
   source locations) must NOT be deduplicated.  The simplified fexpr should keep
   a switch with two raise arms. *)
let[@inline never] two_identical_raises x strict =
  if 0 < x then x else if strict then raise Not_found else raise Not_found
