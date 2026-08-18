(* TEST compile_only = "true"; flambda2; ocamlopt_flags = "-O3 -dcmm -dcanonical-ids";
   setup-ocamlopt.byte-build-env; ocamlopt.byte; check-ocamlopt.byte-output;
*)

(* to_cmm formalism validation: a multi-arm match on an int becomes a Cmm [(switch ...)]
   over the untagged scrutinee, with the out-of-range default guarded by an unsigned
   compare. Rules: TC.Switch (ch. 16), CM.Switch (ch. 15), TC.Prim.TagUntag (ch. 18). Case
   study: middle_end/flambda2/docs/formalism/14-validation/tocmm-02-switch.md The arms
   have distinct effects, so Simplify cannot collapse the match to an affine formula or a
   data table; a genuine [Cswitch] survives. *)

external ext : int -> unit = "ext_fn"

let[@inline never] dispatch n =
  match n with
  | 0 -> ext 100
  | 1 -> ext 200
  | 2 -> ext 300
  | 3 -> ext 400
  | _ -> ext 0
;;
