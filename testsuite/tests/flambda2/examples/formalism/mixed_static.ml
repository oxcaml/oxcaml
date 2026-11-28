(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: a toplevel immutable mixed record constant is lifted to
   a static [Block] whose fields include raw naked scalars, which only a
   Mixed_record scannable shape can hold.
   Rules: P.Static.MixedBlock, OS.Let.Static.
   Case study: middle_end/flambda2/docs/formalism/14-validation/mixed-02-static.md
   Phenomenon: a static Block 0 with mixed fields (string sym, naked float,
   naked int32); no Make_block primitive. *)

type t = { x1 : string; x2 : float#; x3 : int32# }

let c = { x1 = "a"; x2 = #1.0; x3 = #2l }
