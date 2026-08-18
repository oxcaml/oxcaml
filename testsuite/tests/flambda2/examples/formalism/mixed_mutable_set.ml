(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: a write to a mutable flat-suffix field followed by a
   read of the same field does not fold the read to the just-written value,
   because a mutable-block load has coeffects.
   Rules: P.Binary.BlockSet.Mixed, P.Unary.BlockLoad.Mixed,
   P.Effects.ReadingFromBlock.
   Case study: middle_end/flambda2/docs/formalism/14-validation/mixed-03-mutable-set.md
   Phenomenon: the mutable [.mut] load is NOT folded to the written value. *)

type t = { x1 : string; mutable x2 : float#; x3 : int32# }

let write_then_read (t : t) (v : float#) =
  t.x2 <- v;
  t.x2
