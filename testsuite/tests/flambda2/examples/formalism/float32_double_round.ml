(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: WITNESS OF A SOUNDNESS BUG (not a MATCH).
   Constant-folding an int -> float32 conversion double-rounds
   (int -> float64 -> float32) whereas the generated code single-rounds
   (cvtsi2ss), so the folded constant differs from the runtime result of the
   same conversion.
   Rules: P.Unary.NumConv (ch. 05), S.Rewrite.Prim.ConstFold (ch. 10),
   INV.Rewrite.Local coupling point (i) (ch. 13).
   Case study: middle_end/flambda2/docs/formalism/14-validation/float32_double_round.md
   Phenomenon: [float32_of_int 9007199791611905] folds to [0x1p+53s] (= 2^53,
   float32 bits 0x5a000000), the DOUBLE-rounded value. The correct single-rounded
   result is 2^53 + 2^30 (bits 0x5a000001, printed [0x1.000002p+53s]). The
   reference below captures the buggy output; it will flip once
   number_adjuncts.ml#to_naked_float32 is fixed to single-round. *)

external float32_of_int : int -> float32 = "%float32ofint"

let f () = float32_of_int 9007199791611905
