(* TEST
 flambda2;
 flags += "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets";
 { native with dump-simplify; check-fexpr-dump; }
*)

(* The unboxed calling convention splits [pair] into two function parameters
   of the main code that are not user visible; the main code rebinds the
   boxed [pair] from them. Once [pair]'s projections have been simplified
   away, that binding becomes a phantom let referencing the two parameters,
   which must therefore be marked "np". *)

let[@inline never] [@local never] consume (pair [@unboxable] : int * int) =
  fst pair + snd pair
