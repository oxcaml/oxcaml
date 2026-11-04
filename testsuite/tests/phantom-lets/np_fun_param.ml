(* TEST
 flambda2;
 (* Warning 210 is disabled because [unboxing_kind] is currently consulted
    for the return type even when no return unboxing is requested, which
    emits a spurious [Unboxing_impossible] warning here; this is to be fixed
    separately. *)
 flags += "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -w -210";
 { native with dump-simplify; check-fexpr-dump; }
*)

(* The unboxed calling convention splits [pair] into two function parameters
   of the main code that are not user visible; the main code rebinds the
   boxed [pair] from them. Once [pair]'s projections have been simplified
   away, that binding becomes a phantom let referencing the two parameters,
   which must therefore be marked "np". *)

let[@inline never] [@local never] consume (pair [@unboxable] : int * int) =
  fst pair + snd pair
