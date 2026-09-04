(* TEST
 flambda2;
 flags += "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets";
 { native with dump-simplify; check-fexpr-dump; }
*)

(* The results of the two calls to [produce] are bound to non-user-visible
   variables (continuation parameters). Once [pair]'s projections have been
   simplified away, [pair]'s binding becomes a phantom let referencing those
   variables, which must therefore be replaced by variables marked "np" (not
   user visible, but needed by phantom let) so that they remain locatable by
   the debugger. The region variable of [pair]'s local allocation, in
   contrast, must not be so marked: it cannot be referenced by phantom
   defining expressions once translated to Cmm. *)

let[@inline never] [@local never] produce x = x + 1

let test x =
  let pair = produce x, produce x in
  fst pair + snd pair
