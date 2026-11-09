(* TEST
   compile_only = "true";
   flambda2;
   ocamlopt_flags += " -O3 -g -gno-upstream-dwarf";
   ocamlopt_flags += " -flambda2-expert-phantom-lets";
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt with dump-simplify;
   check-fexpr-dump;
 *)

(* The results of the two calls to [produce] are bound to non-user-visible
   variables (continuation parameters). Once [pair]'s projections have been
   simplified away, [pair]'s binding becomes a phantom let referencing those
   variables, which must therefore be marked "needed_by_phantom_let" so that
   they remain locatable by the debugger. The region variable of [pair]'s
   local allocation, in contrast, is not marked: the phantom defining
   expression mentions it, but such expressions never evaluate regions, so
   the debugger has no need of it. *)

let[@inline never] [@local never] produce x = x + 1

let test x =
  let pair = produce x, produce x in
  fst pair + snd pair
