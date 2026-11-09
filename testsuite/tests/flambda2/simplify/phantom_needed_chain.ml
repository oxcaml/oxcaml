(* TEST
   compile_only = "true";
   flambda2;
   ocamlopt_flags += " -O3 -g -gno-upstream-dwarf";
   ocamlopt_flags += " -flambda2-expert-phantom-lets";
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt with dump-simplify;
   check-fexpr-dump;
 *)

(* Once [pair]'s projections have been simplified away, its binding becomes a
   phantom let referencing the non-user-visible temporary holding
   [(produce x * 2) + 1]. That temporary remains a normal let (its value is
   also used by the addition of the two components), so it is marked
   "needed_by_phantom_let". Only variables directly referenced by the defining
   expressions of phantom lets are marked: the multiplication temporary and
   the continuation parameter receiving [produce x]'s result are not, since no
   phantom let refers to them. *)

let[@inline never] [@local never] produce x = x + 1

let test x =
  let pair = ((produce x * 2) + 1, x) in
  fst pair + snd pair
