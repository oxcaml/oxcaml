(* TEST
   compile_only = "true";
   flambda2;
   ocamlopt_flags += " -O3 -g -gno-upstream-dwarf";
   ocamlopt_flags += " -flambda2-expert-phantom-lets";
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt with dump-simplify;
   check-fexpr-dump;
 *)

(* The join-point continuation's parameter [a] is referenced by the phantom
   let for [pair] and is marked "needed_by_phantom_let" (it is user visible,
   so the mark only records that a phantom let refers to it). The
   non-user-visible temporaries computed in the two branches, whose values
   flow into [a], are not marked: only variables directly referenced by the
   defining expressions of phantom lets are. *)

let test b x =
  let a = if b then x + 1 else x * 2 in
  let pair = a, x in
  fst pair + snd pair
