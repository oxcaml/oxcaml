(* TEST
   compile_only = "true";
   flambda2;
   ocamlopt_flags += " -O3 -g -gno-upstream-dwarf";
   ocamlopt_flags += " -flambda2-expert-phantom-lets";
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt with dump-simplify;
   check-fexpr-dump;
 *)

(* As for phantom_needed_pair.ml, but here the variable referenced by the
   phantom let for [pair] is a parameter of a recursive continuation: the
   unboxed contents of [acc], carried around the loop. It must likewise be
   marked "needed_by_phantom_let". The temporaries computed in the loop body
   (the sum and the next loop counter) are not referenced by any phantom let
   and so are not marked. *)

let test n =
  let acc = ref 1 in
  for _i = 1 to n do
    let pair = !acc, !acc in
    acc := fst pair + snd pair
  done;
  !acc
