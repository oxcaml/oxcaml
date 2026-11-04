(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, under which no Simplify dump would be produced). *)
 set OCAMLPARAM = "_,";
 flags += "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets";
 { native with dump-simplify; check-fexpr-dump; }
*)

(* As for np_pair.ml, but here the variable referenced by the phantom let for
   [pair] is a parameter of a recursive continuation: the unboxed contents of
   [acc], carried around the loop. It must likewise be marked "np". *)

let test n =
  let acc = ref 1 in
  for _i = 1 to n do
    let pair = !acc, !acc in
    acc := fst pair + snd pair
  done;
  !acc
