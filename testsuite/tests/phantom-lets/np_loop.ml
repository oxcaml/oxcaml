(* TEST
 flambda2;
 setup-ocamlopt.byte-build-env;
 {
   flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dflambda -dump-into-file";
   module = "np_loop.ml";
   ocamlopt.byte;
 }{
   script = "sh ${test_source_directory}/check-contains-regexp.sh np_loop.cmx.dump acc_[0-9]+_unboxed0/[0-9]+NP";
   script;
 }
*)

(* As for np_pair.ml, but here the variable referenced by the phantom let for
   [pair] is a parameter of a recursive continuation: the unboxed contents of
   [acc], carried around the loop. It must likewise be promoted to NP. *)

let[@inline never] [@local never] consume a b = a * b

let test n =
  let acc = ref 1 in
  for _i = 1 to n do
    let pair = !acc, !acc in
    acc := consume (fst pair) (snd pair)
  done;
  !acc
