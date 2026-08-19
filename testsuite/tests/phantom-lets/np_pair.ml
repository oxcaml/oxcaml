(* TEST
 flambda2;
 setup-ocamlopt.byte-build-env;
 {
   flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dflambda -dump-into-file";
   module = "np_pair.ml";
   ocamlopt.byte;
 }{
   script = "sh ${test_source_directory}/check-contains-regexp.sh np_pair.cmx.dump apply_result/[0-9]+NP pair/[0-9]+UV";
   script;
 }{
   script = "sh ${test_source_directory}/check-absent-regexp.sh np_pair.cmx.dump region/[0-9]+NP";
   script;
 }{
   flags = "-O3 -g -gno-upstream-dwarf -dflambda -dump-into-file";
   module = "np_pair.ml";
   ocamlopt.byte;
 }{
   script = "sh ${test_source_directory}/check-absent-regexp.sh np_pair.cmx.dump /[0-9]+NP";
   script;
 }
*)

(* The results of the calls to [produce] are bound to non-user-visible
   variables (continuation parameters). Once [pair]'s projections have been
   simplified away, [pair]'s binding becomes a phantom let referencing those
   variables, which must therefore be promoted from "not user visible" (N) to
   "not user visible but needed by phantom let" (NP) so that they remain
   locatable by the debugger. The region variable of [pair]'s local
   allocation, in contrast, must not be promoted: it cannot be referenced by
   phantom defining expressions once translated to Cmm. Without
   -flambda2-expert-phantom-lets no promotion should happen at all. *)

let[@inline never] [@local never] produce x = x + 1

let[@inline never] [@local never] consume a b = a * b

let test x =
  let pair = produce x, produce x in
  consume (fst pair) (snd pair)
