(* TEST
 flambda2;
 setup-ocamlopt.byte-build-env;
 {
   module = "rewrite_type_of_constants.mli";
   ocamlopt.byte;
 }{
   flags = "-flambda2-join-points -no-flambda2-reaper";
   module = "rewrite_type_of_constants.ml";
   ocamlopt.byte;
 }{
   program = "rewrite_type_of_constants.cmx";
   output = "rewrite_type_of_constants.cmx.ocamlobjinfo.no-reaper.output";
   ocamlobjinfo;
 }{
   script = "sh ${test_source_directory}/check-has-123456.sh rewrite_type_of_constants.cmx.ocamlobjinfo.no-reaper.output";
   script;
 }{
   flags = "-flambda2-join-points -flambda2-reaper -reaper-local-fields";
   module = "rewrite_type_of_constants.ml";
   ocamlopt.byte;
 }{
   program = "rewrite_type_of_constants.cmx";
   output = "rewrite_type_of_constants.cmx.ocamlobjinfo.reaper.output";
   ocamlobjinfo;
 }{
   script = "sh ${test_source_directory}/check-no-123456.sh rewrite_type_of_constants.cmx.ocamlobjinfo.reaper.output";
   script;
 }
*)

(* This is a test file for PR #5391.

   In the code below, after simplify, we will know that the second field of `x`
   is always exactly `123456`, which will be recorded in the types (if join
   points are enabled).

   However, the reaper is able to notice that the second field of `x` is
   actually dead code and will rewrite both occurrences of `123456` with a
   bogus value (typically, `0`).

   This test checks that the corresponding rewriting also occured in the types,
   so that we shouldn't find any occurences of the constant `123456` in the
   cmx.
 *)

external opaque : 'a -> 'a = "%opaque"
let x = if opaque false then (opaque 0, 123456) else (opaque 1, 123456)
let[@inline never][@local never] f b =
  if b then fst x else 0
