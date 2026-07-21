(* TEST
 flambda2;
 setup-ocamlopt.byte-build-env;
 {
   flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm -dump-into-file";
   module = "phantom_basics.ml";
   ocamlopt.byte;
 }{
   script = "sh ${test_source_directory}/check-contains.sh phantom_basics.cmx.dump let? unused_pair";
   script;
 }{
   flags = "-O3 -g -gno-upstream-dwarf -dcmm -dump-into-file";
   module = "phantom_basics.ml";
   ocamlopt.byte;
 }{
   script = "sh ${test_source_directory}/check-absent.sh phantom_basics.cmx.dump let?";
   script;
 }
*)

(* Variables that are optimised away by Simplify must give rise to phantom
   lets in the Cmm code (visible as [let?] in the -dcmm output) when
   -flambda2-expert-phantom-lets is enabled, and to nothing otherwise. *)

[@@@ocaml.warning "-26-27-32"]

let[@inline never] [@local never] f x y =
  let unused_konst = 42 in
  let unused_alias = x in
  let unused_pair = (x, y) in
  x + y
