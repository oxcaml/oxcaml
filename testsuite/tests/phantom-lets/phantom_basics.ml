(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, the reaper, or dump-dir redirection), which would
    perturb the Cmm output this test checks. *)
 set OCAMLPARAM = "_,";
 setup-ocamlopt.byte-build-env;
 flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm-functions";
 module = "phantom_basics.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Variables that are optimised away by Simplify give rise to phantom lets
   in the Cmm code (visible as [let?] in the output).  The whole Cmm
   output (functions only) is compared against the reference file.
   phantom_disabled.ml checks that nothing appears without
   -flambda2-expert-phantom-lets. *)

[@@@ocaml.warning "-26-27-32"]

let[@inline never] [@local never] f x y =
  let unused_const = 42 in
  let unused_alias = x in
  let unused_pair = (x, y) in
  x + y
