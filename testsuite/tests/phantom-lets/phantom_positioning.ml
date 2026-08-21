(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, the reaper, or dump-dir redirection), which would
    perturb the Cmm dump this test greps. *)
 set OCAMLPARAM = "_,";
 setup-ocamlopt.byte-build-env;
 {
   flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm -dump-into-file";
   module = "phantom_positioning.ml";
   ocamlopt.byte;
 }{
   script = "sh ${test_source_directory}/check-positioning.sh phantom_positioning.cmx.dump";
   script;
 }
*)

(* Positioning of phantom lets whose defining expressions reference
   delayed bindings (see [To_cmm_env.flush_delayed_lets]).

   [precise]: both components of [pair1] are in scope at the flush where
   its phantom let is emitted: a single, fully-precise block.

   [fork]: [pair2]'s phantom let is flushed at the fork, where neither
   component's binder is in scope, so nothing is emitted there.  Each
   branch then uses one component, whose (empty) phantom binder appears
   on that branch; a copy of the pair's phantom let is emitted beneath
   it, describing that component and presenting the other as unavailable
   ([?] in the dump).  The copies bind fresh variables sharing the
   original's provenance, so the backend can present them as a single
   variable.

   [refine]: [a3] is bound by a call, so it is in scope at the fork where
   [pair3]'s phantom let is emitted, but [b3] is sunk past that flush:
   the block initially presents [b3] as unavailable.  When [b3]'s binder
   appears on a branch, a fuller copy is emitted beneath it. *)

[@@@ocaml.warning "-26-27-32"]

let[@inline never] [@local never] precise g x =
  let a1 = g x in
  let b1 = g (x + 1) in
  let pair1 = (a1, b1) in
  a1 + b1

let[@inline never] [@local never] fork p x =
  let a2 = fst p in
  let b2 = snd p in
  let pair2 = (a2, b2) in
  if x > 0 then a2 + 1 else b2 + 2

let[@inline never] [@local never] refine g x =
  let a3 = g x in
  let b3 = fst (Sys.opaque_identity (x, x)) in
  let pair3 = (a3, b3) in
  if x > 0 then b3 + 1 else 0
