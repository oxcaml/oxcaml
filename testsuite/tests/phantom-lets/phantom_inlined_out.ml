(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, the reaper, or dump-dir redirection), which would
    perturb the Cmm output this test checks. *)
 set OCAMLPARAM = "_,";
 setup-ocamlopt.byte-build-env;
 flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm";
 module = "phantom_inlined_out.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Variables referenced by phantom defining expressions whose own bindings
   are inlined out.  The whole Cmm output is compared against the reference
   file.

   [const_component]: [k1] is a constant; it is substituted directly into
   [pair1]'s defining expression when the phantom let is created, so the
   block reads [0: 42; x].

   [load_component]: [f2]'s value is a field load, which is convertible to
   a phantom defining expression; when its binding is inlined out (into the
   [opaque_identity] use), the phantom let left behind describes the load
   ([q[0]] in the output) rather than presenting [f2] as optimised out, and
   [pair2] references it.

   [arith_component]: [s3]'s value is arithmetic, which has no phantom
   defining expression form; the phantom let left behind presents [s3] as
   optimised out (its value remains recoverable through the naming
   operation at its use site). *)

[@@@ocaml.warning "-26-27-32"]

let[@inline never] [@local never] const_component x =
  let k1 = 42 in
  let pair1 = (k1, x) in
  x + k1

let[@inline never] [@local never] load_component q x =
  let f2 = fst q in
  let pair2 = (f2, x) in
  ignore (Sys.opaque_identity f2);
  x

let[@inline never] [@local never] arith_component x =
  let s3 = x + 1 in
  let pair3 = (s3, x) in
  s3 * 2
