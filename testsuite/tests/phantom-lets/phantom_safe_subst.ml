(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, the reaper, or dump-dir redirection), which would
    perturb the Cmm output this test checks. *)
 set OCAMLPARAM = "_,";
 setup-ocamlopt.byte-build-env;
 flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -flambda2-expert-cmm-safe-subst -dcmm";
 module = "phantom_safe_subst.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* The deferred, at-the-let form of [Cphantom_add_equality], reachable
   only under -flambda2-expert-cmm-safe-subst.

   [c] is a memory load, kept (pure, single normal use) across the fork
   at which the pair's phantom let and [c]'s proxy binder are emitted.
   Its use lies beyond a call, whose arbitrary effects push a validity
   stage; under the flag the substitution of the load past the call is
   therefore refused, and [c] materialises as a real let, before the
   call, in a later flush than (and so below) the proxy's binder.  The
   equality supplying the proxy's value is emitted directly under that
   let: the deferred form.  (Without the flag, the load would instead be
   substituted at the use site, past the call, and the value would
   arrive via the equality wrapping the inlined, named expression.)  The
   whole Cmm output is compared against the reference file. *)

[@@@ocaml.warning "-26-27-32"]

let[@inline never] [@local never] effectful () = print_string ""

let[@inline never] [@local never] f q x =
  let c = fst q in
  let unused_pair = (c, x) in
  if x > 0 then (effectful (); c) else 0
