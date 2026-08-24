(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, the reaper, or dump-dir redirection), which would
    perturb the Cmm output this test checks. *)
 set OCAMLPARAM = "_,";
 setup-ocamlopt.byte-build-env;
 flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm";
 module = "phantom_duplicated.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* A binding that is duplicated at its use sites, referenced by a phantom
   defining expression.  [Obj.magic] is an identity primitive, so [b]'s
   defining expression is trivially simple Cmm (a variable) and the
   binding is classified must-inline-and-duplicate: it is substituted at
   both use sites, one per branch.  Each duplicated copy carries its own
   [Cphantom_add_equality] for [b]'s proxy (and [Cnormal_var_optimized_out]
   annotation), supplying the proxy's value on that path; re-marking with
   the same value is idempotent.  The proxy's binder sits, without a
   value, next to the pair's phantom let before the fork.  The whole Cmm
   output is compared against the reference file. *)

[@@@ocaml.warning "-26-27-32"]

let[@inline never] [@local never] f (q : int * int) x =
  let b : int = Obj.magic q in
  let unused_pair = (b, x) in
  if x > 0 then (b, 1) else (2, b)
