(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, the reaper, or dump-dir redirection), which would
    perturb the Cmm output this test checks. *)
 set OCAMLPARAM = "_,";
 setup-ocamlopt.byte-build-env;
 flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm";
 module = "phantom_splittable.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* A splittable primitive binding ([s], a must-inline primitive
   application) referenced by a phantom defining expression.  [s] is
   inlined out at its single use; because the consumer takes the value
   raw, the rebuilt primitive application arrives wrapped in both the
   [Cphantom_add_equality] for [s]'s proxy and the [Cnormal_var_optimized_out]
   annotation.  (Contrast the [arith] case of phantom_add_equality.ml,
   where a consumer that is itself arithmetic loses the wrappers.)  The
   whole Cmm output is compared against the reference file. *)

[@@@ocaml.warning "-26-27-32"]

let[@inline never] [@local never] splittable x =
  let s = x + 1 in
  let unused_pair = (s, x) in
  ignore (Sys.opaque_identity s);
  0
