(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, the reaper, or dump-dir redirection), which would
    perturb the Cmm output this test checks. *)
 set OCAMLPARAM = "_,";
 setup-ocamlopt.byte-build-env;
 flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm-functions";
 module = "phantom_inlined_locals.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* The locals of an inlined function whose defining expressions are
   substituted into their use sites must remain visible to the debugger:
   the substituted expressions are wrapped in naming constructs
   ([normal_var_optimised_out] in the output).  The wrappers are
   annotations only, from which instruction selection produces naming
   operations; the named variables do not require bindings (in
   particular, no phantom lets).  The whole Cmm output (functions only)
   is compared against the reference file.

   The provenances do not yet carry parameter classifications or
   inlining-stack locations; that metadata arrives with the
   Bound_var/Bound_parameter debuginfo patches later in this series, at
   which point the reference will show it. *)

[@@@ocaml.warning "-26-27-32"]

let[@inline always] inner a =
  let sum1 = a + 1 in
  let doubled = sum1 * 2 in
  let diff = doubled - a in
  diff + a

let[@inline never] [@local never] caller x =
  let result = inner x in
  result + 1
