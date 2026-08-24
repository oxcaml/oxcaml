(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, the reaper, or dump-dir redirection), which would
    perturb the Cmm output this test checks. *)
 set OCAMLPARAM = "_,";
 setup-ocamlopt.byte-build-env;
 flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm";
 module = "phantom_inlined_params.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Parameters of an inlined function whose values become known and whose
   bindings are then simplified away.  Simplify's inliner binds parameters
   with ordinary lets, so the deleted bindings give rise to phantom lets
   with real defining expressions in the caller: [a] as an alias of the
   caller's argument, and [b] as the constant [42].  The inlined local [s]
   is inlined out in the usual way, leaving a [Cname_for_debugger]
   annotation.  The whole Cmm output is compared against the reference
   file.

   The provenances of [a] and [b] do not yet carry the inlining-stack
   location or their parameter indices; that metadata arrives with the
   Bound_var/Bound_parameter debuginfo patches later in this series, at
   which point the reference will show it. *)

[@@@ocaml.warning "-26-27-32"]

let[@inline always] inner a b =
  let s = a + b in
  s * b

let[@inline never] [@local never] caller x = inner x 42
