(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, the reaper, or dump-dir redirection), which would
    perturb the Cmm output this test checks. *)
 set OCAMLPARAM = "_,";
 setup-ocamlopt.byte-build-env;
 flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm-functions";
 module = "phantom_closure.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Closure-captured variables that are optimised away (for example when
   the function reading them is inlined at a site where the closure is
   known) must remain describable via phantom lets.  The whole Cmm output
   (functions only) is compared against the reference file. *)

[@@@ocaml.warning "-26-27-32"]

let[@inline never] [@local never] test x =
  let y = x * 3 in
  let[@inline always] g () = y + x in
  g () + 1
