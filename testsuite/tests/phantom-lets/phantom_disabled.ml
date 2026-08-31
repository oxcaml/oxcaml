(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, the reaper, or dump-dir redirection), which would
    perturb the Cmm output this test checks. *)
 set OCAMLPARAM = "_,";
 setup-ocamlopt.byte-build-env;
 flags = "-O3 -g -gno-upstream-dwarf -no-flambda2-expert-phantom-lets -dcmm-functions";
 module = "phantom_disabled.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* As phantom_basics.ml, but with phantom lets explicitly disabled: no
   [let?] appears in the output.  (The flag is passed explicitly rather
   than relying on the default being off, so that this test remains
   correct once phantom lets are enabled by default later in the
   series.) *)

[@@@ocaml.warning "-26-27-32"]

let[@inline never] [@local never] f x y =
  let unused_const = 42 in
  let unused_alias = x in
  let unused_pair = (x, y) in
  x + y
