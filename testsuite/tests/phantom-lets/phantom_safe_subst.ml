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

(* Behaviour under -flambda2-expert-cmm-safe-subst of a moved-down load
   referenced by a phantom defining expression.  [c] is a faultable load,
   kept (pure, single normal use) across the fork at which the pair's
   phantom let and [c]'s proxy binder are emitted, with a division -- a
   control-flow-containing binding -- intervening before its use.

   At present the substitution of [c] at its use site is still permitted,
   so the value arrives via a [Cphantom_add_equality] wrapping the
   inlined, named expression, as without the flag.  Should the validity
   classifications ever tighten so that such a substitution is refused,
   [c] would instead materialise as a real let below the proxy's binder
   and this reference would change to show the equality emitted at that
   let -- the deferred form, currently reachable in no other way (see the
   comment on [materialised] in phantom_add_equality.ml).  The whole Cmm
   output is compared against the reference file. *)

[@@@ocaml.warning "-26-27-32"]

let[@inline never] [@local never] f (s : string) (x : int) =
  let c = String.unsafe_get s 0 in
  let unused_pair = (c, x) in
  if x > 0 then (let d = 100 / x in if d > 1 then c else 'a') else 'b'
