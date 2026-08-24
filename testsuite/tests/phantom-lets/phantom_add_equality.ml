(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, the reaper, or dump-dir redirection), which would
    perturb the Cmm output this test checks. *)
 set OCAMLPARAM = "_,";
 setup-ocamlopt.byte-build-env;
 flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm";
 module = "phantom_add_equality.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Phantom defining expressions are closed at creation: references to
   delayed bindings (which may be moved down, or inlined out) go through
   phantom proxy variables bound immediately beforehand, and constants are
   substituted directly.  The whole Cmm output is compared against the
   reference file.

   [materialised]: [a1] is used twice, so its binding materialises as a
   real let; the pair's phantom let references a proxy for it, which
   receives its value via a [Cphantom_add_equality] operation placed where
   both the real let and the proxy's binder are in scope.

   [inlined_out]: [a2] and [b2] are each used once, on different branches,
   so their bindings are inlined out.  The inlined expressions are wrapped
   both in [Cname_for_debugger] (recording the deleted normal variables) and
   in [Cphantom_add_equality] (supplying the proxies' values); the equality
   operation is transparent, so the expressions are not duplicated.

   [arith]: as [inlined_out], but the inlined-out value flows into
   arithmetic: the Cmm arithmetic helpers currently rebuild such
   expressions and can drop the wrappers (CR in [To_cmm_env.wrap_phantom]),
   so [s4] is presented via its bare value only.

   [constant]: [k3] is bound to a constant, which is known when the pair's
   phantom let is created: the block references a phantom variable bound
   directly to the constant, and no equality is needed. *)

[@@@ocaml.warning "-26-27-32"]

let[@inline never] [@local never] materialised p x =
  let a1 = fst p in
  let pair1 = (a1, x) in
  if x > 0 then a1 + a1 else 0

let[@inline never] [@local never] inlined_out p x =
  let a2 = fst p in
  let b2 = snd p in
  let pair2 = (a2, b2) in
  if x > 0 then a2 else b2

let[@inline never] [@local never] constant x =
  let k3 = 42 in
  let pair3 = (k3, x) in
  x + k3

let[@inline never] [@local never] arith x =
  let s4 = x + 1 in
  let pair4 = (s4, x) in
  s4 * 2
