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

(* The forms taken by the supply of a phantom proxy variable's value.
   Phantom defining expressions are closed at creation: references to
   delayed bindings (which may be moved down, or inlined out) go through
   phantom proxy variables bound immediately beforehand; the proxies'
   values arrive later, either folded into the proxy's binder or via the
   [Cphantom_add_equality] operation.  The whole Cmm output is compared
   against the reference file.

   [materialised]: the combined form.  [a1] is used twice, so its binding
   materialises as a real let, in the same flush as (and outside) the
   proxy's binder; the proxy is therefore bound directly to [a1], with no
   [Cphantom_add_equality] needed.  (The operation is used when the
   binder and the value's appearance are in different places: at a real
   let the binding was sunk past, or at an inlined use site.)

   [combined_temp]: the combined form for a variable that is not
   user-visible.  The pair references a compiler-generated temporary (the
   projection), which materialises as a real let (its other use is in
   return position); as for [materialised], the proxy is bound directly
   to it.  Nothing carries a [Cnormal_var_optimised_out], there being no
   user variable to name.

   [inlined_out]: the operation, wrapping a named expression.  [a2] and
   [b2] are each used once, on different branches, so their bindings are
   inlined out; the proxies' binders (before the fork) and the points
   where the values appear (the inlined use sites, on the branches) are
   in different places, so free-standing equality operations are needed.
   The inlined expressions are wrapped both in
   [Cnormal_var_optimised_out] (recording the deleted normal variables)
   and in [Cphantom_add_equality] (supplying the proxies' values); the
   equality operation is transparent, so the expressions are not
   duplicated.

   [bare_equality]: the operation alone.  As [combined_temp], but the
   temporary's single other use is inlined out (into an
   [opaque_identity]): the inlined expression carries a free-standing
   [Cphantom_add_equality] for the proxy with no
   [Cnormal_var_optimised_out], there being no user variable to name.

   [arith]: as [inlined_out], but the inlined-out value flows into
   arithmetic: the Cmm arithmetic helpers currently rebuild such
   expressions and can drop the wrappers (CR in
   [To_cmm_env.wrap_phantom]), so [s4] is presented via its bare value
   only. *)

[@@@ocaml.warning "-26-27-32"]

let[@inline never] [@local never] materialised p x =
  let a1 = fst p in
  let pair1 = (a1, x) in
  if x > 0 then a1 + a1 else 0

let[@inline never] [@local never] combined_temp q x =
  let unused_pair = (fst q, x) in
  fst q

let[@inline never] [@local never] inlined_out p x =
  let a2 = fst p in
  let b2 = snd p in
  let pair2 = (a2, b2) in
  if x > 0 then a2 else b2

let[@inline never] [@local never] bare_equality q x =
  let unused_pair = (fst q, x) in
  ignore (Sys.opaque_identity (fst q));
  x

let[@inline never] [@local never] arith x =
  let s4 = x + 1 in
  let pair4 = (s4, x) in
  s4 * 2
