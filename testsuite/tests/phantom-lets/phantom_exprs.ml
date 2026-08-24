(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, the reaper, or dump-dir redirection), which would
    perturb the Cmm output this test checks. *)
 set OCAMLPARAM = "_,";
 setup-ocamlopt.byte-build-env;
 flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm";
 module = "phantom_exprs.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* The various forms a phantom defining expression can take, and the kinds
   of variable and constant it can mention.  (The supply of proxy
   variables' values is covered by phantom_add_equality.ml.)  The whole
   Cmm output is compared against the reference file.

   [constant]: [k1] is bound to a constant, which is known when the
   pair's phantom let is created: the block references a phantom variable
   bound directly to the constant, and no equality is needed.

   [consts]: constant components (a string and a boxed float, both static
   data symbols) become phantom variables bound to the symbols, per the
   ANF discipline; the parameter [x], whose binder is in scope, is
   referenced directly.

   [dead_temp]: the projection has no normal uses at all; Simplify itself
   turns it into a phantom let with a real defining expression ([q[0]]),
   which the pair's phantom let references directly -- phantom defining
   expressions may reference phantom-let-bound variables (the ANF
   discipline).

   [unrep]: a defining expression with no phantom form (arithmetic)
   yields an empty phantom let: the variable is presented as optimised
   out.

   [after_use]: the phantom let is placed after its referenced binding
   was already inlined out; the proxy stays without a value (the inlined
   use site, which carries the [Cnormal_var_optimised_out], was emitted
   before the proxy existed). *)

[@@@ocaml.warning "-26-27-32"]

let[@inline never] [@local never] constant x =
  let k1 = 42 in
  let pair1 = (k1, x) in
  x + k1

let[@inline never] [@local never] consts x =
  let unused_triple = ("abc", 1.5, x) in
  x

let[@inline never] [@local never] dead_temp q x =
  let unused_pair = (fst q, x) in
  x

let[@inline never] [@local never] unrep x =
  let unused_arith = x * 3 in
  x

let[@inline never] [@local never] after_use q x =
  let a = fst q in
  ignore (Sys.opaque_identity a);
  let unused_pair = (a, x) in
  x
