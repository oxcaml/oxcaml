(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3 -dcmm -dcanonical-ids";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* to_cmm formalism validation: a mutually-recursive set of closures capturing an
   immediate (int) shows that an UNSCANNED value slot sits BELOW startenv, among
   the function slots -- not at offset >= startenv. This drove a correction to
   R.Obj.Closures (ch. 17), whose value-slot clause is now split on is_scanned.
   Rules: R.Obj.Closures, R.Val.Clos (ch. 17), TC.Prim.ProjectFunctionSlot (ch. 18).
   Case study: middle_end/flambda2/docs/formalism/14-validation/tocmm-06-closure-unscanned.md
   Phenomenon: the set-of-closures block is [odd_code; odd_closinfo; infix(3);
   even_code; even_closinfo; n], startenv = 6, and the captured n is at word 5. *)

let[@inline never] make n =
  let rec even x = if x = 0 then n = 0 else odd (x - 1)
  and odd x = if x = 0 then n <> 0 else even (x - 1) in
  (even, odd)
