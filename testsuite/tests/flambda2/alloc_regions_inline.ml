(* TEST
 compile_only = "true";
 ocamlopt_flags = "-O3";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
*)

(* Inlining binds the callee's allocation region to a [New_alloc_region] whose
   actions are all [Transfer]; such regions should be removed by [Simplify]. *)

let[@inline always] add1 x = x + 1

let[@inline always] add2 x = add1 (add1 x)

let[@zero_alloc] chain t = add2 (add2 t)

(* Inlining a function which has its own zero_alloc region: the inner
   [New_alloc_region] is not removed, since its actions are not all
   [Transfer]. *)
let[@zero_alloc] [@inline always] checked_add1 x = x + 1

let call_checked y = checked_add1 (checked_add1 y)

let[@zero_alloc assume] tail_call_checked y = checked_add1 y
