(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3 -flambda2-result-types-functors-and-closures";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* The closure returned by [make_counter] captures a reference allocated in
   its body, so it cannot be statically allocated at a call site; with result
   types computed for all functions returning closures, the call site still
   learns the code of the closure, and [next ()] is a direct call (inlined
   here) rather than an indirect one. *)

let[@inline never] make_counter n =
  let r = ref n in
  fun[@inline always] () ->
    incr r;
    !r

let f () =
  let next = make_counter 0 in
  next () + next ()
