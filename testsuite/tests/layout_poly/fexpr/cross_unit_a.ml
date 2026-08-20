(* Auxiliary module for cross_unit_b.ml. [bump] is [@inline never] so that
   the instantiating unit keeps the specialized code, the closure built from
   the imported environment block, and direct calls to it. *)

let r = ref 0

let[@inline never] poly_ bump x =
  incr r;
  x
