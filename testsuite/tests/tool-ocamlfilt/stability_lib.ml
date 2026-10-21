(* A small library inlined into [stability.ml]. [stability_lib_edited.ml] is
   the same library with a comment and an unrelated function added at the
   top; see [stability.sh]. *)

let[@inline always] make_adder x = fun[@cold] y -> x + y

let[@inline never] twice f x = f (f x)

let[@inline never] scale_all k xs = List.map (fun[@cold] x -> k * x) xs

(* Partially applied by an [@inline always] wrapper, so that the wrapper's
   closure, a partial application of a function of this unit, is copied into
   the units that inline [bind_a]; see the copies check in [stability.sh]. *)
let[@inline never] subtract ~a ~b = a - b

let[@inline always] bind_a x = subtract ~a:x
