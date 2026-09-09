(* Same as [stability_lib.ml], except for this comment and the
   [unrelated_addition] function: neither may change the demangled name of
   anything else, here or in the units this library is inlined into. *)

let[@inline never] unrelated_addition x = Sys.opaque_identity (x + 99)

let[@inline always] make_adder x = fun[@cold] y -> x + y

let[@inline never] twice f x = f (f x)

let[@inline never] scale_all k xs = List.map (fun[@cold] x -> k * x) xs
