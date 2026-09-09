(* A small library inlined into [stability.ml]. [stability_lib_edited.ml] is
   the same library with a comment and an unrelated function added at the
   top; see [stability.sh]. *)

let[@inline always] make_adder x = fun[@cold] y -> x + y

let[@inline never] twice f x = f (f x)

let[@inline never] scale_all k xs = List.map (fun[@cold] x -> k * x) xs
