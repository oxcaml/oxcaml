(* Re-export the synthetic slots while the callback and numbers are unknown. *)
let[@inline] captured f x y n =
  Specialise_lifted_numeric_lib.captured f x y n

let[@inline] boxed f x y n =
  Specialise_lifted_numeric_lib.boxed f x y n
