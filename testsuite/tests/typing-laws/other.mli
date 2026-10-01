(* A module type with a law, from another unit. *)
module type S = sig
  val f : int -> int
  law? idem (x : int) : f (f x) = f x
end
