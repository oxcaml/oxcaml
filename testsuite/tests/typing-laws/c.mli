(* A unit with the name of the parameter [C] of the functor [Laws] of
   generated files. *)
val f : int -> int
law? identity (x : int) : f x = x
