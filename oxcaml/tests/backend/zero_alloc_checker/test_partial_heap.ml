(* fails to compile: *)
module Two : sig
  val f : int -> int -> int [@@zero_alloc partial]
end = struct
  let f x y = x + y
end
