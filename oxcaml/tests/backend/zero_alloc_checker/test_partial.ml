(* compiles: *)
module One : sig
  val f : int -> int [@@zero_alloc partial]
end = struct
  let f x = x + 1
end

(* compiles: *)
module Two : sig
  val f : int -> (int -> int) @ local [@@zero_alloc partial]
end = struct
  let f x y = x + y
end

(* compiles: *)
module Three : sig
  val f : int -> (int -> int -> int) @ local [@@zero_alloc partial]
end = struct
  let f x y z = x + y + z
end

(* compiles: *)
module Annotated : sig
  val f : int -> (int -> int) @ local [@@zero_alloc partial]
end = struct
  let[@zero_alloc partial] f x y = x + y
end

(* compiles: *)
module External : sig
  val f : local_ int -> int -> int [@@zero_alloc partial]
end = struct
  external f : local_ int -> int -> int = "test_partial_external"
  [@@noalloc]
end

(* compiles: *)
let[@zero_alloc] partial x = exclave_ Two.f x

(* compiles: *)
let[@zero_alloc] full x y = Two.f x y
