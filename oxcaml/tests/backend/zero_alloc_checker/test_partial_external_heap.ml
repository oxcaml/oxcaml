(* fails to compile: *)
module External : sig
  val f : int -> int -> int [@@zero_alloc partial]
end = struct
  external f : int -> int -> int = "test_partial_external_heap" [@@noalloc]
end
