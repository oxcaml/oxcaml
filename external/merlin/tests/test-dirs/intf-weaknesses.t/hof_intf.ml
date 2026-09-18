module type S = sig
  val app : ('a -> 'b) -> 'a -> unit (* trailing comment inside the sig *)
end
