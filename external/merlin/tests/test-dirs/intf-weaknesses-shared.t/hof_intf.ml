module type S = sig
  val app : ('a -> 'b) -> 'a -> unit
end
