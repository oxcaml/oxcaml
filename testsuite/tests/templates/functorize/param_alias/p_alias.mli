module A : sig
  val x : int
end

module B = A

val after_alias : unit -> int
