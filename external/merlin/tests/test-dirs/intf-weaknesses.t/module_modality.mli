type t : immutable_data

module (Foo @@ stateless) : sig
  val f : t -> t
end
