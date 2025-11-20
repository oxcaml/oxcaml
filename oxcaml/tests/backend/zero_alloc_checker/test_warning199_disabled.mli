module A : sig
  type t

  val r : exn

  val foo : t -> t [@@zero_alloc]
end
