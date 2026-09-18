module Sealed : sig
  type t

  val id : t -> t
end

module Alias : sig
  type t

  val id : t -> t
end

module Combined : sig
  type t

  val id : t -> t
  val use : t -> unit
end
