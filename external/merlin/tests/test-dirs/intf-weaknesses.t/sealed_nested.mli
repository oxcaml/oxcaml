module Shadowed : sig
  module Tree : sig
    type t

    val id : t -> t
  end
end

module Tree : sig
  type t

  val id : t -> t
end
