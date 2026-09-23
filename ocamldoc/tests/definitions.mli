module type Base = sig
  type t
  val get : t -> t
end

module type Inline = sig
  type t
  val get : t -> t
end with type t = int

module type Named = Base with type t = int

module type Nested = sig
  module N : Base
end with type N.t = int

module type Payload = sig
  module type S
end with module type S = (Base with type t = int)

module F (X : sig module type S = Base end) : sig
  module type Result = X.S with type t = int
end

module G (X : Base with type t = int) : Base
