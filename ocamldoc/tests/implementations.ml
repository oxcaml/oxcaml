module type Base = sig
  type t
  val get : t -> t
end

module type Inline = sig
  type t
  val get : t -> t
end with type t = int

module M : sig type t val get : t -> t end with type t = int = struct
  type t = int
  let get x = x
end

module F (X : sig type t end with type t = int) = struct
  module type Result = Base with type t = X.t
end

module G (X : sig module type S = Base end) = struct
  module type Result = X.S with type t = int
end
