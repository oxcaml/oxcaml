module type Arg = sig type t end
module F (A : Arg) (B : Arg) (X : Arg) : sig
  module type S = sig type t type u = X.t end
end
module type S = sig
  module A : Arg
  module B : Arg
  module X : Arg
  module N : F(A)(B)(X).S with type t = int
end

module type Strengthened = sig
  module type R = sig type t type u end
  module Target : R
  module N : (R with Target) with type u = Target.u
end

module type Module_constraint = sig
  module type R = sig type t val id : t -> t end
  module Target : R
  module type Inner = sig module P : R end
  module N : Inner with module P = Target
end

module type Modtype_constraint = sig
  module type R = sig type t end
  module Target : R
  module type Inner = sig module type P end
  module N : Inner with module type P = (R with Target)
end
