module type S = sig
  type t
  type u = t list
  module N : sig
    type v = t
    type w
    val use : v -> w -> u
  end
end

module type T = S with type t = int
module type U = T with type N.w = string

module type Record = sig
  type t
  type r = { field : t }
end
module type Int_record = Record with type t = int
module R : Int_record
module type Refined_record =
  Int_record with type r = R.r

module F (X : sig type t end) : sig
  module type Result = S with type t = X.t
end

module type M = sig
  module A : sig type t end
  module B : sig type t = A.t end
end
module type MT = sig module type T module A : T end

module type Recursive = sig
  type t = Leaf | Node of t list
end
module Recursive_impl : Recursive
module type Recursive_alias = Recursive with type t = Recursive_impl.t

module type Chain_base = sig
  type t
  type u = t
  module N : sig type v = u end
end
module type Chain =
  Chain_base with type t = int and type u = int and type N.v = int

module type Shadow_rhs = sig
  type t
  module M : sig type key type 'a t end with type key = t
end
