type v = A | B of int

type r = { f : int; g : string }

type u = #{ h : int }

type i = C of { l : int }

exception E of { x : int }

type o = ..

type o += K of { y : int }

type p = private P1 | P2 of { pl : int }

type abs

module type S = sig
  type t = D | E of { el : int }
end

module F : functor (X : sig type t = FA end) -> sig end
