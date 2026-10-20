(* TEST
 flags = "-extension laws";
 expect;
*)

(* The laws of an included module refer to its values by name, as its
   signature binds them, and so become laws about the items of the
   structure that rebinds them. The structure matches a signature stating
   them through its own items, but [Included.f] is not known to be [I.f]. *)

module type Id = sig
  val f : int -> int
  law? id (x : int) : f x = x
end
module I : Id = struct
  let f x = x
  law? id (x : int) : f x = x
end
module Included = struct include I end
module Included_checked : Id = Included
module Included_checked' : Id = struct include I end
[%%expect {|
module type Id = sig val f : int -> int law? id (x : int) : (f x) = x end
module I : Id
module Included : sig val f : int -> int law? id (x : int) : (f x) = x end
module Included_checked : Id
module Included_checked' : Id
|}]

module Included_checked'' : sig
  val f : int -> int
  law? id (x : int) : I.f x = x
end = Included
[%%expect {|
Line 4, characters 6-14:
4 | end = Included
          ^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int law? id (x : int) : (f x) = x end
       is not included in
         sig val f : int -> int law? id (x : int) : (I.f x) = x end
       Laws do not match:
         law? id (x : int) : (f x) = x
       is not included in
         law? id (x : int) : (I.f x) = x
       The clauses of the two laws differ.
|}]

(* Shadowing an included value that an included law refers to is an
   error, as for any law. *)

module Included_shadowed = struct
  include I
  let f x = x + 1
end
[%%expect {|
Line 3, characters 6-7:
3 |   let f x = x + 1
          ^
Error: Illegal shadowing of the value "f" used by a law.
Line 3, characters 2-29:
3 |   law? id (x : int) : f x = x
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The law "id" refers to the value "f", so "f" must be accessible
  outside of this module.
|}]

(* Functor inclusion works *)

module F (X : sig end) : Id = struct
  let f x = x
  law? id (x : int) : f x = x
end
module E = struct end
module Included_application : Id = struct include F (E) end
module G (X : Id) : Id = struct include X end
[%%expect {|
module F : functor (X : sig end) -> Id
module E : sig end
module Included_application : Id
module G : functor (X : Id) -> Id
|}]

(* The submodules of an included module are included as aliases. A law
   that refers to a value through a submodule still matches: [K.k] refers
   to [N.g], and [Included_outer.N] is an alias of [Outer.N], so both
   sides lead to [Outer.N.g]. Only a value referred to by its bare name is
   not related in this way (see paths.ml). *)

module Outer = struct
  module N = struct let g x = x end
  module K = struct law? k (x : int) : N.g x = x end
end
module Included_outer = struct include Outer end
module Checked : sig
  module N : sig val g : int -> int end
  module K : sig law? k (x : int) : N.g x = x end
end = Included_outer
[%%expect {|
module Outer :
  sig
    module N : sig val g : 'a -> 'a end
    module K : sig law? k (x : int) : (N.g x) = x end
  end
module Included_outer : sig module N = Outer.N module K = Outer.K end
module Checked :
  sig
    module N : sig val g : int -> int end
    module K : sig law? k (x : int) : (N.g x) = x end
  end
|}]
