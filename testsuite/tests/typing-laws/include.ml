(* TEST
 flags = "-extension laws";
 expect;
*)

(* Including a module includes its laws. An included law becomes a law about the
   values of the surrounding structure, because it refers to the included values
   by name. In particular, [Included] below matches [Id] (as opposed to
   referring to [I.f] in its laws). *)

module type Id = sig
  val f : int -> int
  law_? id (x : int) : f x = x
end
module I : Id = struct
  let f x = x
  law_? id (x : int) : f x = x
end
module Included = struct include I end
module Included_checked : Id = Included
module Included_checked' : Id = struct include I end
[%%expect {|
module type Id = sig val f : int -> int law_? id (x : int) : (f x) = x end
module I : Id
module Included : sig val f : int -> int law_? id (x : int) : (f x) = x end
module Included_checked : Id
module Included_checked' : Id
|}]

(* [Included.f] is not known to equal [I.f], so [Included] cannot be used to
   implement a law about [I.f]. *)

module Included_checked'' : sig
  val f : int -> int
  law_? id (x : int) : I.f x = x
end = Included
[%%expect {|
Line 4, characters 6-14:
4 | end = Included
          ^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int law_? id (x : int) : (f x) = x end
       is not included in
         sig val f : int -> int law_? id (x : int) : (I.f x) = x end
       Laws do not match:
         law_? id (x : int) : (f x) = x
       is not included in
         law_? id (x : int) : (I.f x) = x
       The clauses of the two laws differ: "(f x) = x" is not the same as
       "(I.f x) = x".
|}]

(* As for any law, shadowing a value that an included law refers to is an
   error, because it would change which value the law refers to. *)

module Included_shadowed = struct
  include I
  let f x = x + 1
end
[%%expect {|
Line 3, characters 6-7:
3 |   let f x = x + 1
          ^
Error: Illegal shadowing of the value "f" used by a law.
Line 3, characters 2-30:
3 |   law_? id (x : int) : f x = x
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The law "id" refers to the value "f", so "f" must be accessible
  outside of this module.
|}]

(* Including a functor application or a functor parameter works. *)

module F (X : sig end) : Id = struct
  let f x = x
  law_? id (x : int) : f x = x
end
module E = struct end
module Included_application : Id = struct include F (E) end
module Included_parameter (X : Id) : Id = struct include X end
[%%expect {|
module F : functor (X : sig end) -> Id
module E : sig end
module Included_application : Id
module Included_parameter : functor (X : Id) -> Id
|}]

(* Inclusion also works when a law refers to a value of a submodule. *)

module Outer = struct
  module N = struct let g x = x end
  law_? k (x : int) : N.g x = x
end
module Included_outer = struct include Outer end
module Checked : sig
  module N : sig val g : int -> int end
  law_? k (x : int) : N.g x = x
end = Included_outer
[%%expect {|
module Outer :
  sig module N : sig val g : 'a -> 'a end law_? k (x : int) : (N.g x) = x end
module Included_outer :
  sig module N = Outer.N law_? k (x : int) : (N.g x) = x end
module Checked :
  sig
    module N : sig val g : int -> int end
    law_? k (x : int) : (N.g x) = x
  end
|}]
