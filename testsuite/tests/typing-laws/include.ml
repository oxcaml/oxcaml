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
module Included_checked'' : Id = struct include I end
[%%expect {|
module type Id = sig val f : int -> int law? id (x : int) : (f x) = x end
module I : Id
module Included : sig val f : int -> int law? id (x : int) : (f x) = x end
module Included_checked : Id
module Included_checked'' : Id
|}]

module Included_checked' : sig
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
       The clauses of the laws differ.
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
  The law "id" refers to the value "f".
|}]

(* Laws in a submodule or a module type of a module cannot refer to its
   values (see paths.ml), so neither can those of an included module. *)

module I_nested = struct
  let f x = x
  module N = struct
    law? l (x : int) : f x = x
  end
end
[%%expect {|
Line 1:
Error: In module "I_nested":
       Modules do not match:
         sig val f : 'a -> 'a module N = I_nested.N end
       is not included in
         sig
           val f : 'a -> 'a
           module N : sig law? l (x : int) : (f x) = x end
         end
       In module "I_nested.N":
       Modules do not match:
         sig law? l (x : int) : (I_nested.f x) = x end
       is not included in
         sig law? l (x : int) : (f x) = x end
       In module "I_nested.N":
       Laws do not match:
         law? l (x : int) : (I_nested.f x) = x
       is not included in
         law? l (x : int) : (f x) = x
       The first refers to "I_nested.f" where the second refers to "f". The laws
       of a module referred to by a path cannot be compared with the laws of
       a signature.
|}]

module I_modtype = struct
  exception E
  module type S = sig
    law? l : (try raise E with E -> true)
  end
end
[%%expect {|
Line 1:
Error: In module "I_modtype":
       Modules do not match:
         sig exception E module type S = I_modtype.S end
       is not included in
         sig
           exception E
           module type S = sig law? l : (try raise E with | E -> true) end
         end
       In module "I_modtype":
       Module type declarations do not match:
         module type S = I_modtype.S
       does not match
         module type S = sig law? l : (try raise E with | E -> true) end
       At position "module I_modtype : sig module type S = <here> end"
       Module types do not match:
         I_modtype.S
       is not equal to
         sig law? l : (try raise E with | E -> true) end
       At position "module I_modtype : sig module type S = <here> end"
       Laws do not match:
         law? l : (try raise I_modtype.E with | I_modtype.E -> true)
       is not included in
         law? l : (try raise E with | E -> true)
       The first refers to "I_modtype.E" where the second refers to "E". The laws
       of a module referred to by a path cannot be compared with the laws of
       a signature.
|}]

(* The same holds when including the application of a functor to a path,
   or a functor parameter. *)

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

(* Laws referring to the items of submodules are included too; the
   submodules are included as aliases. *)

module I_other = struct
  let f x = x
  module N = struct let g x = x end
  law? pos : 1 > 0
  module K = struct
    law? k (x : int) : N.g x = x
  end
end
module Included_other = struct include I_other end
module Checked : sig
  val f : int -> int
  module N : sig val g : int -> int end
  law? pos : 1 > 0
  module K : sig
    law? k (x : int) : N.g x = x
  end
end = Included_other
[%%expect {|
module I_other :
  sig
    val f : 'a -> 'a
    module N : sig val g : 'a -> 'a end
    law? pos : 1 > 0
    module K : sig law? k (x : int) : (N.g x) = x end
  end
module Included_other :
  sig
    val f : 'a -> 'a
    module N = I_other.N
    law? pos : 1 > 0
    module K = I_other.K
  end
module Checked :
  sig
    val f : int -> int
    module N : sig val g : int -> int end
    law? pos : 1 > 0
    module K : sig law? k (x : int) : (N.g x) = x end
  end
|}]

(* Including a module expression that is not a path: its signature is not
   strengthened, so its laws refer to the included items themselves. *)

module Included_structure : Id = struct
  include struct
    let f x = x
    law? id (x : int) : f x = x
  end
end
module Included_constrained : Id = struct include (I : Id) end
module Included_application' : Id = struct include F (struct end) end
[%%expect {|
module Included_structure : Id
module Included_constrained : Id
module Included_application' : Id
|}]
