(* TEST
 flags = "-extension laws";
 expect;
*)

(* The values of a structure including a module are recorded to be those
   of the module: the included laws, which refer to them, are about the
   items of the structure, and the structure matches a signature stating
   them, whether through its own items or through the included module. *)

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
module Included_checked' :
  sig val f : int -> int law? id (x : int) : (I.f x) = x end
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

(* The same for laws in a submodule or a module type of the included
   module, which are included as an alias and a path. *)

module I_nested = struct
  let f x = x
  module N = struct
    law? l (x : int) : f x = x
  end
end
module Included_nested = struct include I_nested end
module Included_nested_checked : sig
  val f : int -> int
  module N : sig
    law? l (x : int) : f x = x
  end
end = Included_nested
[%%expect {|
module I_nested :
  sig val f : 'a -> 'a module N : sig law? l (x : int) : (f x) = x end end
module Included_nested : sig val f : 'a -> 'a module N = I_nested.N end
module Included_nested_checked :
  sig val f : int -> int module N : sig law? l (x : int) : (f x) = x end end
|}]

module I_modtype = struct
  exception E
  module type S = sig
    law? l : (try raise E with E -> true)
  end
end
module Included_modtype = struct include I_modtype end
module Included_modtype_checked : sig
  exception E
  module type S = sig
    law? l : (try raise E with E -> true)
  end
end = Included_modtype
[%%expect {|
module I_modtype :
  sig
    exception E
    module type S = sig law? l : (try raise E with | E -> true) end
  end
module Included_modtype : sig exception E module type S = I_modtype.S end
module Included_modtype_checked :
  sig
    exception E
    module type S = sig law? l : (try raise E with | E -> true) end
  end
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
