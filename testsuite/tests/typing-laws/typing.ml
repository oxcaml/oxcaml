(* TEST
 flags = "-extension laws";
 expect;
*)

(* A law in a structure is type checked. It becomes an item of its
   signature. *)

module M = struct
  let length = List.length
  law_? length_nonneg (xs : 'a list) : length xs >= 0
  law_? trivial : 1 = 1
end
[%%expect {|
module M :
  sig
    val length : 'a list -> int
    law_? length_nonneg (xs : 'a list) : (length xs) >= (0)
    law_? trivial : (1) = (1)
  end
|}]

module Bad = struct
  law_? ill_typed (x : bool) : 1 = x
end
[%%expect {|
Line 2, characters 35-36:
2 |   law_? ill_typed (x : bool) : 1 = x
                                       ^
Error: The value "x" has type "bool" but an expression was expected of type "int"
|}]

(* A law in a signature sees the preceding items. *)

module type S = sig
  type t
  val create : int -> t
  val to_int : t -> int
  law_? roundtrip (n : int) : n >= 0 implies_ to_int (create n) = n
end
[%%expect {|
module type S =
  sig
    type t
    val create : int -> t
    val to_int : t -> int
    law_? roundtrip (n : int) : n >= (0) implies_ (to_int (create n)) = n
  end
|}]

(* A law may have several assumptions. *)

law_? two_assumptions (n : int) (m : int) : n >= 0 implies_ m >= 0 implies_ n + m >= 0
[%%expect {|
law_? two_assumptions (n : int) (m : int) : n >= (0) implies_
  m >= (0) implies_ (n + m) >= (0)
|}]

(* The conclusion must have type [bool]. *)

law_? not_bool (xs : int list) : List.length xs
[%%expect {|
Line 1, characters 33-47:
1 | law_? not_bool (xs : int list) : List.length xs
                                     ^^^^^^^^^^^^^^
Error: This expression has type "int" but an expression was expected of type
         "bool"
|}]

(* So must every assumption. *)

law_? assumption_not_bool (xs : int list) : List.length xs implies_ 1 = 1
[%%expect {|
Line 1, characters 44-58:
1 | law_? assumption_not_bool (xs : int list) : List.length xs implies_ 1 = 1
                                                ^^^^^^^^^^^^^^
Error: This expression has type "int" but an expression was expected of type
         "bool"
|}]

(* A clause may only refer to the parameters and to the items in scope. *)

law_? unbound (xs : int list) : List.length ys = 0
[%%expect {|
Line 1, characters 44-46:
1 | law_? unbound (xs : int list) : List.length ys = 0
                                                ^^
Error: Unbound value "ys"
|}]

(* A parameter of a type that is not representable is rejected. *)

type any : any
law_? not_representable (x : any) : 1 = 1
[%%expect {|
type any : any
Line 2, characters 29-32:
2 | law_? not_representable (x : any) : 1 = 1
                                 ^^^
Error: The parameters of a law must be representable.
       The layout of any is any
         because of the definition of any at line 1, characters 0-14.
       But the layout of any must be representable
         because it's the type of a law parameter.
|}]

(* Every parameter must have a type annotation. (The reasoning here is that we
   want to use the typedtree type expressions for printing. See [printing.ml]
   for why we do not just print the inferred types.) *)

law_? foo_bar (x : int) y (z : int) : x + y = z
[%%expect {|
Line 1, characters 24-25:
1 | law_? foo_bar (x : int) y (z : int) : x + y = z
                            ^
Error: The law parameter "y" must have a type annotation.
|}]

(* The type annotation contains no holes [_]. *)

law_? wildcard (xs : _ list) : List.length xs >= 0
[%%expect {|
Line 1, characters 21-22:
1 | law_? wildcard (xs : _ list) : List.length xs >= 0
                         ^
Error: The type annotation of a law parameter cannot contain "_".
|}]

(* A type variable of an annotation stays polymorphic. *)

law_? refl (x : 'a) : x = x
[%%expect {|
law_? refl (x : 'a) : x = x
|}]

(* The clauses cannot instantiate a type variable of an annotation. (For the
   same printing reason, and because the law would be confusing to read.) *)

law_? instantiated (x : 'a) : x + 1 = x
[%%expect {|
Line 1, characters 24-26:
1 | law_? instantiated (x : 'a) : x + 1 = x
                            ^^
Error: The type variable "'a" of the law parameters stands for "int".
       Write that type in the annotation instead.
|}]

(* Nor can they unify two type variables. *)

law_? unified (x : 'a) (y : 'b) : x = y
[%%expect {|
Line 1, characters 28-30:
1 | law_? unified (x : 'a) (y : 'b) : x = y
                                ^^
Error: The type variable "'b" of the law parameters stands for "'a".
       Write that type in the annotation instead.
|}]

(* Nor can they refine the kind of a type variable. *)

let immutable (_ : ('a : immutable_data)) = true
law_? refined (x : 'a) : immutable x
[%%expect {|
val immutable : ('a : immutable_data). 'a -> bool = <fun>
Line 2, characters 19-21:
2 | law_? refined (x : 'a) : immutable x
                       ^^
Error: The type variable "'a" of the law parameters has the kind
       "immutable_data". Write that kind in the annotation instead.
|}]

(* Two parameters of a law cannot have the same name (because we generate record
   fields from them). *)

law_? dup (x : int) (x : int) : x = x
[%%expect {|
Line 1, characters 21-22:
1 | law_? dup (x : int) (x : int) : x = x
                         ^
Error: The law parameter "x" is bound several times.
|}]

(* A law cannot refer to a value that a later item of the structure
   shadows: the signature would have no way to refer to it. *)

module Shadowed = struct
  let f x = x
  law_? l (x : int) : f x = x
  let f x = x + 1
end
[%%expect {|
Line 4, characters 6-7:
4 |   let f x = x + 1
          ^
Error: Illegal shadowing of the value "f" used by a law.
Line 3, characters 2-29:
3 |   law_? l (x : int) : f x = x
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The law "l" refers to the value "f", so "f" must be accessible
  outside of this module.
|}]

(* Nor to a value introduced by [open struct], for the same reason. *)

module Hidden = struct
  open struct let helper x = x end
  law_? l (x : int) : helper x = x
end
[%%expect {|
Line 2, characters 2-34:
2 |   open struct let helper x = x end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "helper" introduced by this open is used by a law.
Line 3, characters 2-34:
3 |   law_? l (x : int) : helper x = x
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The law "l" refers to the value "helper", so "helper" must be accessible
  outside of this module.
|}]

(* The same holds for a law in a submodule. *)

module Shadowed_in_submodule = struct
  let f x = x
  module Inner = struct
    law_? l (x : int) : f x = x
  end
  let f x = x + 1
end
[%%expect {|
Line 6, characters 6-7:
6 |   let f x = x + 1
          ^
Error: Illegal shadowing of the value "f" used by a law.
Line 4, characters 4-31:
4 |     law_? l (x : int) : f x = x
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The law "l" refers to the value "f", so "f" must be accessible
  outside of this module.
|}]

(* And for a law in a module type. *)

module Shadowed_in_module_type = struct
  let f x = x
  module type S = sig
    law_? l (x : int) : f x = x
  end
  let f x = x + 1
end
[%%expect {|
Line 6, characters 6-7:
6 |   let f x = x + 1
          ^
Error: Illegal shadowing of the value "f" used by a law.
Line 4, characters 4-31:
4 |     law_? l (x : int) : f x = x
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The law "l" refers to the value "f", so "f" must be accessible
  outside of this module.
|}]

(* A recursive module cannot have laws, as it could claim the laws of its
   signature without stating them. *)

let g x = x
module type Unstated = sig law_? unstated (x : int) : g x = x + 1 end
[%%expect {|
val g : 'a -> 'a = <fun>
module type Unstated = sig law_? unstated (x : int) : (g x) = (x + (1)) end
|}]

module rec Unstated_alias : Unstated = Unstated_alias
[%%expect {|
Line 1, characters 28-36:
1 | module rec Unstated_alias : Unstated = Unstated_alias
                                ^^^^^^^^
Error: The recursive module "Unstated_alias" has the law "unstated".
       Laws are not allowed in recursive modules.
|}]

module rec Unstated_left : Unstated = struct
  include (Unstated_right : Unstated)
end
and Unstated_right : Unstated = struct
  include (Unstated_left : Unstated)
end
[%%expect {|
Line 1, characters 27-35:
1 | module rec Unstated_left : Unstated = struct
                               ^^^^^^^^
Error: The recursive module "Unstated_left" has the law "unstated".
       Laws are not allowed in recursive modules.
|}]

(* The laws of its submodules and of the results of its functors count too. *)

module rec Submodule : sig module N : Unstated end = Submodule
[%%expect {|
Line 1, characters 23-50:
1 | module rec Submodule : sig module N : Unstated end = Submodule
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The recursive module "Submodule" has the law "unstated".
       Laws are not allowed in recursive modules.
|}]

module rec Functor : sig module F (X : sig end) : Unstated end = Functor
[%%expect {|
Line 1, characters 21-62:
1 | module rec Functor : sig module F (X : sig end) : Unstated end = Functor
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The recursive module "Functor" has the law "unstated".
       Laws are not allowed in recursive modules.
|}]

(* Laws are disallowed in recursive modules in general, even if the module does
   state the law. *)

module rec Stated : sig
  law_? stated (x : int) : g x = x
end = struct
  law_? stated (x : int) : g x = x
end
[%%expect {|
Lines 1-3, characters 20-3:
1 | ....................sig
2 |   law_? stated (x : int) : g x = x
3 | end.........
Error: The recursive module "Stated" has the law "stated".
       Laws are not allowed in recursive modules.
|}]

(* In a recursive module, only the laws that it claims about its implementation
   via its signature are rejected. Laws are still allowed in module type
   definitions ([T] below) or as functor parameters ([X] below). They are also
   allowed in the implementation of the recursive module if they are not
   mentioned in the signature ([hidden] below). *)

module rec Allowed : sig
  module type T = Unstated
  module F (X : Unstated) : sig end
end = struct
  module type T = Unstated
  module F (X : Unstated) = struct end
end
[%%expect {|
module rec Allowed :
  sig
    module type T = Unstated
    module F : functor (X : Unstated) -> sig end
  end
|}]

module rec Hidden_law : sig val f : int -> int end = struct
  let f x = x
  law_? hidden (x : int) : f x = x
end
[%%expect {|
module rec Hidden_law : sig val f : int -> int end
|}]
