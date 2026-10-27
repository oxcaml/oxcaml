(* TEST
 flags = "-extension laws";
 expect;
*)

(* A law in a structure is type checked and becomes an item of its
   signature. *)

module M = struct
  let length = List.length
  law? length_nonneg (xs : 'a list) : length xs >= 0
  law? trivial : 1 = 1
end
[%%expect {|
module M :
  sig
    val length : 'a list -> int
    law? length_nonneg (xs : 'a list) : (length xs) >= (0)
    law? trivial : (1) = (1)
  end
|}]

(* A law in a signature sees the preceding items. *)

module type S = sig
  type t
  val create : int -> t
  val to_int : t -> int
  law? roundtrip (n : int) : n >= 0 ===> to_int (create n) = n
end
[%%expect {|
module type S =
  sig
    type t
    val create : int -> t
    val to_int : t -> int
    law? roundtrip (n : int) : n >= (0) ===> (to_int (create n)) = n
  end
|}]

(* A law may have several assumptions. *)

law? two_assumptions (n : int) (m : int) : n >= 0 ===> m >= 0 ===> n + m >= 0
[%%expect {|
law? two_assumptions (n : int) (m : int) : n >= (0) ===> m >= (0) ===>
  (n + m) >= (0)
|}]

(* The conclusion must have type [bool]. *)

law? not_bool (xs : int list) : List.length xs
[%%expect {|
Line 1, characters 32-46:
1 | law? not_bool (xs : int list) : List.length xs
                                    ^^^^^^^^^^^^^^
Error: This expression has type "int" but an expression was expected of type
         "bool"
|}]

(* So must every assumption. *)

law? assumption_not_bool (xs : int list) : List.length xs ===> 1 = 1
[%%expect {|
Line 1, characters 43-57:
1 | law? assumption_not_bool (xs : int list) : List.length xs ===> 1 = 1
                                               ^^^^^^^^^^^^^^
Error: This expression has type "int" but an expression was expected of type
         "bool"
|}]

(* A clause may only refer to the parameters and to the items in scope. *)

law? unbound (xs : int list) : List.length ys = 0
[%%expect {|
Line 1, characters 43-45:
1 | law? unbound (xs : int list) : List.length ys = 0
                                               ^^
Error: Unbound value "ys"
|}]

(* A parameter of a type that is not representable is rejected. *)

type any : any
law? not_representable (x : any) : 1 = 1
[%%expect {|
type any : any
Line 2, characters 28-31:
2 | law? not_representable (x : any) : 1 = 1
                                ^^^
Error: The parameters of a law must be representable.
       The layout of any is any
         because of the definition of any at line 1, characters 0-14.
       But the layout of any must be representable
         because it's the type of a law parameter.
|}]

(* Every parameter has a type annotation. *)

law? foo_bar (x : int) y (z : int) : x + y = z
[%%expect {|
Line 1, characters 23-24:
1 | law? foo_bar (x : int) y (z : int) : x + y = z
                           ^
Error: The law parameter "y" must have a type annotation.
|}]

(* The annotation names every type: it contains no [_]. *)

law? wildcard (xs : _ list) : List.length xs >= 0
[%%expect {|
Line 1, characters 20-21:
1 | law? wildcard (xs : _ list) : List.length xs >= 0
                        ^
Error: The type annotation of a law parameter cannot contain "_".
|}]

(* A type variable of an annotation stays polymorphic. *)

law? refl (x : 'a) : x = x
[%%expect {|
law? refl (x : 'a) : x = x
|}]

(* The clauses cannot instantiate a type variable of an annotation, *)

law? instantiated (x : 'a) : x + 1 = x
[%%expect {|
Line 1, characters 23-25:
1 | law? instantiated (x : 'a) : x + 1 = x
                           ^^
Error: The type variable "'a" of the law parameters stands for "int".
       Write that type in the annotation instead.
|}]

(* nor unify two of them. *)

law? unified (x : 'a) (y : 'b) : x = y
[%%expect {|
Line 1, characters 27-29:
1 | law? unified (x : 'a) (y : 'b) : x = y
                               ^^
Error: The type variable "'b" of the law parameters stands for "'a".
       Write that type in the annotation instead.
|}]

(* A variable bound in the annotation is not one of the law. *)

law? poly (o : < m : 'a. 'a -> 'a >) : 1 = 1
[%%expect {|
law? poly (o : < m : 'a. 'a -> 'a >) : (1) = (1)
|}]

(* Two parameters of a law cannot have the same name. *)

law? dup (x : int) (x : int) : x = x
[%%expect {|
Line 1, characters 20-21:
1 | law? dup (x : int) (x : int) : x = x
                        ^
Error: The law parameter "x" is bound several times.
|}]

(* A law cannot refer to a value that a later item of the structure
   shadows: the signature would have no way to refer to it. *)

module Shadowed = struct
  let f x = x
  law? l (x : int) : f x = x
  let f x = x + 1
end
[%%expect {|
Line 4, characters 6-7:
4 |   let f x = x + 1
          ^
Error: Illegal shadowing of the value "f" used by a law.
Line 3, characters 2-28:
3 |   law? l (x : int) : f x = x
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  The law "l" refers to the value "f", so "f" must be accessible
  outside of this module.
|}]

(* Nor to a value introduced by [open struct], for the same reason. *)

module Hidden = struct
  open struct let helper x = x end
  law? l (x : int) : helper x = x
end
[%%expect {|
Line 2, characters 2-34:
2 |   open struct let helper x = x end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "helper" introduced by this open is used by a law.
Line 3, characters 2-33:
3 |   law? l (x : int) : helper x = x
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The law "l" refers to the value "helper", so "helper" must be accessible
  outside of this module.
|}]

(* The same holds for a law in a submodule, which the error locates. *)

module Shadowed_in_submodule = struct
  let f x = x
  module Inner = struct
    law? l (x : int) : f x = x
  end
  let f x = x + 1
end
[%%expect {|
Line 6, characters 6-7:
6 |   let f x = x + 1
          ^
Error: Illegal shadowing of the value "f" used by a law.
Line 4, characters 4-30:
4 |     law? l (x : int) : f x = x
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
  The law "l" refers to the value "f", so "f" must be accessible
  outside of this module.
|}]

(* And for a law in a module type. *)

module Shadowed_in_module_type = struct
  let f x = x
  module type S = sig
    law? l (x : int) : f x = x
  end
  let f x = x + 1
end
[%%expect {|
Line 6, characters 6-7:
6 |   let f x = x + 1
          ^
Error: Illegal shadowing of the value "f" used by a law.
Line 4, characters 4-30:
4 |     law? l (x : int) : f x = x
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
  The law "l" refers to the value "f", so "f" must be accessible
  outside of this module.
|}]

(* Each clause is scoped on its own: a variable bound by a [let] in an
   assumption is not in scope in the conclusion. *)

law? scoping (n : int) : let m = n + 1 in m > n ===> m = n + 1
[%%expect {|
Line 1, characters 53-54:
1 | law? scoping (n : int) : let m = n + 1 in m > n ===> m = n + 1
                                                         ^
Error: Unbound value "m"
|}]

(* Laws in generative functors are rejected: the laws file names the types
   of the result of a functor through an application to the choices of a
   backend, which a generative functor does not permit. *)

module Gen () = struct
  let x = 1
  law? p : x = x
end
[%%expect {|
Line 3, characters 2-16:
3 |   law? p : x = x
      ^^^^^^^^^^^^^^
Error: Laws in the body of a generative functor are not supported.
|}]

module type Gen = functor () -> sig
  val x : int
  law? p : x = x
end
[%%expect {|
Line 3, characters 2-16:
3 |   law? p : x = x
      ^^^^^^^^^^^^^^
Error: Laws in the body of a generative functor are not supported.
|}]

module Nested () = struct
  module M = struct
    law? p : 1 = 1
  end
end
[%%expect {|
Line 3, characters 4-18:
3 |     law? p : 1 = 1
        ^^^^^^^^^^^^^^
Error: Laws in the body of a generative functor are not supported.
|}]
