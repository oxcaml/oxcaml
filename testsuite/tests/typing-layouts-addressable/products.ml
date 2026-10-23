(* TEST
 expect;
*)

(* [&] makes its components addressable: [k1 & k2] is [k1 addressable & k2
   addressable]. *)

(**** [addressable] on a component or on a product is redundant ****)

type t : bits8 addressable & bits16
[%%expect{|
type t : bits8 addressable & bits16
|}]

type t : (bits8 & bits16) addressable
[%%expect{|
type t : (bits8 & bits16) addressable
|}]

type t : (bits8 & bits16) addressable mod portable
[%%expect{|
type t : (bits8 & bits16) addressable mod portable
|}]

(**** Equalities ****)

module M : sig
  type t : bits8 & bits16
end = struct
  type t : bits8 addressable & bits16 addressable
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : bits8 addressable & bits16 addressable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits8 addressable & bits16 addressable end
       is not included in
         sig type t : bits8 & bits16 end
       Type declarations do not match:
         type t : bits8 addressable & bits16 addressable
       is not included in
         type t : bits8 & bits16
       The layout of the first is bits8 addressable & bits16 addressable
         because of the definition of t at line 4, characters 2-49.
       But the layout of the first must be a sublayout of bits8 & bits16
         because of the definition of t at line 2, characters 2-25.
|}]

module M : sig
  type t : bits8 addressable & bits16 addressable
end = struct
  type t : bits8 & bits16
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : bits8 & bits16
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits8 & bits16 end
       is not included in
         sig type t : bits8 addressable & bits16 addressable end
       Type declarations do not match:
         type t : bits8 & bits16
       is not included in
         type t : bits8 addressable & bits16 addressable
       The layout of the first is bits8 & bits16
         because of the definition of t at line 4, characters 2-25.
       But the layout of the first must be a sublayout of
           bits8 addressable & bits16 addressable
         because of the definition of t at line 2, characters 2-49.
|}]

module M : sig
  type t : (bits8 & bits16) addressable
end = struct
  type t : bits8 & bits16
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : bits8 & bits16
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits8 & bits16 end
       is not included in
         sig type t : (bits8 & bits16) addressable end
       Type declarations do not match:
         type t : bits8 & bits16
       is not included in
         type t : (bits8 & bits16) addressable
       The layout of the first is bits8 & bits16
         because of the definition of t at line 4, characters 2-25.
       But the layout of the first must be a sublayout of
           (bits8 & bits16) addressable
         because of the definition of t at line 2, characters 2-39.
|}]

module M : sig
  type t : bits8 & bits16
end = struct
  type t : (bits8 & bits16) addressable
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : (bits8 & bits16) addressable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : (bits8 & bits16) addressable end
       is not included in
         sig type t : bits8 & bits16 end
       Type declarations do not match:
         type t : (bits8 & bits16) addressable
       is not included in
         type t : bits8 & bits16
       The layout of the first is (bits8 & bits16) addressable
         because of the definition of t at line 4, characters 2-39.
       But the layout of the first must be a sublayout of bits8 & bits16
         because of the definition of t at line 2, characters 2-25.
|}]

(* The components must still agree *)
module M : sig
  type t : bits8 & bits16
end = struct
  type t : bits8 & bits8
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : bits8 & bits8
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits8 & bits8 end
       is not included in
         sig type t : bits8 & bits16 end
       Type declarations do not match:
         type t : bits8 & bits8
       is not included in
         type t : bits8 & bits16
       The layout of the first is bits8 & bits8
         because of the definition of t at line 4, characters 2-24.
       But the layout of the first must be a sublayout of bits8 & bits16
         because of the definition of t at line 2, characters 2-25.
|}]

(**** Products are addressable ****)

type ('a : any addressable) req
[%%expect{|
type ('a : any addressable) req
|}]

type t : bits8 & bits16
type ok = t req
[%%expect{|
type t : bits8 & bits16
Line 2, characters 10-11:
2 | type ok = t req
              ^
Error: This type "t" should be an instance of type "('a : any addressable)"
       The layout of t is bits8 & bits16
         because of the definition of t at line 1, characters 0-23.
       But the layout of t must be a sublayout of any addressable
         because of the definition of req at line 1, characters 0-31.
|}]

type t : float64 & value
type ok = t req
[%%expect{|
type t : float64 & value
Line 2, characters 10-11:
2 | type ok = t req
              ^
Error: This type "t" should be an instance of type "('a : any addressable)"
       The layout of t is float64 & value
         because of the definition of t at line 1, characters 0-24.
       But the layout of t must be a sublayout of any addressable
         because of the definition of req at line 1, characters 0-31.
|}]

type ok = #(float# * string) req
[%%expect{|
Line 1, characters 10-28:
1 | type ok = #(float# * string) req
              ^^^^^^^^^^^^^^^^^^
Error: This type "#(float# * string)" should be an instance of type
         "('a : any addressable)"
       The layout of #(float# * string) is float64 & value non_float
         because it is an unboxed tuple.
       But the layout of #(float# * string) must be a sublayout of
           any addressable
         because of the definition of req at line 1, characters 0-31.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

type t : any & any
type ok = t req
[%%expect{|
type t : any & any
Line 2, characters 10-11:
2 | type ok = t req
              ^
Error: This type "t" should be an instance of type "('a : any addressable)"
       The layout of t is any & any
         because of the definition of t at line 1, characters 0-18.
       But the layout of t must be a sublayout of any addressable
         because of the definition of req at line 1, characters 0-31.
|}]

module M : sig
  type t : any addressable
end = struct
  type t : any & any
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : any & any
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : any & any end
       is not included in
         sig type t : any addressable end
       Type declarations do not match:
         type t : any & any
       is not included in
         type t : any addressable
       The layout of the first is any & any
         because of the definition of t at line 4, characters 2-20.
       But the layout of the first must be a sublayout of any addressable
         because of the definition of t at line 2, characters 2-26.
|}]

(**** Unboxed records and tuples ****)

type t : bits8 addressable & bits16 addressable = #{ a : int8#; b : int16# }
[%%expect{|
Line 1, characters 0-76:
1 | type t : bits8 addressable & bits16 addressable = #{ a : int8#; b : int16# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is bits8 & bits16
         because it is an unboxed record.
       But the layout of type "t" must be a sublayout of
           bits8 addressable & bits16 addressable
         because of the annotation on the declaration of the type t.
|}]

type t : (bits8 & bits16) addressable = #{ a : int8#; b : int16# }
[%%expect{|
Line 1, characters 0-66:
1 | type t : (bits8 & bits16) addressable = #{ a : int8#; b : int16# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is bits8 & bits16
         because it is an unboxed record.
       But the layout of type "t" must be a sublayout of
           (bits8 & bits16) addressable
         because of the annotation on the declaration of the type t.
|}]

(* Projecting out of a product gives the field's own kind *)
type t = #{ a : int8#; b : int16# }
let f (x : t) : ('a : bits8) = x.#a
[%%expect{|
type t = #{ a : int8#; b : int16#; }
val f : t -> int8# = <fun>
|}]

(* A made-addressable component is printed as the plain component *)
type ('a : bits8 addressable) t = #{ a : 'a; b : int }
type ('a : bits8) u = #{ a : 'a; b : int }
module M : sig
  type ('a : bits8 addressable) t : bits8 & value
  type ('a : bits8) u : bits8 addressable & value
end = struct
  type ('a : bits8 addressable) t = #{ a : 'a; b : int }
  type ('a : bits8) u = #{ a : 'a; b : int }
end
[%%expect{|
type ('a : bits8 addressable) t = #{ a : 'a; b : int; }
type ('a : bits8) u = #{ a : 'a; b : int; }
Lines 6-9, characters 6-3:
6 | ......struct
7 |   type ('a : bits8 addressable) t = #{ a : 'a; b : int }
8 |   type ('a : bits8) u = #{ a : 'a; b : int }
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type ('a : bits8 addressable) t = #{ a : 'a; b : int; }
           type ('a : bits8) u = #{ a : 'a; b : int; }
         end
       is not included in
         sig
           type ('a : bits8 addressable) t : bits8 & value
           type ('a : bits8) u : bits8 addressable & value
         end
       Type declarations do not match:
         type ('a : bits8 addressable) t = #{ a : 'a; b : int; }
       is not included in
         type ('a : bits8 addressable) t : bits8 & value
       The layout of the first is bits8 addressable & value non_pointer
         because of the definition of t at line 7, characters 2-56.
       But the layout of the first must be a sublayout of bits8 & value
         because of the definition of t at line 4, characters 2-49.
       Note: The layout of immediate is value non_pointer.
|}]

(* Plain and made-addressable components both fit a product kind *)
let f (x : ('a : bits8)) : ('b : bits8 & value) = #(x, 1)
[%%expect{|
val f : ('a : bits8). 'a -> #('a * int) = <fun>
|}]

let f (x : ('a : bits8)) : ('b : bits8 addressable & value) = #(x, 1)
[%%expect{|
Line 1, characters 64-65:
1 | let f (x : ('a : bits8)) : ('b : bits8 addressable & value) = #(x, 1)
                                                                    ^
Error: The value "x" has type "('a : bits8)"
       but an expression was expected of type "('b : bits8 addressable)"
       The layout of 'a is bits8 addressable
         because of the annotation on the type variable 'b.
       But the layout of 'a must overlap with bits8
         because of the annotation on the type variable 'a.
|}]

let f (x : ('a : bits8 addressable)) : #('a * int) = #(x, 1)
[%%expect{|
val f : ('a : bits8 addressable). 'a -> #('a * int) = <fun>
|}]

let f (x : ('a : bits8 addressable)) : ('b : bits8 & value) =
  (#(x, 1) : #('a * int))
[%%expect{|
Line 2, characters 2-25:
2 |   (#(x, 1) : #('a * int))
      ^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "#('a * int)"
       but an expression was expected of type "('b : bits8 & value)"
       The layout of #('a * int) is bits8 addressable & value non_pointer
         because it is an unboxed tuple.
       But the layout of #('a * int) must be a sublayout of bits8 & value
         because of the annotation on the type variable 'b.
       Note: The layout of immediate is value non_pointer.
|}]

(* Known incompleteness: a type variable whose kind is inferred from a product
   kind gets the plain kind (here, the tuple's component variables are created
   before [x] is checked against them), after which a made-addressable type no
   longer fits. *)
let f (x : ('a : bits8 addressable)) : ('b : bits8 & value) = #(x, 1)
[%%expect{|
Line 1, characters 64-65:
1 | let f (x : ('a : bits8 addressable)) : ('b : bits8 & value) = #(x, 1)
                                                                    ^
Error: The value "x" has type "('a : bits8 addressable)"
       but an expression was expected of type "('b : bits8)"
       The layout of 'a is bits8
         because of the annotation on the type variable 'b.
       But the layout of 'a must overlap with bits8 addressable
         because of the annotation on the type variable 'a.
|}]

let f (x : ('a : bits8 addressable)) : ('b : bits8 addressable & value) =
  #(x, 1)
[%%expect{|
val f : ('a : bits8 addressable). 'a -> #('a * int) = <fun>
|}]

(* The same incompleteness through a shared sort variable; whether it
   bites depends on the order of the checks (principal mode differs). *)
type t8a : bits8 addressable
external magic : ('a : any) ('b : any). 'a -> 'b = "%identity" [@@layout_poly]
let f (x : ('a : bits8 addressable)) : #(int8# * int) = magic #(x, 1)
[%%expect{|
type t8a : bits8 addressable
external magic : ('a : any) ('b : any). 'a -> 'b = "%identity"
  [@@layout_poly]
Line 3, characters 64-65:
3 | let f (x : ('a : bits8 addressable)) : #(int8# * int) = magic #(x, 1)
                                                                    ^
Error: The value "x" has type "('a : bits8 addressable)"
       but an expression was expected of type "('b : bits8)"
       The layout of 'a is bits8
         because it's the type of a tuple element.
       But the layout of 'a must overlap with bits8 addressable
         because of the annotation on the type variable 'a.
|}, Principal{|
type t8a : bits8 addressable
external magic : ('a : any) ('b : any). 'a -> 'b = "%identity"
  [@@layout_poly]
Line 3, characters 56-69:
3 | let f (x : ('a : bits8 addressable)) : #(int8# * int) = magic #(x, 1)
                                                            ^^^^^^^^^^^^^
Error: This expression has type "('a : bits8 addressable & value_or_null)"
       but an expression was expected of type "#(int8# * int)"
       The layout of #(int8# * int) is bits8 & value non_pointer
         because it is an unboxed tuple.
       But the layout of #(int8# * int) must be a sublayout of
           bits8 addressable & value
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
       Note: The layout of immediate is value non_pointer.
|}]

let f (y : int8#) : #(t8a * int) = magic #(y, 1)
[%%expect{|
Line 1, characters 43-44:
1 | let f (y : int8#) : #(t8a * int) = magic #(y, 1)
                                               ^
Error: The value "y" has type "int8#" but an expression was expected of type
         "('a : bits8 addressable)"
       The layout of int8# is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# must be a sublayout of bits8 addressable
         because it's the type of a tuple element.
|}, Principal{|
Line 1, characters 35-48:
1 | let f (y : int8#) : #(t8a * int) = magic #(y, 1)
                                       ^^^^^^^^^^^^^
Error: This expression has type "('a : bits8 & value_or_null)"
       but an expression was expected of type "#(t8a * int)"
       The layout of #(t8a * int) is bits8 addressable & value non_pointer
         because it is an unboxed tuple.
       But the layout of #(t8a * int) must be a sublayout of bits8 & value
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
       Note: The layout of immediate is value non_pointer.
|}]

type ('a : bits8 & value) r
type ('a : bits8 addressable & value) ra
type ('a : (bits8 & value) addressable) rpa
type ok = #(int8# * int) r
type ok = #(int8# * int) ra
type ok = #(int8# * int) rpa
[%%expect{|
type ('a : bits8 & value) r
type ('a : bits8 addressable & value) ra
type ('a : (bits8 & value) addressable) rpa
type ok = #(int8# * int) r
Line 5, characters 10-24:
5 | type ok = #(int8# * int) ra
              ^^^^^^^^^^^^^^
Error: This type "#(int8# * int)" should be an instance of type
         "('a : bits8 addressable & value)"
       The layout of #(int8# * int) is bits8 & value non_pointer
         because it is an unboxed tuple.
       But the layout of #(int8# * int) must be a sublayout of
           bits8 addressable & value
         because of the definition of ra at line 2, characters 0-40.
       Note: The layout of immediate is value non_pointer.
|}]

type ok = #(t8a * int) r
type ok = #(t8a * int) ra
type ok = #(t8a * int) rpa
[%%expect{|
Line 1, characters 10-22:
1 | type ok = #(t8a * int) r
              ^^^^^^^^^^^^
Error: This type "#(t8a * int)" should be an instance of type
         "('a : bits8 & value)"
       The layout of #(t8a * int) is bits8 addressable & value non_pointer
         because it is an unboxed tuple.
       But the layout of #(t8a * int) must be a sublayout of bits8 & value
         because of the definition of r at line 1, characters 0-27.
       Note: The layout of immediate is value non_pointer.
|}]

(* The intersection of two product kinds *)
let f (x : ('a : bits8 addressable & value)) (y : ('b : bits8 & value)) =
  if true then x else y
[%%expect{|
Line 2, characters 22-23:
2 |   if true then x else y
                          ^
Error: The value "y" has type "('b : bits8 & value)"
       but an expression was expected of type
         "('a : bits8 addressable & value)"
       The layout of 'a is bits8 addressable & value
         because of the annotation on the type variable 'a.
       But the layout of 'a must overlap with bits8 & value
         because of the annotation on the type variable 'b.
|}]

(**** Inference ****)

(* A component inferred from a product kind takes the plain kind, so it can
   hold a value of either kind. *)
let f y : ('b : bits8 addressable & value) = #(y, 1)
[%%expect{|
val f : ('a : bits8 addressable). 'a -> #('a * int) = <fun>
|}]

let f (y : ('a : any)) : ('b : bits8 addressable & value) = #(y, 1)
[%%expect{|
val f : ('a : bits8 addressable). 'a -> #('a * int) = <fun>
|}]

let f (y : ('a : bits8 addressable)) : ('b : bits8 addressable & value) =
  #(y, 1)
[%%expect{|
val f : ('a : bits8 addressable). 'a -> #('a * int) = <fun>
|}]

type ('a : any, 'b : any) t = #{ a : 'a; b : 'b }
let f (x : ('a, 'b) t) : ('c : bits8 addressable & bits16) = x
[%%expect{|
type ('a : any, 'b : any) t = #{ a : 'a; b : 'b; }
val f : ('a : bits8 addressable) ('b : bits16). ('a, 'b) t -> ('a, 'b) t =
  <fun>
|}]

let f (x : (('a : bits8 addressable), 'b) t)
  : ('c : bits8 addressable & bits16) = x
[%%expect{|
val f : ('a : bits8 addressable) ('b : bits16). ('a, 'b) t -> ('a, 'b) t =
  <fun>
|}]

(* Unifying the kind of a product with a sort variable *)
external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
let f (x : ('a : bits8 addressable)) = id #(x, 1)
[%%expect{|
external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
val f : ('a : bits8 addressable). 'a -> #('a * int) = <fun>
|}]

(* Making a sort variable addressable does not make its components
   addressable *)
external id_addressable : ('a : any addressable). 'a -> 'a = "%identity"
  [@@layout_poly]
let f y = id_addressable #(y, 1)
[%%expect{|
external id_addressable : ('a : any addressable). 'a -> 'a = "%identity"
  [@@layout_poly]
val f : 'a -> #('a * int) = <fun>
|}]

let f (y : int8#) = id_addressable #(y, 1)
[%%expect{|
Line 1, characters 37-38:
1 | let f (y : int8#) = id_addressable #(y, 1)
                                         ^
Error: The value "y" has type "int8#" but an expression was expected of type
         "('a : '_representable_layout_1 addressable)"
       The layout of int8# is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# must be addressable
         because it's the type of a tuple element.
|}]

(**** [@unpacked] ****)

type tup : bits32 addressable & bits32
external e : (tup [@unpacked]) -> int = "foo" "bar"
[%%expect{|
type tup : bits32 addressable & bits32
external e : (tup [@unpacked]) -> int = "foo" "bar"
|}]
