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

(* The same incompleteness through a shared sort variable.

   The difference for principal mode is because the incompleteness is
   order-dependent. *)
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

(* A component inferred from an ampersand kind gets the plain kind *)
let f y : ('b : bits8 addressable & value) = #(y, 1)
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

(**** [@unpacked] ****)

type tup : bits32 addressable & bits32
external e : (tup [@unpacked]) -> int = "foo" "bar"
[%%expect{|
type tup : bits32 addressable & bits32
external e : (tup [@unpacked]) -> int = "foo" "bar"
|}]
