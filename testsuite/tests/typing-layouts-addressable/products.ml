(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(**** A product makes its components addressable ****)

(* So [addressable] on a product is redundant *)
type t : (bits8 & bits16) addressable
[%%expect{|
type t : (bits8 & bits16) addressable
|}]

type t : (float64 & void) addressable mod portable
[%%expect{|
type t : (float64 & void) addressable mod portable
|}]

type t : (bits8 addressable & bits16 addressable) addressable
[%%expect{|
Line 1, characters 50-61:
1 | type t : (bits8 addressable & bits16 addressable) addressable
                                                      ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 addressable & bits16 addressable".

type t : bits8 addressable & bits16 addressable
|}]

(* ... and so is [addressable] on a component *)
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

(* Every product is below [any addressable] *)
type ('a : any addressable) req
type ok = #(float# * string) req
type ok = #(int8# * int16#) req
[%%expect{|
type ('a : any addressable) req
Line 2, characters 10-28:
2 | type ok = #(float# * string) req
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

module M : sig
  type t : any addressable
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
         sig type t : any addressable end
       Type declarations do not match:
         type t : bits8 & bits16
       is not included in
         type t : any addressable
       The layout of the first is bits8 & bits16
         because of the definition of t at line 4, characters 2-25.
       But the layout of the first must be a sublayout of any addressable
         because of the definition of t at line 2, characters 2-26.
|}]

(* Mismatches between components are still seen *)
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

(**** [@layout_poly] at [any addressable] accepts unboxed products ****)

external id_addressable : ('a : any addressable). 'a -> 'a = "%identity"
  [@@layout_poly]
[%%expect{|
external id_addressable : ('a : any addressable). 'a -> 'a = "%identity"
  [@@layout_poly]
|}]

let f (x : #(int8# * int16#)) = id_addressable x
[%%expect{|
Line 1, characters 47-48:
1 | let f (x : #(int8# * int16#)) = id_addressable x
                                                   ^
Error: The value "x" has type "#(int8# * int16#)"
       but an expression was expected of type
         "('a : '_representable_layout_1 addressable)"
       The layout of #(int8# * int16#) is bits8 & bits16
         because it is an unboxed tuple.
       But the layout of #(int8# * int16#) must be addressable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

let g (x : #(float# * string)) = id_addressable x
[%%expect{|
Line 1, characters 48-49:
1 | let g (x : #(float# * string)) = id_addressable x
                                                    ^
Error: The value "x" has type "#(float# * string)"
       but an expression was expected of type
         "('a : '_representable_layout_2 addressable)"
       The layout of #(float# * string) is float64 & value non_float
         because it is an unboxed tuple.
       But the layout of #(float# * string) must be addressable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(* A component checked against [any addressable] need not itself be
   addressable: the product makes it so *)
let f (x : ('a : any)) (y : int16#) = id_addressable #(x, y)
[%%expect{|
Line 1, characters 58-59:
1 | let f (x : ('a : any)) (y : int16#) = id_addressable #(x, y)
                                                              ^
Error: The value "y" has type "int16#" but an expression was expected of type
         "('a : '_representable_layout_3 addressable)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be addressable
         because it's the type of unboxed tuple element.
|}]

let g (x : int8#) (y : int16#) = f x y
[%%expect{|
Line 1, characters 33-34:
1 | let g (x : int8#) (y : int16#) = f x y
                                     ^
Error: Unbound value "f"
|}]

(**** A component that is already addressable fits a plain component ****)

type u : bits8 addressable
type ('a : bits8 & bits16) req
type ok = #(u * int16#) req
[%%expect{|
type u : bits8 addressable
type ('a : bits8 & bits16) req
Line 3, characters 10-23:
3 | type ok = #(u * int16#) req
              ^^^^^^^^^^^^^
Error: This type "#(u * int16#)" should be an instance of type
         "('a : bits8 & bits16)"
       The layout of #(u * int16#) is bits8 addressable & bits16
         because it is an unboxed tuple.
       But the layout of #(u * int16#) must be a sublayout of bits8 & bits16
         because of the definition of req at line 2, characters 0-30.
|}]

let f (x : ('a : bits8 & bits16)) (y : #(u * int16#)) = if true then x else y
[%%expect{|
Line 1, characters 76-77:
1 | let f (x : ('a : bits8 & bits16)) (y : #(u * int16#)) = if true then x else y
                                                                                ^
Error: The value "y" has type "#(u * int16#)"
       but an expression was expected of type "('a : bits8 & bits16)"
       The layout of #(u * int16#) is bits8 addressable & bits16
         because it is an unboxed tuple.
       But the layout of #(u * int16#) must be a sublayout of bits8 & bits16
         because of the annotation on the type variable 'a.
|}]

module M : sig
  type t : bits8 & bits16
end = struct
  type t = #(u * int16#)
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #(u * int16#)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #(u * int16#) end
       is not included in
         sig type t : bits8 & bits16 end
       Type declarations do not match:
         type t = #(u * int16#)
       is not included in
         type t : bits8 & bits16
       The layout of the first is bits8 addressable & bits16
         because it is an unboxed tuple.
       But the layout of the first must be a sublayout of bits8 & bits16
         because of the definition of t at line 2, characters 2-25.
|}]
