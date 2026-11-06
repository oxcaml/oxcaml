(* TEST
 expect;
*)

(* [&] makes its components addressable: [k1 & k2] is [k1 addressable & k2
   addressable]. *)

(**** [addressable] on a component or on a product is redundant ****)

type t : bits8 addressable & bits16
[%%expect{|
type t : bits8 & bits16
|}]

type t : (bits8 & bits16) addressable
[%%expect{|
Line 1, characters 26-37:
1 | type t : (bits8 & bits16) addressable
                              ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 & bits16".

type t : bits8 & bits16
|}]

(**** Equalities ****)

module M : sig
  type t : bits8 & bits16
end = struct
  type t : bits8 addressable & bits16 addressable
end
[%%expect{|
module M : sig type t : bits8 & bits16 end
|}]

module M : sig
  type t : (bits8 & bits16) addressable
end = struct
  type t : bits8 & bits16
end
[%%expect{|
Line 2, characters 28-39:
2 |   type t : (bits8 & bits16) addressable
                                ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 & bits16".

module M : sig type t : bits8 & bits16 end
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
type ok = t req
|}]

type ok = #(float# * string) req
[%%expect{|
type ok = #(float# * string) req
|}]

type t : any & any
type ok = t req
[%%expect{|
type t : any & any
type ok = t req
|}]

(**** Unboxed records and tuples ****)

type t : bits8 addressable & bits16 addressable = #{ a : int8#; b : int16# }
[%%expect{|
type t = #{ a : int8#; b : int16#; }
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
module M :
  sig
    type ('a : bits8 addressable) t : bits8 & value
    type ('a : bits8) u : bits8 & value
  end
|}]

(* Plain and made-addressable components both fit a product kind *)
let f (x : ('a : bits8)) : ('b : bits8 & value) = #(x, 1)
[%%expect{|
val f : ('a : bits8). 'a -> #('a * int) = <fun>
|}]

let f (x : ('a : bits8 addressable)) : #('a * int) = #(x, 1)
[%%expect{|
val f : ('a : bits8 addressable). 'a -> #('a * int) = <fun>
|}]

let f (x : ('a : bits8 addressable)) : ('b : bits8 & value) =
  (#(x, 1) : #('a * int))
[%%expect{|
val f : ('a : bits8 addressable). 'a -> #('a * int) = <fun>
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
val f : ('a : bits8 addressable). 'a -> #(int8# * int) = <fun>
|}]

type ('a : bits8 & value) r
type ('a : bits8 addressable & value) ra
type ('a : (bits8 & value) addressable) rpa
type ok = #(int8# * int) r
type ok = #(int8# * int) ra
type ok = #(int8# * int) rpa
[%%expect{|
type ('a : bits8 & value) r
type ('a : bits8 & value) ra
Line 3, characters 27-38:
3 | type ('a : (bits8 & value) addressable) rpa
                               ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 & value".

type ('a : bits8 & value) rpa
type ok = #(int8# * int) r
type ok = #(int8# * int) ra
type ok = #(int8# * int) rpa
|}]

type ok = #(t8a * int) r
type ok = #(t8a * int) ra
type ok = #(t8a * int) rpa
[%%expect{|
type ok = #(t8a * int) r
type ok = #(t8a * int) ra
type ok = #(t8a * int) rpa
|}]

(* The intersection of two product kinds *)
let f (x : ('a : bits8 addressable & value)) (y : ('b : bits8 & value)) =
  if true then x else y
[%%expect{|
val f : ('a : bits8 & value). 'a -> 'a -> 'a = <fun>
|}]

(**** Inference ****)

(* A component inferred from an ampersand kind gets the plain kind *)
let f y : ('b : bits8 addressable & value) = #(y, 1)
[%%expect{|
val f : ('a : bits8). 'a -> #('a * int) = <fun>
|}]

type ('a : any, 'b : any) t = #{ a : 'a; b : 'b }
let f (x : ('a, 'b) t) : ('c : bits8 addressable & bits16) = x
[%%expect{|
type ('a : any, 'b : any) t = #{ a : 'a; b : 'b; }
val f : ('a : bits8) ('b : bits16). ('a, 'b) t -> ('a, 'b) t = <fun>
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

(**** [@unpacked] ****)

type tup : bits32 addressable & bits32
external e : (tup [@unpacked]) -> int = "foo" "bar"
[%%expect{|
type tup : bits32 & bits32
external e : (tup [@unpacked]) -> int = "foo" "bar"
|}]

(**** [inherit] on a component ****)

(* An [inherit] component is not made addressable *)
type t : bits8 & bits16 inherit
[%%expect{|
type t : bits8 & bits16 inherit
|}]

module M : sig
  type t : bits8 & bits16
end = struct
  type t : bits8 & bits16 inherit
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : bits8 & bits16 inherit
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits8 & bits16 inherit end
       is not included in
         sig type t : bits8 & bits16 end
       Type declarations do not match:
         type t : bits8 & bits16 inherit
       is not included in
         type t : bits8 & bits16
       The layout of the first is bits8 & bits16 inherit
         because of the definition of t at line 4, characters 2-33.
       But the layout of the first must be a sublayout of bits8 & bits16
         because of the definition of t at line 2, characters 2-25.
|}]

module M : sig
  type t : bits8 & bits16 inherit
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
         sig type t : bits8 & bits16 inherit end
       Type declarations do not match:
         type t : bits8 & bits16
       is not included in
         type t : bits8 & bits16 inherit
       The layout of the first is bits8 & bits16
         because of the definition of t at line 4, characters 2-25.
       But the layout of the first must be a sublayout of
           bits8 & bits16 inherit
         because of the definition of t at line 2, characters 2-33.
|}]

(* Addressability does not distribute through products: a product with an
   [inherit] component made addressable is not the product of the components
   made addressable. *)
type t : (bits8 & bits16 inherit) addressable
[%%expect{|
type t : (bits8 & bits16 inherit) addressable
|}]

module M : sig
  type t : bits8 & bits16
end = struct
  type t : (bits8 & bits16 inherit) addressable
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : (bits8 & bits16 inherit) addressable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : (bits8 & bits16 inherit) addressable end
       is not included in
         sig type t : bits8 & bits16 end
       Type declarations do not match:
         type t : (bits8 & bits16 inherit) addressable
       is not included in
         type t : bits8 & bits16
       The layout of the first is (bits8 & bits16 inherit) addressable
         because of the definition of t at line 4, characters 2-47.
       But the layout of the first must be a sublayout of bits8 & bits16
         because of the definition of t at line 2, characters 2-25.
|}]

module M : sig
  type t : (bits8 & bits16 inherit) addressable
end = struct
  type t : bits8 & bits16 inherit
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : bits8 & bits16 inherit
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits8 & bits16 inherit end
       is not included in
         sig type t : (bits8 & bits16 inherit) addressable end
       Type declarations do not match:
         type t : bits8 & bits16 inherit
       is not included in
         type t : (bits8 & bits16 inherit) addressable
       The layout of the first is bits8 & bits16 inherit
         because of the definition of t at line 4, characters 2-33.
       But the layout of the first must be a sublayout of
           (bits8 & bits16 inherit) addressable
         because of the definition of t at line 2, characters 2-47.
|}]

type ti : bits8 & bits16 inherit
type ta : (bits8 & bits16 inherit) addressable
type ok = ta req
[%%expect{|
type ti : bits8 & bits16 inherit
type ta : (bits8 & bits16 inherit) addressable
type ok = ta req
|}]

type bad = ti req
[%%expect{|
Line 1, characters 11-13:
1 | type bad = ti req
               ^^
Error: This type "ti" should be an instance of type "('a : any addressable)"
       The layout of ti is bits8 & bits16 inherit
         because of the definition of ti at line 1, characters 0-32.
       But the layout of ti must be a sublayout of any addressable
         because of the definition of req at line 1, characters 0-31.
|}]

(* [inherit] on an inherently addressable component changes nothing *)
module M : sig
  type t : bits64 & value
end = struct
  type t : bits64 inherit & value
end
[%%expect{|
Line 4, characters 11-25:
4 |   type t : bits64 inherit & value
               ^^^^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64".

module M : sig type t : bits64 & value end
|}]

(* [inherit] is only allowed on a component of a product *)
type t : bits8 inherit
[%%expect{|
Line 1, characters 9-22:
1 | type t : bits8 inherit
             ^^^^^^^^^^^^^
Error: "inherit" must be on the outside of a component of a product kind.
|}]

type t : bits8 & bits16 inherit addressable
[%%expect{|
Line 1, characters 17-31:
1 | type t : bits8 & bits16 inherit addressable
                     ^^^^^^^^^^^^^^
Error: "inherit" must be on the outside of a component of a product kind.
|}]

type t : (bits8 inherit) mod portable
[%%expect{|
Line 1, characters 9-24:
1 | type t : (bits8 inherit) mod portable
             ^^^^^^^^^^^^^^^
Error: "inherit" must be on the outside of a component of a product kind.
|}]

type t : (bits8 & bits16 inherit) mod portable
[%%expect{|
type t : bits8 mod portable & (bits16 mod portable) inherit
|}]

(* A tuple makes its components addressable, so its kind has no [inherit]
   component *)
type ('a : bits8 & bits16 inherit) ri
type bad = #(int8# * int16#) ri
[%%expect{|
type ('a : bits8 & bits16 inherit) ri
Line 2, characters 11-28:
2 | type bad = #(int8# * int16#) ri
               ^^^^^^^^^^^^^^^^^
Error: This type "#(int8# * int16#)" should be an instance of type
         "('a : bits8 & bits16 inherit)"
       The layout of #(int8# * int16#) is bits8 & bits16
         because it is an unboxed tuple.
       But the layout of #(int8# * int16#) must be a sublayout of
           bits8 & bits16 inherit
         because of the definition of ri at line 1, characters 0-37.
|}]

let f (x : int8#) (y : int16#) : ('a : bits8 & bits16 inherit) = #(x, y)
[%%expect{|
Line 1, characters 65-72:
1 | let f (x : int8#) (y : int16#) : ('a : bits8 & bits16 inherit) = #(x, y)
                                                                     ^^^^^^^
Error: This expression has type "#('b * 'c)"
       but an expression was expected of type "('a : bits8 & bits16 inherit)"
       The layout of #('a * 'b) is bits8 & '_representable_layout_1
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of
           bits8 & bits16 inherit
         because of the annotation on the type variable 'a.
|}]

(* [@unpacked] accepts a product with an [inherit] component, made addressable
   or not *)
type tup : bits32 & bits32 inherit
external e : (tup [@unpacked]) -> int = "foo" "bar"
[%%expect{|
type tup : bits32 & bits32 inherit
external e : (tup [@unpacked]) -> int = "foo" "bar"
|}]

type tup : (bits32 & bits32 inherit) addressable
external e : (tup [@unpacked]) -> int = "foo" "bar"
[%%expect{|
type tup : (bits32 & bits32 inherit) addressable
external e : (tup [@unpacked]) -> int = "foo" "bar"
|}]
