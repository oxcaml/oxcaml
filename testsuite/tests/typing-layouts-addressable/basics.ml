(* TEST
 expect;
*)

(**** Acceptance and printing ****)

type t8 : bits8 addressable
[%%expect{|
type t8 : bits8 addressable
|}]

type t : any addressable
[%%expect{|
type t : any addressable
|}]

type t : (bits8 & bits16) addressable
[%%expect{|
type t : (bits8 & bits16) addressable
|}]

type t : bits8 addressable & bits16
[%%expect{|
type t : bits8 addressable & bits16
|}]

type t : void addressable
[%%expect{|
type t : void addressable
|}]

type t : float64 addressable
[%%expect{|
type t : float64 addressable
|}]

(**** Applying [addressable] to an already-addressable kind is redundant ****)

type t : value addressable
[%%expect{|
Line 1, characters 15-26:
1 | type t : value addressable
                   ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "value".

type t
|}]

type t : bits64 addressable
[%%expect{|
Line 1, characters 16-27:
1 | type t : bits64 addressable
                    ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64".

type t : bits64
|}]

type t : (bits64 & word) addressable
[%%expect{|
Line 1, characters 25-36:
1 | type t : (bits64 & word) addressable
                             ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64 & word".

type t : bits64 & word
|}]

type t : bits8 addressable addressable
[%%expect{|
Line 1, characters 27-38:
1 | type t : bits8 addressable addressable
                               ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 addressable".

type t : bits8 addressable
|}]

(**** Equalities: [k addressable = k] for addressable [k] ****)

module M : sig
  type t : bits64 addressable
end = struct
  type t : bits64
end
[%%expect{|
Line 2, characters 18-29:
2 |   type t : bits64 addressable
                      ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64".

module M : sig type t : bits64 end
|}]

module M : sig
  type t : bits64
end = struct
  type t : bits64 addressable
end
[%%expect{|
Line 4, characters 18-29:
4 |   type t : bits64 addressable
                      ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64".

module M : sig type t : bits64 end
|}]

module M : sig
  type t : value addressable
end = struct
  type t = string
end
[%%expect{|
Line 2, characters 17-28:
2 |   type t : value addressable
                     ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "value".

module M : sig type t end
|}]

module M : sig
  type t : word addressable
end = struct
  type t : word
end
[%%expect{|
Line 2, characters 16-27:
2 |   type t : word addressable
                    ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "word".

module M : sig type t : word end
|}]

module M : sig
  type t : vec128 addressable
end = struct
  type t : vec128
end
[%%expect{|
Line 2, characters 18-29:
2 |   type t : vec128 addressable
                      ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "vec128".

module M : sig type t : vec128 end
|}]

module M : sig
  type t : (bits64 & word) addressable
end = struct
  type t : bits64 & word
end
[%%expect{|
Line 2, characters 27-38:
2 |   type t : (bits64 & word) addressable
                               ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64 & word".

module M : sig type t : bits64 & word end
|}]

(* A product all of whose components are made addressable is addressable *)
module M : sig
  type t : (bits8 addressable & bits16 addressable) addressable
end = struct
  type t : bits8 addressable & bits16 addressable
end
[%%expect{|
Line 2, characters 52-63:
2 |   type t : (bits8 addressable & bits16 addressable) addressable
                                                        ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 addressable & bits16 addressable".

module M : sig type t : bits8 addressable & bits16 addressable end
|}]

module M : sig
  type t : bits8 addressable & bits16 addressable
end = struct
  type t : (bits8 addressable & bits16 addressable) addressable
end
[%%expect{|
Line 4, characters 52-63:
4 |   type t : (bits8 addressable & bits16 addressable) addressable
                                                        ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 addressable & bits16 addressable".

module M : sig type t : bits8 addressable & bits16 addressable end
|}]

(* ... also with a mix of intrinsically and made addressable components *)
module M : sig
  type t : (bits8 addressable & word) addressable
end = struct
  type t : bits8 addressable & word
end
[%%expect{|
Line 2, characters 38-49:
2 |   type t : (bits8 addressable & word) addressable
                                          ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 addressable & word".

module M : sig type t : bits8 addressable & word end
|}]

(**** Inequalities: [k addressable] and [k] are incomparable for
      unaddressable [k] ****)

module M : sig
  type t : bits8 addressable
end = struct
  type t : bits8
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : bits8
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits8 end
       is not included in
         sig type t : bits8 addressable end
       Type declarations do not match:
         type t : bits8
       is not included in
         type t : bits8 addressable
       The layout of the first is bits8
         because of the definition of t at line 4, characters 2-16.
       But the layout of the first must be a sublayout of bits8 addressable
         because of the definition of t at line 2, characters 2-28.
|}]

module M : sig
  type t : bits8
end = struct
  type t : bits8 addressable
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : bits8 addressable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits8 addressable end
       is not included in
         sig type t : bits8 end
       Type declarations do not match:
         type t : bits8 addressable
       is not included in
         type t : bits8
       The layout of the first is bits8 addressable
         because of the definition of t at line 4, characters 2-28.
       But the layout of the first must be a sublayout of bits8
         because of the definition of t at line 2, characters 2-16.
|}]

module M : sig
  type t : float64 addressable
end = struct
  type t : float64
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : float64
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : float64 end
       is not included in
         sig type t : float64 addressable end
       Type declarations do not match:
         type t : float64
       is not included in
         type t : float64 addressable
       The layout of the first is float64
         because of the definition of t at line 4, characters 2-18.
       But the layout of the first must be a sublayout of float64 addressable
         because of the definition of t at line 2, characters 2-30.
|}]

module M : sig
  type t : void
end = struct
  type t : void addressable
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : void addressable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : void addressable end
       is not included in
         sig type t : void end
       Type declarations do not match:
         type t : void addressable
       is not included in
         type t : void
       The layout of the first is void addressable
         because of the definition of t at line 4, characters 2-27.
       But the layout of the first must be a sublayout of void
         because of the definition of t at line 2, characters 2-15.
|}]

module M : sig
  type t : untagged_immediate addressable
end = struct
  type t : untagged_immediate
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : untagged_immediate
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : untagged_immediate end
       is not included in
         sig type t : untagged_immediate addressable end
       Type declarations do not match:
         type t : untagged_immediate
       is not included in
         type t : untagged_immediate addressable
       The layout of the first is untagged_immediate
         because of the definition of t at line 4, characters 2-29.
       But the layout of the first must be a sublayout of
           untagged_immediate addressable
         because of the definition of t at line 2, characters 2-41.
|}]

(* Mismatches under [addressable] are seen *)
module M : sig
  type t : bits16 addressable
end = struct
  type t : bits8 addressable
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : bits8 addressable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits8 addressable end
       is not included in
         sig type t : bits16 addressable end
       Type declarations do not match:
         type t : bits8 addressable
       is not included in
         type t : bits16 addressable
       The layout of the first is bits8 addressable
         because of the definition of t at line 4, characters 2-28.
       But the layout of the first must be a sublayout of bits16 addressable
         because of the definition of t at line 2, characters 2-29.
|}]

(**** [any addressable] is a superkind of exactly the addressable kinds ****)

type ('a : any addressable) req
[%%expect{|
type ('a : any addressable) req
|}]

type ok = string req
[%%expect{|
type ok = string req
|}]

type t64 : bits64
type ok = t64 req
[%%expect{|
type t64 : bits64
type ok = t64 req
|}]

type ok = t8 req
[%%expect{|
type ok = t8 req
|}]

type ok = int64# req
[%%expect{|
type ok = int64# req
|}]

(* An unboxed product is addressable iff all of its components are *)
type ok = #(int64# * string) req
[%%expect{|
type ok = #(int64# * string) req
|}]

type bad = #(float# * string) req
[%%expect{|
Line 1, characters 11-29:
1 | type bad = #(float# * string) req
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

type tb8 : bits8
type bad = tb8 req
[%%expect{|
type tb8 : bits8
Line 2, characters 11-14:
2 | type bad = tb8 req
               ^^^
Error: This type "tb8" should be an instance of type "('a : any addressable)"
       The layout of tb8 is bits8
         because of the definition of tb8 at line 1, characters 0-16.
       But the layout of tb8 must be a sublayout of any addressable
         because of the definition of req at line 1, characters 0-31.
|}]

type tf : float64
type bad = tf req
[%%expect{|
type tf : float64
Line 2, characters 11-13:
2 | type bad = tf req
               ^^
Error: This type "tf" should be an instance of type "('a : any addressable)"
       The layout of tf is float64
         because of the definition of tf at line 1, characters 0-17.
       But the layout of tf must be a sublayout of any addressable
         because of the definition of req at line 1, characters 0-31.
|}]

type tany : any
type bad = tany req
[%%expect{|
type tany : any
Line 2, characters 11-15:
2 | type bad = tany req
               ^^^^
Error: This type "tany" should be an instance of type "('a : any addressable)"
       The layout of tany is any
         because of the definition of tany at line 1, characters 0-15.
       But the layout of tany must be a sublayout of any addressable
         because of the definition of req at line 1, characters 0-31.
|}]

type tv : void
type bad = tv req
[%%expect{|
type tv : void
Line 2, characters 11-13:
2 | type bad = tv req
               ^^
Error: This type "tv" should be an instance of type "('a : any addressable)"
       The layout of tv is void
         because of the definition of tv at line 1, characters 0-14.
       But the layout of tv must be a sublayout of any addressable
         because of the definition of req at line 1, characters 0-31.
|}]

module M : sig
  type t : any addressable
end = struct
  type t : bits64
end
[%%expect{|
module M : sig type t : any addressable end
|}]

module M : sig
  type t : any addressable
end = struct
  type t : bits8
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : bits8
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits8 end
       is not included in
         sig type t : any addressable end
       Type declarations do not match:
         type t : bits8
       is not included in
         type t : any addressable
       The layout of the first is bits8
         because of the definition of t at line 4, characters 2-16.
       But the layout of the first must be a sublayout of any addressable
         because of the definition of t at line 2, characters 2-26.
|}]

module M : sig
  type t : any addressable
end = struct
  type t : any
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : any
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : any end
       is not included in
         sig type t : any addressable end
       Type declarations do not match:
         type t : any
       is not included in
         type t : any addressable
       The layout of the first is any
         because of the definition of t at line 4, characters 2-14.
       But the layout of the first must be a sublayout of any addressable
         because of the definition of t at line 2, characters 2-26.
|}]

module M : sig
  type t : any
end = struct
  type t : any addressable
end
[%%expect{|
module M : sig type t : any end
|}]

(**** Addressability does not distribute through products ****)

module M : sig
  type t : bits8 addressable & bits8 addressable
end = struct
  type t : (bits8 & bits8) addressable
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : (bits8 & bits8) addressable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : (bits8 & bits8) addressable end
       is not included in
         sig type t : bits8 addressable & bits8 addressable end
       Type declarations do not match:
         type t : (bits8 & bits8) addressable
       is not included in
         type t : bits8 addressable & bits8 addressable
       The layout of the first is (bits8 & bits8) addressable
         because of the definition of t at line 4, characters 2-38.
       But the layout of the first must be a sublayout of
           bits8 addressable & bits8 addressable
         because of the definition of t at line 2, characters 2-48.
|}]

module M : sig
  type t : (bits8 & bits8) addressable
end = struct
  type t : bits8 addressable & bits8 addressable
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : bits8 addressable & bits8 addressable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits8 addressable & bits8 addressable end
       is not included in
         sig type t : (bits8 & bits8) addressable end
       Type declarations do not match:
         type t : bits8 addressable & bits8 addressable
       is not included in
         type t : (bits8 & bits8) addressable
       The layout of the first is bits8 addressable & bits8 addressable
         because of the definition of t at line 4, characters 2-48.
       But the layout of the first must be a sublayout of
           (bits8 & bits8) addressable
         because of the definition of t at line 2, characters 2-38.
|}]

(**** Representability: addressability does not change the representation of
      a type outside of a block ****)

let f (x : t8) = x
[%%expect{|
val f : t8 -> t8 = <fun>
|}]

let g (x : t8) (y : string) = y
[%%expect{|
val g : t8 -> string -> string = <fun>
|}]

type r : bits8 addressable & bits64 = #{ a : t8; b : int64# }
[%%expect{|
type r = #{ a : t8; b : int64#; }
|}]

let proj (#{ a; b = _ } : r) = a
[%%expect{|
val proj : r -> t8 = <fun>
|}]

(* [any addressable] is still unrepresentable *)
let f (type a : any addressable) (x : a) = x
[%%expect{|
Line 1, characters 33-40:
1 | let f (type a : any addressable) (x : a) = x
                                     ^^^^^^^
Error: This pattern matches values of type "a"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_1 addressable)"
       The layout of a is any addressable
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be addressable
         because we must know concretely how to pass a function argument.
|}]

(**** Type parameters at addressable kinds ****)

type ('a : bits8 addressable) req8
[%%expect{|
type ('a : bits8 addressable) req8
|}]

type ok = t8 req8
[%%expect{|
type ok = t8 req8
|}]

type bad = int req8
[%%expect{|
Line 1, characters 11-14:
1 | type bad = int req8
               ^^^
Error: This type "int" should be an instance of type "('a : bits8 addressable)"
       The layout of int is value non_pointer
         because it is the primitive type int.
       But the layout of int must be a sublayout of bits8 addressable
         because of the definition of req8 at line 1, characters 0-34.
       Note: The layout of immediate is value non_pointer.
|}]

type bad = tb8 req8
[%%expect{|
Line 1, characters 11-14:
1 | type bad = tb8 req8
               ^^^
Error: This type "tb8" should be an instance of type "('a : bits8 addressable)"
       The layout of tb8 is bits8
         because of the definition of tb8 at line 1, characters 0-16.
       But the layout of tb8 must be a sublayout of bits8 addressable
         because of the definition of req8 at line 1, characters 0-34.
|}]

(**** Interaction with scannable axes ****)

type t : value non_pointer addressable
[%%expect{|
Line 1, characters 27-38:
1 | type t : value non_pointer addressable
                               ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "value non_pointer".

type t : value non_pointer
|}]

type t : any addressable non_null
[%%expect{|
type t : any non_null addressable
|}]

(* In an intersection, one side can contribute a scannable axis and the other
   addressability *)
type ('a : any non_null) refined = 'a req
[%%expect{|
Line 1, characters 6-23:
1 | type ('a : any non_null) refined = 'a req
          ^^^^^^^^^^^^^^^^^
Warning 181 [imprecise-kind-annotation]: The type variable `'a'
  was annotated with kind `any non_null'
  but was inferred to have kind `any non_null addressable'.

type ('a : any non_null addressable) refined = 'a req
|}]

(* The scannable-axis operator is ignored on a non-scannable kind, but
   [addressable] is not ignored *)
type t : bits8 addressable non_null
[%%expect{|
Line 1, characters 9-35:
1 | type t : bits8 addressable non_null
             ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_null" have no effect on the kind "bits8".

type t : bits8 addressable
|}]

type t : bits8 non_null addressable
[%%expect{|
Line 1, characters 9-35:
1 | type t : bits8 non_null addressable
             ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_null" have no effect on the kind "bits8".

type t : bits8 addressable
|}]

type t : value addressable mod portable [@@warning "-183"]
[%%expect{|
type t : value mod portable
|}]

(**** [@unpacked] looks through [addressable] on products ****)

(* Addressability does not change the out-of-block product representation,
   so a made-addressable product can be unpacked into C stub arguments. *)
type tup : (bits8 & bits16) addressable
[%%expect{|
type tup : (bits8 & bits16) addressable
|}]

external ext_unpack_addressable : (tup [@unpacked]) -> int = "foo" "bar"
[%%expect{|
external ext_unpack_addressable : (tup [@unpacked]) -> int = "foo" "bar"
|}]

(* ... but a made-addressable non-product still cannot be unpacked *)
external bad : (t8 [@unpacked]) -> int = "foo" "bar"
[%%expect{|
Line 1, characters 16-18:
1 | external bad : (t8 [@unpacked]) -> int = "foo" "bar"
                    ^^
Error: Don't know how to unpack this type.
       Only types with product layouts can be marked "unpacked".
|}]

(**** Errors ****)

type t : bits8 addressabl
[%%expect{|
Line 1, characters 15-25:
1 | type t : bits8 addressabl
                   ^^^^^^^^^^
Error: Unknown kind modifier addressabl
|}]

(* [addressable] on an abstract kind is recorded and applied when the kind is
   expanded; see [abstract_kinds.ml] for more tests. *)
kind_ k

type t : k addressable
[%%expect{|
kind_ k
type t : k addressable
|}]

kind_ ka = value addressable
[%%expect{|
Line 1, characters 17-28:
1 | kind_ ka = value addressable
                     ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "value".

kind_ ka = value
|}]

type t : ka
[%%expect{|
type t
|}]

(**** Printing of partially-solved sorts ****)

(* A wrapper made redundant by unification (here, the second component's
   variable is filled with [value]) is not printed back. *)
let bad (x : ('a : (float64 & value) addressable)) (y : #('b * 'c)) =
  if true then x else y
[%%expect{|
Line 2, characters 22-23:
2 |   if true then x else y
                          ^
Error: The value "y" has type "#('b * 'c)" but an expression was expected of type
         "('a : (float64 & value) addressable)"
       The layout of #('b * 'c) is
           '_representable_layout_2 addressable & value_or_null
         because it is an unboxed tuple.
       But the layout of #('b * 'c) must be a sublayout of
           (float64 & value) addressable
         because of the annotation on the type variable 'a.
|}]
