(* TEST
 expect;
*)

(* Inference involving the [addressable] kind operator. Addressability is
   tracked on sorts, so sort variables can be constrained to be
   addressable. *)

type ('a : any addressable) require_addressable
[%%expect{|
type ('a : any addressable) require_addressable
|}]

(* A type variable bounded by [any addressable] can be instantiated at
   addressable kinds... *)

type 'a s
type ok = int require_addressable s
[%%expect{|
type 'a s
type ok = int require_addressable s
|}]

(* [any addressable] is not representable (it is a bound covering layouts
   of every width), so like [any] it cannot be used for a function
   argument. Note the [addressable] constraint on the required
   representable layout in the error message. *)

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
       But the layout of a must be representable
         because we must know concretely how to pass a function argument.
|}]

(* A newtype at a representable made-addressable kind can be used as a
   function argument: the fresh sort variable is unified with the
   made-addressable sort. *)

let f (type a : bits8 addressable) (x : a) = x
[%%expect{|
val f : ('a : bits8 addressable). 'a -> 'a = <fun>
|}]

let f (type a : (bits8 & value) addressable) (x : a) = x
[%%expect{|
val f : ('a : (bits8 & value) addressable). 'a -> 'a = <fun>
|}]

(* Unifying a type variable bounded by [any addressable] with a type of an
   addressable kind succeeds... *)

let ok (x : ('a : any addressable)) (y : int64#) = if true then x else y
[%%expect{|
val ok : int64# -> int64# -> int64# = <fun>
|}]

(* ...but with a type of an unaddressable kind fails. *)

let bad (x : ('a : any addressable)) (y : float#) = if true then x else y
[%%expect{|
Line 1, characters 72-73:
1 | let bad (x : ('a : any addressable)) (y : float#) = if true then x else y
                                                                            ^
Error: The value "y" has type "float#" but an expression was expected of type
         "('a : '_representable_layout_2 addressable)"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be representable
         because we must know concretely how to return a function result.
|}]

(* Type parameters at unaddressable kinds cannot instantiate
   [any addressable] parameters. *)

let bad (x : ('a : float64) require_addressable) = x
[%%expect{|
Line 1, characters 13-27:
1 | let bad (x : ('a : float64) require_addressable) = x
                 ^^^^^^^^^^^^^^
Error: This type "('a : float64)" should be an instance of type
         "('b : any addressable)"
       The layout of 'a is float64
         because of the annotation on the type variable 'a.
       But the layout of 'a must overlap with any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

(* As for [any], sort variables under an addressability constraint default
   to [value] at generalization. *)

let g (x : ('a : any addressable)) = x
[%%expect{|
val g : 'a -> 'a = <fun>
|}]
