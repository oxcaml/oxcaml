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

type t : (bits8 & bits16) addressable mod portable
[%%expect{|
Line 1, characters 26-37:
1 | type t : (bits8 & bits16) addressable mod portable
                              ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 & bits16".

type t : bits8 mod portable & bits16 mod portable
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
  type t : bits8 addressable & bits16 addressable
end = struct
  type t : bits8 & bits16
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

module M : sig
  type t : bits8 & bits16
end = struct
  type t : (bits8 & bits16) addressable
end
[%%expect{|
Line 4, characters 28-39:
4 |   type t : (bits8 & bits16) addressable
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

type t : float64 & value
type ok = t req
[%%expect{|
type t : float64 & value
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

module M : sig
  type t : any addressable
end = struct
  type t : any & any
end
[%%expect{|
module M : sig type t : any addressable end
|}]

(**** Unboxed records and tuples ****)

type t : bits8 addressable & bits16 addressable = #{ a : int8#; b : int16# }
[%%expect{|
type t = #{ a : int8#; b : int16#; }
|}]

type t : (bits8 & bits16) addressable = #{ a : int8#; b : int16# }
[%%expect{|
Line 1, characters 26-37:
1 | type t : (bits8 & bits16) addressable = #{ a : int8#; b : int16# }
                              ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 & bits16".

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

let f (x : ('a : bits8)) : ('b : bits8 addressable & value) = #(x, 1)
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

let f (x : ('a : bits8 addressable)) : ('b : bits8 addressable & value) =
  #(x, 1)
[%%expect{|
Line 2, characters 4-5:
2 |   #(x, 1)
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

let f (y : int8#) : #(t8a * int) = magic #(y, 1)
[%%expect{|
val f : int8# -> #(t8a * int) = <fun>
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

(* A component inferred from a product kind takes the plain kind, so it can
   hold a value of either kind. *)
let f y : ('b : bits8 addressable & value) = #(y, 1)
[%%expect{|
val f : ('a : bits8). 'a -> #('a * int) = <fun>
|}]

let f (y : ('a : any)) : ('b : bits8 addressable & value) = #(y, 1)
[%%expect{|
val f : ('a : bits8). 'a -> #('a * int) = <fun>
|}]

let f (y : ('a : bits8 addressable)) : ('b : bits8 addressable & value) =
  #(y, 1)
[%%expect{|
Line 2, characters 4-5:
2 |   #(y, 1)
        ^
Error: The value "y" has type "('a : bits8 addressable)"
       but an expression was expected of type "('b : bits8)"
       The layout of 'a is bits8
         because of the annotation on the type variable 'b.
       But the layout of 'a must overlap with bits8 addressable
         because of the annotation on the type variable 'a.
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

let f (y : int8#) = id_addressable #(y, 1)
[%%expect{|
val f : int8# -> #(int8# * int) = <fun>
|}]

(**** [@unpacked] ****)

type tup : bits32 addressable & bits32
external e : (tup [@unpacked]) -> int = "foo" "bar"
[%%expect{|
type tup : bits32 & bits32
external e : (tup [@unpacked]) -> int = "foo" "bar"
|}]
