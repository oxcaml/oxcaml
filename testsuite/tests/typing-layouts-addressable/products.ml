(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(**** A product makes its components addressable ****)

(* So [addressable] on a product is redundant *)
type t : (bits8 & bits16) addressable
[%%expect{|
Line 1, characters 26-37:
1 | type t : (bits8 & bits16) addressable
                              ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 & bits16".

type t : bits8 & bits16
|}]

type t : (float64 & void) addressable mod portable
[%%expect{|
Line 1, characters 26-37:
1 | type t : (float64 & void) addressable mod portable
                              ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "float64 & void".

type t : float64 mod portable & void mod portable
|}]

type t : (bits8 addressable & bits16 addressable) addressable
[%%expect{|
Line 1, characters 50-61:
1 | type t : (bits8 addressable & bits16 addressable) addressable
                                                      ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 addressable & bits16 addressable".

type t : bits8 & bits16
|}]

(* ... and so is [addressable] on a component *)
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

(* Every product is below [any addressable] *)
type ('a : any addressable) req
type ok = #(float# * string) req
type ok = #(int8# * int16#) req
[%%expect{|
type ('a : any addressable) req
type ok = #(float# * string) req
type ok = #(int8# * int16#) req
|}]

module M : sig
  type t : any addressable
end = struct
  type t : bits8 & bits16
end
[%%expect{|
module M : sig type t : any addressable end
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
val f : #(int8# * int16#) -> #(int8# * int16#) = <fun>
|}]

let g (x : #(float# * string)) = id_addressable x
[%%expect{|
val g : #(float# * string) -> #(float# * string) = <fun>
|}]

(* A component checked against [any addressable] need not itself be
   addressable: the product makes it so *)
let f (x : ('a : any)) (y : int16#) =
  let _ = id_addressable #(x, y) in
  (x : int8#)
[%%expect{|
val f : int8# -> int16# -> int8# = <fun>
|}]

(**** A component that is already addressable fits a plain component ****)

type u : bits8 addressable
type ('a : bits8 & bits16) req
type ok = #(u * int16#) req
[%%expect{|
type u : bits8 addressable
type ('a : bits8 & bits16) req
type ok = #(u * int16#) req
|}]

let f (x : ('a : bits8 & bits16)) (y : #(u * int16#)) = if true then x else y
[%%expect{|
val f : #(u * int16#) -> #(u * int16#) -> #(u * int16#) = <fun>
|}]

module M : sig
  type t : bits8 & bits16
end = struct
  type t = #(u * int16#)
end
[%%expect{|
module M : sig type t : bits8 & bits16 end
|}]
