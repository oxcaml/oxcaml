(* TEST
 expect;
*)

(* Inference involving the [addressable] kind operator. Addressability is
   tracked on sorts, so sort variables can be constrained to be
   addressable. *)

type ('a : any addressable) require_addressable
[%%expect{|
Line 1, characters 15-26:
1 | type ('a : any addressable) require_addressable
                   ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* A type variable bounded by [any addressable] can be instantiated at
   addressable kinds... *)

type 'a s
type ok = int require_addressable s
[%%expect{|
type 'a s
Line 2, characters 14-33:
2 | type ok = int require_addressable s
                  ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

(* A newtype at kind [any addressable] can be used where a representable
   layout is required: the sort variable is constrained to be addressable,
   then defaults to [value]. *)

let f (type a : any addressable) (x : a) = x
[%%expect{|
Line 1, characters 20-31:
1 | let f (type a : any addressable) (x : a) = x
                        ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* Unification of a sort variable with an addressable sort succeeds under
   an addressability constraint... *)

let f (type a : any addressable) (g : a -> a) (x : a) = g x
let apply_at_bits64 (g : int64# -> int64#) (x : int64#) = f g x
[%%expect{|
Line 1, characters 20-31:
1 | let f (type a : any addressable) (g : a -> a) (x : a) = g x
                        ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* ...but unification with an unaddressable sort fails. *)

let apply_at_float64 (g : float# -> float#) (x : float#) = f g x
[%%expect{|
Line 1, characters 59-60:
1 | let apply_at_float64 (g : float# -> float#) (x : float#) = f g x
                                                               ^
Error: Unbound value "f"
|}]

(* Type parameters at unaddressable kinds cannot instantiate
   [any addressable] parameters. *)

let bad (x : ('a : float64) require_addressable) = x
[%%expect{|
Line 1, characters 28-47:
1 | let bad (x : ('a : float64) require_addressable) = x
                                ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

(* Sort variables of ordinary inferred functions are not addressable by
   default from the type alone; a concrete addressable type fixes them. *)

let g (x : ('a : any addressable)) = x
let ok = g #0L
[%%expect{|
Line 1, characters 21-32:
1 | let g (x : ('a : any addressable)) = x
                         ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

let bad = g #0.0
[%%expect{|
Line 1, characters 10-11:
1 | let bad = g #0.0
              ^
Error: Unbound value "g"
|}]
