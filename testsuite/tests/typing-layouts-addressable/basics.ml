(* TEST
   flags = "-no-ikinds";
   expect;
*)

(* Tests for the [addressable] kind operator. An addressable kind is one whose
   types store all of their information in the data portion of a block when
   boxed. [value], [word], [bits64], and the vector layouts are addressable;
   [any], [void], [untagged_immediate], [bits8], [bits16], [bits32],
   [float32], and [float64] are not. Products of addressable kinds are
   addressable (but products do not make their components addressable).
   [k addressable = k] when [k] is already addressable, and [k] and
   [k addressable] are incomparable otherwise.

   This file is duplicated as [basics_ikinds.ml], which runs with ikinds
   enabled. *)

(**********************************************************************)
(* Test 1: acceptance and representability of marked base kinds *)

type t8 : bits8 addressable

let f (t : t8) = t

[%%expect{|
Line 1, characters 16-27:
1 | type t8 : bits8 addressable
                    ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type tf : float64 addressable
type tv : void addressable

let f2 (t : tf) = t

[%%expect{|
Line 1, characters 18-29:
1 | type tf : float64 addressable
                      ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(**********************************************************************)
(* Test 2: applying [addressable] to an addressable kind is a no-op
   (and warns) *)

type t_value : value addressable

[%%expect{|
Line 1, characters 21-32:
1 | type t_value : value addressable
                         ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t_bits64 : bits64 addressable

[%%expect{|
Line 1, characters 23-34:
1 | type t_bits64 : bits64 addressable
                           ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t_word : word addressable

[%%expect{|
Line 1, characters 19-30:
1 | type t_word : word addressable
                       ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t_vec : vec128 addressable

[%%expect{|
Line 1, characters 20-31:
1 | type t_vec : vec128 addressable
                        ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t_imm : immediate addressable

[%%expect{|
Line 1, characters 23-34:
1 | type t_imm : immediate addressable
                           ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* The no-op collapse gives equal kinds: inclusion passes both ways. *)

module M2a : sig
  type t : bits64
end = struct
  type t : bits64 addressable
end

module M2b : sig
  type t : bits64 addressable
end = struct
  type t : bits64
end

[%%expect{|
Line 4, characters 18-29:
4 |   type t : bits64 addressable
                      ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(**********************************************************************)
(* Test 3: [bits8] and [bits8 addressable] are incomparable *)

module M3a : sig
  type t : bits8
end = struct
  type t : bits8 addressable
end

[%%expect{|
Line 4, characters 17-28:
4 |   type t : bits8 addressable
                     ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

module M3b : sig
  type t : bits8 addressable
end = struct
  type t : bits8
end

[%%expect{|
Line 2, characters 17-28:
2 |   type t : bits8 addressable
                     ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type ('a : bits8) req8
type ('a : bits8 addressable) req8a

[%%expect{|
type ('a : bits8) req8
Line 2, characters 17-28:
2 | type ('a : bits8 addressable) req8a
                     ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type bad = t8 req8

[%%expect{|
Line 1, characters 11-13:
1 | type bad = t8 req8
               ^^
Error: Unbound type constructor "t8"
|}]

type t8_plain : bits8
type bad = t8_plain req8a

[%%expect{|
type t8_plain : bits8
Line 2, characters 20-25:
2 | type bad = t8_plain req8a
                        ^^^^^
Error: Unbound type constructor "req8a"
Hint:              Did you mean "req8"?
|}]

type ok = t8 req8a

[%%expect{|
Line 1, characters 13-18:
1 | type ok = t8 req8a
                 ^^^^^
Error: Unbound type constructor "req8a"
Hint:              Did you mean "req8"?
|}]

(* Same for float64. *)

module M3c : sig
  type t : float64
end = struct
  type t : float64 addressable
end

[%%expect{|
Line 4, characters 19-30:
4 |   type t : float64 addressable
                       ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(**********************************************************************)
(* Test 4: [any addressable] as a bound *)

type ('a : any addressable) reqa

[%%expect{|
Line 1, characters 15-26:
1 | type ('a : any addressable) reqa
                   ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type ok1 = string reqa
type ok2 = int64# reqa
type ok3 = nativeint# reqa
type ok4 = t8 reqa

[%%expect{|
Line 1, characters 18-22:
1 | type ok1 = string reqa
                      ^^^^
Error: Unbound type constructor "reqa"
Hint:              Did you mean "req8"?
|}]

type bad1 = float# reqa

[%%expect{|
Line 1, characters 19-23:
1 | type bad1 = float# reqa
                       ^^^^
Error: Unbound type constructor "reqa"
Hint:              Did you mean "req8"?
|}]

type bad2 = t8_plain reqa

[%%expect{|
Line 1, characters 21-25:
1 | type bad2 = t8_plain reqa
                         ^^^^
Error: Unbound type constructor "reqa"
Hint:              Did you mean "req8"?
|}]

type t_any : any
type bad3 = t_any reqa

[%%expect{|
type t_any : any
Line 2, characters 18-22:
2 | type bad3 = t_any reqa
                      ^^^^
Error: Unbound type constructor "reqa"
Hint:              Did you mean "req8"?
|}]

(* [any addressable] is not representable, like [any]. *)

let f4 : ('a : any addressable). 'a -> 'a = fun x -> x

[%%expect{|
Line 1, characters 19-30:
1 | let f4 : ('a : any addressable). 'a -> 'a = fun x -> x
                       ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(**********************************************************************)
(* Test 5: every addressable kind <= any addressable <= any *)

type taa : any addressable

type ('a : any) req_any
type ok = taa req_any

[%%expect{|
Line 1, characters 15-26:
1 | type taa : any addressable
                   ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

module M5a : sig
  type t : any
end = struct
  type t : any addressable
end

[%%expect{|
Line 4, characters 15-26:
4 |   type t : any addressable
                   ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

module M5b : sig
  type t : any addressable
end = struct
  type t : any
end

[%%expect{|
Line 2, characters 15-26:
2 |   type t : any addressable
                   ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(**********************************************************************)
(* Test 6: products *)

(* A product of addressable kinds is addressable. *)

type p1 : bits64 & value
type ok = p1 reqa

[%%expect{|
type p1 : bits64 & value
Line 2, characters 13-17:
2 | type ok = p1 reqa
                 ^^^^
Error: Unbound type constructor "reqa"
Hint:              Did you mean "req8"?
|}]

(* A product with an unaddressable component is not. *)

type p2 : bits8 & value
type bad = p2 reqa

[%%expect{|
type p2 : bits8 & value
Line 2, characters 14-18:
2 | type bad = p2 reqa
                  ^^^^
Error: Unbound type constructor "reqa"
Hint:              Did you mean "req8"?
|}]

(* ... unless that component is made addressable. *)

type p3 : bits8 addressable & value
type ok = p3 reqa

[%%expect{|
Line 1, characters 16-27:
1 | type p3 : bits8 addressable & value
                    ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* A whole product can be marked addressable. *)

type mp : (bits8 & bits16) addressable
type ok = mp reqa

[%%expect{|
Line 1, characters 27-38:
1 | type mp : (bits8 & bits16) addressable
                               ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* Marking a product is not the same as marking its components. *)

module M6a : sig
  type t : (bits8 & bits16) addressable
end = struct
  type t : bits8 addressable & bits16 addressable
end

[%%expect{|
Line 4, characters 17-28:
4 |   type t : bits8 addressable & bits16 addressable
                     ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

module M6b : sig
  type t : bits8 addressable & bits16 addressable
end = struct
  type t : (bits8 & bits16) addressable
end

[%%expect{|
Line 4, characters 28-39:
4 |   type t : (bits8 & bits16) addressable
                                ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* Marking a product of addressables is a no-op. *)

module M6c : sig
  type t : (bits64 & value) addressable
end = struct
  type t : bits64 & value
end

[%%expect{|
Line 2, characters 28-39:
2 |   type t : (bits64 & value) addressable
                                ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* Unboxed tuples are products of the component kinds. *)

type ok = #(int64# * string) reqa

[%%expect{|
Line 1, characters 29-33:
1 | type ok = #(int64# * string) reqa
                                 ^^^^
Error: Unbound type constructor "reqa"
Hint:              Did you mean "req8"?
|}]

type bad = #(float# * float#) reqa

[%%expect{|
Line 1, characters 30-34:
1 | type bad = #(float# * float#) reqa
                                  ^^^^
Error: Unbound type constructor "reqa"
Hint:              Did you mean "req8"?
|}]

(**********************************************************************)
(* Test 7: abstract kinds *)

kind_ kb8 = bits8

type t7 : kb8 addressable
type ok = t7 req8a

[%%expect{|
kind_ kb8 = bits8
Line 3, characters 14-25:
3 | type t7 : kb8 addressable
                  ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

kind_ kb64 = bits64

type t7' : kb64 addressable

[%%expect{|
kind_ kb64 = bits64
Line 3, characters 16-27:
3 | type t7' : kb64 addressable
                    ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

kind_ k

type ta : k addressable
type tk : k

[%%expect{|
kind_ k
Line 3, characters 12-23:
3 | type ta : k addressable
                ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* [k addressable <= any addressable] holds without expanding [k]... *)

type ok = ta reqa

[%%expect{|
Line 1, characters 13-17:
1 | type ok = ta reqa
                 ^^^^
Error: Unbound type constructor "reqa"
Hint:              Did you mean "req8"?
|}]

(* ...but [k <= any addressable] does not hold for abstract [k]. *)

type bad = tk reqa

[%%expect{|
Line 1, characters 14-18:
1 | type bad = tk reqa
                  ^^^^
Error: Unbound type constructor "reqa"
Hint:              Did you mean "req8"?
|}]

(* [k addressable] and [k] are incomparable for abstract [k]. *)

module M7a : sig
  type t : k
end = struct
  type t : k addressable
end

[%%expect{|
Line 4, characters 13-24:
4 |   type t : k addressable
                 ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

module M7b : sig
  type t : k addressable
end = struct
  type t : k
end

[%%expect{|
Line 2, characters 13-24:
2 |   type t : k addressable
                 ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* Matching kind_ declarations. *)

module M7c : sig
  kind_ k' = bits8 addressable
end = struct
  kind_ k' = bits8 addressable
end

[%%expect{|
Line 4, characters 13-30:
4 |   kind_ k' = bits8 addressable
                 ^^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "addressable" have no effect on the kind "bits8".

Line 4, characters 19-30:
4 |   kind_ k' = bits8 addressable
                       ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

module M7d : sig
  kind_ k' = bits8 addressable
end = struct
  kind_ k' = bits8
end

[%%expect{|
Line 2, characters 13-30:
2 |   kind_ k' = bits8 addressable
                 ^^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "addressable" have no effect on the kind "bits8".

Line 2, characters 19-30:
2 |   kind_ k' = bits8 addressable
                       ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

module M7e : sig
  kind_ k' = bits64 addressable
end = struct
  kind_ k' = bits64
end

[%%expect{|
Line 2, characters 13-31:
2 |   kind_ k' = bits64 addressable
                 ^^^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "addressable" have no effect on the kind "bits64".

Line 2, characters 20-31:
2 |   kind_ k' = bits64 addressable
                        ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(**********************************************************************)
(* Test 8: idempotence *)

type t8i : bits8 addressable addressable

module M8 : sig
  type t : bits8 addressable
end = struct
  type t : bits8 addressable addressable
end

[%%expect{|
Line 1, characters 17-28:
1 | type t8i : bits8 addressable addressable
                     ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(**********************************************************************)
(* Test 9: mixing with [mod] and scannable axes *)

type t9a : bits8 addressable mod portable

[%%expect{|
Line 1, characters 17-28:
1 | type t9a : bits8 addressable mod portable
                     ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t9b : any non_null addressable
type t9c : any addressable non_null

[%%expect{|
Line 1, characters 24-35:
1 | type t9b : any non_null addressable
                            ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t9d : k addressable non_pointer

[%%expect{|
Line 1, characters 13-24:
1 | type t9d : k addressable non_pointer
                 ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* Unknown operator words still error. *)

type t9e : bits8 addressablee

[%%expect{|
Line 1, characters 17-29:
1 | type t9e : bits8 addressablee
                     ^^^^^^^^^^^^
Error: Unknown kind modifier addressablee
|}]

(**********************************************************************)
(* Test 10: printing in error messages *)

type ('a : bits16) req16
type bad = t8 req16

[%%expect{|
type ('a : bits16) req16
Line 2, characters 11-13:
2 | type bad = t8 req16
               ^^
Error: Unbound type constructor "t8"
|}]
