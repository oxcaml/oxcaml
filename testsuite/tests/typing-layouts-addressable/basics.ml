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
type t8 : bits8 addressable
val f : t8 -> t8 = <fun>
|}]

type tf : float64 addressable
type tv : void addressable

let f2 (t : tf) = t

[%%expect{|
type tf : float64 addressable
type tv : void addressable
val f2 : tf -> tf = <fun>
|}]

(**********************************************************************)
(* Test 2: applying [addressable] to an addressable kind is a no-op
   (and warns) *)

type t_value : value addressable

[%%expect{|
Line 1, characters 21-32:
1 | type t_value : value addressable
                         ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "value".

type t_value
|}]

type t_bits64 : bits64 addressable

[%%expect{|
Line 1, characters 23-34:
1 | type t_bits64 : bits64 addressable
                           ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64".

type t_bits64 : bits64
|}]

type t_word : word addressable

[%%expect{|
Line 1, characters 19-30:
1 | type t_word : word addressable
                       ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "word".

type t_word : word
|}]

type t_vec : vec128 addressable

[%%expect{|
Line 1, characters 20-31:
1 | type t_vec : vec128 addressable
                        ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "vec128".

type t_vec : vec128
|}]

type t_imm : immediate addressable

[%%expect{|
Line 1, characters 23-34:
1 | type t_imm : immediate addressable
                           ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "immediate".

type t_imm : immediate
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
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64".

module M2a : sig type t : bits64 end
Line 8, characters 18-29:
8 |   type t : bits64 addressable
                      ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64".

module M2b : sig type t : bits64 end
|}]

(**********************************************************************)
(* Test 3: [bits8] and [bits8 addressable] are incomparable *)

module M3a : sig
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

module M3b : sig
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

type ('a : bits8) req8
type ('a : bits8 addressable) req8a

[%%expect{|
type ('a : bits8) req8
type ('a : bits8 addressable) req8a
|}]

type bad = t8 req8

[%%expect{|
Line 1, characters 11-13:
1 | type bad = t8 req8
               ^^
Error: This type "t8" should be an instance of type "('a : bits8)"
       The layout of t8 is bits8 addressable
         because of the definition of t8 at line 1, characters 0-27.
       But the layout of t8 must be a sublayout of bits8
         because of the definition of req8 at line 1, characters 0-22.
|}]

type t8_plain : bits8
type bad = t8_plain req8a

[%%expect{|
type t8_plain : bits8
Line 2, characters 11-19:
2 | type bad = t8_plain req8a
               ^^^^^^^^
Error: This type "t8_plain" should be an instance of type
         "('a : bits8 addressable)"
       The layout of t8_plain is bits8
         because of the definition of t8_plain at line 1, characters 0-21.
       But the layout of t8_plain must be a sublayout of bits8 addressable
         because of the definition of req8a at line 2, characters 0-35.
|}]

type ok = t8 req8a

[%%expect{|
type ok = t8 req8a
|}]

(* Same for float64. *)

module M3c : sig
  type t : float64
end = struct
  type t : float64 addressable
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : float64 addressable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : float64 addressable end
       is not included in
         sig type t : float64 end
       Type declarations do not match:
         type t : float64 addressable
       is not included in
         type t : float64
       The layout of the first is float64 addressable
         because of the definition of t at line 4, characters 2-30.
       But the layout of the first must be a sublayout of float64
         because of the definition of t at line 2, characters 2-18.
|}]

(**********************************************************************)
(* Test 4: [any addressable] as a bound *)

type ('a : any addressable) reqa

[%%expect{|
type ('a : any addressable) reqa
|}]

type ok1 = string reqa
type ok2 = int64# reqa
type ok3 = nativeint# reqa
type ok4 = t8 reqa

[%%expect{|
type ok1 = string reqa
type ok2 = int64# reqa
type ok3 = nativeint# reqa
type ok4 = t8 reqa
|}]

type bad1 = float# reqa

[%%expect{|
Line 1, characters 12-18:
1 | type bad1 = float# reqa
                ^^^^^^
Error: This type "float#" should be an instance of type "('a : any addressable)"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be a sublayout of any addressable
         because of the definition of reqa at line 1, characters 0-32.
|}]

type bad2 = t8_plain reqa

[%%expect{|
Line 1, characters 12-20:
1 | type bad2 = t8_plain reqa
                ^^^^^^^^
Error: This type "t8_plain" should be an instance of type
         "('a : any addressable)"
       The layout of t8_plain is bits8
         because of the definition of t8_plain at line 1, characters 0-21.
       But the layout of t8_plain must be a sublayout of any addressable
         because of the definition of reqa at line 1, characters 0-32.
|}]

type t_any : any
type bad3 = t_any reqa

[%%expect{|
type t_any : any
Line 2, characters 12-17:
2 | type bad3 = t_any reqa
                ^^^^^
Error: This type "t_any" should be an instance of type "('a : any addressable)"
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be a sublayout of any addressable
         because of the definition of reqa at line 1, characters 0-32.
|}]

(* [any addressable] is not representable, like [any]. *)

let f4 : ('a : any addressable). 'a -> 'a = fun x -> x

[%%expect{|
Line 1, characters 44-54:
1 | let f4 : ('a : any addressable). 'a -> 'a = fun x -> x
                                                ^^^^^^^^^^
Error: This definition has type "'b -> 'b" which is less general than
         "('a : any addressable). 'a -> 'a"
       The layout of 'a is any addressable
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because we must know concretely how to pass a function argument.
|}]

(**********************************************************************)
(* Test 5: every addressable kind <= any addressable <= any *)

type taa : any addressable

type ('a : any) req_any
type ok = taa req_any

[%%expect{|
type taa : any addressable
type ('a : any) req_any
type ok = taa req_any
|}]

module M5a : sig
  type t : any
end = struct
  type t : any addressable
end

[%%expect{|
module M5a : sig type t : any end
|}]

module M5b : sig
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

(**********************************************************************)
(* Test 6: products *)

(* A product of addressable kinds is addressable. *)

type p1 : bits64 & value
type ok = p1 reqa

[%%expect{|
type p1 : bits64 & value
type ok = p1 reqa
|}]

(* A product with an unaddressable component is not. *)

type p2 : bits8 & value
type bad = p2 reqa

[%%expect{|
type p2 : bits8 & value
Line 2, characters 11-13:
2 | type bad = p2 reqa
               ^^
Error: This type "p2" should be an instance of type "('a : any addressable)"
       The layout of p2 is bits8 & value
         because of the definition of p2 at line 1, characters 0-23.
       But the layout of p2 must be a sublayout of any addressable
         because of the definition of reqa at line 1, characters 0-32.
|}]

(* ... unless that component is made addressable. *)

type p3 : bits8 addressable & value
type ok = p3 reqa

[%%expect{|
type p3 : bits8 addressable & value
type ok = p3 reqa
|}]

(* A whole product can be marked addressable. *)

type mp : (bits8 & bits16) addressable
type ok = mp reqa

[%%expect{|
type mp : (bits8 & bits16) addressable
type ok = mp reqa
|}]

(* Marking a product is not the same as marking its components. *)

module M6a : sig
  type t : (bits8 & bits16) addressable
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
         sig type t : (bits8 & bits16) addressable end
       Type declarations do not match:
         type t : bits8 addressable & bits16 addressable
       is not included in
         type t : (bits8 & bits16) addressable
       The layout of the first is bits8 addressable & bits16 addressable
         because of the definition of t at line 4, characters 2-49.
       But the layout of the first must be a sublayout of
           (bits8 & bits16) addressable
         because of the definition of t at line 2, characters 2-39.
|}]

module M6b : sig
  type t : bits8 addressable & bits16 addressable
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
         sig type t : bits8 addressable & bits16 addressable end
       Type declarations do not match:
         type t : (bits8 & bits16) addressable
       is not included in
         type t : bits8 addressable & bits16 addressable
       The layout of the first is (bits8 & bits16) addressable
         because of the definition of t at line 4, characters 2-39.
       But the layout of the first must be a sublayout of
           bits8 addressable & bits16 addressable
         because of the definition of t at line 2, characters 2-49.
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
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64 & value".

module M6c : sig type t : bits64 & value end
|}]

(* Unboxed tuples are products of the component kinds. *)

type ok = #(int64# * string) reqa

[%%expect{|
type ok = #(int64# * string) reqa
|}]

type bad = #(float# * float#) reqa

[%%expect{|
Line 1, characters 11-29:
1 | type bad = #(float# * float#) reqa
               ^^^^^^^^^^^^^^^^^^
Error: This type "#(float# * float#)" should be an instance of type
         "('a : any addressable)"
       The layout of #(float# * float#) is float64 & float64
         because it is an unboxed tuple.
       But the layout of #(float# * float#) must be a sublayout of
           any addressable
         because of the definition of reqa at line 1, characters 0-32.
|}]

(**********************************************************************)
(* Test 7: abstract kinds *)

kind_ kb8 = bits8

type t7 : kb8 addressable
type ok = t7 req8a

[%%expect{|
kind_ kb8 = bits8
type t7 : bits8 addressable
type ok = t7 req8a
|}]

kind_ kb64 = bits64

type t7' : kb64 addressable

[%%expect{|
kind_ kb64 = bits64
Line 3, characters 16-27:
3 | type t7' : kb64 addressable
                    ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "kb64".

type t7' : bits64
|}]

kind_ k

type ta : k addressable
type tk : k

[%%expect{|
kind_ k
type ta : k addressable
type tk : k
|}]

(* [k addressable <= any addressable] holds without expanding [k]... *)

type ok = ta reqa

[%%expect{|
type ok = ta reqa
|}]

(* ...but [k <= any addressable] does not hold for abstract [k]. *)

type bad = tk reqa

[%%expect{|
Line 1, characters 11-13:
1 | type bad = tk reqa
               ^^
Error: This type "tk" should be an instance of type "('a : any addressable)"
       The kind of tk is k
         because of the definition of tk at line 4, characters 0-11.
       But the kind of tk must be a subkind of any addressable
         because of the definition of reqa at line 1, characters 0-32.
|}]

(* [k addressable] and [k] are incomparable for abstract [k]. *)

module M7a : sig
  type t : k
end = struct
  type t : k addressable
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : k addressable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : k addressable end
       is not included in
         sig type t : k end
       Type declarations do not match:
         type t : k addressable
       is not included in
         type t : k
       The kind of the first is k addressable
         because of the definition of t at line 4, characters 2-24.
       But the kind of the first must be a subkind of k
         because of the definition of t at line 2, characters 2-12.
|}]

module M7b : sig
  type t : k addressable
end = struct
  type t : k
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : k
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : k end
       is not included in
         sig type t : k addressable end
       Type declarations do not match:
         type t : k
       is not included in
         type t : k addressable
       The kind of the first is k
         because of the definition of t at line 4, characters 2-12.
       But the kind of the first must be a subkind of k addressable
         because of the definition of t at line 2, characters 2-24.
|}]

(* Matching kind_ declarations. *)

module M7c : sig
  kind_ k' = bits8 addressable
end = struct
  kind_ k' = bits8 addressable
end

[%%expect{|
module M7c : sig kind_ k' = bits8 addressable end
|}]

module M7d : sig
  kind_ k' = bits8 addressable
end = struct
  kind_ k' = bits8
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   kind_ k' = bits8
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k' = bits8 end
       is not included in
         sig kind_ k' = bits8 addressable end
       Kind declarations do not match:
         kind_ k' = bits8
       is not included in
         kind_ k' = bits8 addressable
       Their definitions are not equal.
|}]

module M7e : sig
  kind_ k' = bits64 addressable
end = struct
  kind_ k' = bits64
end

[%%expect{|
Line 2, characters 20-31:
2 |   kind_ k' = bits64 addressable
                        ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64".

module M7e : sig kind_ k' = bits64 end
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
Line 1, characters 29-40:
1 | type t8i : bits8 addressable addressable
                                 ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 addressable".

type t8i : bits8 addressable
Line 6, characters 29-40:
6 |   type t : bits8 addressable addressable
                                 ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 addressable".

module M8 : sig type t : bits8 addressable end
|}]

(**********************************************************************)
(* Test 9: mixing with [mod] and scannable axes *)

type t9a : bits8 addressable mod portable

[%%expect{|
type t9a : bits8 addressable mod portable
|}]

type t9b : any non_null addressable
type t9c : any addressable non_null

[%%expect{|
type t9b : any non_null addressable
type t9c : any non_null addressable
|}]

type t9d : k addressable non_pointer

[%%expect{|
type t9d : k non_pointer addressable
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
Error: This type "t8" should be an instance of type "('a : bits16)"
       The layout of t8 is bits8 addressable
         because of the definition of t8 at line 1, characters 0-27.
       But the layout of t8 must be a sublayout of bits16
         because of the definition of req16 at line 1, characters 0-24.
|}]

(* Miscellaneous uses of the [addressable] kind operator: locally abstract
   types, layout-poly externals, and instantiating kind-constrained type
   parameters. *)

type addressable_bits8 : bits8 addressable
let addressable_id (x : addressable_bits8) = x
let locally_abstract_addressable
    (type a : bits8 addressable) (x : a) =
  x

[%%expect{|
type addressable_bits8 : bits8 addressable
val addressable_id : addressable_bits8 -> addressable_bits8 = <fun>
val locally_abstract_addressable : ('a : bits8 addressable). 'a -> 'a = <fun>
|}]

external[@layout_poly] addressable_poly_id :
  ('a : any addressable). 'a -> 'a = "%identity"

let addressable_poly_id_bits8 (x : addressable_bits8) =
  addressable_poly_id x

[%%expect{|
external addressable_poly_id : ('a : any addressable). 'a -> 'a = "%identity"
  [@@layout_poly]
val addressable_poly_id_bits8 : addressable_bits8 -> addressable_bits8 =
  <fun>
|}]

type unrepresentable_addressable : any addressable
let unrepresentable_addressable_id (x : unrepresentable_addressable) = x

[%%expect{|
type unrepresentable_addressable : any addressable
Line 2, characters 35-68:
2 | let unrepresentable_addressable_id (x : unrepresentable_addressable) = x
                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type "unrepresentable_addressable"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_1)"
       The layout of unrepresentable_addressable is any addressable
         because of the definition of unrepresentable_addressable at line 1, characters 0-50.
       But the layout of unrepresentable_addressable must be representable
         because we must know concretely how to pass a function argument.
|}]

type ('a : any addressable) requires_addressable
type accepted_addressable_bits8 =
  addressable_bits8 requires_addressable

[%%expect{|
type ('a : any addressable) requires_addressable
type accepted_addressable_bits8 = addressable_bits8 requires_addressable
|}]

type addressable_bits64 : bits64 addressable
type plain_bits64 : bits64
type accepted_plain_bits64 = plain_bits64 requires_addressable

[%%expect{|
Line 1, characters 33-44:
1 | type addressable_bits64 : bits64 addressable
                                     ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64".

type addressable_bits64 : bits64
type plain_bits64 : bits64
type accepted_plain_bits64 = plain_bits64 requires_addressable
|}]

type inherent_addressable_value : value
type inherent_addressable_word : word
type inherent_addressable_vec128 : vec128
type inherent_addressable_vec256 : vec256
type inherent_addressable_vec512 : vec512
type accepted_inherent_value =
  inherent_addressable_value requires_addressable
type accepted_inherent_word =
  inherent_addressable_word requires_addressable
type accepted_inherent_vec128 =
  inherent_addressable_vec128 requires_addressable
type accepted_inherent_vec256 =
  inherent_addressable_vec256 requires_addressable
type accepted_inherent_vec512 =
  inherent_addressable_vec512 requires_addressable

[%%expect{|
type inherent_addressable_value
type inherent_addressable_word : word
type inherent_addressable_vec128 : vec128
type inherent_addressable_vec256 : vec256
type inherent_addressable_vec512 : vec512
type accepted_inherent_value =
    inherent_addressable_value requires_addressable
type accepted_inherent_word = inherent_addressable_word requires_addressable
type accepted_inherent_vec128 =
    inherent_addressable_vec128 requires_addressable
type accepted_inherent_vec256 =
    inherent_addressable_vec256 requires_addressable
type accepted_inherent_vec512 =
    inherent_addressable_vec512 requires_addressable
|}]

type addressed_product : (bits8 & bits16) addressable
type component_addressed_product : bits8 addressable & bits64
type inherent_addressable_product : bits64 & word
type accepted_addressed_product = addressed_product requires_addressable
type accepted_component_addressed_product =
  component_addressed_product requires_addressable
type accepted_inherent_addressable_product =
  inherent_addressable_product requires_addressable

[%%expect{|
type addressed_product : (bits8 & bits16) addressable
type component_addressed_product : bits8 addressable & bits64
type inherent_addressable_product : bits64 & word
type accepted_addressed_product = addressed_product requires_addressable
type accepted_component_addressed_product =
    component_addressed_product requires_addressable
type accepted_inherent_addressable_product =
    inherent_addressable_product requires_addressable
|}]

type ('a : any) requires_any
type accepted_as_any = addressable_bits8 requires_any

[%%expect{|
type ('a : any) requires_any
type accepted_as_any = addressable_bits8 requires_any
|}]

type plain_bits8 : bits8
type rejected_plain_bits8 = plain_bits8 requires_addressable

[%%expect{|
type plain_bits8 : bits8
Line 2, characters 28-39:
2 | type rejected_plain_bits8 = plain_bits8 requires_addressable
                                ^^^^^^^^^^^
Error: This type "plain_bits8" should be an instance of type
         "('a : any addressable)"
       The layout of plain_bits8 is bits8
         because of the definition of plain_bits8 at line 1, characters 0-24.
       But the layout of plain_bits8 must be a sublayout of any addressable
         because of the definition of requires_addressable at line 1, characters 0-48.
|}]

type ('a : bits8) requires_plain_bits8
type rejected_addressable_as_plain =
  addressable_bits8 requires_plain_bits8

[%%expect{|
type ('a : bits8) requires_plain_bits8
Line 3, characters 2-19:
3 |   addressable_bits8 requires_plain_bits8
      ^^^^^^^^^^^^^^^^^
Error: This type "addressable_bits8" should be an instance of type "('a : bits8)"
       The layout of addressable_bits8 is bits8 addressable
         because of the definition of addressable_bits8 at line 1, characters 0-42.
       But the layout of addressable_bits8 must be a sublayout of bits8
         because of the definition of requires_plain_bits8 at line 1, characters 0-38.
|}]

type plain_product : bits8 & bits64
type rejected_plain_product = plain_product requires_addressable

[%%expect{|
type plain_product : bits8 & bits64
Line 2, characters 30-43:
2 | type rejected_plain_product = plain_product requires_addressable
                                  ^^^^^^^^^^^^^
Error: This type "plain_product" should be an instance of type
         "('a : any addressable)"
       The layout of plain_product is bits8 & bits64
         because of the definition of plain_product at line 1, characters 0-35.
       But the layout of plain_product must be a sublayout of any addressable
         because of the definition of requires_addressable at line 1, characters 0-48.
|}]

type addressable_twice : bits8 addressable addressable
type addressable_float64 : float64 addressable
type accepted_addressable_float64 =
  addressable_float64 requires_addressable

[%%expect{|
Line 1, characters 43-54:
1 | type addressable_twice : bits8 addressable addressable
                                               ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 addressable".

type addressable_twice : bits8 addressable
type addressable_float64 : float64 addressable
type accepted_addressable_float64 = addressable_float64 requires_addressable
|}]
