(* TEST
 expect;
*)

(* Subkinding for the [addressable] kind operator. Every addressable kind
   is a subkind of [any addressable]. If [k] is not addressable, then [k]
   and [k addressable] are incomparable. *)

type ('a : any addressable) require_addressable
type ('a : any) require_any
[%%expect{|
type ('a : any addressable) require_addressable
type ('a : any) require_any
|}]

(* Addressable base layouts are subkinds of [any addressable]. *)

type t_value : value
type ok = t_value require_addressable
[%%expect{|
type t_value
type ok = t_value require_addressable
|}]

type t_value_or_null : value_or_null
type ok = t_value_or_null require_addressable
[%%expect{|
type t_value_or_null : value_or_null
type ok = t_value_or_null require_addressable
|}]

type t_bits64 : bits64
type ok = t_bits64 require_addressable
[%%expect{|
type t_bits64 : bits64
type ok = t_bits64 require_addressable
|}]

type t_word : word
type ok = t_word require_addressable
[%%expect{|
type t_word : word
type ok = t_word require_addressable
|}]

type t_vec128 : vec128
type ok = t_vec128 require_addressable
[%%expect{|
type t_vec128 : vec128
type ok = t_vec128 require_addressable
|}]

(* Unaddressable base layouts are not subkinds of [any addressable]. *)

type t_bits8 : bits8
type bad = t_bits8 require_addressable
[%%expect{|
type t_bits8 : bits8
Line 2, characters 11-18:
2 | type bad = t_bits8 require_addressable
               ^^^^^^^
Error: This type "t_bits8" should be an instance of type "('a : any addressable)"
       The layout of t_bits8 is bits8
         because of the definition of t_bits8 at line 1, characters 0-20.
       But the layout of t_bits8 must be a sublayout of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

type t_float64 : float64
type bad = t_float64 require_addressable
[%%expect{|
type t_float64 : float64
Line 2, characters 11-20:
2 | type bad = t_float64 require_addressable
               ^^^^^^^^^
Error: This type "t_float64" should be an instance of type
         "('a : any addressable)"
       The layout of t_float64 is float64
         because of the definition of t_float64 at line 1, characters 0-24.
       But the layout of t_float64 must be a sublayout of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

type t_void : void
type bad = t_void require_addressable
[%%expect{|
type t_void : void
Line 2, characters 11-17:
2 | type bad = t_void require_addressable
               ^^^^^^
Error: This type "t_void" should be an instance of type "('a : any addressable)"
       The layout of t_void is void
         because of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

type t_untagged : untagged_immediate
type bad = t_untagged require_addressable
[%%expect{|
type t_untagged : untagged_immediate
Line 2, characters 11-21:
2 | type bad = t_untagged require_addressable
               ^^^^^^^^^^
Error: This type "t_untagged" should be an instance of type
         "('a : any addressable)"
       The layout of t_untagged is untagged_immediate
         because of the definition of t_untagged at line 1, characters 0-36.
       But the layout of t_untagged must be a sublayout of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

(* [any] is not a subkind of [any addressable]... *)

type t_any : any
type bad = t_any require_addressable
[%%expect{|
type t_any : any
Line 2, characters 11-16:
2 | type bad = t_any require_addressable
               ^^^^^
Error: This type "t_any" should be an instance of type "('a : any addressable)"
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be a sublayout of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

(* ...but [any addressable] is a subkind of [any]. *)

type t_any_addr : any addressable
type ok = t_any_addr require_any
[%%expect{|
type t_any_addr : any addressable
type ok = t_any_addr require_any
|}]

(* Kinds made addressable are subkinds of [any addressable]. *)

type t_bits8_addr : bits8 addressable
type ok = t_bits8_addr require_addressable
[%%expect{|
type t_bits8_addr : bits8 addressable
type ok = t_bits8_addr require_addressable
|}]

type t_float64_addr : float64 addressable
type ok = t_float64_addr require_addressable
[%%expect{|
type t_float64_addr : float64 addressable
type ok = t_float64_addr require_addressable
|}]

(* A product of addressable components is addressable. *)

type t_prod : value & bits64
type ok = t_prod require_addressable
[%%expect{|
type t_prod : value & bits64
type ok = t_prod require_addressable
|}]

(* A product with an unaddressable component is not addressable... *)

type t_prod_bad : value & bits8
type bad = t_prod_bad require_addressable
[%%expect{|
type t_prod_bad : value & bits8
Line 2, characters 11-21:
2 | type bad = t_prod_bad require_addressable
               ^^^^^^^^^^
Error: This type "t_prod_bad" should be an instance of type
         "('a : any addressable)"
       The layout of t_prod_bad is value & bits8
         because of the definition of t_prod_bad at line 1, characters 0-31.
       But the layout of t_prod_bad must be a sublayout of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

(* ...unless it is made addressable. *)

type t_prod_addr : (value & bits8) addressable
type ok = t_prod_addr require_addressable
[%%expect{|
type t_prod_addr : (value & bits8) addressable
type ok = t_prod_addr require_addressable
|}]

(* Concrete types with addressable kinds work too. *)

type ok = int require_addressable
type ok = string require_addressable
type ok = int64# require_addressable
type ok = nativeint# require_addressable
[%%expect{|
type ok = int require_addressable
type ok = string require_addressable
type ok = int64# require_addressable
type ok = nativeint# require_addressable
|}]

type bad = float# require_addressable
[%%expect{|
Line 1, characters 11-17:
1 | type bad = float# require_addressable
               ^^^^^^
Error: This type "float#" should be an instance of type "('a : any addressable)"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be a sublayout of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

(* Absorption: [(value & bits64) addressable] = [value & bits64], in both
   directions. *)

module M : sig
  type t : (value & bits64) addressable
end = struct
  type t : value & bits64
end
[%%expect{|
Line 2, characters 28-39:
2 |   type t : (value & bits64) addressable
                                ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "value & bits64".

module M : sig type t : value & bits64 end
|}]

module M : sig
  type t : value & bits64
end = struct
  type t : (value & bits64) addressable
end
[%%expect{|
Line 4, characters 28-39:
4 |   type t : (value & bits64) addressable
                                ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "value & bits64".

module M : sig type t : value & bits64 end
|}]

(* [bits8] and [bits8 addressable] are incomparable. *)

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

(* [addressable] does not distribute through products:
   [(bits8 & value) addressable] and [bits8 addressable & value] are
   incomparable. *)

module M : sig
  type t : bits8 addressable & value
end = struct
  type t : (bits8 & value) addressable
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : (bits8 & value) addressable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : (bits8 & value) addressable end
       is not included in
         sig type t : bits8 addressable & value end
       Type declarations do not match:
         type t : (bits8 & value) addressable
       is not included in
         type t : bits8 addressable & value
       The layout of the first is (bits8 & value) addressable
         because of the definition of t at line 4, characters 2-38.
       But the layout of the first must be a sublayout of
           bits8 addressable & value
         because of the definition of t at line 2, characters 2-36.
|}]

module M : sig
  type t : (bits8 & value) addressable
end = struct
  type t : bits8 addressable & value
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : bits8 addressable & value
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits8 addressable & value end
       is not included in
         sig type t : (bits8 & value) addressable end
       Type declarations do not match:
         type t : bits8 addressable & value
       is not included in
         type t : (bits8 & value) addressable
       The layout of the first is bits8 addressable & value
         because of the definition of t at line 4, characters 2-36.
       But the layout of the first must be a sublayout of
           (bits8 & value) addressable
         because of the definition of t at line 2, characters 2-38.
|}]

(* [k addressable] kinds are equal to themselves. *)

module M : sig
  type t : bits8 addressable
end = struct
  type t : bits8 addressable
end
[%%expect{|
module M : sig type t : bits8 addressable end
|}]

module M : sig
  type t : (bits8 & value) addressable
end = struct
  type t : (bits8 & value) addressable
end
[%%expect{|
module M : sig type t : (bits8 & value) addressable end
|}]

(* [any addressable] in signatures. *)

module M : sig
  type t : any addressable
end = struct
  type t : bits8 addressable
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
  type t : any
end = struct
  type t : any addressable
end
[%%expect{|
module M : sig type t : any end
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
