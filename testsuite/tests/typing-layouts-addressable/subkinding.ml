(* TEST
 expect;
*)

(* Subkinding for the [addressable] kind operator. Every addressable kind
   is a subkind of [any addressable]. If [k] is not addressable, then [k]
   and [k addressable] are incomparable. *)

type ('a : any addressable) require_addressable
type ('a : any) require_any
[%%expect{|
Line 1, characters 15-26:
1 | type ('a : any addressable) require_addressable
                   ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* Addressable base layouts are subkinds of [any addressable]. *)

type t_value : value
type ok = t_value require_addressable
[%%expect{|
type t_value
Line 2, characters 18-37:
2 | type ok = t_value require_addressable
                      ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

type t_value_or_null : value_or_null
type ok = t_value_or_null require_addressable
[%%expect{|
type t_value_or_null : value_or_null
Line 2, characters 26-45:
2 | type ok = t_value_or_null require_addressable
                              ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

type t_bits64 : bits64
type ok = t_bits64 require_addressable
[%%expect{|
type t_bits64 : bits64
Line 2, characters 19-38:
2 | type ok = t_bits64 require_addressable
                       ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

type t_word : word
type ok = t_word require_addressable
[%%expect{|
type t_word : word
Line 2, characters 17-36:
2 | type ok = t_word require_addressable
                     ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

type t_vec128 : vec128
type ok = t_vec128 require_addressable
[%%expect{|
type t_vec128 : vec128
Line 2, characters 19-38:
2 | type ok = t_vec128 require_addressable
                       ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

(* Unaddressable base layouts are not subkinds of [any addressable]. *)

type t_bits8 : bits8
type bad = t_bits8 require_addressable
[%%expect{|
type t_bits8 : bits8
Line 2, characters 19-38:
2 | type bad = t_bits8 require_addressable
                       ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

type t_float64 : float64
type bad = t_float64 require_addressable
[%%expect{|
type t_float64 : float64
Line 2, characters 21-40:
2 | type bad = t_float64 require_addressable
                         ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

type t_void : void
type bad = t_void require_addressable
[%%expect{|
type t_void : void
Line 2, characters 18-37:
2 | type bad = t_void require_addressable
                      ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

type t_untagged : untagged_immediate
type bad = t_untagged require_addressable
[%%expect{|
type t_untagged : untagged_immediate
Line 2, characters 22-41:
2 | type bad = t_untagged require_addressable
                          ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

(* [any] is not a subkind of [any addressable]... *)

type t_any : any
type bad = t_any require_addressable
[%%expect{|
type t_any : any
Line 2, characters 17-36:
2 | type bad = t_any require_addressable
                     ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

(* ...but [any addressable] is a subkind of [any]. *)

type t_any_addr : any addressable
type ok = t_any_addr require_any
[%%expect{|
Line 1, characters 22-33:
1 | type t_any_addr : any addressable
                          ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* Kinds made addressable are subkinds of [any addressable]. *)

type t_bits8_addr : bits8 addressable
type ok = t_bits8_addr require_addressable
[%%expect{|
Line 1, characters 26-37:
1 | type t_bits8_addr : bits8 addressable
                              ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t_float64_addr : float64 addressable
type ok = t_float64_addr require_addressable
[%%expect{|
Line 1, characters 30-41:
1 | type t_float64_addr : float64 addressable
                                  ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* A product of addressable components is addressable. *)

type t_prod : value & bits64
type ok = t_prod require_addressable
[%%expect{|
type t_prod : value & bits64
Line 2, characters 17-36:
2 | type ok = t_prod require_addressable
                     ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

(* A product with an unaddressable component is not addressable... *)

type t_prod_bad : value & bits8
type bad = t_prod_bad require_addressable
[%%expect{|
type t_prod_bad : value & bits8
Line 2, characters 22-41:
2 | type bad = t_prod_bad require_addressable
                          ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

(* ...unless it is made addressable. *)

type t_prod_addr : (value & bits8) addressable
type ok = t_prod_addr require_addressable
[%%expect{|
Line 1, characters 35-46:
1 | type t_prod_addr : (value & bits8) addressable
                                       ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* Concrete types with addressable kinds work too. *)

type ok = int require_addressable
type ok = string require_addressable
type ok = int64# require_addressable
type ok = nativeint# require_addressable
[%%expect{|
Line 1, characters 14-33:
1 | type ok = int require_addressable
                  ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

type bad = float# require_addressable
[%%expect{|
Line 1, characters 18-37:
1 | type bad = float# require_addressable
                      ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
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
Error: Unknown kind modifier addressable
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
Error: Unknown kind modifier addressable
|}]

(* [bits8] and [bits8 addressable] are incomparable. *)

module M : sig
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

module M : sig
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

(* [addressable] does not distribute through products:
   [(bits8 & value) addressable] and [bits8 addressable & value] are
   incomparable. *)

module M : sig
  type t : bits8 addressable & value
end = struct
  type t : (bits8 & value) addressable
end
[%%expect{|
Line 4, characters 27-38:
4 |   type t : (bits8 & value) addressable
                               ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

module M : sig
  type t : (bits8 & value) addressable
end = struct
  type t : bits8 addressable & value
end
[%%expect{|
Line 4, characters 17-28:
4 |   type t : bits8 addressable & value
                     ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* [k addressable] kinds are equal to themselves. *)

module M : sig
  type t : bits8 addressable
end = struct
  type t : bits8 addressable
end
[%%expect{|
Line 4, characters 17-28:
4 |   type t : bits8 addressable
                     ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

module M : sig
  type t : (bits8 & value) addressable
end = struct
  type t : (bits8 & value) addressable
end
[%%expect{|
Line 4, characters 27-38:
4 |   type t : (bits8 & value) addressable
                               ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* [any addressable] in signatures. *)

module M : sig
  type t : any addressable
end = struct
  type t : bits8 addressable
end
[%%expect{|
Line 4, characters 17-28:
4 |   type t : bits8 addressable
                     ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

module M : sig
  type t : any addressable
end = struct
  type t : bits8
end
[%%expect{|
Line 2, characters 15-26:
2 |   type t : any addressable
                   ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

module M : sig
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

module M : sig
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
