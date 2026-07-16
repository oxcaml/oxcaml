(* TEST
 flags = "-extension-universe alpha -w -181";
 expect;
*)

type ('a : any addressable) require_addressable

[%%expect {|
type ('a : any addressable) require_addressable
|}]

(* [addressable] does not change the calling convention of its argument. *)
type t : bits8 addressable
let f (t : t) = t
type check_t = t require_addressable

[%%expect {|
type t : bits8 addressable mod external_
val f : t -> t = <fun>
type check_t = t require_addressable
|}]

type u : bits8
type bad_unaddressable = u require_addressable

[%%expect {|
type u : bits8
Line 2, characters 25-26:
2 | type bad_unaddressable = u require_addressable
                             ^
Error: This type "u" should be an instance of type "('a : any addressable)"
       The kind of u is bits8
         because of the definition of u at line 1, characters 0-14.
       But the kind of u must be a subkind of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

type ('a : bits8) require_bits8
type bad_addressable = t require_bits8

[%%expect {|
type ('a : bits8) require_bits8
Line 2, characters 23-24:
2 | type bad_addressable = t require_bits8
                           ^
Error: This type "t" should be an instance of type "('a : bits8)"
       The kind of t is bits8 addressable mod external_
         because of the definition of t at line 1, characters 0-26.
       But the kind of t must be a subkind of bits8
         because of the definition of require_bits8 at line 1, characters 0-31.
|}]

(* The intrinsically-addressable atomic kinds. *)
type t_void : void
type t_bits64 : bits64
type t_word : word
type t_vec128 : vec128
type t_vec256 : vec256
type t_vec512 : vec512
type t_value : value

type check_void = t_void require_addressable
type check_bits64 = t_bits64 require_addressable
type check_word = t_word require_addressable
type check_vec128 = t_vec128 require_addressable
type check_vec256 = t_vec256 require_addressable
type check_vec512 = t_vec512 require_addressable
type check_value = t_value require_addressable

[%%expect {|
type t_void : void
type t_bits64 : bits64
type t_word : word
type t_vec128 : vec128
type t_vec256 : vec256
type t_vec512 : vec512
type t_value
type check_void = t_void require_addressable
type check_bits64 = t_bits64 require_addressable
type check_word = t_word require_addressable
type check_vec128 = t_vec128 require_addressable
type check_vec256 = t_vec256 require_addressable
type check_vec512 = t_vec512 require_addressable
type check_value = t_value require_addressable
|}]

(* The remaining atomic kinds are not intrinsically addressable. *)
type t_untagged_immediate : untagged_immediate
type t_bits16 : bits16
type t_bits32 : bits32
type t_float32 : float32
type t_float64 : float64
type t_any : any

[%%expect {|
type t_untagged_immediate : untagged_immediate
type t_bits16 : bits16
type t_bits32 : bits32
type t_float32 : float32
type t_float64 : float64
type t_any : any
|}]

type bad_untagged_immediate = t_untagged_immediate require_addressable
[%%expect {|
Line 1, characters 30-50:
1 | type bad_untagged_immediate = t_untagged_immediate require_addressable
                                  ^^^^^^^^^^^^^^^^^^^^
Error: This type "t_untagged_immediate" should be an instance of type
         "('a : any addressable)"
       The kind of t_untagged_immediate is untagged_immediate
         because of the definition of t_untagged_immediate at line 1, characters 0-46.
       But the kind of t_untagged_immediate must be a subkind of
           any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

type bad_bits16 = t_bits16 require_addressable
[%%expect {|
Line 1, characters 18-26:
1 | type bad_bits16 = t_bits16 require_addressable
                      ^^^^^^^^
Error: This type "t_bits16" should be an instance of type
         "('a : any addressable)"
       The kind of t_bits16 is bits16
         because of the definition of t_bits16 at line 2, characters 0-22.
       But the kind of t_bits16 must be a subkind of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

type bad_bits32 = t_bits32 require_addressable
[%%expect {|
Line 1, characters 18-26:
1 | type bad_bits32 = t_bits32 require_addressable
                      ^^^^^^^^
Error: This type "t_bits32" should be an instance of type
         "('a : any addressable)"
       The kind of t_bits32 is bits32
         because of the definition of t_bits32 at line 3, characters 0-22.
       But the kind of t_bits32 must be a subkind of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

type bad_float32 = t_float32 require_addressable
[%%expect {|
Line 1, characters 19-28:
1 | type bad_float32 = t_float32 require_addressable
                       ^^^^^^^^^
Error: This type "t_float32" should be an instance of type
         "('a : any addressable)"
       The kind of t_float32 is float32
         because of the definition of t_float32 at line 4, characters 0-24.
       But the kind of t_float32 must be a subkind of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

type bad_float64 = t_float64 require_addressable
[%%expect {|
Line 1, characters 19-28:
1 | type bad_float64 = t_float64 require_addressable
                       ^^^^^^^^^
Error: This type "t_float64" should be an instance of type
         "('a : any addressable)"
       The kind of t_float64 is float64
         because of the definition of t_float64 at line 5, characters 0-24.
       But the kind of t_float64 must be a subkind of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

type bad_any = t_any require_addressable
[%%expect {|
Line 1, characters 15-20:
1 | type bad_any = t_any require_addressable
                   ^^^^^
Error: This type "t_any" should be an instance of type "('a : any addressable)"
       The kind of t_any is any
         because of the definition of t_any at line 6, characters 0-16.
       But the kind of t_any must be a subkind of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

(* Products are addressable, and [&] makes their components addressable. *)
type product : bits8 & bits16
type check_product = product require_addressable
type ('a : bits8 addressable & bits16 addressable) require_explicit_product
type check_product_components = product require_explicit_product

type product_with_any : any & bits8
type check_product_with_any = product_with_any require_addressable

type unboxed_record : any addressable = #{ x : int; y : int }

[%%expect {|
type product : bits8 & bits16
type check_product = product require_addressable
type ('a : bits8 & bits16) require_explicit_product
type check_product_components = product require_explicit_product
type product_with_any : any & bits8
type check_product_with_any = product_with_any require_addressable
type unboxed_record = #{ x : int; y : int; }
|}]

(* Applying [addressable] to an addressable kind is a no-op. *)
type idempotent : bits8 addressable addressable
type redundant_intrinsic : bits64 addressable
type redundant_product : (bits8 & bits16) addressable

[%%expect {|
Line 1, characters 36-47:
1 | type idempotent : bits8 addressable addressable
                                        ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 addressable".

type idempotent : bits8 addressable mod external_
Line 2, characters 34-45:
2 | type redundant_intrinsic : bits64 addressable
                                      ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64".

type redundant_intrinsic : bits64
Line 3, characters 42-53:
3 | type redundant_product : (bits8 & bits16) addressable
                                              ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 & bits16".

type redundant_product : bits8 & bits16
|}]

(* The operator composes with abstract kinds and kind aliases. *)
kind_ k
type abstract : k addressable
type check_abstract = abstract require_addressable

[%%expect {|
kind_ k
type abstract : k addressable
type check_abstract = abstract require_addressable
|}]

kind_ addressable_alias = bits8 addressable
type through_alias : addressable_alias
type check_alias = through_alias require_addressable

[%%expect {|
kind_ addressable_alias = bits8 addressable mod external_
type through_alias : bits8 addressable mod external_
type check_alias = through_alias require_addressable
|}]

module type Addressable = sig
  type t : any addressable
end

module Addressable_bits8 : Addressable = struct
  type t : bits8 addressable
end

[%%expect {|
module type Addressable = sig type t : any addressable end
module Addressable_bits8 : Addressable @@ stateless
|}]

(* A fresh representable sort variable can learn an addressability constraint;
   it subsequently defaults to [value], as other unconstrained sorts do. *)
type 'a inferred_addressable = 'a require_addressable

[%%expect {|
type 'a inferred_addressable = 'a require_addressable
|}]

(* Regression: inferring ordinary value sorts must remain unaffected. *)
type ('a : value) padding
type 'a check_value = (int -> 'a) padding

type ('a, 'b) padding2 =
  | Arg_padding : int -> (int -> 'a, 'a) padding2

[%%expect {|
type 'a padding
type 'a check_value = (int -> 'a) padding
type ('a, 'b) padding2 = Arg_padding : int -> (int -> 'a, 'a) padding2
|}]

(* Addressability does not discard the scannable-axis bounds on [any]. *)
type addressable_non_null : any non_null addressable
type ('a : any non_null) require_non_null
type check_addressable_non_null = addressable_non_null require_non_null

[%%expect {|
type addressable_non_null : any non_null addressable
type ('a : any non_null) require_non_null
type check_addressable_non_null = addressable_non_null require_non_null
|}]

type addressable_any : any addressable
type bad_nullability = addressable_any require_non_null

[%%expect {|
type addressable_any : any addressable
Line 2, characters 23-38:
2 | type bad_nullability = addressable_any require_non_null
                           ^^^^^^^^^^^^^^^
Error: This type "addressable_any" should be an instance of type
         "('a : any non_null)"
       The layout of addressable_any is any addressable
         because of the definition of addressable_any at line 1, characters 0-38.
       But the layout of addressable_any must be a sublayout of any non_null
         because of the definition of require_non_null at line 2, characters 0-41.
|}]
