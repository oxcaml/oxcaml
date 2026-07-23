(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

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
