(* TEST
 include stdlib_upstream_compatible;
 {
   flags = "-extension layouts_beta";
   expect;
 }
 {
   flags = "-extension layouts_beta -no-ikinds";
   expect;
 }
*)

(* Addressability is printed as a kind operator. *)

type t_any_addressable : any addressable
type t_bits8_addressable : bits8 addressable
type t_float64_addressable : float64 addressable

kind_ k
type t_abstract_addressable : k addressable

[%%expect{|
type t_any_addressable : any addressable
type t_bits8_addressable : bits8 addressable
type t_float64_addressable : float64 addressable
kind_ k
type t_abstract_addressable : k addressable
|}]

(* Addressability does not affect a type's standalone representation. *)

type t : bits8 addressable
let f (t : t) = t

[%%expect{|
type t : bits8 addressable
val f : t -> t = <fun>
|}]

(* The inherently-addressable kinds are subkinds of [any addressable]. *)

type ('a : any addressable) require_addressable

type t_void : void
type t_bits64 : bits64
type t_word : word
type t_vec128 : vec128
type t_vec256 : vec256
type t_vec512 : vec512
type t_value : value
type t_product : bits8 & float64

type accepts_void = t_void require_addressable
type accepts_bits64 = t_bits64 require_addressable
type accepts_word = t_word require_addressable
type accepts_vec128 = t_vec128 require_addressable
type accepts_vec256 = t_vec256 require_addressable
type accepts_vec512 = t_vec512 require_addressable
type accepts_value = t_value require_addressable
type accepts_product = t_product require_addressable
type accepts_explicit = t_bits8_addressable require_addressable
type accepts_abstract = t_abstract_addressable require_addressable

[%%expect{|
type ('a : any addressable) require_addressable
type t_void : void
type t_bits64 : bits64
type t_word : word
type t_vec128 : vec128
type t_vec256 : vec256
type t_vec512 : vec512
type t_value
type t_product : bits8 & float64
type accepts_void = t_void require_addressable
type accepts_bits64 = t_bits64 require_addressable
type accepts_word = t_word require_addressable
type accepts_vec128 = t_vec128 require_addressable
type accepts_vec256 = t_vec256 require_addressable
type accepts_vec512 = t_vec512 require_addressable
type accepts_value = t_value require_addressable
type accepts_product = t_product require_addressable
type accepts_explicit = t_bits8_addressable require_addressable
type accepts_abstract = t_abstract_addressable require_addressable
|}]

(* Applying [addressable] to an inherently-addressable kind is the identity. *)

type t_bits64_addressable : bits64 addressable
type ('a : bits64) require_bits64
type equal_to_bits64 = t_bits64_addressable require_bits64

[%%expect{|
Line 1, characters 35-46:
1 | type t_bits64_addressable : bits64 addressable
                                       ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64".

type t_bits64_addressable : bits64
type ('a : bits64) require_bits64
type equal_to_bits64 = t_bits64_addressable require_bits64
|}]

(* Non-addressable kinds do not satisfy an addressable bound. *)

type t_bits8 : bits8
type rejects_bits8 = t_bits8 require_addressable

[%%expect{|
type t_bits8 : bits8
Line 2, characters 21-28:
2 | type rejects_bits8 = t_bits8 require_addressable
                         ^^^^^^^
Error: This type "t_bits8" should be an instance of type "('a : any addressable)"
       The kind of t_bits8 is bits8
         because of the definition of t_bits8 at line 1, characters 0-20.
       But the kind of t_bits8 must be a subkind of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

type t_any : any
type rejects_any = t_any require_addressable

[%%expect{|
type t_any : any
Line 2, characters 19-24:
2 | type rejects_any = t_any require_addressable
                       ^^^^^
Error: This type "t_any" should be an instance of type "('a : any addressable)"
       The kind of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the kind of t_any must be a subkind of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

(* [k] and [k addressable] are distinct when [k] is not addressable. *)

type ('a : bits8) require_bits8
type rejects_addressable_bits8 = t_bits8_addressable require_bits8

[%%expect{|
type ('a : bits8) require_bits8
Line 2, characters 33-52:
2 | type rejects_addressable_bits8 = t_bits8_addressable require_bits8
                                     ^^^^^^^^^^^^^^^^^^^
Error: This type "t_bits8_addressable" should be an instance of type
         "('a : bits8)"
       The kind of t_bits8_addressable is bits8 addressable
         because of the definition of t_bits8_addressable at line 2, characters 0-44.
       But the kind of t_bits8_addressable must be a subkind of bits8
         because of the definition of require_bits8 at line 1, characters 0-31.
|}]

type t_abstract : k
type rejects_abstract = t_abstract require_addressable

[%%expect{|
type t_abstract : k
Line 2, characters 24-34:
2 | type rejects_abstract = t_abstract require_addressable
                            ^^^^^^^^^^
Error: This type "t_abstract" should be an instance of type
         "('a : any addressable)"
       The kind of t_abstract is k
         because of the definition of t_abstract at line 1, characters 0-19.
       But the kind of t_abstract must be a subkind of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

(* In particular, [any addressable] is a subkind of [any]. *)

type ('a : any) require_any
type accepts_addressable_any = t_any_addressable require_any

[%%expect{|
type ('a : any) require_any
type accepts_addressable_any = t_any_addressable require_any
|}]
