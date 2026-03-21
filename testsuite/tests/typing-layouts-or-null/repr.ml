(* TEST
 expect;
*)

type t_immediate =
  | Nullish [@repr null]
  | Int of int [@repr immediate]

[%%expect{|
type t_immediate = Nullish [@repr null] | Int of int [@repr immediate]
|}]

let map_immediate = function
  | Nullish -> Nullish
  | Int n -> Int (n + 1)

[%%expect{|
val map_immediate : t_immediate -> t_immediate = <fun>
|}]

type with_boxed =
  | Nullish_boxed [@repr null]
  | Int_boxed of int [@repr immediate]
  | Boxed of string

[%%expect{|
type with_boxed =
    Nullish_boxed
  [@repr null]
  | Int_boxed of int
  [@repr immediate]
  | Boxed of string
|}]

let classify = function
  | Nullish_boxed -> "null"
  | Int_boxed _ -> "int"
  | Boxed _ -> "boxed"

[%%expect{|
val classify : with_boxed -> string = <fun>
|}]

type ('a : value pointer) pointer_or_null =
  | Null_ptr [@repr null]
  | Ptr of 'a [@repr pointer]

[%%expect{|
type ('a : value pointer) pointer_or_null =
    Null_ptr
  [@repr null]
  | Ptr of 'a
  [@repr pointer]
|}]

type int_or_err =
  | Int_or_err of int [@repr immediate]
  | Error of string [@repr pointer]

[%%expect{|
type int_or_err =
    Int_or_err of int
  [@repr immediate]
  | Error of string
  [@repr pointer]
|}]

type bytes_or_int =
  | Int_bytes of int [@repr immediate]
  | Bytes_value of bytes [@repr pointer]

type array_or_int =
  | Int_array of int [@repr immediate]
  | Array_value of int array [@repr pointer]

type ref_or_int =
  | Int_ref of int [@repr immediate]
  | Ref_value of int ref [@repr pointer]

type lazy_or_int =
  | Int_lazy of int [@repr immediate]
  | Lazy_value of int lazy_t [@repr pointer]

type fn_or_int =
  | Int_fn of int [@repr immediate]
  | Fn_value of (int -> int) [@repr pointer]

class point (x : int) = object
  method x = x
end

type obj_or_int =
  | Int_obj of int [@repr immediate]
  | Obj_value of < x : int > [@repr pointer]

[%%expect{|
type bytes_or_int =
    Int_bytes of int
  [@repr immediate]
  | Bytes_value of bytes
  [@repr pointer]
type array_or_int =
    Int_array of int
  [@repr immediate]
  | Array_value of int array
  [@repr pointer]
type ref_or_int =
    Int_ref of int
  [@repr immediate]
  | Ref_value of int ref
  [@repr pointer]
type lazy_or_int =
    Int_lazy of int
  [@repr immediate]
  | Lazy_value of int lazy_t
  [@repr pointer]
type fn_or_int =
    Int_fn of int
  [@repr immediate]
  | Fn_value of (int -> int)
  [@repr pointer]
class point : int -> object method x : int end
type obj_or_int =
    Int_obj of int
  [@repr immediate]
  | Obj_value of < x : int >
  [@repr pointer]
|}]

type ('a : value) repr_value =
  | Null_value [@repr null]
  | Value of 'a [@repr value]

[%%expect{|
type 'a repr_value = Null_value [@repr null] | Value of 'a [@repr value]
|}]

let to_option_value = function
  | Null_value -> None
  | Value x -> Some x

[%%expect{|
val to_option_value : 'a repr_value -> 'a option = <fun>
|}]

type ('a : value pointer) null_immediate_pointer =
  | NIP [@repr null]
  | IIP of int [@repr immediate]
  | PIP of 'a [@repr pointer]

[%%expect{|
type ('a : value pointer) null_immediate_pointer =
    NIP
  [@repr null]
  | IIP of int
  [@repr immediate]
  | PIP of 'a
  [@repr pointer]
|}]

let classify_nip = function
  | NIP -> 0
  | IIP n -> n
  | PIP _ -> -1

[%%expect{|
val classify_nip : ('a : value pointer). 'a null_immediate_pointer -> int =
  <fun>
|}]

type bad_immediate =
  | K
  | I of int [@repr immediate]

[%%expect{|
Lines 1-3, characters 0-30:
1 | type bad_immediate =
2 |   | K
3 |   | I of int [@repr immediate]
Error: Invalid [@repr] declaration:
       [@repr immediate] must not coexist with ordinary constant constructors.
|}]

type dup_immediate =
  | I1 of int [@repr immediate]
  | I2 of int [@repr immediate]

[%%expect{|
Lines 1-3, characters 0-31:
1 | type dup_immediate =
2 |   | I1 of int [@repr immediate]
3 |   | I2 of int [@repr immediate]
Error: Invalid [@repr] declaration:
       there may be at most one [@repr immediate] constructor.
|}]

type ptr : value pointer

type bad_pointer =
  | P of ptr [@repr pointer]
  | B of string

[%%expect{|
type ptr : value pointer
Lines 3-5, characters 0-15:
3 | type bad_pointer =
4 |   | P of ptr [@repr pointer]
5 |   | B of string
Error: Invalid [@repr] declaration:
       [@repr pointer] may only coexist with [@repr null] and [@repr immediate].
|}]

type bad_pointer_constant =
  | P of ptr [@repr pointer]
  | C

[%%expect{|
Lines 1-3, characters 0-5:
1 | type bad_pointer_constant =
2 |   | P of ptr [@repr pointer]
3 |   | C
Error: Invalid [@repr] declaration:
       [@repr pointer] may only coexist with [@repr null] and [@repr immediate].
|}]

type ('a : value) bad_value_mix =
  | N [@repr null]
  | V of 'a [@repr value]
  | I of int [@repr immediate]

[%%expect{|
Lines 1-4, characters 0-30:
1 | type ('a : value) bad_value_mix =
2 |   | N [@repr null]
3 |   | V of 'a [@repr value]
4 |   | I of int [@repr immediate]
Error: Invalid [@repr] declaration:
       [@repr value] may only coexist with [@repr null].
|}]

type ('a : value) bad_value_boxed =
  | Nvb [@repr null]
  | Vvb of 'a [@repr value]
  | Cvb

[%%expect{|
Lines 1-4, characters 0-7:
1 | type ('a : value) bad_value_boxed =
2 |   | Nvb [@repr null]
3 |   | Vvb of 'a [@repr value]
4 |   | Cvb
Error: Invalid [@repr] declaration:
       [@repr value] may only coexist with [@repr null].
|}]

type ('a : value) bad_value_boxed_payload =
  | Nvp [@repr null]
  | Vvp of 'a [@repr value]
  | Bvp of string

[%%expect{|
Lines 1-4, characters 0-17:
1 | type ('a : value) bad_value_boxed_payload =
2 |   | Nvp [@repr null]
3 |   | Vvp of 'a [@repr value]
4 |   | Bvp of string
Error: Invalid [@repr] declaration:
       [@repr value] may only coexist with [@repr null].
|}]

let partial_with_boxed = function
  | Nullish_boxed -> 0
  | Int_boxed _ -> 1

[%%expect{|
Lines 1-3, characters 25-20:
1 | .........................function
2 |   | Nullish_boxed -> 0
3 |   | Int_boxed _ -> 1
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
+

val partial_with_boxed : with_boxed -> int = <fun>
|}]

let redundant_with_boxed = function
  | Nullish_boxed -> 0
  | Int_boxed _ -> 1
  | Boxed _ -> 2
  | Boxed _ -> 3

[%%expect{|
Line 5, characters 4-11:
5 |   | Boxed _ -> 3
        ^^^^^^^
Warning 11 [redundant-case]: this match case is unused.

val redundant_with_boxed : with_boxed -> int = <fun>
|}]

module type Immediate_sig = sig
  type t =
    | Null_sig [@repr null]
    | Int_sig of int [@repr immediate]
end

module Immediate_ok : Immediate_sig = struct
  type t =
    | Null_sig [@repr null]
    | Int_sig of int [@repr immediate]
end

[%%expect{|
module type Immediate_sig =
  sig type t = Null_sig [@repr null] | Int_sig of int [@repr immediate] end
module Immediate_ok : Immediate_sig
|}]

module Immediate_bad : Immediate_sig = struct
  type t =
    | Null_sig [@repr null]
    | Int_sig of int
end

[%%expect{|
Lines 1-5, characters 39-3:
1 | .......................................struct
2 |   type t =
3 |     | Null_sig [@repr null]
4 |     | Int_sig of int
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Null_sig [@repr null] | Int_sig of int end
       is not included in
         Immediate_sig
       Type declarations do not match:
         type t = Null_sig [@repr null] | Int_sig of int
       is not included in
         type t = Null_sig [@repr null] | Int_sig of int [@repr immediate]
       Their internal representations differ:
       the first declaration has a constructor represented as a null pointer.
       Hint: add [@@or_null] or [@@or_null_reexport].
|}]
