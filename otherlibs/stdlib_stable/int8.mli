(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Signed 8-bit integer values.

    These integers are {8} bits wide and use two's complement representation.
    All operations are taken modulo 2{^8}. They do not fail on overflow. *)

(** {1:ints 8-bit Integers} *)

type t = int8 [@@immediate]
(** The type for 8-bit integer values. *)

val size : int
(** The number of bits in an integer of type {!int8}. *)

val zero : int8
(** The 8-bit integer 0. *)

val one : int8
(** The 8-bit integer 1. *)

val minus_one : int8
(** The 8-bit integer -1. *)

external neg : int8 -> int8 = "%int8_neg"
(** Unary negation. *)

external add : int8 -> int8 -> int8 = "%int8_add"
(** Addition. *)

external sub : int8 -> int8 -> int8 = "%int8_sub"
(** Subtraction. *)

external mul : int8 -> int8 -> int8 = "%int8_mul"
(** Multiplication. *)

external div : int8 -> int8 -> int8 = "%int8_div"
(** Integer division. This division rounds the real quotient of
    its arguments towards zero, as specified for {!Stdlib.(/)}.
    @raise Division_by_zero if the second argument is zero. *)

val unsigned_div : int8 -> int8 -> int8
(** Same as {!div}, except that arguments and result are interpreted as {e
    unsigned} integers. *)

external rem : int8 -> int8 -> int8 = "%int8_mod"
(** Integer remainder. If [y] is not zero, [rem x y = sub x (mul (div x y)
    y)]. If [y] is zero, [rem x y] raises [Division_by_zero]. *)

val unsigned_rem : int8 -> int8 -> int8
(** Same as {!rem}, except that arguments and result are interpreted as {e
    unsigned} integers. *)

external succ : int8 -> int8 = "%int8_succ"
(** [succ x] is [add x 1]. *)

external pred : int8 -> int8 = "%int8_pred"
(** [pred x] is [sub x 1]. *)

val abs : int8 -> int8
(** [abs x] is the absolute value of [x]. That is [x] if [x] is positive and
    [neg x] if [x] is negative. {b Warning.} This may be negative if the
    argument is {!min_int}. *)

val max_int : int8
(** [max_int] is the greatest representable integer,
    [2{^[size - 1]} - 1]. *)

val min_int : int8
(** [min_int] is the smallest representable integer,
    [-2{^[size - 1]}]. *)

external logand : int8 -> int8 -> int8 = "%int8_and"
(** Bitwise logical and. *)

external logor : int8 -> int8 -> int8 = "%int8_or"
(** Bitwise logical or. *)

external logxor : int8 -> int8 -> int8 = "%int8_xor"
(** Bitwise logical exclusive or. *)

val lognot : int8 -> int8
(** Bitwise logical negation. *)

external shift_left : int8 -> int -> int8 = "%int8_lsl"
(** [shift_left x n] shifts [x] to the left by [n] bits. The result
    is unspecified if [n < 0] or [n >= ]{!size}. *)

external shift_right : int8 -> int -> int8 = "%int8_asr"
(** [shift_right x n] shifts [x] to the right by [n] bits. This is an
    arithmetic shift: the sign bit of [x] is replicated and inserted
    in the vacated bits. The result is unspecified if [n < 0] or
    [n >=]{!size}. *)

external shift_right_logical : int8 -> int -> int8 = "%int8_lsr"
(** [shift_right x n] shifts [x] to the right by [n] bits. This is a
    logical shift: zeroes are inserted in the vacated bits regardless
    of the sign of [x]. The result is unspecified if [n < 0] or
    [n >=]{!size}. *)

(** {1:preds Predicates and comparisons} *)

external equal : int8 -> int8 -> bool = "%int8_equal"
(** [equal x y] is [true] if and only if [x = y]. *)

external compare : int8 -> int8 -> int = "%int8_compare"
(** [compare x y] is {!Stdlib.compare}[ x y] but more efficient. *)

external unsigned_compare : int8 -> int8 -> int = "%int8_unsigned_compare"
(** Same as {!compare}, except that arguments are interpreted as {e unsigned} integers. *)

val min : int8 -> int8 -> int8
(** Return the lesser of the two arguments. *)

val max : int8 -> int8 -> int8
(** Return the greater of the two arguments. *)

(** {1:convert Converting} *)

external to_int : int8 -> int = "%int_of_int8"
(** [to_int x] is [x] as an {!int}. *)

external of_int : int -> int8 = "%int8_of_int"
(** [of_int x] truncates the representation of [x] to fit in {!int8}. *)

val unsigned_to_int : int8 -> int
(** Same as {!to_int}, but interprets the argument as an {e unsigned} integer. *)

external to_float : int8 -> float = "%float_of_int8"
(** [to_float x] is [x] as a floating point number. *)

external of_float : float -> int8 = "%int8_of_float"
(** [of_float x] truncates [x] to an integer. The result is
    unspecified if the argument is [nan] or falls outside the range of
    representable integers. *)

val to_string : int8 -> string
(** [to_string x] is the written representation of [x] in decimal. *)

external of_string : string -> int8 = "caml_int8_of_string"
(** Convert the given string to an 8-bit integer.
    The string is read in decimal (by default, or if the string
    begins with [0u]) or in hexadecimal, octal or binary if the
    string begins with [0x], [0o] or [0b] respectively.

    The [0u] prefix reads the input as an unsigned integer in the range
    [[0, 2*Int8.max_int+1]].  If the input exceeds {!Int8.max_int}
    it is converted to the signed integer
    [Int8.min_int + input - Int8.max_int - 1].

    The [_] (underscore) character can appear anywhere in the string
    and is ignored.
    @raise Failure if the given string is not
    a valid representation of an integer, or if the integer represented
    exceeds the range of integers representable in type [int8]. *)

val seeded_hash : int -> int8 -> int
(** A seeded hash function for ints, with the same output value as
    {!Hashtbl.seeded_hash}. This function allows this module to be passed as
    argument to the functor {!Hashtbl.MakeSeeded}. *)

val hash : int8 -> int
(** An unseeded hash function for ints, with the same output value as
    {!Hashtbl.hash}. This function allows this module to be passed as argument
    to the functor {!Hashtbl.Make}. *)
