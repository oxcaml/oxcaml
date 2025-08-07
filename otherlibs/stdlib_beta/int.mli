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

(** Signed integer values.

    These integers are {!Sys.int_size} bits wide
    and use two's complement representation.
    All operations are taken modulo 2{^[Sys.int_size]}.
    They do not fail on overflow. *)

(** {1:ints Integers} *)

(** The type for integer values. *)
type t = int [@@immediate]

(** The number of bits in an integer of type {!int}. *)
val size : int

(** The integer 0. *)
val zero : int

(** The integer 1. *)
val one : int

(** The integer -1. *)
val minus_one : int

(** Unary negation. *)
external neg : int -> int = "%int_neg"

(** Addition. *)
external add : int -> int -> int = "%int_add"

(** Subtraction. *)
external sub : int -> int -> int = "%int_sub"

(** Multiplication. *)
external mul : int -> int -> int = "%int_mul"

(** Integer division. This division rounds the real quotient of
    its arguments towards zero, as specified for {!Stdlib.(/)}.
    @raise Division_by_zero if the second argument is zero. *)
external div : int -> int -> int = "%int_div"

(** Same as {!div}, except that arguments and result are interpreted as {e
    unsigned} integers. *)
val unsigned_div : int -> int -> int

(** Integer remainder. If [y] is not zero, [rem x y = sub x (mul (div x y)
    y)]. If [y] is zero, [rem x y] raises [Division_by_zero]. *)
external rem : int -> int -> int = "%int_mod"

(** Same as {!rem}, except that arguments and result are interpreted as {e
    unsigned} integers. *)
val unsigned_rem : int -> int -> int

(** [succ x] is [add x 1]. *)
external succ : int -> int = "%int_succ"

(** [pred x] is [sub x 1]. *)
external pred : int -> int = "%int_pred"

(** [abs x] is the absolute value of [x]. That is [x] if [x] is positive and
    [neg x] if [x] is negative. {b Warning.} This may be negative if the
    argument is {!min_int}. *)
val abs : int -> int

(** [max_int] is the greatest representable integer,
    [2{^[size - 1]} - 1]. *)
val max_int : int

(** [min_int] is the smallest representable integer,
    [-2{^[size - 1]}]. *)
val min_int : int

(** Bitwise logical and. *)
external logand : int -> int -> int = "%int_and"

(** Bitwise logical or. *)
external logor : int -> int -> int = "%int_or"

(** Bitwise logical exclusive or. *)
external logxor : int -> int -> int = "%int_xor"

(** Bitwise logical negation. *)
val lognot : int -> int

(** [shift_left x n] shifts [x] to the left by [n] bits. The result
    is unspecified if [n < 0] or [n >= ]{!size}. *)
external shift_left : int -> int -> int = "%int_lsl"

(** [shift_right x n] shifts [x] to the right by [n] bits. This is an
    arithmetic shift: the sign bit of [x] is replicated and inserted
    in the vacated bits. The result is unspecified if [n < 0] or
    [n >=]{!size}. *)
external shift_right : int -> int -> int = "%int_asr"

(** [shift_right x n] shifts [x] to the right by [n] bits. This is a
    logical shift: zeroes are inserted in the vacated bits regardless
    of the sign of [x]. The result is unspecified if [n < 0] or
    [n >=]{!size}. *)
external shift_right_logical : int -> int -> int = "%int_lsr"

(** {1:preds Predicates and comparisons} *)

(** [equal x y] is [true] if and only if [x = y]. *)
external equal : int -> int -> bool = "%int_equal"

(** [compare x y] is {!Stdlib.compare}[ x y] but more efficient. *)
external compare : int -> int -> int = "%int_compare"

(** Same as {!compare}, except that arguments
    are interpreted as {e unsigned} integers. *)
val unsigned_compare : int -> int -> int

(** Return the lesser of the two arguments. *)
val min : int -> int -> int

(** Return the greater of the two arguments. *)
val max : int -> int -> int

(** {1:convert Converting} *)

external to_int : int -> int = "%identity"

external of_int : int -> int = "%identity"

(** Same as {!to_int}, but interprets the argument
    as an {e unsigned} integer. *)
val unsigned_to_int : int -> int option

(** [to_float x] is [x] as a floating point number. *)
external to_float : int -> float = "%float_of_int"

(** [of_float x] truncates [x] to an integer. The result is
    unspecified if the argument is [nan] or falls outside the range of
    representable integers. *)
external of_float : float -> int = "%int_of_float"

(** [to_string x] is the written representation of [x] in decimal. *)
val to_string : int -> string

(** Convert the given string to an integer.
    The string is read in decimal (by default, or if the string
    begins with [0u]) or in hexadecimal, octal or binary if the
    string begins with [0x], [0o] or [0b] respectively.

    The [0u] prefix reads the input as an unsigned integer in the range
    [[0, 2*Int.max_int+1]].  If the input exceeds {!Int.max_int}
    it is converted to the signed integer
    [Int.min_int + input - Int.max_int - 1].

    The [_] (underscore) character can appear anywhere in the string
    and is ignored.
    @raise Failure if the given string is not
    a valid representation of an integer, or if the integer represented
    exceeds the range of integers representable in type [int]. *)
external of_string : string -> int = "caml_int_of_string"

(** A seeded hash function for ints, with the same output value as
    {!Hashtbl.seeded_hash}. This function allows this module to be passed as
    argument to the functor {!Hashtbl.MakeSeeded}. *)
val seeded_hash : int -> int -> int

(** An unseeded hash function for ints, with the same output value as
    {!Hashtbl.hash}. This function allows this module to be passed as argument
    to the functor {!Hashtbl.Make}. *)
val hash : int -> int
