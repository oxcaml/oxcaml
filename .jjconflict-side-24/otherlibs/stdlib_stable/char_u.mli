(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  James Rayman, Jane Street, New York                   *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

@@ portable

(** The type for untagged characters *)
type t = char#

(** Converting between other types *)

(** Return the ASCII code of the argument.
    @raise Invalid_argument if the argument is outside the range 0--255. *)
val code : char# -> int

(** Return the character with the given ASCII code. *)
val chr : int -> char#

(** Return a string representing the given character,
    with special characters escaped following the lexical conventions
    of OCaml.
    All characters outside the ASCII printable range (32..126) are
    escaped, as well as backslash, double-quote, and single-quote. *)
val escaped : char# -> string

external of_int8_u : int8# -> char# = "%identity"

external to_int8_u : char# -> int8# = "%identity"

val of_char : char -> char#

val to_char : char# -> char

(** Convert the given character to its equivalent lowercase character,
    using the US-ASCII character set. *)
val lowercase_ascii : char# -> char#

(** Convert the given character to its equivalent uppercase character,
    using the US-ASCII character set. *)
val uppercase_ascii : char# -> char#

(** [equal x y] is [true] if and only if [x = y]. *)
val equal: char# -> char# -> bool

(** [compare x y] is {!Stdlib.compare}[ x y] but more efficient. *)
val compare: char# -> char# -> int

(** A seeded hash function for chars, with the same output value as
    {!Hashtbl.seeded_hash}. This function allows this module to be passed as
    argument to the functor {!Hashtbl.MakeSeeded}. *)
val seeded_hash : int -> char# -> int

(** An unseeded hash function for chars, with the same output value as
    {!Hashtbl.hash}. This function allows this module to be passed as argument
    to the functor {!Hashtbl.Make}. *)
val hash : char# -> int
