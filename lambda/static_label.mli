(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** Static exception labels used in Lambda intermediate representation.

    Static exceptions provide a mechanism for local control flow transfer
    within functions. Unlike regular exceptions, the target handler is
    statically known and control flow cannot escape function boundaries.
*)

type t

include Identifiable.S with type t := t

val to_string : t -> string

val format : Format.formatter -> t -> unit

(** Special static label used for anticipated static raises (guards).
    This corresponds to the legacy hardcoded value 0. *)
val fail : t

(** A sequence for generating fresh static labels. *)
type sequence

(** Create a new sequence for generating static labels. *)
val make_sequence : unit -> sequence

(** Reset a sequence to start from label 0. *)
val reset : sequence -> unit

(** Get the next label that would be generated without consuming it. *)
val get : sequence -> t

(** Generate and consume the next label from the sequence. *)
val get_and_incr : sequence -> t

(** Convert a static label to its underlying integer representation.
    This function should only be used when interfacing with legacy code
    that expects raw integers. *)
val to_int_unsafe : t -> int

(** Convert an integer to a static label.
    This function should only be used when interfacing with legacy code
    that provides raw integers. *)
val of_int_unsafe : int -> t