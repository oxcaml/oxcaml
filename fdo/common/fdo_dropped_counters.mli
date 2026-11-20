(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

(** FDO counters dropped by optimizations.

    A counter is dropped when an optimization removes the branch or call it is
    attached to while keeping the underlying code (for example, the entry
    counter of an inlined function). Because no instruction remains to measure
    it, the counter's execution count is unknown rather than zero.

    Dropped counters are recorded with the reason they were dropped and
    registered per compilation unit for reporting. *)

module Reason : sig
  type t =
    | Inlined_call
        (** Entry counters of a function whose call site was inlined. *)
    | Self_tail_call_to_jump
        (** Counter of a self tail call converted to a jump. *)
    | Switch_to_single_arm
        (** Counters of a switch reduced to a single reachable arm. *)
    | Switch_with_identical_arms
        (** Counters of a switch whose arms all jump to the same action. *)
    | Switch_to_lookup_table
        (** Counters of a switch lowered to a lookup table. *)

  val to_string : t -> string
end

(** Dropped counters, mapped to the reason each was first dropped. *)
type t

val empty : t

val add : t -> reason:Reason.t -> Fdo_annotation.t -> t

val union : t -> t -> t

(** In the order of the counters. *)
val to_list : t -> (Fdo_counter.t * Reason.t) list

(** One counter per line, with its reason. *)
val print : Format.formatter -> t -> unit

(** {2 The counters dropped in the current compilation unit} *)

val register : reason:Reason.t -> Fdo_annotation.t -> unit

val register_all : t -> unit

val registered : unit -> t

(** To be called before compiling a unit. *)
val reset : unit -> unit
