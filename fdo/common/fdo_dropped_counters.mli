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

(** FDO counters that the compiler drops: those whose edge or instruction an
    optimization removes while the code they count stays, such as the entry
    counter of an inlined function. No instruction is left to measure a dropped
    counter, so its count is unknown rather than zero.

    Counters are dropped along with the reason, and registered for the
    compilation unit so that the loss can be reported. *)

module Reason : sig
  type t =
    | Inlined_call
        (** The entry counters of a function, where a call to it is inlined. *)
    | Self_tail_call_to_jump
        (** The counter of a self tail call that becomes a jump. *)
    | Switch_to_single_arm
        (** The counters of the only arm of a switch that can be taken. *)
    | Switch_with_identical_arms
        (** The counters of the arms of a switch that all do the same. *)
    | Switch_to_lookup_table
        (** The counters of the arms of a switch that becomes a table lookup. *)

  val to_string : t -> string
end

(** A set of dropped counters, each with the reason it was first dropped for. *)
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
