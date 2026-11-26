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

(** The counters attached to one point of the program, see [Fdo_counter]. The
    counters can represent either a control-flow edge, such as a switch arm, or
    a function entry. Callsites can have at most one counter and hence don't use
    this type. *)

type t

val empty : t

val is_empty : t -> bool

val singleton : Fdo_counter.t -> t

val of_list : Fdo_counter.t list -> t

val to_list : t -> Fdo_counter.t list

(** The union of two counter sets, for points that are merged into one.
    Duplicates are removed. *)
val combine : t -> t -> t

(** [combine] over a list, in order. *)
val union : t list -> t

(** Duplicates created by [f] are removed. *)
val map : (Fdo_counter.t -> Fdo_counter.t) -> t -> t

(** Ignores order. *)
val equal : t -> t -> bool

(** [(counter ...)], on one line. *)
val print : Format.formatter -> t -> unit

(** {2 Dumps of the backend IRs}

    Every IR prints its counters in the same way, so that tests can read them
    from any dump: [keyword(counter ...)], on one line. Each of these printers
    prints nothing when there are no counters, and a space before them
    otherwise. *)

(** The counters of an edge, as [fdo_annotation(...)]. *)
val print_edge : Format.formatter -> t -> unit

(** The same, for IRs that print them before the code of the edge: with a break
    after them instead of a space before. *)
val print_edge_before : Format.formatter -> t -> unit

(** The same as [keyword(...)], for edges of another kind. *)
val print_tagged : keyword:string -> Format.formatter -> t -> unit

(** The counter of a call, as [callsite_counter(...)]. *)
val print_callsite_counter : Format.formatter -> Fdo_counter.t option -> unit

(** The counters bundled with a function body hash. Used for function entries.
*)
module With_body_hash : sig
  type annotation = t

  type t

  val create :
    annotation:annotation -> body_hash:Fdo_counter.Body_hash.t option -> t

  val annotation : t -> annotation

  val body_hash : t -> Fdo_counter.Body_hash.t option

  val with_annotation : t -> annotation -> t

  val with_body_hash : t -> Fdo_counter.Body_hash.t -> t

  val empty : t

  val is_empty : t -> bool

  val equal : t -> t -> bool

  (** [(counter ... body_hash:hash)], on one line. *)
  val print : Format.formatter -> t -> unit

  (** As [fdo_entry_annotation(...)], see {!print_edge}. *)
  val print_entry : Format.formatter -> t -> unit
end
