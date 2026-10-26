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

(** Pseudo-instrumentation counters for feedback-directed optimization (FDO).

    A counter marks a point in the control flow of the source program: a
    function entry, a branch edge, or a call site. Counters are attached to the
    code early, carried through every optimization, and emitted only as
    metadata. The resulting binary then contains no "actual" counters, just a
    list of labels for every address containing a branch.

    We then collect counts on how many times branches are taken within
    particular dynamic calling contexts from the profile of a run. These are
    then mapped back to counters, giving an execution count per counter. A later
    build of the same program regenerates the same counters and looks up their
    counts to guide decisions such as block layout and function ordering.

    A counter is a {!Position.t} together with its inlining stack (the call
    sites (innermost first) that its code was inlined through). Counts are kept
    per inlining context, so code inlined at different call sites is profiled
    separately. As code is transformed, counters are rewritten by:
    - {!inline}: the counters of an inlined body get the call site as context;
    - {!specialize}: when inlining a call copies a function defined in the
      callee (e.g. by an inlined functor application), the call site and its
      inlining context are appended to the copy context of the copy's counters.

    Profiles must match across builds, so identities come from source scopes and
    numbering (see {!Fdo_counter_identity}). Function entries and module-level
    calls survive edits of function bodies, while interior positions do not. *)

module Position_hash = Fdo_counter_identity.Position_hash
module Body_hash = Fdo_counter_identity.Body_hash
module Name = Fdo_counter_identity.Name
module Function = Fdo_counter_identity.Function
module Position = Fdo_counter_identity.Position

module Hashed : sig
  (** Position hashes, innermost first. *)
  type t = Position_hash.t list
end

type t

(** A counter at the given position, with no inlining context. *)
val of_position : Position.t -> t

(** The innermost position: where the counter is in the source program. *)
val position : t -> Position.t

(** The call sites, innermost first, that the counter's code was inlined
    through. *)
val inlining_stack : t -> Position.t list

(** [t], inlined at the call site [at]. [at] is a counter rather than a position
    because the call site may itself be in inlined code: its inlining context is
    appended too. A counter [at] that is not at a call is a fatal error. *)
val inline : t -> at:t -> t

(** [t], in a copy of its function made when the call at [at] was inlined. Only
    the outermost position, the one in the copied function, gets [at] in its
    copy context. The inlining stack keeps its length. *)
val specialize : t -> at:t -> t

val equal : t -> t -> bool

val compare : t -> t -> int

(** Canonical string representation for metadata and dumps. *)
val to_string : t -> string

(** Hash the position and each inlining level, innermost first. *)
val hash : t -> Hashed.t

module Tbl : Hashtbl.S with type key = t

(** Append counters not already present, preserving first-occurrence order. *)
val add_all : t list -> t list -> t list
