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

(** Pseudo-instrumentation counters for feedback-directed optimization.

    A counter marks a point in the control flow of the source program: a
    function entry, a branch edge or a call site. Counters are attached to the
    code early, carried through every optimization, and emitted as metadata
    rather than as instructions, so an instrumented binary runs the same code as
    an uninstrumented one. Branch samples from a run are mapped back to
    counters, giving the profile: an execution count per counter. A later build
    of the same program regenerates the same counters and looks up their counts,
    to guide decisions such as block layout and function ordering.

    A counter is a {!Position.t} together with its inlining stack: the call
    sites, innermost first, that its code was inlined through. Counts are kept
    per inlining context, so code inlined at different call sites is profiled
    separately. As code is transformed, counters are rewritten by:
    - {!inline}: the counters of an inlined body get the call site as context;
    - {!specialize}: when inlining a call copies a function defined in the
      callee (e.g. by an inlined functor application), the counters of the copy
      are renamed after it.

    Profiles must match across builds, so identities come from source scopes and
    numbering, never from symbols (see {!Fdo_counter_identity}). Function
    entries and named module instantiations survive edits of the bodies;
    interior positions do not. *)

module Edge = Fdo_counter_identity.Edge
module Hash = Fdo_counter_identity.Hash
module Function_body_hash = Fdo_counter_identity.Function_body_hash
module Definition_id = Fdo_counter_identity.Definition_id
module Position = Fdo_counter_identity.Position

module Hashed : sig
  (** Position hashes, innermost first. *)
  type t = Hash.t list
end

type t =
  { position : Position.t;
    inlining_stack : Position.t list  (** Call sites, innermost first. *)
  }

(** [t], inlined at the call site [at]. [at] is a counter rather than a position
    because the call site may itself be in inlined code: its inlining context is
    appended too. *)
val inline : t -> at:t -> t

(** [t], in a copy of its function made when the call at [at] was inlined. Only
    the outermost position, the one in the copied function, is renamed; the
    inlining stack keeps its length. *)
val specialize : t -> at:t -> t

(** Compare counters structurally, not by hash. *)
val equal : t -> t -> bool

(** Canonical, unambiguous spelling for metadata and dumps. *)
val to_string : t -> string

(** Hash the position and each inlining level, innermost first. *)
val hash : t -> Hashed.t

(** Append counters not already present, preserving first-occurrence order. *)
val add_all : t list -> t list -> t list
