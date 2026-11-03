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

(** FDO counters (see [Fdo_counter]) whose instruction Simplify removes while
    the code they count stays: the call-site counter of an inlined call, with
    the entry counters of the callee; the counter of a self tail call turned
    into a jump; the counters of the arms of a switch that collapses into a
    single jump. Such a counter is preserved by attaching it to the edges into
    the straight-line code it was in, which execute exactly as often.

    A region is a stretch of straight-line control flow: a function body or a
    continuation handler, up to its terminator. The counters preserved in a
    region go onto the edges into it: the arms of the switches targeting its
    continuation, the entry edge of its function; and, transitively, the edges
    into the regions that continue into it (by a plain [Apply_cont], or as the
    return point of an application), since everything entering such a region
    enters it too.

    Regions are named by continuations and code ids, which are unique within the
    simplification of a compilation unit. The counters are recorded during the
    downward traversal and when rebuilding, and read when rebuilding the
    switches and functions whose edges they go onto, after the regions they lead
    to have been rebuilt. *)

module Region : sig
  type t =
    | Function_entry of Code_id.t
    | Handler of Continuation.t
end

type t

val empty : t

(** The given counters are preserved in [region]. *)
val add_counters : t -> Region.t -> Fdo_annotation.t -> t

(** [region] continues into the handler of [cont]: it jumps to it
    unconditionally, or applies a function that returns to it. *)
val add_continuation_into : t -> Region.t -> Continuation.t -> t

(** The counters to attach to the edges into [region]. *)
val counters_into : t -> Region.t -> Fdo_annotation.t

(** The entry annotation of the code with the given code id, with the counters
    to attach to the edges into its body's first region. *)
val entry_annotation :
  t ->
  Code_id.t ->
  Fdo_annotation.With_body_hash.t ->
  Fdo_annotation.With_body_hash.t
