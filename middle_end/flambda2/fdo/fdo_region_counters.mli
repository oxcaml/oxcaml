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

(** An [Fdo_region_counters.t] keeps track of FDO counters whose instruction
    Simplify removes while the code they count stays. This includes:

    * The entry counters of the callee of an inlined call

    * The counter of a self tail call turned into a jump

    * The counters of the arms of a switch that collapses into a single jump.

    A [Region.t] is a stretch of straight-line control flow: a function body or
    a continuation handler, up to its terminator. The counters described above
    are preserved by attaching them to the edges into its [Region.t], which
    execute exactly as often.

    The counters are recorded during the downward traversal and when rebuilding.
    They are read when rebuilding a switch arm, before its target can change,
    and for the function entry when the whole body has been rebuilt. An arm
    therefore misses the counters recorded after it was rebuilt (see
    [Expr_builder.preserve_fdo_counters_of_collapsed_switch]). *)

module Region : sig
  type t =
    | Function_entry of Code_id.t
    | Handler of Continuation.t
end

type t

val empty : t

(** The given counters are preserved in [region]. *)
val add_counters : t -> Region.t -> Fdo_annotation.t -> t

(** [region] continues into the handler of [cont], by the use [use_id] (for
    which [cont] may have been specialized). *)
val add_successor :
  t -> Region.t -> Continuation.t -> use_id:Apply_cont_rewrite_id.t -> t

(** The counters to attach to a continuation with use [use_id] (for which [cont]
    may have been specialized). *)
val counters_into_handler :
  t ->
  specialization_map:Continuation.t Continuation_callsite_map.t ->
  Continuation.t ->
  use_id:Apply_cont_rewrite_id.t ->
  Fdo_annotation.t

(** The entry annotation of the code with the given code id, with the counters
    to attach to its entry edge. *)
val entry_annotation :
  t ->
  specialization_map:Continuation.t Continuation_callsite_map.t ->
  Code_id.t ->
  Fdo_annotation.With_body_hash.t ->
  Fdo_annotation.With_body_hash.t
