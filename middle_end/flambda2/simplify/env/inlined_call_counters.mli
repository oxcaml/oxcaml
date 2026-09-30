(******************************************************************************
 *                                  OxCaml                                    *
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

(** Pseudo-instrumentation counters for the calls Simplify inlines out (see
    [Fdo_counter]). The call graph decoded from a profile joins the entry
    counter of a called function with the counter of the call site, and an
    inlined call is instrumented the same way; but having no instruction of its
    own left to carry it, the counter is attached to the edges into the code the
    call was inlined into, which execute exactly as often as the inlined call.

    A region is a stretch of straight-line control flow: a function body or a
    continuation handler, up to its terminator (a switch, an application, an
    apply_cont). The counters of the calls inlined in a region go onto the edges
    into the region: the arms of the switches targeting its continuation, the
    entry edge of its function; and, transitively, the edges into the regions
    that continue into it, by a plain apply_cont or as the return point of an
    application, since everything that enters such a region enters it too.

    The tables are shared by the whole simplification of a compilation unit
    (regions are named by continuations and code ids, which are unique), filled
    during downward traversals and read while rebuilding, when the switches and
    functions (whose edges the counters go onto) are rebuilt after the regions
    they lead to have been traversed. *)

type region =
  | Function_entry of Code_id.t
  | Handler of Continuation.t

type t

val create : unit -> t

(** Calls with the given counters were inlined in [region]. *)
val add_inlined_calls : t -> region -> Fdo_counter.t list -> unit

(** [region] continues into the handler of [cont]: it jumps to it
    unconditionally, or applies a function that returns to it. *)
val add_continuation_into : t -> region -> Continuation.t -> unit

(** The counters to attach to the edges into [region]. *)
val counters_into : t -> region -> Fdo_counter.t list
