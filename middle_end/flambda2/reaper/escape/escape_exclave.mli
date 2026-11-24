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

(*= OxCaml-Written-By: ai:fable-5.2
    OxCaml-Review-Status: ai:fable-5.2:xhigh *)

(** Where a function made to return at mode local (see [Escape_analysis]) may
    free its temporaries.

    Such a function allocates what it returns in its caller's region. Its other
    local allocations can live in a region of its own provided that this region
    is closed before anything returned is allocated, as [exclave_] does. This
    module looks for the points of the body where the region can be closed (the
    {e exclave points}); the part of the body after them is the {e zone}. See
    [escape_exclave.ml] for the conditions. *)

type t

(** The plan for the body of the given function, or [None] if the function
    should not be given a region of its own: either it could not be closed early
    enough, or nothing would be allocated in it. *)
val compute :
  Escape_solution.t ->
  continuation_info:Traverse_acc.Continuation_info.t Continuation.Map.t ->
  code_id:Code_id.t ->
  return_continuation:Continuation.t ->
  Rev_expr.t ->
  t option

(** Whether the handler of the continuation is in the zone: the region of the
    function is closed when it runs. *)
val in_zone : t -> Continuation.t -> bool

(** Whether the region of the function must be closed before jumping to the
    continuation from outside of the zone. *)
val closed_before_jump : t -> Continuation.t -> bool

(** Whether the region of the function must be closed at the start of the
    handler of the continuation. *)
val closed_on_entry : t -> Continuation.t -> bool

(** Whether the region of the function must be closed before the application
    ending the handler of the given continuation ([None] for the start of the
    body of the function), which is outside of the zone. *)
val closed_before_apply : t -> Continuation.t option -> bool
