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

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

(* See [loop_comparisons.mli] for the interface. *)

module Affine = Fourier_motzkin.Affine

let oriented_continue_comparison ~iv_is_left ~continue_when_true
    (cmp : Cmm.integer_comparison) : Cmm.integer_comparison =
  let cmp = if iv_is_left then cmp else Cmm.swap_integer_comparison cmp in
  if continue_when_true then cmp else Cmm.negate_integer_comparison cmp

type termination =
  | Terminates
  | Terminates_if_bound_in_range
  | Unknown

(* Continue-condition (oriented with the IV operand on the left): the loop
   continues iff [iv cmp bound] holds, and each iteration replaces the tested
   value [v] by [v + step], computed by 64-bit machine arithmetic.

   Monotone progression forces the comparison to flip only when the addition
   cannot wrap. The unconditionally safe cases:

   - [Ceq]: the next tested value differs from the current one modulo 2^64
   (since [0 < |step| < 2^63]), so the equality fails after at most one more
   iteration whatever else happens.

   - [Clt] with [step = 1]: continuing means [v <= bound - 1], so [v + 1 <=
   bound <= max_int64] — no wrap, strictly increasing, and the test eventually
   fails. Symmetrically [Cgt] with [step = -1].

   The other monotone cases ([Cle]; [Clt]/[Cgt] with larger steps; [Cge]) can
   diverge by wrapping when the bound sits near the numeric limit (e.g. continue
   [v <= bound] with [bound = max_int64]: at [v = bound] the increment wraps and
   the loop spins forever — which is also the OCaml semantics of such a source
   loop). Those return [Terminates_if_bound_in_range]: the caller must prove
   that the bound's machine value [b] satisfies [b <= ocaml_max_int]
   (up-counting) or [b >= -ocaml_max_int] (down-counting). That suffices for any
   OCaml-[int] step: continuing gives [v <= b] (up; [Clt] is stronger), so [v +
   step <= ocaml_max_int + ocaml_max_int < max_int64] — no wrap — and
   symmetrically downwards.

   Unsigned comparisons and [Cne] are not monotone under signed progression and
   always return [Unknown]. *)
let continue_terminates ~step (cmp : Cmm.integer_comparison) : termination =
  if step = 0
  then Unknown
  else
    match cmp with
    | Ceq -> Terminates
    | Clt ->
      if step = 1
      then Terminates
      else if step > 0
      then Terminates_if_bound_in_range
      else Unknown
    | Cle -> if step > 0 then Terminates_if_bound_in_range else Unknown
    | Cgt ->
      if step = -1
      then Terminates
      else if step < 0
      then Terminates_if_bound_in_range
      else Unknown
    | Cge -> if step < 0 then Terminates_if_bound_in_range else Unknown
    | Cne | Cult | Cugt | Cule | Cuge -> Unknown

(* Facts implied by the (possibly negated) signed comparison [la cmp lb].
   Unsigned comparisons and [Cne] cannot be expressed as a single affine
   inequality, so they contribute nothing. *)
let facts ~negate (cmp : Cmm.integer_comparison) la lb : Affine.t list =
  let cmp = if negate then Cmm.negate_integer_comparison cmp else cmp in
  match cmp with
  | Cge -> [Affine.sub la lb]
  | Cgt -> [Affine.add_const (Affine.sub la lb) (-1)]
  | Cle -> [Affine.sub lb la]
  | Clt -> [Affine.add_const (Affine.sub lb la) (-1)]
  | Ceq -> [Affine.sub la lb; Affine.sub lb la]
  | Cne | Cult | Cugt | Cule | Cuge -> []
