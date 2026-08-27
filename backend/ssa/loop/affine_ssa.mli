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

[@@@ocaml.warning "+a-40-41-42"]

(** Linearization of SSA integer values into {!Fourier_motzkin.Affine} forms,
    and collection of the affine facts implied by dominating branch guards.

    This is the SSA-facing layer of the range-based loop reasoning: it turns SSA
    values and control-flow guards into affine inequalities, which
    {!Fourier_motzkin} then reasons about. The termination analysis uses it to
    discharge the no-overflow side conditions of
    {!Loop_comparisons.continue_terminates}. *)

open Ssa.Export
module Affine = Fourier_motzkin.Affine

(** Interns SSA values as affine atoms, so equal values share a coefficient. A
    fresh [ctx] should be used per query. *)
type ctx

val new_ctx : unit -> ctx

(** [linearize ctx side v] is the affine form of [v]'s machine-integer value.
    Steps that are only soundly bounded (right shifts, or-with-mask) intern an
    atom and push the bounding inequalities onto [side], which the caller must
    include among its facts. Anything not decomposed becomes an atom.

    Soundness: the returned form models the machine value {e exactly} (as an
    unbounded-integer expression over the atoms' machine values, together with
    the [side] facts) for {e all} runtime values: decompositions whose integer
    evaluation could escape signed 64-bit range — where machine wrap-around
    would break the model — are certified away via {!Affine_expr.Range} and
    atomized instead. Facts built from these forms (see
    {!Loop_comparisons.facts}, {!guards_at}) are therefore true of the machine
    values whenever the corresponding tests were taken. *)
val linearize : ctx -> Affine.t list ref -> finished Value.t -> Affine.t

(** [guards_at ctx side target] collects the affine facts that hold on entry to
    [target], from the signed comparisons on its immediate-dominator chain whose
    taken edge dominates [target]. Side conditions from [linearize] are pushed
    onto [side]. *)
val guards_at : ctx -> Affine.t list ref -> finished Block.t -> Affine.t list
