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

(** Basic induction variable detection over the SSA representation.

    A basic induction variable (BIV) is a block parameter [i] of a loop header
    (see {!Natural_loop}) such that, on every back edge, the incoming value is
    [i + c], [c + i] or [i - c] for some loop-invariant [c]. *)

open Ssa.Export

type loop = Natural_loop.loop =
  { header : finished Block.t;
    body : Block.Set.t;
    back_edges : finished Block.t list
  }

type step =
  | Step_const of int
  | Step_var of finished Value.t

type biv =
  { loop : loop;
    param_index : int;
    update : finished Value.t list;
        (** The incoming value on each back edge (one entry per back-edge
            [Continue (Goto header)]). *)
    step : step;
    sign : [`Add | `Sub]
  }

(** Map each [Op]'s id to the block in which it is defined. *)
type op_def = (finished, finished Block.t) Instruction.Id.Tbl.t

val compute_op_def : finished Ssa.graph -> op_def

(** The natural loops of the graph, each with its basic induction variables. *)
val analyze : op_def:op_def -> finished Ssa.graph -> (loop * biv list) list

(** [is_header_param block index v] is [true] iff [v] is the [Block_param]
    referring to parameter [index] of [block]. *)
val is_header_param : finished Block.t -> int -> finished Value.t -> bool

(** The signed per-iteration step of a constant-step BIV ([+c] steps give [c],
    [-c] steps give [-c]); [None] for variable steps. *)
val signed_step : biv -> int option

(** [loop.back_edges] as a set. *)
val back_edge_set : loop -> Block.Set.t

(** [is_loop_invariant ~op_def loop_body v] is [true] iff [v] cannot change
    across iterations of a loop whose body is [loop_body]: it is a compile-time
    constant, or its defining block (for an operation result) / owning block
    (for a [Block_param]) lies outside [loop_body]. *)
val is_loop_invariant : op_def:op_def -> Block.Set.t -> finished Value.t -> bool
