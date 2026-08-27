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

(** Detect basic induction variables that are "dead": ones whose only uses in
    the whole SSA graph are as arguments of their own back-edge update
    expression(s) and of the loop's exit comparison. Such IVs contribute no
    observable state beyond iteration. A loop in which every header parameter is
    such a dead IV iterates without doing any observable work through its
    parameters; combined with a termination proof and an effect-free body it can
    be removed entirely (see {!Delete_empty_loops}). *)

open Ssa.Export

type biv_result =
  { biv : Induction_var.biv;
    dead : bool
        (** [true] iff the BIV's only uses in the whole graph are the loop's
            exit comparison and its own back-edge update — i.e. it would become
            removable if the exit test stopped mentioning it. *)
  }

type loop_result =
  { loop : Induction_var.loop;
    bivs : biv_result list;
    useless : bool
        (** [true] iff every block parameter of the loop header was classified
            as a basic induction variable and each is dead. *)
  }

val analyze :
  finished Ssa.graph ->
  (Induction_var.loop * Induction_var.biv list) list ->
  loop_result list
