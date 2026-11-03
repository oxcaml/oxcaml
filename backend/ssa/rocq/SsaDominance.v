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

(* The dominance tests the loop analyses run beyond the [dominates] test
   SsaModel.v takes from the compiler: strict dominance, and the test that a
   block is entered from one block only. The extraction of this file,
   SsaDominance.ml, is the implementation.

   This file needs no review: what the tests establish is proved where they
   are used (InductionVar.v, AffineSsa.v and LoopGuardExecution.v, the last
   from [SsaDominanceProofs.singleton_predecessor_dominance]). *)

From Stdlib Require Import Bool List.
From OxCamlSsa Require Import SsaModel.
Import ListNotations.

(* [dominator] dominates [block] and is not it. *)
Definition strictly_dominates (dominator block : block) : bool :=
  dominates dominator block && negb (block_eqb dominator block).

(* [pred] is the sole predecessor of [succ], which is not the graph's entry:
   control enters [succ] only from [pred]. The entry has a predecessor when the
   function calls itself in tail position ([Continue (Goto entry)]), yet is
   also entered from outside; on a finished graph, where every block is
   reachable from the entry, a block that dominates its only predecessor can
   only be the entry, which the first test excludes. *)
Definition sole_predecessor (pred succ : block) : bool :=
  negb (dominates succ pred)
  && match predecessors succ with
     | [only_pred] => block_eqb only_pred pred
     | [] | _ :: _ :: _ => false
     end.
