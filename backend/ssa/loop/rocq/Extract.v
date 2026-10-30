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

(* The extraction of the loop models to OxCaml, one file per model, written
   next to this file (rocq/Makefile then moves SsaModel.ml, SsaGraph.ml and
   SsaDominance.ml and SsaSemantics.ml, the models of the SSA graph, next to
   their sources in backend/ssa/rocq/): those four, FourierMotzkin.ml,
   LoopComparisons.ml, AffineExpr.ml, NaturalLoop.ml, AffineSsa.ml,
   InductionVar.ml, LoopTermination.ml, DeadInductionVar.ml,
   DeleteEmptyLoops.ml, and, for the models of rocq/ with no OxCaml
   counterpart, OxCamlList.ml and IntArith.ml. The pass itself,
   delete_empty_loops.ml in the parent directory, calls
   [DeleteEmptyLoops.find_deletions] and rewrites the graph at the loops it
   returns; that rewriting, and the table of defining blocks the analyses are
   given, are not modelled.

   The entry points extracted are [find_deletions] and, for the unit test
   oxcaml/tests/backend/ssa/loop_comparisons_facts.ml, the affine forms with
   [feasible], [entails] and [facts]; everything they use comes with them.

   Everything about the functions is proved in the model files. The mapping of
   the models' types and primitives onto OxCaml's is what has to be trusted by
   inspection; it is in OxCamlExtract.v and IntArithExtract.v (rocq/),
   IntegerComparisonExtract.v and OperationExtract.v (backend/rocq/),
   SsaModelExtract.v (backend/ssa/rocq/) and the *Extract.v files here. *)

From Stdlib Require Import Extraction.
From OxCamlStdlib Require Import OxCamlExtract IntArithExtract.
From OxCamlBackend Require Import IntegerComparisonExtract OperationExtract.
From OxCamlSsa Require Import SsaModelExtract SsaGraph SsaDominance
  SsaSemantics.
From OxCamlLoop Require Import FourierMotzkin FourierMotzkinExtract
  LoopComparisons AffineExpr AffineExprExtract NaturalLoop AffineSsa
  AffineSsaExtract InductionVar InductionVarExtract LoopTermination
  DeadInductionVar DeleteEmptyLoops.

Extract Inlined Constant DeleteEmptyLoops.graph_block_limit =>
  "4_294_967_296".

Separate Extraction
  Affine feasible entails facts
  DeleteEmptyLoops.find_deletions.
