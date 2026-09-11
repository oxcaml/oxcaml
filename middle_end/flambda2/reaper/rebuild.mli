(******************************************************************************
 *                             flambda-backend                                *
 *                                                                            *
 *             Nathanaëlle Courant, Pierre Chambart, OCamlPro                 *
 *                        Mark Shinwell, Jane Street                          *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024--2025 OCamlPro SAS                                      *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

(** Full typing information for normal Reaper rebuilding and type exports.
    Backend-only rebuilding passes [None] and conservatively erases rich
    subkinds without querying the type database. *)
type typing =
  { context : Types_rewriter.rewrite_context;
    code_deps : Traverse_acc.code_dep Code_id.Map.t;
    env : Typing_env.t option
  }

type result = private
  { body : Flambda.Expr.t;
    all_code : Code.t Code_id.Map.t;
    code_ids_to_remember : Code_id.Set.t;
    free_names : Name_occurrences.t
  }

val rebuild :
  machine_width:Target_system.Machine_width.t ->
  ordered_code_ids:Code_id.t array ->
  continuation_info:Traverse_acc.continuation_info Continuation.Map.t ->
  fixed_arity_continuations:Continuation.Set.t ->
  typing:typing option ->
  Rebuild_solution.t ->
  (Code_id.t -> Code_metadata.t) ->
  Rev_expr.t ->
  Rev_expr.rev_code Code_id.Map.t ->
  result
