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

(** The traversal side of the escape analysis (see [Escape_analysis]): these
    functions are called from [Traverse] to record the facts the analysis needs
    (into the [Escape_graph] of the accumulator) and to maintain its part of the
    traversal environment ([Escape_env]). When the analysis is disabled for the
    accumulator, they record nothing and return the environment unchanged. *)

(** The environment for the toplevel of the unit. *)
val toplevel_env :
  Traverse_acc.t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  Escape_env.t

(** The environment for the body of a function, after recording its return and
    exception variables as exits of its region scope. *)
val function_env :
  Traverse_acc.t ->
  code_id:Code_id.t ->
  is_opaque:bool ->
  my_alloc_mode:Alloc_mode.For_applications.t ->
  return:Variable.t list ->
  exn:Variable.t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  Escape_env.t

(** Note a jump to the given continuation: a region scope from which an
    exception may be raised to a handler defined outside of it gets no region.
*)
val note_jump_to : Traverse_env.t -> Traverse_acc.t -> Continuation.t -> unit

(** Record the facts about an application: its exception continuation (see
    [note_jump_to]); for a tail call, that its arguments and callee leave the
    enclosing region scopes; and where its results, bound to [return_args],
    land. *)
val record_apply :
  Traverse_env.t ->
  Traverse_acc.t ->
  Flambda.Apply.t ->
  return_args:Variable.t list ->
  unit

(** To be called on every [Let], with the environment before the binding:
    records a heap allocation as a candidate for stack allocation if the current
    point is in a region scope, and returns the environment for the body, which
    keeps track of the regions pre-existing in the term. *)
val enter_let :
  Traverse_env.t ->
  Traverse_acc.t ->
  bound_pattern:Bound_pattern.t ->
  Flambda.Named.t ->
  Traverse_env.t

(** Enter the handler of a non-recursive continuation with the given parameters.
*)
val enter_handler :
  Traverse_env.t -> Traverse_acc.t -> params:Variable.t list -> Traverse_env.t

(** Enter the handlers of the recursive [Let_cont] binding [conts], which form a
    region scope of their own (see [Escape_graph.recursive_scope]). The
    environment must already contain [conts]; [params] are all the parameters of
    all the handlers. *)
val enter_recursive_handlers :
  Traverse_env.t ->
  Traverse_acc.t ->
  conts:Continuation.t list ->
  params:Variable.t list ->
  Traverse_env.t
