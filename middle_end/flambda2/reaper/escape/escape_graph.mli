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

(** The facts recorded by the traversal for the escape analysis (see
    [Escape_analysis]): extra tables for the Datalog database built from the
    [Global_flow_graph], plus what the rebuilding needs to know about region
    scopes, and eventually the solved database itself.

    A region scope is either the body of a function (the scope node is its code
    id) or the handlers of a recursive [Let_cont] (the scope node is a dummy
    variable, see [recursive_scope]).

    This is deliberately kept out of the types of the reaper that can be
    exported for LTO ([Global_flow_graph.graph], [Traverse.Problem.t],
    [Traverse.Skeleton.t], [Analysis.Solution.t]): the escape analysis only runs
    when a single unit is traversed, solved and rebuilt in one go (see
    [Reaper.run]). *)

type t

(** A graph for one run of the reaper on a compilation unit. The analysis is
    enabled for it if [-X reaper-escape=1] was given and stack allocation is
    available. *)
val create : unit -> t

(** A graph for which the analysis is disabled. *)
val disabled : t

(** When the analysis is disabled, the functions below record nothing, so that
    it costs nothing. *)
val enabled : t -> bool

(** Add the recorded facts to a Datalog database. *)
val to_datalog : t -> Datalog.database -> Datalog.database

module Relations : sig
  type 'a atom = [> `Atom of Datalog.atom] as 'a

  type 'a term = 'a Datalog.Term.t

  (** [allocated_in ~alloc ~scope] means that [alloc] is a heap allocation
      (block, boxed number or set of closures) that is a candidate for stack
      allocation, performed in the region scope [scope]. *)
  val allocated_in :
    alloc:Code_id_or_name.t term -> scope:Code_id_or_name.t term -> _ atom

  (** [scope_exit ~scope ~param] means that a value flowing into [param] leaves
      the region scope [scope]: [param] is a parameter of a continuation defined
      outside of the scope (for a function, the variables standing for its
      return and exception values). *)
  val scope_exit :
    scope:Code_id_or_name.t term -> param:Code_id_or_name.t term -> _ atom

  (** [unsafe_scope scope] means that no allocation in the region scope [scope]
      may be made local, because the region could not be closed on some path
      leaving the scope (an exception caught outside of it). *)
  val unsafe_scope : Code_id_or_name.t term -> _ atom

  (** [return_of ~scope ~result]: [result] stands for a return value of the
      function whose code id is [scope]. *)
  val return_of :
    scope:Code_id_or_name.t term -> result:Code_id_or_name.t term -> _ atom

  (** [call_result ~result ~scope]: [result] receives a return value of a
      known-arity application which takes place in region scope [scope]. *)
  val call_result :
    result:Code_id_or_name.t term -> scope:Code_id_or_name.t term -> _ atom

  (** Like [call_result] but for an application taking place outside of any
      region scope. *)
  val unscoped_call_result : Code_id_or_name.t term -> _ atom

  (** [result] receives a return value of an unknown-arity application. *)
  val indirect_call_result : Code_id_or_name.t term -> _ atom

  (** As [indirect_call_result], for an application taking place in region scope
      [scope] (resp. outside of any region scope): where a partial application
      would be allocated (see [Escape_analysis]). *)
  val scoped_indirect_call_result :
    result:Code_id_or_name.t term -> scope:Code_id_or_name.t term -> _ atom

  val unscoped_indirect_call_result : Code_id_or_name.t term -> _ atom
end

(** Record that [alloc] is a heap allocation in the region scope [scope] which
    is a candidate for stack allocation. *)
val add_allocated_in :
  t -> alloc:Code_id_or_name.t -> scope:Code_id_or_name.t -> unit

(** Record that values flowing into [param] leave the region scope [scope]. *)
val add_scope_exit : t -> scope:Code_id_or_name.t -> param:Variable.t -> unit

(** Record that no allocation of the given region scope may be made local. *)
val add_unsafe_scope : t -> Code_id_or_name.t -> unit

(** Record that [result] stands for a return value of the function [scope]. *)
val add_return_of : t -> scope:Code_id_or_name.t -> result:Variable.t -> unit

(** Record that [result] receives a return value of a known-arity application in
    region scope [scope]. *)
val add_call_result : t -> result:Variable.t -> scope:Code_id_or_name.t -> unit

(** As [add_call_result], for an application outside of any region scope. *)
val add_unscoped_call_result : t -> Variable.t -> unit

(** Record that [result] receives a return value of an unknown-arity
    application. *)
val add_indirect_call_result : t -> Variable.t -> unit

(** Record that [result] receives a return value of an unknown-arity application
    in region scope [scope] (resp. outside of any region scope). *)
val add_scoped_indirect_call_result :
  t -> result:Variable.t -> scope:Code_id_or_name.t -> unit

val add_unscoped_indirect_call_result : t -> Variable.t -> unit

(** Record a region scope, with the continuations that are in scope where it
    starts: these are the continuations a jump to which leaves the scope. *)
val add_scope :
  t -> scope:Code_id_or_name.t -> outer_conts:Continuation.Set.t -> unit

(** Whether the given continuation, which must be in scope somewhere inside the
    region scope [scope] (itself recorded with [add_scope]), is defined inside
    that region scope. *)
val scope_contains_continuation :
  t -> scope:Code_id_or_name.t -> Continuation.t -> bool

(** Record the dummy variable naming the region scope formed by the handlers of
    the recursive [Let_cont] binding the given continuation. *)
val add_recursive_scope : t -> Continuation.t -> scope:Variable.t -> unit

(** Record an unknown-arity application of [callee] to [num_args] arguments in
    tail position. *)
val add_tail_callee : t -> Variable.t -> num_args:int -> unit

(** The callees recorded by [add_tail_callee], with the greatest number of
    arguments they are applied to. *)
val tail_callees : t -> int Code_id_or_name.Map.t

(** Record the number of parameters of a function, as seen by the generic
    application functions (1 for a tupled function). *)
val add_code_arity : t -> Code_id.t -> num_params:int -> unit

val code_arity : t -> Code_id.t -> int option

(** The dummy variable recorded by [add_recursive_scope], if any. *)
val recursive_scope : t -> Continuation.t -> Variable.t option

(** Record the database resulting from [Escape_analysis.perform_analysis]. *)
val set_solved : t -> Datalog.database -> unit

(** The database recorded by [set_solved], if any. *)
val solved : t -> Datalog.database option
