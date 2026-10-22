(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The traversal side of the escape analysis (see [Escape_analysis]): these
    functions are called from [Traverse] to record the facts the analysis needs
    (into [Escape_graph]) and to maintain its part of the traversal environment
    ([Escape_env]). *)

(** The environment for the toplevel of the unit. *)
val toplevel_env :
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

(** Record where the results of a known-arity application land. *)
val record_call_results :
  Traverse_env.t ->
  Traverse_acc.t ->
  call_widget:Code_id_or_name.t ->
  Variable.t list ->
  unit

(** Record the results of an unknown-arity application. *)
val record_indirect_call_results : Traverse_acc.t -> Variable.t list -> unit

(** Note a jump to the given continuation: a region scope from which an
    exception may be raised to a handler defined outside of it gets no region.
*)
val note_jump_to : Traverse_env.t -> Traverse_acc.t -> Continuation.t -> unit

(** Record the facts about an application: its exception continuation (see
    [note_jump_to]), and, for a tail call, that its arguments and callee leave
    the enclosing region scopes. *)
val record_apply : Traverse_env.t -> Traverse_acc.t -> Flambda.Apply.t -> unit

(** Record that a continuation is defined inside the enclosing region scopes. *)
val add_scope_cont : Traverse_env.t -> Traverse_acc.t -> Continuation.t -> unit

(** Record a heap allocation as a candidate for stack allocation, if the current
    point is in a region scope. *)
val record_candidate_allocation :
  Traverse_env.t ->
  Traverse_acc.t ->
  bound_pattern:Bound_pattern.t ->
  Flambda.Named.t ->
  unit

(** Update the environment for the body of a [Let]: keeps track of the regions
    pre-existing in the term. *)
val enter_let :
  Traverse_env.t ->
  Traverse_acc.t ->
  bound_pattern:Bound_pattern.t ->
  Flambda.Named.t ->
  Traverse_env.t

(** Enter the handlers of a recursive [Let_cont], which form a region scope of
    their own; returns the dummy variable naming it (see
    [Rev_expr.Let_cont_rec.region_scope]). *)
val enter_recursive_handlers :
  Traverse_env.t -> Traverse_acc.t -> Variable.t * Traverse_env.t

(** Enter the handler of a continuation with the given parameters. *)
val enter_handler : Traverse_env.t -> params:Variable.t list -> Traverse_env.t
