(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaelle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Environment threaded through the downward traversal of a function body.
    Tracks the context needed to register dependencies at each expression. *)

(** [Normal params] carries the variables representing a continuation's
    parameters. *)
type cont_kind = Normal of Variable.t list

(** Where heap allocations may be turned into stack allocations (see
    [Escape_analysis]):

    - [No_region_scope]: nowhere in the current function, not even in recursive
      continuation handlers. This is used when the analysis is disabled, and for
      functions that may allocate their result in the caller's region (such
      allocations must not happen while an inner region is open).

    - [Toplevel]: not at the current point, but recursive continuation handlers
      may open their own region scopes. Used for the toplevel of the unit.

    - [Scope scope]: allocations at the current point are candidates, in the
      region scope named by [scope]: the code id of the enclosing function, or a
      dummy variable naming a recursive [Let_cont] whose handlers get a region
      each (so that a function-level region does not grow with every iteration).
*)
type region_scope =
  | No_region_scope
  | Toplevel
  | Scope of Code_id_or_name.t

(** Controls whether the reaper preserves direct function calls when the code_id
    being called might otherwise be dead. The typical situation is a call to a
    code_id [f], which has two newer versions [f1] and [f2], and the closure
    that is used could have either code_id [f1] or [f2], but never [f].

    - [Yes]: always preserve direct calls: this has the consequence that it
      needs to keep old code_ids ([f] in this case) alive.
    - [No]: never preserve direct calls to dead code_ids: the call will be
      replaced by [Indirect_known_arity] if the reaper isn't able to determine
      the possibly called code_ids.
    - [Auto]: preserve only if the closure is [any_source], preventing a
      determination of a set of possibly called code_ids. In this case, direct
      calls can never be completely degraded to [Indirect_known_arity] that
      doesn't know the set of possibly called code_ids: if that were to happen,
      the direct call is instead kept. *)
type should_preserve_direct_calls =
  | Yes
  | No
  | Auto

(** The type of downwards environments. *)
type t

(** Create a new downwards environment. *)
val create :
  parent:Rev_expr.rev_expr_holed ->
  conts:cont_kind Continuation.Map.t ->
  current_code_id:Code_id.t option ->
  should_preserve_direct_calls:should_preserve_direct_calls ->
  all_constants:Name.t ->
  function_slots_to_keep:Function_slot.Set.t ->
  value_slots_to_keep:Value_slot.Set.t ->
  region_scope:region_scope ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  t

(** The region scope in which allocations at the current point are candidates
    for stack allocation, if any. There is none while a region pre-existing in
    the term (see [enter_foreign_region]) is open, since allocations would go to
    that region instead. *)
val region_scope : t -> Code_id_or_name.t option

(** All region scopes enclosing the current point, innermost first. *)
val enclosing_region_scopes : t -> Code_id_or_name.t list

(** The return continuation of the enclosing function (or of the unit, at
    toplevel): applications returning to it are tail calls. *)
val return_continuation : t -> Continuation.t

(** The exception continuation of the enclosing function (or of the unit). *)
val exn_continuation : t -> Continuation.t

(** Enter the handlers of a recursive [Let_cont] whose region scope is named by
    [scope]. *)
val enter_recursive_handler : t -> scope:Variable.t -> t

(** Note that the given region, defined by a [Begin_region] or
    [Begin_try_region] primitive pre-existing in the term (as inserted for
    [local_] allocations or [try] bodies), is open at the current point. *)
val enter_foreign_region : t -> Variable.t -> t

(** Note that the given region has been closed by an [End_region] or
    [End_try_region] primitive. *)
val exit_foreign_region : t -> Variable.t -> t

(** Note that the given try-region, defined by a [Begin_try_region] primitive,
    is open at the current point. Unlike the regions above, try-regions are not
    closed on the normal path, so allocations made while they are open belong to
    the enclosing region and remain candidates. *)
val enter_try_region : t -> Variable.t -> t

(** Note that the given try-region has been closed by an [End_try_region]
    primitive. *)
val exit_try_region : t -> Variable.t -> t

(** Enter the handler of a continuation with the given parameters. *)
val enter_handler : t -> params:Variable.t list -> t

(** The parameters of the enclosing continuation handlers that were defined
    while the given try-region was open: closing that try-region frees
    everything allocated since it was opened, so no such value may be among the
    arguments passed to these handlers. *)
val handler_params_freed_by_end_of_try_region :
  t -> Variable.t -> Variable.t list

(** The parameters of all continuations currently in scope. *)
val all_continuation_params : t -> Variable.t list

(** The reversed expression context above the current point in the traversal. *)
val parent : t -> Rev_expr.rev_expr_holed

(** Get parameters of a given continuation *)
val find_cont : t -> Continuation.t -> cont_kind

(** Add new continuation in scope with given parameters. *)
val add_cont : t -> Continuation.t -> cont_kind -> t

(** The code id of the function currently being traversed, or [None] at the top
    level. *)
val current_code_id : t -> Code_id.t option

(** Whether direct calls in the current function should be preserved by the
    reaper. Set per function body based on the [reaper_preserve_direct_calls]
    flag and whether zero-alloc checking is active. *)
val should_preserve_direct_calls : t -> should_preserve_direct_calls

(** A distinguished [any_source] symbol to which all compile-time constants are
    mapped. *)
val all_constants : t -> Name.t

(** Return a copy of the environment with a new parent context. *)
val with_parent : t -> Rev_expr.rev_expr_holed -> t

val function_slots_to_keep : t -> Function_slot.Set.t

val should_keep_function_slot : t -> Function_slot.t -> bool

val value_slots_to_keep : t -> Value_slot.Set.t

val should_keep_value_slot : t -> Value_slot.t -> bool
