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
  le_monde_exterieur:Name.t ->
  all_constants:Name.t ->
  t

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

(** A distinguished [any_source] symbol representing the external world.
    Dependencies on this node model side effects. *)
val le_monde_exterieur : t -> Name.t

(** A distinguished [any_source] symbol to which all compile-time constants are
    mapped. *)
val all_constants : t -> Name.t

(** Return a copy of the environment with a new parent context. *)
val with_parent : t -> Rev_expr.rev_expr_holed -> t
