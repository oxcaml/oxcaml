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

(** The type of downwards environments. *)
type t

(** Create a new downwards environment. *)
val create :
  parent:Rev_expr.rev_expr_holed ->
  conts:cont_kind Continuation.Map.t ->
  current_code_id:Code_id.t option ->
  all_constants:Name.t ->
  function_slots_to_keep:Function_slot.Set.t ->
  value_slots_to_keep:Value_slot.Set.t ->
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

(** A distinguished [any_source] symbol to which all compile-time constants are
    mapped. *)
val all_constants : t -> Name.t

(** Return a copy of the environment with a new parent context. *)
val with_parent : t -> Rev_expr.rev_expr_holed -> t

val function_slots_to_keep : t -> Function_slot.Set.t

val should_keep_function_slot : t -> Function_slot.t -> bool

val value_slots_to_keep : t -> Value_slot.Set.t

val should_keep_value_slot : t -> Value_slot.t -> bool
