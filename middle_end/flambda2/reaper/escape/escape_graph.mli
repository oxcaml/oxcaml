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

(** The facts recorded by the traversal for the escape analysis (see
    [Escape_analysis]): extra tables of the Datalog database built from the
    [Global_flow_graph], plus the continuations defined in each region scope.

    A region scope is either the body of a function (the scope node is its code
    id) or the handlers of a recursive [Let_cont] (the scope node is a dummy
    variable, see [Rev_expr.Let_cont_rec.region_scope]). *)

type t

val create : unit -> t

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

  (** [call_result ~result ~scope ~call_widget]: [result] receives a return
      value of the known-arity application modelled by [call_widget], which
      takes place in region scope [scope]. *)
  val call_result :
    result:Code_id_or_name.t term ->
    scope:Code_id_or_name.t term ->
    call_widget:Code_id_or_name.t term ->
    _ atom

  (** Like [call_result] but for an application taking place outside of any
      region scope. *)
  val unscoped_call_result : Code_id_or_name.t term -> _ atom

  (** [result] receives a return value of an unknown-arity application. *)
  val indirect_call_result : Code_id_or_name.t term -> _ atom

  (** [partial_application_node ~node ~code_id]: [node] stands for the closure
      resulting from a partial application of a function with code [code_id]
      (see [Traverse_acc.create_unknown_arity_call_witnesses]); such a closure
      points at the closure being applied. *)
  val partial_application_node :
    node:Code_id_or_name.t term -> code_id:Code_id_or_name.t term -> _ atom
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

(** Record that [result] receives a return value of the known-arity application
    [call_widget], in region scope [scope]. *)
val add_call_result :
  t ->
  result:Variable.t ->
  scope:Code_id_or_name.t ->
  call_widget:Code_id_or_name.t ->
  unit

(** As [add_call_result], for an application outside of any region scope. *)
val add_unscoped_call_result : t -> Variable.t -> unit

(** Record that [result] receives a return value of an unknown-arity
    application. *)
val add_indirect_call_result : t -> Variable.t -> unit

(** Record that [node] stands for the closure resulting from a partial
    application of the function [code_id]. *)
val add_partial_application_node :
  t -> node:Code_id_or_name.t -> code_id:Code_id.t -> unit

(** Record that a continuation is defined inside the region scope [scope]. *)
val add_scope_cont : t -> scope:Code_id_or_name.t -> Continuation.t -> unit

(** Whether the given continuation has been recorded, by [add_scope_cont], as
    defined inside the region scope [scope]. *)
val scope_contains_continuation :
  t -> scope:Code_id_or_name.t -> Continuation.t -> bool

(** The continuations defined inside each region scope. *)
val scope_conts : t -> Continuation.Set.t Code_id_or_name.Map.t
