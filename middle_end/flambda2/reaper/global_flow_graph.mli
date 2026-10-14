(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type graph

val to_datalog : graph -> Datalog.database

module Relations : sig
  type 'a atom = [> `Atom of Datalog.atom] as 'a

  type 'a term = 'a Datalog.Term.t

  val alias :
    to_:Code_id_or_name.t term -> from:Code_id_or_name.t term -> _ atom

  val use : to_:Code_id_or_name.t term -> from:Code_id_or_name.t term -> _ atom

  (** [accessor ~to_ field ~base] represents a {b directed} flow from the field
      [field] of variable [base] into the variable [to_].

      It corresponds to a projection [to_ = base.field]. *)
  val accessor :
    to_:Code_id_or_name.t term ->
    Field.t term ->
    base:Code_id_or_name.t term ->
    _ atom

  (** [constructor ~base field ~from] represents a {b directed} flow from
      variable [from] into the field [field] or allocation [base].

      It corresponds to an allocation [base = { field = from; _ }] (there might
      be other fields in the allocation). *)
  val constructor :
    base:Code_id_or_name.t term ->
    Field.t term ->
    from:Code_id_or_name.t term ->
    _ atom

  (** [argument ~from param ~base] represents a {b directed} flow from variable
      [from] into the parameter [param] of function [base].

      It corresponds to a call [base(param = from, _)] (there might be other
      parameters in the call).

      Note that [base] is a virtual object representing function-like calls; see
      the comment about encoding of function calls in {!module-Dep_solver}. *)
  val argument :
    from:Code_id_or_name.t term ->
    Cofield.t term ->
    base:Code_id_or_name.t term ->
    _ atom

  (** [parameter ~base param ~to_] represents a {b directed} flow from function
      parameter [param] into variable [to_].

      There is no surface language syntax for this; we write [to_ = base@param].

      Note that [base] is a virtual object representing function-like calls; see
      the comment about encoding of function calls in {!module-Dep_solver}. *)
  val parameter :
    base:Code_id_or_name.t term ->
    Cofield.t term ->
    to_:Code_id_or_name.t term ->
    _ atom

  val propagate :
    if_used:Code_id_or_name.t term ->
    to_:Code_id_or_name.t term ->
    from:Code_id_or_name.t term ->
    _ atom

  val alias_if_any_source :
    if_any_source:Code_id_or_name.t term ->
    to_:Code_id_or_name.t term ->
    from:Code_id_or_name.t term ->
    _ atom

  val any_usage : Code_id_or_name.t term -> _ atom

  val any_source : Code_id_or_name.t term -> _ atom

  (** [any_source_if_used ~if_used x] means [any_source x], but only if
      [if_used] has [any_usage]. *)
  val any_source_if_used :
    if_used:Code_id_or_name.t term -> Code_id_or_name.t term -> _ atom

  (* [zero_alloc_source x] means that [x] has any source, but furthermore, that
     all fields read from [x] are themselves [zero_alloc_source] (and hence
     [any_source]), even if they are local fields. This is not fully tracked,
     and will be lost if a [zero_alloc_source] variable is stored somewhere and
     then read from there. It is only intended as a hack to make zero_alloc
     checking work, waiting for the redesign that does not depend on keeping
     dead code alive. *)
  val zero_alloc_source : Code_id_or_name.t term -> _ atom

  (* An entry (code_id, v) in this relation means that [v] is the "my_closure"
     variable of the code associated to [code_id]. *)
  val code_id_my_closure :
    code_id:Code_id_or_name.t term ->
    my_closure:Code_id_or_name.t term ->
    _ atom

  (** [allocated_in ~alloc ~scope] means that [alloc] is a heap allocation
      (block, boxed number or set of closures) that is a candidate for stack
      allocation, performed in the region scope [scope]. A region scope is
      either the body of a function (the scope node is its code id) or the
      handlers of a recursive [Let_cont] (the scope node is a dummy variable);
      see [Escape_analysis]. *)
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

val add_alias : graph -> to_:Code_id_or_name.t -> from:Code_id_or_name.t -> unit

val add_use_dep :
  graph -> to_:Code_id_or_name.t -> from:Code_id_or_name.t -> unit

val add_accessor_dep :
  graph -> to_:Code_id_or_name.t -> Field.t -> base:Code_id_or_name.t -> unit

val add_constructor_dep :
  graph -> base:Code_id_or_name.t -> Field.t -> from:Code_id_or_name.t -> unit

val add_argument_dep :
  graph -> from:Code_id_or_name.t -> Cofield.t -> base:Code_id_or_name.t -> unit

val add_parameter_dep :
  graph -> base:Code_id_or_name.t -> Cofield.t -> to_:Code_id_or_name.t -> unit

val add_propagate_dep :
  graph ->
  if_used:Code_id_or_name.t ->
  to_:Code_id_or_name.t ->
  from:Code_id_or_name.t ->
  unit

val add_alias_if_any_source_dep :
  graph ->
  if_any_source:Code_id_or_name.t ->
  to_:Code_id_or_name.t ->
  from:Code_id_or_name.t ->
  unit

val add_any_usage : graph -> Code_id_or_name.t -> unit

val add_any_source : graph -> Code_id_or_name.t -> unit

val add_any_source_if_used :
  graph -> if_used:Code_id_or_name.t -> Code_id_or_name.t -> unit

val add_zero_alloc_source : graph -> Code_id_or_name.t -> unit

val add_code_id_my_closure : graph -> Code_id.t -> Variable.t -> unit

val add_allocated_in :
  graph -> alloc:Code_id_or_name.t -> scope:Code_id_or_name.t -> unit

val add_scope_exit :
  graph -> scope:Code_id_or_name.t -> param:Variable.t -> unit

val add_unsafe_scope : graph -> Code_id_or_name.t -> unit

val add_return_of :
  graph -> scope:Code_id_or_name.t -> result:Variable.t -> unit

val add_call_result :
  graph ->
  result:Variable.t ->
  scope:Code_id_or_name.t ->
  call_widget:Code_id_or_name.t ->
  unit

val add_unscoped_call_result : graph -> Variable.t -> unit

val add_indirect_call_result : graph -> Variable.t -> unit

val add_partial_application_node :
  graph -> node:Code_id_or_name.t -> code_id:Code_id.t -> unit

val create : unit -> graph

val add_opaque_let_dependency :
  graph -> to_:Bound_pattern.t -> from:Name_occurrences.t -> unit

val print_iter_edges :
  print_edge:(Code_id_or_name.t * Code_id_or_name.t * string -> unit) ->
  graph ->
  unit
