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

(** Accumulator used during the downward traversal of the Flambda2 term to build
    the global flow graph for the reaper. *)

module Graph = Global_flow_graph

type continuation_info =
  { is_exn_handler : bool;
    params : Variable.t list;
    arity : Flambda_kind.With_subkind.t list
  }

module Env : sig
  type cont_kind = Normal of Variable.t list

  type should_preserve_direct_calls =
    | Yes
    | No
    | Auto

  type t

  val create :
    parent:Rev_expr.rev_expr_holed ->
    conts:cont_kind Continuation.Map.t ->
    current_code_id:Code_id.t option ->
    should_preserve_direct_calls:should_preserve_direct_calls ->
    le_monde_exterieur:Name.t ->
    all_constants:Name.t ->
    t

  val parent : t -> Rev_expr.rev_expr_holed

  val conts : t -> cont_kind Continuation.Map.t

  val current_code_id : t -> Code_id.t option

  val should_preserve_direct_calls : t -> should_preserve_direct_calls

  val le_monde_exterieur : t -> Name.t

  val all_constants : t -> Name.t

  val with_parent : t -> Rev_expr.rev_expr_holed -> t

  val with_conts : t -> cont_kind Continuation.Map.t -> t
end

(** Information about a function's code that is needed for building the
    dependency graph. Created by [prepare_code] during traversal and looked up
    when processing function bodies and call sites. *)
type code_dep =
  { arity : [`Complex] Flambda_arity.t;
    params : Variable.t list;
    my_closure : Variable.t;
    return : Variable.t list;
    exn : Variable.t;
    is_tupled : bool;
    known_arity_call_witness : Code_id_or_name.t;
    unknown_arity_call_witnesses : Code_id_or_name.t list
  }

(** A record of a direct function application, to be resolved into graph edges
    once all code has been traversed. *)
type apply_dep =
  { function_containing_apply_expr : Code_id.t option;
    apply_code_id : Code_id.t;
    apply_closure : Simple.t option;
    apply_call_witness : Code_id_or_name.t
  }

type t

(** Create a fresh, empty accumulator. *)
val create : unit -> t

(** Record the kind of a name. *)
val kind : t -> Name.t -> Flambda_kind.t -> unit

(** Record the kind of a bound parameter's variable. *)
val bound_parameter_kind : t -> Bound_parameter.t -> unit

(** Record the kind of [name] by inferring it from [simple]. *)
val alias_kind : t -> Name.t -> Simple.t -> unit

(** Return the map of all recorded kinds. *)
val kinds : t -> Flambda_kind.t Name.Map.t

(** Mark a continuation as having fixed arity: the rebuild pass may not change
    its number of parameters. *)
val fixed_arity_continuation : t -> Continuation.t -> unit

(** Return the set of all fixed-arity continuations. *)
val fixed_arity_continuations : t -> Continuation.Set.t

(** Record metadata about a continuation (parameters, arity, and whether it is
    an exception handler). *)
val continuation_info :
  t ->
  Continuation.t ->
  params:Variable.t list ->
  arity:Flambda_kind.With_subkind.t list ->
  is_exn_handler:bool ->
  unit

(** Return the map of all recorded continuation metadata. *)
val get_continuation_info : t -> continuation_info Continuation.Map.t

(** Register a [code_dep] for a code id, recording the function's parameters,
    returns, and call witnesses in the accumulator. *)
val add_code : t -> Code_id.t -> code_dep -> unit

(** Look up the [code_dep] for a code id. Returns [None] if the code id has not
    been registered (e.g. it belongs to another compilation unit). *)
val find_code : t -> Code_id.t -> code_dep option

(** Return the map of all registered code deps. *)
val code_deps : t -> code_dep Code_id.Map.t

(** Add an alias edge: if [to_] is used, [from] is also used, and they share the
    same source. *)
val add_alias : t -> to_:Code_id_or_name.t -> from:Code_id_or_name.t -> unit

(** Convenience wrapper around [add_alias] for variables. *)
val add_alias_vars : t -> to_:Variable.t -> from:Variable.t -> unit

(** Add a use-dependency edge: if [to_] is used then [from] is also used. Unlike
    [add_alias], source information does not propagate. *)
val add_use_dep : t -> to_:Code_id_or_name.t -> from:Code_id_or_name.t -> unit

(** Add an accessor edge: if [to_] is used, then [base] is used and the field
    [relation] of [base] flows into [to_]. *)
val add_accessor_dep :
  t -> to_:Code_id_or_name.t -> Field.t -> base:Code_id_or_name.t -> unit

(** Add a constructor edge: [from] flows into field [relation] of [base]. *)
val add_constructor_dep :
  t -> base:Code_id_or_name.t -> Field.t -> from:Code_id_or_name.t -> unit

(** Add an argument edge (dual of accessor): if [base] is used, [from] flows
    into the cofield [relation] of [base]. *)
val add_argument_dep :
  t -> from:Code_id_or_name.t -> Cofield.t -> base:Code_id_or_name.t -> unit

(** Add a parameter edge (dual of constructor): the cofield [relation] of [base]
    flows into [to_]. *)
val add_parameter_dep :
  t -> base:Code_id_or_name.t -> Cofield.t -> to_:Code_id_or_name.t -> unit

(** Add a conditional propagation edge: if [if_used] is used then add an alias
    from [from] to [to_]. *)
val add_propagate_dep :
  t ->
  if_used:Code_id_or_name.t ->
  to_:Code_id_or_name.t ->
  from:Code_id_or_name.t ->
  unit

(** Add a conditional alias edge: if [if_any_source] is marked as [any_source]
    then add an alias from [from] to [to_]. *)
val add_alias_if_any_source_dep :
  t ->
  if_any_source:Code_id_or_name.t ->
  to_:Code_id_or_name.t ->
  from:Code_id_or_name.t ->
  unit

(** Mark a node as unconditionally used. *)
val add_any_usage : t -> Code_id_or_name.t -> unit

(** Mark a node as having any possible source (i.e. it could contain any value).
*)
val add_any_source : t -> Code_id_or_name.t -> unit

(** Mark a node as a source for zero-alloc checking purposes. *)
val add_zero_alloc_source : t -> Code_id_or_name.t -> unit

(** Record the [my_closure] variable associated with a code id. *)
val add_code_id_my_closure : t -> Code_id.t -> Variable.t -> unit

(** Convert a [Simple.t] to a dependency graph node. Constants map to the
    [all_constants] node; variables map to themselves; symbols from other
    compilation units are marked [any_source]. *)
val simple_to_node : t -> denv:Env.t -> Simple.t -> Code_id_or_name.t

(** Mark a [Simple.t] as used, conditional on the current function (if any)
    being used. At the top level, marks it unconditionally. *)
val add_cond_any_usage : t -> denv:Env.t -> Simple.t -> unit

(** Mark a node as [any_source], conditional on the current function (if any)
    being used. At the top level, marks it unconditionally. *)
val add_cond_any_source : t -> denv:Env.t -> Code_id_or_name.t -> unit

(** Record a direct function application to be resolved later by [deps]. Only
    used for applications to code ids in the current compilation unit. *)
val add_apply : t -> apply_dep -> unit

(** Create the call witness node for a known-arity function definition. The
    witness carries parameter, return, exception, and code-id edges
    corresponding to the function's signature. *)
val create_known_arity_call_witness :
  t ->
  Code_id.t ->
  params:Variable.t list ->
  returns:Variable.t list ->
  exn:Variable.t ->
  Code_id_or_name.t

(** Create a call widget for a known-arity application. Links the apply's
    arguments to the witness's parameters, and the witness's returns and
    exception to the continuation parameters. Returns a node that can be
    connected to the callee's closure via an accessor dependency. *)
val make_known_arity_apply_widget :
  t ->
  denv:Env.t ->
  Apply_expr.t ->
  returns:Variable.t list ->
  exn:Variable.t ->
  Code_id_or_name.t

(** Create the call witness nodes for an unknown-arity function definition. For
    tupled functions, a single witness with tuple-field accessors is created.
    For curried functions, a chain of witnesses is created, one per complex
    parameter, linked via partial-application nodes. *)
val create_unknown_arity_call_witnesses :
  t ->
  Code_id.t ->
  is_tupled:bool ->
  arity:[`Complex] Flambda_arity.t ->
  params:Variable.t list ->
  returns:Variable.t list ->
  exn:Variable.t ->
  Code_id_or_name.t list

(** Create a call widget for an unknown-arity application, analogous to
    [make_known_arity_apply_widget] but for calls where the callee's arity is
    not statically known. *)
val make_unknown_arity_apply_widget :
  t ->
  denv:Env.t ->
  Apply_expr.t ->
  returns:Variable.t list ->
  exn:Variable.t ->
  Code_id_or_name.t

(** Record a dependency between a closure binding and its code id. This is
    resolved later by [deps] to connect closures to their function code in the
    graph. *)
val add_set_of_closures_dep :
  t -> Name.t -> Code_id.t -> only_full_applications:bool -> unit

(** Finalize the graph by resolving all deferred apply and set-of-closures
    dependencies, and return the completed dependency graph. *)
val deps : t -> all_constants:Name.t -> Graph.graph
