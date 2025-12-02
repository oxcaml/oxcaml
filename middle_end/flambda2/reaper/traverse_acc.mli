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

  type t =
    { parent : Rev_expr.rev_expr_holed;
      conts : cont_kind Continuation.Map.t;
      current_code_id : Code_id.t option;
      should_preserve_direct_calls : should_preserve_direct_calls;
      le_monde_exterieur : Name.t;
      all_constants : Name.t
    }
end

type code_dep =
  { arity : [`Complex] Flambda_arity.t;
    params : Variable.t list;
    my_closure : Variable.t;
    return : Variable.t list; (* Dummy variable representing return value *)
    exn : Variable.t; (* Dummy variable representing exn return value *)
    is_tupled : bool;
    known_arity_call_witness : Code_id_or_name.t;
    unknown_arity_call_witnesses : Code_id_or_name.t list
  }

type apply_dep =
  { function_containing_apply_expr : Code_id.t option;
    apply_code_id : Code_id.t;
    apply_closure : Simple.t option;
    apply_call_witness : Code_id_or_name.t
  }

type t

val create : unit -> t

(* Functions about variable kinds *)
val kind : Name.t -> Flambda_kind.t -> t -> unit

val bound_parameter_kind : Bound_parameter.t -> t -> unit

val alias_kind : Name.t -> Simple.t -> t -> unit

val kinds : t -> Flambda_kind.t Name.Map.t

(* Fixed arity continuations may not have their arity changed. *)
val fixed_arity_continuation : t -> Continuation.t -> unit

val fixed_arity_continuations : t -> Continuation.Set.t

(* Continuation information map. *)
val continuation_info : t -> Continuation.t -> continuation_info -> unit

val get_continuation_info : t -> continuation_info Continuation.Map.t

(* Code *)
val add_code : Code_id.t -> code_dep -> t -> unit

val find_code : t -> Code_id.t -> code_dep

val code_deps : t -> code_dep Code_id.Map.t

(* Directly adding edges to the graph *)
val add_alias : t -> to_:Code_id_or_name.t -> from:Code_id_or_name.t -> unit

val add_use_dep : t -> to_:Code_id_or_name.t -> from:Code_id_or_name.t -> unit

val add_accessor_dep :
  t -> to_:Code_id_or_name.t -> Field.t -> base:Code_id_or_name.t -> unit

val add_constructor_dep :
  t -> base:Code_id_or_name.t -> Field.t -> from:Code_id_or_name.t -> unit

val add_coaccessor_dep :
  t -> to_:Code_id_or_name.t -> Cofield.t -> base:Code_id_or_name.t -> unit

val add_coconstructor_dep :
  t -> base:Code_id_or_name.t -> Cofield.t -> from:Code_id_or_name.t -> unit

val add_propagate_dep :
  t ->
  if_used:Code_id_or_name.t ->
  to_:Code_id_or_name.t ->
  from:Code_id_or_name.t ->
  unit

val add_copropagate_dep :
  t ->
  if_any_source:Code_id_or_name.t ->
  to_:Code_id_or_name.t ->
  from:Code_id_or_name.t ->
  unit

val add_any_usage : t -> Code_id_or_name.t -> unit

val add_any_source : t -> Code_id_or_name.t -> unit

val add_code_id_my_closure : t -> Code_id.t -> Variable.t -> unit

(* *)

val simple_to_node : t -> denv:Env.t -> Simple.t -> Code_id_or_name.t

val add_cond_any_usage : t -> denv:Env.t -> Simple.t -> unit

val add_cond_any_source : t -> denv:Env.t -> Code_id_or_name.t -> unit

val add_apply : apply_dep -> t -> unit

val create_known_arity_call_witness :
  t ->
  Code_id.t ->
  params:Variable.t list ->
  returns:Variable.t list ->
  exn:Variable.t ->
  Code_id_or_name.t

val make_known_arity_apply_widget :
  t ->
  denv:Env.t ->
  params:Simple.t list ->
  returns:Variable.t list ->
  exn:Variable.t ->
  Code_id_or_name.t

val create_unknown_arity_call_witnesses :
  t ->
  Code_id.t ->
  is_tupled:bool ->
  arity:[`Complex] Flambda_arity.t ->
  params:Variable.t list ->
  returns:Variable.t list ->
  exn:Variable.t ->
  Code_id_or_name.t list

val make_unknown_arity_apply_widget :
  t ->
  denv:Env.t ->
  arity:[`Complex] Flambda_arity.t ->
  params:Simple.t list ->
  returns:Variable.t list ->
  exn:Variable.t ->
  Code_id_or_name.t

val add_set_of_closures_dep :
  Name.t -> Code_id.t -> only_full_applications:bool -> t -> unit

val deps : t -> all_constants:Name.t -> Graph.graph
