(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import

type resolver =
  Compilation_unit.t -> Flambda2_types.Typing_env.Serializable.t option

type get_imported_names = unit -> Name.Set.t

type get_imported_code = unit -> Exported_code.t

type t

(** Print a human-readable version of the given environment. *)
val print : Format.formatter -> t -> unit

(** Create a new environment, marked as being at the toplevel of a compilation
    unit. *)
val create :
  round:int ->
  resolver:resolver ->
  get_imported_names:get_imported_names ->
  get_imported_code:get_imported_code ->
  propagating_float_consts:bool ->
  unit_toplevel_exn_continuation:Continuation.t ->
  unit_toplevel_return_continuation:Continuation.t ->
  toplevel_my_region:Variable.t ->
  toplevel_my_ghost_region:Variable.t ->
  t

val all_code : t -> Code.t Code_id.Map.t

val resolver :
  t -> Compilation_unit.t -> Flambda2_types.Typing_env.Serializable.t option

val propagating_float_consts : t -> bool

val at_unit_toplevel : t -> bool

val set_at_unit_toplevel_state : t -> bool -> t

val is_defined_at_toplevel : t -> Variable.t -> bool

val add_symbol_projection : t -> Variable.t -> Symbol_projection.t -> t

val find_symbol_projection : t -> Variable.t -> Symbol_projection.t option

val unit_toplevel_return_continuation : t -> Continuation.t

val unit_toplevel_exn_continuation : t -> Continuation.t

val increment_continuation_scope : t -> t

val bump_current_level_scope : t -> t

val get_continuation_scope : t -> Scope.t

val typing_env : t -> Flambda2_types.Typing_env.t

val define_continuations : t -> Continuation.t list -> t

val define_variable : t -> Bound_var.t -> Flambda_kind.t -> t

val define_extra_variable : t -> Bound_var.t -> Flambda_kind.t -> t

val add_name : t -> Bound_name.t -> Flambda2_types.t -> t

val add_variable : t -> Bound_var.t -> Flambda2_types.t -> t

val add_equation_on_variable : t -> Variable.t -> Flambda2_types.t -> t

val mem_variable : t -> Variable.t -> bool

val add_symbol : t -> Symbol.t -> Flambda2_types.t -> t

val define_symbol : t -> Symbol.t -> Flambda_kind.t -> t

val mem_symbol : t -> Symbol.t -> bool

val find_symbol : t -> Symbol.t -> Flambda2_types.t

val add_equation_on_symbol : t -> Symbol.t -> Flambda2_types.t -> t

val define_name : t -> Bound_name.t -> Flambda_kind.t -> t

val define_name_if_undefined : t -> Bound_name.t -> Flambda_kind.t -> t

val add_equation_on_name : t -> Name.t -> Flambda2_types.t -> t

val define_parameters : extra:bool -> t -> params:Bound_parameters.t -> t

val add_parameters :
  extra:bool ->
  ?name_mode:Name_mode.t ->
  t ->
  Bound_parameters.t ->
  param_types:Flambda2_types.t list ->
  t

val add_parameters_with_unknown_types :
  extra:bool ->
  ?alloc_modes:Alloc_mode.For_types.t list ->
  ?name_mode:Name_mode.t ->
  t ->
  Bound_parameters.t ->
  t

val mark_parameters_as_toplevel : t -> Bound_parameters.t -> t

val extend_typing_environment :
  t -> Flambda2_types.Typing_env_extension.With_extra_variables.t -> t

val with_typing_env : t -> Flambda2_types.Typing_env.t -> t

val map_typing_env :
  t -> f:(Flambda2_types.Typing_env.t -> Flambda2_types.Typing_env.t) -> t

val check_simple_is_bound : t -> Simple.t -> unit

val define_code : t -> code_id:Code_id.t -> code:Code.t -> t

val mem_code : t -> Code_id.t -> bool

(** This function raises if the code ID is unbound. *)
val find_code_exn : t -> Code_id.t -> Code_or_metadata.t

val set_inlined_debuginfo : t -> from:t -> t

val merge_inlined_debuginfo : t -> from_apply_expr:Inlined_debuginfo.t -> t

val add_inlined_debuginfo : t -> Debuginfo.t -> Debuginfo.t

val round : t -> int

val set_inlining_state : t -> Inlining_state.t -> t

val get_inlining_state : t -> Inlining_state.t

val add_cse :
  t -> Flambda_primitive.Eligible_for_cse.t -> bound_to:Simple.t -> t

val find_cse : t -> Flambda_primitive.Eligible_for_cse.t -> Simple.t option

val find_comparison_result : t -> Variable.t -> Comparison_result.t option

val cse : t -> Common_subexpression_elimination.t

val comparison_results : t -> Comparison_result.t Variable.Map.t

val with_cse : t -> Common_subexpression_elimination.t -> t

module Disable_inlining_reason : sig
  type t =
    | Stub
    | Speculative_inlining
end

val set_do_not_rebuild_terms_and_disable_inlining :
  t -> Disable_inlining_reason.t -> t

val set_disable_inlining : t -> Disable_inlining_reason.t -> t

module Disable_inlining : sig
  type t =
    | Disable_inlining of Disable_inlining_reason.t
    | Do_not_disable_inlining
end

val disable_inlining : t -> Disable_inlining.t

val enter_set_of_closures : t -> in_stub:bool -> t

val set_rebuild_terms : t -> t

val are_rebuilding_terms : t -> Are_rebuilding_terms.t

val enter_closure :
  Code_id.t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  my_closure:Variable.t ->
  t ->
  t

val closure_info : t -> Closure_info.t

val inlining_arguments : t -> Inlining_arguments.t

val set_inlining_arguments : Inlining_arguments.t -> t -> t

val enter_inlined_apply :
  called_code:Code.t -> apply:Apply.t -> was_inline_always:bool -> t -> t

val generate_phantom_lets : t -> bool

val inlining_history_tracker : t -> Inlining_history.Tracker.t

val set_inlining_history_tracker : Inlining_history.Tracker.t -> t -> t

val relative_history : t -> Inlining_history.Relative.t

val loopify_state : t -> Loopify_state.t

val set_loopify_state : Loopify_state.t -> t -> t

val with_code_age_relation : Code_age_relation.t -> t -> t

val defined_variables_by_scope : t -> Lifted_cont_params.t list

val enter_continuation_handler : Lifted_cont_params.t -> t -> t

val variables_defined_in_current_continuation : t -> Lifted_cont_params.t

val cost_of_lifting_continuations_out_of_current_one : t -> int

val add_lifting_cost : int -> t -> t

val must_inline : t -> bool

val replay_history : t -> Replay_history.t

val with_replay_history : (Replay_history.t * bool) option -> t -> t

val map_specialization_cost :
  f:(Specialization_cost.t -> Specialization_cost.t) -> t -> t

val specialization_cost : t -> Specialization_cost.t

val denv_for_lifted_continuation : denv_for_join:t -> denv:t -> t
