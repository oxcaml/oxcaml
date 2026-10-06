(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** OxCaml specific command line flags *)

val dump_cfg : bool ref
val cfg_invariants : bool ref
val regalloc : Clflags.Register_allocator.t ref
val default_regalloc_linscan_threshold : int
val regalloc_linscan_threshold : int ref
val regalloc_params : string list ref
val regalloc_validate : bool ref

val vectorize : bool ref
val dump_vectorize : bool ref
val default_vectorize_max_block_size : int
val vectorize_max_block_size : int ref

val cfg_peephole_optimize: bool ref

val x86_peephole_optimize : bool ref
val x86_peephole_remove_mov_to_dead_register : bool ref
val x86_peephole_remove_redundant_cmp : bool ref
val x86_peephole_remove_redundant_extension : bool ref
val x86_peephole_combine_add_rsp : bool ref
val x86_peephole_remove_redundant_test : bool ref

val cfg_stack_checks : bool ref
val cfg_stack_checks_threshold : int ref

val cfg_eliminate_dead_trap_handlers : bool ref

val cfg_prologue_validate : bool ref
val cfg_prologue_shrink_wrap : bool ref
val cfg_prologue_shrink_wrap_threshold : int ref
val omit_leaf_frame_pointers : bool ref

val cfg_merge_blocks : bool ref

val cfg_block_layout : bool ref

val cfg_value_propagation : bool ref
val cfg_value_propagation_float : bool ref
val cfg_value_propagation_flow : bool ref

val reorder_blocks_random : int option ref
val basic_block_sections : bool ref
val module_entry_functions_section : bool ref

val dasm_comments : bool ref


val default_heap_reduction_threshold : int
val heap_reduction_threshold : int ref
val dump_zero_alloc : bool ref
val disable_zero_alloc_checker : bool ref
val disable_precise_zero_alloc_checker : bool ref
val davail : bool ref
val dranges : bool ref

type zero_alloc_checker_details_cutoff =
  | Keep_all
  | At_most of int  (* n > 0 *)
  | No_details

val zero_alloc_checker_details_cutoff : zero_alloc_checker_details_cutoff ref
val default_zero_alloc_checker_details_cutoff : zero_alloc_checker_details_cutoff
val zero_alloc_checker_details_extra : bool ref

type zero_alloc_checker_join =
  | Keep_all
  | Widen of int  (* n > 0 *)
  | Error of int (* n > 0 *)

val zero_alloc_checker_join : zero_alloc_checker_join ref
val default_zero_alloc_checker_join : zero_alloc_checker_join

module Function_layout : sig
  type t =
    | Topological
    | Source

  val to_string : t -> string
  val of_string : string -> t option
  val default :t

  val all : t list
end


val function_layout : Function_layout.t ref
val disable_builtin_check : bool ref
val disable_poll_insertion : bool ref
val allow_long_frames : bool ref
val max_long_frames_threshold : int
val long_frames_threshold : int ref
val branch_relaxation_max_displacement : int ref
val caml_apply_inline_fast_path : bool ref

type function_result_types =
  | Never
  | Functors_only
  | Functors_and_static_closures
      (** Functors, plus functions returning closures whose environments only
          refer to the functions' parameters (or constants), i.e. closures that
          would be statically allocated at a call site with known arguments. *)
  | Functors_and_closures
      (** Functors, plus all functions returning closures. *)
  | All_functions
type reaper_preserve_direct_calls = Never | Always | Zero_alloc | Auto
type join_algorithm = Binary | N_way | Checked
type opt_level = Oclassic | O2 | O3 | O4
type 'a or_default = Set of 'a | Default

val dump_inlining_paths : bool ref

val opt_level : opt_level or_default ref

val internal_assembler : bool ref

val verify_binary_emitter : bool ref

val gc_timings : bool ref

val use_cached_generic_functions : bool ref
val cached_generic_functions_path : string ref

val dissector_assume_lld_without_64_bit_eh_frames : bool ref

val dissector_max_linker_parallelism : Misc.Maybe_bounded.t ref

val manual_module_init : bool ref

val symbol_visibility_protected : bool ref

val dump_llvmir : bool ref
val keep_llvmir : bool ref
val llvm_path : string option ref
val llvm_flags : string ref

module Flambda2 : sig
  val debug : bool ref
  val reaper_debug_flags : string list ref

  module Default : sig
    val classic_mode : bool
    val join_points : bool
    val unbox_along_intra_function_control_flow : bool
    val backend_cse_at_toplevel : bool
    val cse_depth : int
    val join_depth : int
    val join_algorithm : join_algorithm
    val function_result_types : function_result_types
    val enable_reaper : bool
    val reaper_preserve_direct_calls : reaper_preserve_direct_calls
    val reaper_local_fields : bool
    val reaper_unbox : bool
    val reaper_max_unbox_size : int
    val reaper_change_calling_conventions : bool
    val simplify_stubs : bool
    val unicode : bool
    val kind_checks : bool
    val match_in_match : bool
  end

  (* CR-someday lmaurer: We could eliminate most of the per-flag boilerplate using GADTs
     and heterogeneous maps. Whether that's an improvement is a fair question. *)

  type flags = {
    classic_mode : bool;
    join_points : bool;
    unbox_along_intra_function_control_flow : bool;
    backend_cse_at_toplevel : bool;
    cse_depth : int;
    join_depth : int;
    join_algorithm : join_algorithm;
    function_result_types : function_result_types;
    enable_reaper : bool;
    reaper_preserve_direct_calls : reaper_preserve_direct_calls;
    reaper_local_fields : bool;
    reaper_unbox : bool;
    reaper_max_unbox_size : int;
    reaper_change_calling_conventions : bool;
    simplify_stubs : bool;
    unicode : bool;
    kind_checks : bool;
    match_in_match : bool;
  }

  val default_for_opt_level : opt_level or_default -> flags

  val function_result_types : function_result_types or_default ref

  val classic_mode : bool or_default ref
  val join_points : bool or_default ref
  val unbox_along_intra_function_control_flow : bool or_default ref
  val backend_cse_at_toplevel : bool or_default ref
  val cse_depth : int or_default ref
  val join_depth : int or_default ref
  val join_algorithm : join_algorithm or_default ref
  val enable_reaper : bool or_default ref
  val reaper_preserve_direct_calls : reaper_preserve_direct_calls or_default ref
  val reaper_local_fields : bool or_default ref
  val reaper_unbox : bool or_default ref
  val reaper_max_unbox_size : int or_default ref
  val reaper_change_calling_conventions : bool or_default ref
  val simplify_stubs : bool or_default ref
  val unicode : bool or_default ref
  val kind_checks : bool or_default ref
  val match_in_match : bool or_default ref

  module Dump : sig
    type target = Nowhere | Main_dump_stream | File of Misc.filepath
    type pass = Last_pass | This_pass of string

    val rawfexpr : target ref
    val fexpr : target ref
    val fexpr_after : pass ref
    val fexpr_annot : bool ref
    val fexpr_annot_after : string list ref
    val slot_offsets : bool ref
    val freshen : bool ref
    val flow : bool ref
    val simplify : bool ref
    val reaper : bool ref
    val code_sizes : bool ref
    val inlining_stats : bool ref
  end

  (** In the result types of functors, keep the types of variables that are only
      reachable through the value slots of the returned closures (instead of
      replacing them by Unknown). *)
  val functor_result_types_through_value_slots : bool ref

  (** As [functor_result_types_through_value_slots], but for functions that are
      not functors.  Only has an effect when result types are computed for such
      functions (see [function_result_types]). *)
  val function_result_types_through_value_slots : bool ref

  (** Which model estimates the size of the machine code for Flambda terms:
      [V1] is the original model, [V2] the current one (see [Code_size]). *)
  type code_size_model = V1 | V2

  val code_size_model : code_size_model ref

  module Expert : sig
    module Default : sig
      val fallback_inlining_heuristic : bool
      val inline_effects_in_cmm : bool
      val cmm_safe_subst : bool
      val phantom_lets : bool
      val max_block_size_for_projections : int option
      val max_unboxing_depth : int
      val can_inline_recursive_functions : bool
      val max_function_simplify_run : int
      val shorten_symbol_names : bool
      val cont_lifting_budget : int
      val cont_spec_threshold : float
    end

    type flags = {
      fallback_inlining_heuristic : bool;
      inline_effects_in_cmm : bool;
      cmm_safe_subst : bool;
      phantom_lets : bool;
      max_block_size_for_projections : int option;
      max_unboxing_depth : int;
      can_inline_recursive_functions : bool;
      max_function_simplify_run : int;
      shorten_symbol_names : bool;
      cont_lifting_budget : int;
      cont_spec_threshold : float;
    }

    val default_for_opt_level : opt_level or_default -> flags

    val fallback_inlining_heuristic : bool or_default ref
    val inline_effects_in_cmm : bool or_default ref
    val cmm_safe_subst : bool or_default ref
    val phantom_lets : bool or_default ref
    val max_block_size_for_projections : int option or_default ref
    val max_unboxing_depth : int or_default ref
    val can_inline_recursive_functions : bool or_default ref
    val max_function_simplify_run : int or_default ref
    val shorten_symbol_names : bool or_default ref
    val cont_lifting_budget : int or_default ref
    val cont_spec_threshold : float or_default ref
  end

  module Debug : sig
    module Default : sig
      val concrete_types_only_on_canonicals : bool
      val keep_invalid_handlers : bool
    end

    val concrete_types_only_on_canonicals : bool ref
    val keep_invalid_handlers : bool ref
  end

  module Inlining : sig
    type inlining_arguments = private {
      max_depth : int;
      max_rec_depth : int;
      call_cost : float;
      alloc_cost : float;
      prim_cost : float;
      branch_cost : float;
      indirect_call_cost : float;
      poly_compare_cost : float;
      small_function_size : int;
      large_function_size : int;
      small_functor_size : int;
      large_functor_size : int;
      threshold : float;
    }

    (** How the result of a speculative inlining is judged: [Threshold]
        compares the evaluated cost metrics against the inlining threshold;
        [Ratio] compares the size of the inlined body, less the call-site
        credit and the bonuses for removed operations, as a fraction of the
        callee's size before inlining, against [speculative_inlining_ratio].
        With a speculative inlining budget, the threshold remains the
        budget of each speculation under either criterion. *)
    type speculative_inlining_criterion =
      | Threshold
      | Ratio

    module Default : sig
      val default_arguments : inlining_arguments
      val speculative_inlining_only_if_arguments_useful : bool
      val speculative_inlining_track_lifted_constants : bool
      val speculative_inlining_charge_uninlined_calls : bool
      val speculative_inlining_nested : bool
      val speculative_inlining_credit_caller_allocations : bool
      val speculative_inlining_merge_return_continuation : bool
      val speculative_inlining_uninlined_call_cost_factor : float
      val speculative_inlining_budget : bool
      val speculative_inlining_budget_size_ratio : float
      val speculative_inlining_criterion : speculative_inlining_criterion
      val speculative_inlining_ratio : float
      val speculative_inlining_budget_size : float
      val speculative_inlining_budget_max_credit : float
      val speculative_inlining_credit_call_site : bool
      val speculative_inlining_bonus_call : float
      val speculative_inlining_bonus_alloc : float
      val speculative_inlining_bonus_prim : float
      val speculative_inlining_bonus_branch : float
      val speculative_inlining_bonus_indirect_call : float
      val speculative_inlining_bonus_poly_compare : float
    end

    val oclassic_arguments : inlining_arguments
    val o2_arguments : inlining_arguments
    val o3_arguments : inlining_arguments

    val max_depth : Clflags.Int_arg_helper.parsed ref
    val max_rec_depth : Clflags.Int_arg_helper.parsed ref

    val call_cost : Clflags.Float_arg_helper.parsed ref
    val alloc_cost : Clflags.Float_arg_helper.parsed ref
    val prim_cost : Clflags.Float_arg_helper.parsed ref
    val branch_cost : Clflags.Float_arg_helper.parsed ref
    val indirect_call_cost : Clflags.Float_arg_helper.parsed ref
    val poly_compare_cost : Clflags.Float_arg_helper.parsed ref

    val small_function_size : Clflags.Int_arg_helper.parsed ref
    val large_function_size : Clflags.Int_arg_helper.parsed ref

    val small_functor_size : Clflags.Int_arg_helper.parsed ref
    val large_functor_size : Clflags.Int_arg_helper.parsed ref

    val threshold : Clflags.Float_arg_helper.parsed ref

    val speculative_inlining_only_if_arguments_useful : bool ref

    val speculative_inlining_track_lifted_constants : bool ref

    val speculative_inlining_charge_uninlined_calls : bool ref

    (** Inside the outermost speculative inlining, speculate on calls to
        speculatively-inlinable functions instead of leaving them as calls
        (one level of nested speculation). *)
    val speculative_inlining_nested : bool ref

    (** When judging a speculative inlining, credit the allocations of the
        caller that flow only into the call and that the inlined body no
        longer refers to, since they will be deleted. *)
    val speculative_inlining_credit_caller_allocations : bool ref

    (** When inlining a call whose return continuation is used only by that
        call, copy the continuation's handler into the inlined body, so that
        it is simplified (and judged, when the inlining is speculative) with
        what is known about the returned values. *)
    val speculative_inlining_merge_return_continuation : bool ref

    val speculative_inlining_uninlined_call_cost_factor : float ref

    val speculative_inlining_budget : bool ref

    val speculative_inlining_budget_size_ratio : float ref

    val speculative_inlining_criterion : speculative_inlining_criterion ref

    val speculative_inlining_ratio : float ref

    (** The budget of a speculative inlining (see [speculative_inlining_budget]);
        zero or less means: the inlining threshold. *)
    val speculative_inlining_budget_size : float ref

    (** Removed operations may offset at most this multiple of the budget's
        size within one speculative region; negative means no limit. *)
    val speculative_inlining_budget_max_credit : float ref

    val speculative_inlining_credit_call_site : bool ref

    val speculative_inlining_bonus_call : float ref

    val speculative_inlining_bonus_alloc : float ref

    val speculative_inlining_bonus_prim : float ref

    val speculative_inlining_bonus_branch : float ref

    val speculative_inlining_bonus_indirect_call : float ref

    val speculative_inlining_bonus_poly_compare : float ref

    val report_bin : bool ref

    (** Whether [-flambda2-inline-2026] was given. *)
    val inline_2026 : bool ref

    val inline_2026_small_function_size : int

    val inline_2026_speculative_inlining_budget : float

    (** Enable the v2 code size model, lifted-constant tracking, the
        speculative inlining budget, the ratio criterion with the call-site
        credit with a budget of [inline_2026_speculative_inlining_budget],
        a small function size of [inline_2026_small_function_size],
        result types for functors and closures, and functor result types
        through value slots. *)
    val set_inline_2026 : unit -> unit
  end
end

val opt_flag_handler : Clflags.Opt_flag_handler.t
