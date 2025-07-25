(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2005 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)



(** Command line flags *)

(** Optimization parameters represented as ints indexed by round number. *)
module Int_arg_helper : sig
  type parsed

  val parse : string -> string -> parsed ref -> unit

  type parse_result =
    | Ok
    | Parse_failed of exn
  val parse_no_error : string -> parsed ref -> parse_result

  val get : key:int -> parsed -> int
  val default : int -> parsed
end

(** Optimization parameters represented as floats indexed by round number. *)
module Float_arg_helper : sig
  type parsed

  val parse : string -> string -> parsed ref -> unit

  type parse_result =
    | Ok
    | Parse_failed of exn
  val parse_no_error : string -> parsed ref -> parse_result

  val get : key:int -> parsed -> float
  val default : float -> parsed
end
val set_int_arg :
    int option -> Int_arg_helper.parsed ref -> int -> int option -> unit
val set_float_arg :
    int option -> Float_arg_helper.parsed ref -> float -> float option -> unit

type profile_column = [ `Time | `Alloc | `Top_heap | `Abs_top_heap | `Counters ]
type profile_granularity_level = File_level | Function_level | Block_level
type flambda_invariant_checks = No_checks | Light_checks | Heavy_checks

val objfiles : string list ref
val ccobjs : string list ref
val dllibs : string list ref
val cmi_file : string option ref
val compile_only : bool ref
val output_name : string option ref
val include_dirs : string list ref
val hidden_include_dirs : string list ref
val include_paths_files : string list ref
val hidden_include_paths_files : string list ref
val no_std_include : bool ref
val no_cwd : bool ref
val print_types : bool ref
val make_archive : bool ref
val debug : bool ref
val debug_full : bool ref
val unsafe : bool ref
val use_linscan : bool ref
val link_everything : bool ref
val custom_runtime : bool ref
val no_check_prims : bool ref
val bytecode_compatible_32 : bool ref
val output_c_object : bool ref
val output_complete_object : bool ref
val output_complete_executable : bool ref
val all_ccopts : string list ref
val classic : bool ref
val nopervasives : bool ref
val match_context_rows : int ref
val safer_matching : bool ref
val open_modules : string list ref
val preprocessor : string option ref
val all_ppx : string list ref
val absname : bool ref
val directory : string option ref
val annotations : bool ref
val binary_annotations : bool ref
val binary_annotations_cms : bool ref
val store_occurrences : bool ref
val use_threads : bool ref
val noassert : bool ref
val verbose : bool ref
val verbose_types : bool ref
val noprompt : bool ref
val nopromptcont : bool ref
val init_file : string option ref
val noinit : bool ref
val noversion : bool ref
val use_prims : string ref
val use_runtime : string ref
val plugin : bool ref
val principal : bool ref
val real_paths : bool ref
val recursive_types : bool ref
val strict_sequence : bool ref
val strict_formats : bool ref
val applicative_functors : bool ref
val make_runtime : bool ref
val c_compiler : string option ref
val no_auto_link : bool ref
val dllpaths : string list ref
val make_package : bool ref
val for_package : string option ref
val error_size : int ref
val float_const_prop : bool ref
val transparent_modules : bool ref
val unique_ids : bool ref
val locations : bool ref
val parameters : string list ref
val as_parameter : bool ref
val as_argument_for : string option ref
val instantiate : bool ref
val dump_source : bool ref
val dump_parsetree : bool ref
val dump_typedtree : bool ref
val dump_shape : bool ref
val dump_rawlambda : bool ref
val dump_lambda : bool ref
val dump_blambda : bool ref
val dump_letreclambda : bool ref
val dump_rawclambda : bool ref
val dump_clambda : bool ref
val dump_rawflambda : bool ref
val dump_flambda : bool ref
val dump_flambda_let : int option ref
val dump_instr : bool ref
val keep_camlprimc_file : bool ref
val keep_asm_file : bool ref
val optimize_for_speed : bool ref
val dump_cmm : bool ref
val dump_cse : bool ref
val dump_linear : bool ref
val debug_ocaml : bool ref
val keep_startup_file : bool ref
val native_code : bool ref
val default_inline_threshold : float
val inline_threshold : Float_arg_helper.parsed ref
val inlining_report : bool ref
val simplify_rounds : int option ref
val default_simplify_rounds : int ref
val rounds : unit -> int
val default_inline_max_unroll : int
val inline_max_unroll : Int_arg_helper.parsed ref
val default_inline_toplevel_threshold : int
val inline_toplevel_threshold : Int_arg_helper.parsed ref
val default_inline_call_cost : int
val default_inline_alloc_cost : int
val default_inline_prim_cost : int
val default_inline_branch_cost : int
val default_inline_indirect_cost : int
val default_inline_lifting_benefit : int
val inline_call_cost : Int_arg_helper.parsed ref
val inline_alloc_cost : Int_arg_helper.parsed ref
val inline_prim_cost : Int_arg_helper.parsed ref
val inline_branch_cost : Int_arg_helper.parsed ref
val inline_indirect_cost : Int_arg_helper.parsed ref
val inline_lifting_benefit : Int_arg_helper.parsed ref
val default_inline_branch_factor : float
val inline_branch_factor : Float_arg_helper.parsed ref
val dont_write_files : bool ref
val std_include_flag : string -> string
val std_include_dir : unit -> string list
val shared : bool ref
val dlcode : bool ref
val pic_code : bool ref
val runtime_variant : string ref
val ocamlrunparam : string ref
val with_runtime : bool ref
val force_slash : bool ref
val keep_docs : bool ref
val keep_locs : bool ref
val opaque : bool ref
val default_timings_precision : int
val timings_precision : int ref
val profile_columns : profile_column list ref
val profile_granularity : profile_granularity_level ref
val all_profile_granularity_levels : string list
val set_profile_granularity : string -> unit
val flambda_invariant_checks : flambda_invariant_checks ref
val unbox_closures : bool ref
val unbox_closures_factor : int ref
val default_unbox_closures_factor : int
val unbox_free_vars_of_closures : bool ref
val unbox_specialised_args : bool ref
val clambda_checks : bool ref
val cmm_invariants : bool ref
val default_inline_max_depth : int
val inline_max_depth : Int_arg_helper.parsed ref
val remove_unused_arguments : bool ref
val dump_flambda_verbose : bool ref
val classic_inlining : bool ref
val afl_instrument : bool ref
val afl_inst_ratio : int ref
val function_sections : bool ref
val probes : bool ref
val llvm_backend : bool ref

val all_passes : string list ref
val dumped_pass : string -> bool
val set_dumped_pass : string -> bool -> unit

val dump_into_file : bool ref
val dump_into_csv : bool ref
val dump_dir : string option ref

(* Support for flags that can also be set from an environment variable *)
type 'a env_reader = {
  parse : string -> 'a option;
  print : 'a -> string;
  usage : string;
  env_var : string;
}

val color : Misc.Color.setting option ref
val color_reader : Misc.Color.setting env_reader

val error_style : Misc.Error_style.setting option ref
val error_style_reader : Misc.Error_style.setting env_reader

val unboxed_types : bool ref

val dump_debug_uids : bool ref         (* -ddebug-uids *)

val dump_debug_uid_tables : bool ref   (* -ddebug-uid-tables *)

val insn_sched : bool ref
val insn_sched_default : bool

module Opt_flag_handler : sig
  type t = {
    set_oclassic : unit -> unit;
    set_o2 : unit -> unit;
    set_o3 : unit -> unit;
  }

  val default : t

  val set : t -> unit
end

val set_oclassic : unit -> unit
val set_o2 : unit -> unit
val set_o3 : unit -> unit

module Compiler_ir : sig
  type t = Linear | Cfg | Llvmir
  val all : t list
  val to_string : t -> string
  val extension : t -> string
  val extract_extension_with_pass : string -> (t * string) option
end

module Compiler_pass : sig
  type t = Parsing | Typing | Lambda | Middle_end
         | Linearization | Emit | Simplify_cfg | Selection
         | Register_allocation | Llvmize
  val of_string : string -> t option
  val to_string : t -> string
  val is_compilation_pass : t -> bool
  val available_pass_names : filter:(t -> bool) -> native:bool -> string list
  val can_save_ir_after : t -> bool
  val can_save_ir_before : t -> bool
  val compare : t -> t -> int
  val to_output_filename: t -> prefix:string -> string
  val of_input_filename: string -> t option
end
val stop_after : Compiler_pass.t option ref
val should_stop_after : Compiler_pass.t -> bool
val set_save_ir_after : Compiler_pass.t -> bool -> unit
val set_save_ir_before : Compiler_pass.t -> bool -> unit
val should_save_ir_after : Compiler_pass.t -> bool
val should_save_ir_before : Compiler_pass.t -> bool

val is_flambda2 : unit -> bool

val arg_spec : (string * Arg.spec * string) list ref

(* [add_arguments __LOC__ args] will add the arguments from [args] at
   the end of [arg_spec], checking that they have not already been
   added by [add_arguments] before. A warning is printed showing the
   locations of the function from which the argument was previously
   added. *)
val add_arguments : string -> (string * Arg.spec * string) list -> unit

(* [create_usage_msg program] creates a usage message for [program] *)
val create_usage_msg: string -> string
(* [print_arguments usage] print the standard usage message *)
val print_arguments : string -> unit

(* [reset_arguments ()] clear all declared arguments *)
val reset_arguments : unit -> unit

(* [zero_alloc_check] specifies which zero_alloc attributes to check. *)
val zero_alloc_check : Zero_alloc_annotations.Check.t ref
(* [zero_alloc_assert] specifies which zero_alloc attributes to add. *)
val zero_alloc_assert : Zero_alloc_annotations.Assert.t ref

val no_auto_include_otherlibs : bool ref

val prepend_directory : string -> string
