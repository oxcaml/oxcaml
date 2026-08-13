type profile_column = [ `Time | `Alloc | `Top_heap | `Abs_top_heap | `Counters ]
type shape_format = Old_merlin | Debugging_shapes
type visible_include =
  { path : string;
    cmx_guaranteed : bool;
  }

<<<<<<< Merlin:gocamldebug
(** {0 OCaml compiler compatible command-line parameters}

    For compatibility with typechecker.
    Argument parsing / build environment construction happens elsewhere.
*)

(** {1 Relevant settings}
    Parameters from OCaml compiler which affect Merlin behavior. *)
val cmi_file             : string option ref
val include_dirs         : visible_include list ref
val hidden_include_dirs  : string list ref
val include_paths_files : string list ref
val hidden_include_paths_files : string list ref
val print_variance       : bool ref
val fast                 : bool ref
val classic              : bool ref
val all_ppx              : string list ref
val principal            : bool ref
val real_paths           : bool ref
val recursive_types      : bool ref
val strict_sequence      : bool ref
val applicative_functors : bool ref
val nopervasives         : bool ref
val strict_formats       : bool ref
||||||| Compiler:last-imported
module Dwarf_config_defaults : sig
  val shape_reduce_depth : int option
  val shape_eval_depth : int option
  val max_cms_files_per_unit : int option
  val max_cms_files_per_variable : int option
  val max_type_to_shape_depth : int option
  val max_shape_reduce_steps_per_variable : int option
  val max_evaluation_steps_per_variable : int option
  val shape_reduce_fuel : int option
end

val objfiles : string list ref
val ccobjs : string list ref
val dllibs : string list ref
val cmi_file : string option ref
val compile_only : bool ref
val output_name : string option ref
val include_dirs : visible_include list ref
val hidden_include_dirs : string list ref
val include_manifests : string list ref
val hidden_include_manifests : string list ref
val no_std_include : bool ref
val no_cwd : bool ref
val print_types : bool ref
val make_archive : bool ref
val debug : bool ref
val debug_full : bool ref
val dwarf_c_toolchain_flag : string ref
val dwarf_fission : dwarf_fission ref
val dwarf_pedantic : bool ref
val gdwarf_config_shape_reduce_depth : int option ref
val gdwarf_config_shape_eval_depth : int option ref
val gdwarf_config_max_cms_files_per_unit : int option ref
val gdwarf_config_max_cms_files_per_variable : int option ref
val gdwarf_config_max_type_to_shape_depth : int option ref
val gdwarf_config_max_shape_reduce_steps_per_variable : int option ref
val gdwarf_config_max_evaluation_steps_per_variable : int option ref
val gdwarf_config_shape_reduce_fuel : int option ref
val gdwarf_fidelity : gdwarf_fidelity option ref
val gdwarf_fidelity_of_string : string -> gdwarf_fidelity option
val set_gdwarf_fidelity : gdwarf_fidelity -> unit
val unsafe : bool ref
val use_linscan : bool ref
val link_everything : bool ref
val requires_metaprogramming : bool ref
val uses_metaprogramming : bool ref
val custom_runtime : bool ref
val no_check_prims : bool ref
val bytecode_compatible_32 : bool ref
val thunkify_cu_init : bool ref
val output_c_object : bool ref
val output_complete_object : bool ref
val output_complete_executable : bool ref
val all_ccopts : string list ref
val classic : bool ref
val nopervasives : bool ref
val match_context_rows : int ref
val safer_matching : bool ref
=======
module Dwarf_config_defaults : sig
  val shape_reduce_depth : int option
  val shape_eval_depth : int option
  val max_cms_files_per_unit : int option
  val max_cms_files_per_variable : int option
  val max_type_to_shape_depth : int option
  val max_shape_reduce_steps_per_variable : int option
  val max_evaluation_steps_per_variable : int option
  val shape_reduce_fuel : int option
end

val objfiles : string list ref
val ccobjs : string list ref
val dllibs : string list ref
val cmi_file : string option ref
val compile_only : bool ref
val output_name : string option ref
val include_dirs : visible_include list ref
val hidden_include_dirs : string list ref
val include_manifests : string list ref
val hidden_include_manifests : string list ref
val no_std_include : bool ref
val no_cwd : bool ref
val print_types : bool ref
val make_archive : bool ref
val debug : bool ref
val debug_ocamldebug_types : bool ref
val debug_full : bool ref
val dwarf_c_toolchain_flag : string ref
val dwarf_fission : dwarf_fission ref
val dwarf_pedantic : bool ref
val gdwarf_config_shape_reduce_depth : int option ref
val gdwarf_config_shape_eval_depth : int option ref
val gdwarf_config_max_cms_files_per_unit : int option ref
val gdwarf_config_max_cms_files_per_variable : int option ref
val gdwarf_config_max_type_to_shape_depth : int option ref
val gdwarf_config_max_shape_reduce_steps_per_variable : int option ref
val gdwarf_config_max_evaluation_steps_per_variable : int option ref
val gdwarf_config_shape_reduce_fuel : int option ref
val gdwarf_fidelity : gdwarf_fidelity option ref
val gdwarf_fidelity_of_string : string -> gdwarf_fidelity option
val set_gdwarf_fidelity : gdwarf_fidelity -> unit
val unsafe : bool ref
val use_linscan : bool ref
val link_everything : bool ref
val requires_metaprogramming : bool ref
val uses_metaprogramming : bool ref
val custom_runtime : bool ref
val no_check_prims : bool ref
val bytecode_compatible_32 : bool ref
val thunkify_cu_init : bool ref
val output_c_object : bool ref
val output_complete_object : bool ref
val output_complete_executable : bool ref
val all_ccopts : string list ref
val classic : bool ref
val nopervasives : bool ref
val match_context_rows : int ref
val safer_matching : bool ref
>>>>>>> Compiler:HEAD
type open_arg =
  | Open of string
  | Open_cmi of string

val open_args            : open_arg list ref
val parameters           : string list ref
val as_parameter         : bool ref
val as_argument_for      : string option ref
val zero_alloc_check     : Zero_alloc_annotations.Check.t ref
val zero_alloc_assert    : Zero_alloc_annotations.Assert.t ref
val infer_with_bounds    : bool ref
val kind_verbosity : int ref
(* Dedicated flag for the ikinds kind checker (enabled by default). *)
val ikinds : bool ref

(** {1 Dummy values}
    Ignored by merlin but kept for compatibility with upstream code. *)
val annotations          : bool ref
val binary_annotations   : bool ref
val binary_annotations_cms   : bool ref
val shape_format         : shape_format ref
val store_occurrences    : bool ref
val print_types          : bool ref
val native_code          : bool ref
val dont_write_files     : bool ref
val error_size           : int ref (* max size of module related errors *)
val keep_locs            : bool ref
val keep_docs            : bool ref
val transparent_modules  : bool ref
val for_package          : string option ref
val debug                : bool ref
val unsafe               : bool ref
val opaque               : bool ref
val unboxed_types        : bool ref
val profile_columns : profile_column list ref
val dwarf_pedantic : bool ref
val gdwarf_config_shape_eval_depth : int option ref
val gdwarf_config_max_type_to_shape_depth : int option ref
val gdwarf_config_max_evaluation_steps_per_variable : int option ref
val locs : bool ref
val locations            : bool ref
val ikinds_debug : bool ref
val no_alias_deps : bool ref
val unique_ids : bool ref
val dump_dir : string option ref
val verbose_types : bool ref
val canonical_ids : bool ref
val error_style : Misc.Error_style.setting option ref
