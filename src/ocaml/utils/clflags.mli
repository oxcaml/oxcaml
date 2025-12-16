type profile_column = [ `Time | `Alloc | `Top_heap | `Abs_top_heap | `Counters ]
type shape_format = Old_merlin | Debugging_shapes

(** {0 OCaml compiler compatible command-line parameters}

    For compatibility with typechecker.
    Argument parsing / build environment construction happens elsewhere.
*)

(** {1 Relevant settings}
    Parameters from OCaml compiler which affect Merlin behavior. *)
val cmi_file             : string option ref
val include_dirs         : string list ref
val hidden_include_dirs  : string list ref
val include_paths_files : string list ref
val hidden_include_paths_files : string list ref
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
val open_modules         : string list ref
val parameters           : string list ref
val as_parameter         : bool ref
val as_argument_for      : string option ref
val zero_alloc_check     : Zero_alloc_annotations.Check.t ref
val zero_alloc_assert    : Zero_alloc_annotations.Assert.t ref
val infer_with_bounds    : bool ref

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
val opaque               : bool ref
val unboxed_types        : bool ref
val profile_columns : profile_column list ref
val dwarf_pedantic : bool ref
val gdwarf_config_shape_eval_depth : int option ref
val gdwarf_config_max_type_to_shape_depth : int option ref
val gdwarf_config_max_evaluation_steps_per_variable : int option ref
val locs : bool ref
val locations            : bool ref
