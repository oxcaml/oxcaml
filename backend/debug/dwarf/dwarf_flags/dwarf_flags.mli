type debug_thing =
  | Debug_ocamldebug
  | Debug_js_of_ocaml
  | Debug_subprocs
  | Debug_backtraces
  | Debug_bounds_checking
  | Debug_disable_bytecode_opt
  | Debug_dwarf_cfi
  | Debug_dwarf_loc
  | Debug_dwarf_functions
  | Debug_dwarf_scopes
  | Debug_dwarf_vars
  | Debug_dwarf_call_sites
  | Debug_dwarf_cmm

val debug_thing : debug_thing -> bool

val set_debug_thing : debug_thing -> unit

val clear_debug_thing : debug_thing -> unit

val describe_debug_default : debug_thing -> string

val describe_debug_default_negated : debug_thing -> string

val use_g : unit -> unit

val use_g0 : unit -> unit

val use_g1 : unit -> unit

val use_g2 : unit -> unit

val use_g3 : unit -> unit

val restrict_to_upstream_dwarf : bool ref

val dwarf_max_function_complexity : int ref

val dwarf_for_startup_file : bool ref

type dwarf_version =
  | Four
  | Five

val gdwarf_version : dwarf_version ref

val default_gdwarf_version : dwarf_version

val gdwarf_offsets : bool ref

val default_gdwarf_offsets : bool

val gdwarf_self_tail_calls : bool ref

val default_gdwarf_self_tail_calls : bool

type dwarf_format =
  | Thirty_two
  | Sixty_four

val gdwarf_format : dwarf_format ref

val default_gdwarf_format : dwarf_format

val default_ddebug_invariants : bool

val ddebug_invariants : bool ref

val gdwarf_may_alter_codegen : bool ref

(** Setting this to [true] will emit sufficient DWARF to get inlined frame
    information, but won't emit information e.g. about local variables (unless
    [restrict_to_upstream_dwarf] is set to [false], although that implies
    this variable being set to [true]). *)
val dwarf_inlined_frames : bool ref

val default_gdwarf_compression : string

val gdwarf_compression : string ref

(** Get the DWARF compression flag to pass to the C toolchain.
    Returns a flag of the form " -gz=<compression>" (note the leading space).
    Returns an empty string if compression is not enabled.
    Note: The -gz= option is common between GCC and Clang, unlike
    --compress-debug-sections.
    See: https://maskray.me/blog/2022-01-23-compressed-debug-sections *)
val get_dwarf_c_toolchain_flag : unit -> string

(** Get the DWARF compression flag to pass to the assembler.
    Similar to get_dwarf_c_toolchain_flag but uses the assembler-specific flag.
    Returns a string with a leading space if not empty. *)
val get_dwarf_as_toolchain_flag : unit -> string

(** Get the DWARF compression format to use.
    Returns Some compression if compression is enabled and not "none" or empty.
    Returns None otherwise. *)
val get_dwarf_compression_format : unit -> string option

(** Get the DWARF compression format to use with objcopy.
    Returns Some compression only if compression is enabled and objcopy supports it.
    Returns None otherwise. *)
val get_dwarf_objcopy_compression_format : unit -> string option
