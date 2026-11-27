(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* System configuration *)

val version: string
<<<<<<< Merlin:dallsopp/relocatable-oxcaml-revolutions
        (* The current version number of the system *)
||||||| Compiler:last-imported
(** The current version number of the system *)

val bindir: string
(** The directory containing the binary programs. If the compiler was configured
    with [--with-relative-libdir] then this will be the directory containing the
    currently executing runtime. *)

val standard_library_relative: string option
(** The explicit relative path from the compiler binaries to the standard
    libraries directory if the compiler was configured with
    [--with-relative-libdir], or [None] otherwise.

    @since 5.5 *)

val standard_library_default: string
(** The effective value for the default directory containing the standard
    libraries. This is always an absolute path, computed using
    {!standard_library_relative} if necessary.

    @since 5.5 *)

val standard_library: string
(** The effective directory containing the standard libraries, taking CAMLLIB
    and OCAMLLIB into account. *)

val ccomp_type: string
(** The "kind" of the C compiler, assembler and linker used: one of
    "cc" (for Unix-style C compilers)
    "msvc" (for Microsoft Visual C++ and MASM) *)

val c_compiler: string
(** The compiler to use for compiling C files *)

val c_compiler_vendor: string
(** The vendor and version of the C compiler. Consists of hyphenated values
    where the first part indicates the C compiler's vendor and remaining parts
    indicate the version of the C compiler, as reported by that C compiler's
    preprocessor.

    Possible values are:
    - {v msvc v} - Windows, using Microsoft Visual Studio. {v _MSC_VER v}
                   follows and optionally {v clang- v} if {v clang-cl v},
                   rather than Visual Studio itself, is in use.
    - {v icc v} - Intel C Compiler.
    - {v mingw v} - Windows, using the mingw-w64 project. The major and minor
                    version of mingw-w64 itself follow and then either
                    {v gcc- v} or {v clang- v} indicating which C compiler is in
                    use.
    - {v clang v} - Clang LLVM C Compiler.
    - {v gcc v} - GNU Compiler Collection C Compiler.
    - {v xlc v} - IBM XL C Compiler.
    - {v sunc v} - Oracle Solaris Studio.
    - {v unknown v} - unknown compiler vendor. No additional version info.

    @since 5.5 *)

val c_output_obj: string
(** Name of the option of the C compiler for specifying the output
    file *)

val c_has_debug_prefix_map : bool
(** Whether the C compiler supports -fdebug-prefix-map *)
=======
(** The current version number of the system *)

val release_number: int
(** The release number for the compiler

    @since 5.5 *)

val is_official_release: bool
(** True if the compiler is an unmodified official OCaml release

    @since 5.5 *)

val bindir: string
(** The directory containing the binary programs. If the compiler was configured
    with [--with-relative-libdir] then this will be the directory containing the
    currently executing runtime. *)

val standard_library_relative: string option
(** The explicit relative path from the compiler binaries to the standard
    libraries directory if the compiler was configured with
    [--with-relative-libdir], or [None] otherwise.

    @since 5.5 *)

val target_bindir: string
(** The directory containing the runtime binaries on the target system

    @since 5.5 *)

val standard_library_default: string
(** The effective value for the default directory containing the standard
    libraries. This is always an absolute path, computed using
    {!standard_library_relative} if necessary.

    @since 5.5 *)

val standard_library: string
(** The effective directory containing the standard libraries, taking CAMLLIB
    and OCAMLLIB into account. *)

val ccomp_type: string
(** The "kind" of the C compiler, assembler and linker used: one of
    "cc" (for Unix-style C compilers)
    "msvc" (for Microsoft Visual C++ and MASM) *)

val c_compiler: string
(** The compiler to use for compiling C files *)

val c_compiler_vendor: string
(** The vendor and version of the C compiler. Consists of hyphenated values
    where the first part indicates the C compiler's vendor and remaining parts
    indicate the version of the C compiler, as reported by that C compiler's
    preprocessor.

    Possible values are:
    - {v msvc v} - Windows, using Microsoft Visual Studio. {v _MSC_VER v}
                   follows and optionally {v clang- v} if {v clang-cl v},
                   rather than Visual Studio itself, is in use.
    - {v icc v} - Intel C Compiler.
    - {v mingw v} - Windows, using the mingw-w64 project. The major and minor
                    version of mingw-w64 itself follow and then either
                    {v gcc- v} or {v clang- v} indicating which C compiler is in
                    use.
    - {v clang v} - Clang LLVM C Compiler.
    - {v gcc v} - GNU Compiler Collection C Compiler.
    - {v xlc v} - IBM XL C Compiler.
    - {v sunc v} - Oracle Solaris Studio.
    - {v unknown v} - unknown compiler vendor. No additional version info.

    @since 5.5 *)

val c_output_obj: string
(** Name of the option of the C compiler for specifying the output
    file *)

val c_has_debug_prefix_map : bool
(** Whether the C compiler supports -fdebug-prefix-map *)
>>>>>>> Compiler:HEAD

val as_debug_prefix_map_flag : string
(** The flag to use for assembler debug prefix map ("" if none) *)

val interface_suffix: string ref
        (* Suffix for interface file names *)

val cmi_magic_number: string
        (* Magic number for compiled interface files *)
val ast_intf_magic_number: string
        (* Magic number for file holding an interface syntax tree *)
val ast_impl_magic_number: string
        (* Magic number for file holding an implementation syntax tree *)
val cmt_magic_number: string
        (* Magic number for compiled interface files *)
val cms_magic_number: string
        (* Magic number for compiled shapes files *)
val index_magic_number: string
        (* Magic number for index files *)

val max_tag: int
        (* Biggest tag that can be stored in the header of a regular block. *)

val flat_float_array: bool

val reserved_header_bits : int
val syntax_quotations : bool
(** Whether quote [<[_]>] and splice [$] syntax is enabled. *)

val default_keyword_edition : (int * int) option * string list

type name_mangling_scheme =
  | Flat
  | Structured

exception Invalid_name_mangling_scheme of string

val name_mangling_scheme : name_mangling_scheme
<<<<<<< Merlin:dallsopp/relocatable-oxcaml-revolutions
||||||| Compiler:last-imported
(** The name mangling scheme to use *)

val ext_obj: string
(** Extension for object files, e.g. [.o] under Unix. *)

val ext_asm: string
(** Extension for assembler files, e.g. [.s] under Unix. *)

val ext_lib: string
(** Extension for library files, e.g. [.a] under Unix. *)

val ext_dll: string
(** Extension for dynamically-loaded libraries, e.g. [.so] under Unix.*)

val ext_exe: string
(** Extension for executable programs, e.g. [.exe] under Windows.

    @since 4.12 *)

val default_executable_name: string
(** Name of executable produced by linking if none is given with -o,
    e.g. [a.out] under Unix. *)

val systhread_supported : bool
(** Whether the system thread library is implemented *)

val flexdll_dirs : string list
(** Directories needed for the FlexDLL objects *)

val host : string
(** Whether the compiler is a cross-compiler *)

val target : string
(** Whether the compiler is a cross-compiler *)

val flambda : bool
(** Whether the compiler was configured for Flambda 1 *)

val flambda2 : bool
(** Whether the compiler was configured for Flambda 2 *)

val oxcaml : bool
(** [true] if the compiler was built in a OxCaml repo, [false] if
    the compiler was built as per upstream. *)

val with_flambda_invariants : bool
(** Whether the invariants checks for flambda are enabled *)

val with_cmm_invariants : bool
(** Whether the invariants checks for Cmm are enabled *)

val with_codegen_invariants : bool
(** Whether the invariant checks for native code generation are enabled. *)

val reserved_header_bits : int
(** How many bits of a block's header are reserved. This corresponds to the
   HEADER_RESERVED_BITS C preprocessor macro, which is unconditionally set to a
   constant by the configure script in order to enable mixed block support. *)

val custom_ops_struct_size : int
(** Size in bytes of the custom operations structure. *)

val flat_float_array : bool
(** Whether the compiler and runtime automagically flatten float
    arrays *)

val align_double : bool
(** Whether the compiler and runtime need to align double values.
    If [false], a [floatarray] value can be cast to a C array of doubles. *)

val align_int64 : bool
(** Whether the compiler and runtime need to align int64 values *)

val function_sections : bool
(** Whether the compiler was configured to generate
    each function in a separate section *)

val probes : bool
(** Whether the target supports tracing probes *)

val windows_unicode: bool
(** Whether Windows Unicode runtime is enabled *)

val naked_pointers : bool
(** Whether the runtime supports naked pointers

    @since 4.14 *)

val supports_shared_libraries: bool
(** Whether shared libraries are supported

    @since 4.08 *)

val native_dynlink: bool
(** Whether native shared libraries are supported

    @since 5.1 *)

val afl_instrument : bool
(** Whether afl-fuzz instrumentation is generated by default *)

val stack_allocation : bool
(** Whether to stack allocate local values *)

val poll_insertion : bool
(** Whether to insert poll points *)

val ar_supports_response_files: bool
(** Whether ar supports @FILE arguments. *)

val ar_supports_deterministic_archives: bool
(** Whether ar supports the D modifier, which records zero for the timestamp,
    uid and gid of each member and a fixed mode. *)

val no_stack_checks : bool
(** [true] if stack checks are disabled. *)

val multidomain : bool
(** Whether creating multiple domains is allowed.
    Requires stack checks and poll insertion. *)

val tsan : bool
(** Whether ThreadSanitizer instrumentation is enabled *)

val shebangscripts : bool
(** Whether the target supports shebang scripts

    @since 5.5 *)

val parameterised_modules : bool
(** Whether parameterised modules are supported *)

val syntax_quotations : bool
(** Whether quote [<[_]>] and splice [$] syntax is enabled. *)
=======
(** The name mangling scheme to use *)

val ext_obj: string
(** Extension for object files, e.g. [.o] under Unix. *)

val ext_asm: string
(** Extension for assembler files, e.g. [.s] under Unix. *)

val ext_lib: string
(** Extension for library files, e.g. [.a] under Unix. *)

val ext_dll: string
(** Extension for dynamically-loaded libraries, e.g. [.so] under Unix.*)

val ext_exe: string
(** Extension for executable programs, e.g. [.exe] under Windows.

    @since 4.12 *)

val default_executable_name: string
(** Name of executable produced by linking if none is given with -o,
    e.g. [a.out] under Unix. *)

val systhread_supported : bool
(** Whether the system thread library is implemented *)

val flexdll_dirs : string list
(** Directories needed for the FlexDLL objects *)

val host : string
(** Whether the compiler is a cross-compiler *)

val target : string
(** Whether the compiler is a cross-compiler *)

val flambda : bool
(** Whether the compiler was configured for Flambda 1 *)

val flambda2 : bool
(** Whether the compiler was configured for Flambda 2 *)

val oxcaml : bool
(** [true] if the compiler was built in a OxCaml repo, [false] if
    the compiler was built as per upstream. *)

val with_flambda_invariants : bool
(** Whether the invariants checks for flambda are enabled *)

val with_cmm_invariants : bool
(** Whether the invariants checks for Cmm are enabled *)

val with_codegen_invariants : bool
(** Whether the invariant checks for native code generation are enabled. *)

val reserved_header_bits : int
(** How many bits of a block's header are reserved. This corresponds to the
   HEADER_RESERVED_BITS C preprocessor macro, which is unconditionally set to a
   constant by the configure script in order to enable mixed block support. *)

val custom_ops_struct_size : int
(** Size in bytes of the custom operations structure. *)

val flat_float_array : bool
(** Whether the compiler and runtime automagically flatten float
    arrays *)

val align_double : bool
(** Whether the compiler and runtime need to align double values.
    If [false], a [floatarray] value can be cast to a C array of doubles. *)

val align_int64 : bool
(** Whether the compiler and runtime need to align int64 values *)

val function_sections : bool
(** Whether the compiler was configured to generate
    each function in a separate section *)

val probes : bool
(** Whether the target supports tracing probes *)

val windows_unicode: bool
(** Whether Windows Unicode runtime is enabled *)

val naked_pointers : bool
(** Whether the runtime supports naked pointers

    @since 4.14 *)

val supports_shared_libraries: bool
(** Whether shared libraries are supported

    @since 4.08 *)

val native_dynlink: bool
(** Whether native shared libraries are supported

    @since 5.1 *)

val afl_instrument : bool
(** Whether afl-fuzz instrumentation is generated by default *)

val stack_allocation : bool
(** Whether to stack allocate local values *)

val poll_insertion : bool
(** Whether to insert poll points *)

val ar_supports_response_files: bool
(** Whether ar supports @FILE arguments. *)

val ar_supports_deterministic_archives: bool
(** Whether ar supports the D modifier, which records zero for the timestamp,
    uid and gid of each member and a fixed mode. *)

val no_stack_checks : bool
(** [true] if stack checks are disabled. *)

val multidomain : bool
(** Whether creating multiple domains is allowed.
    Requires stack checks and poll insertion. *)

val tsan : bool
(** Whether ThreadSanitizer instrumentation is enabled *)

val shebangscripts : bool
(** Whether the target supports shebang scripts

    @since 5.5 *)

(** Launch mechanisms for bytecode executables

    @since 5.5 *)
type launch_method =
| Executable
    (** Use the executable launcher stub *)
| Shebang of string option
    (** Use a shebang-style launcher. Whenever possible, the interpreter will be
        the runtime itself, but if the path to the runtime is not valid for a
        shebang line, then a shell script is generated. When this is necessary,
        the parameter in [Shebang (Some sh)] is the full path to [sh]; if the
        parameter is [None], then the linker searches PATH for [sh]. *)

val launch_method : launch_method
(** Default launch mechanism for bytecode executables

    @since 5.5 *)

(** Mechanisms used by tendered bytecode executables to locate the interpreter

    @since 5.5 *)
type search_method =
| Disable
    (** Interpreter searching disabled - check fixed absolute location only *)
| Fallback
    (** Check fixed absolute location first, but fall back to a search if that
        fails *)
| Enable
    (** Always search for the interpreter *)

val search_method : search_method
(** Default search mechanism for bytecode executables

    @since 5.5 *)

val suffixing : bool
(** Whether the runtime executable and shared library filenames and C stub
    library filenames are being mangled with Runtime IDs and the {!target}.

    @since 5.5 *)

val bytecode_runtime_id : string
(** The Runtime ID for this build of the bytecode runtime system

    @since 5.5 *)

val native_runtime_id : string
(** The Runtime ID for this build of the native runtime system

    @since 5.5 *)

val parameterised_modules : bool
(** Whether parameterised modules are supported *)

val syntax_quotations : bool
(** Whether quote [<[_]>] and splice [$] syntax is enabled. *)
>>>>>>> Compiler:HEAD

(** Access to configuration values *)
val print_config : out_channel -> unit

val config_var : string -> string option
(** the configuration value of a variable, if it exists *)

(**/**)

val merlin : bool

module Magic_numbers : sig
  type t =
    { cmi_magic_number : string;
      ast_intf_magic_number : string;
      ast_impl_magic_number : string;
      cmt_magic_number : string;
      cms_magic_number : string;
      index_magic_number : string
    }

  val current : t

  val to_json : t -> Std.json
end

(**/**)
