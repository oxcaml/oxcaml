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
<<<<<<< Merlin:tt
        (* The current version number of the system *)
||||||| Compiler:last-imported
(** The current version number of the system *)

val bindir: string
(** The directory containing the binary programs *)

val standard_library: string
(** The directory containing the standard libraries *)

val ccomp_type: string
(** The "kind" of the C compiler, assembler and linker used: one of
    "cc" (for Unix-style C compilers)
    "msvc" (for Microsoft Visual C++ and MASM) *)

val c_compiler: string
(** The compiler to use for compiling C files *)

val c_output_obj: string
(** Name of the option of the C compiler for specifying the output
    file *)

val c_has_debug_prefix_map : bool
(** Whether the C compiler supports -fdebug-prefix-map *)
=======
(** The current version number of the system *)

val bindir: string
(** The directory containing the binary programs *)

val standard_library: string
(** The directory containing the standard libraries *)

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

<<<<<<< Merlin:tt
val reserved_header_bits : int
val syntax_quotations : bool
(** Whether quote [<[_]>] and splice [$] syntax is enabled. *)
||||||| Compiler:last-imported
val stack_safety_margin: int
(** Size in words of the safety margin between the bottom of
    the stack and the stack pointer. This margin can be used by
    intermediate computations of some instructions, or the event
    handler. *)

val native_compiler: bool
(** Whether the native compiler is available or not

    @since 5.1 *)

val architecture: string
(** Name of processor type for the native-code compiler *)

val model: string
(** Name of processor submodel for the native-code compiler *)

val system: string
(** Name of operating system for the native-code compiler *)

val target_os_type: string
(** Operating system targetted by the native-code compiler. One of
-  ["Unix"] (for all Unix versions, including Linux and macOS),
-  ["Win32"] (for MS-Windows, OCaml compiled with MSVC++ or MinGW-w64),
-  ["Cygwin"] (for MS-Windows, OCaml compiled with Cygwin). *)

val asm: string
(** The assembler (and flags) to use for assembling
    ocamlopt-generated code. *)

val asm_cfi_supported: bool
(** Whether assembler understands CFI directives *)

val asm_file0_supported: bool
(** Whether the assembler accepts the DWARF-5 [.file 0] and [.loc 0]
    directives, which define the line table's primary source file entry *)

val asm_size_type_directives: bool
(** Whether the [.size] and [.type] assembler directives can be used

    @since 5.4 *)

val with_frame_pointers : bool
(** Whether assembler should maintain frame pointers *)

val with_address_sanitizer : bool
(** Whether code generation should expose native OCaml operations to
    AddressSanitizer. *)
=======
val stack_safety_margin: int
(** Size in words of the safety margin between the bottom of
    the stack and the stack pointer. This margin can be used by
    intermediate computations of some instructions, or the event
    handler. *)

val native_compiler: bool
(** Whether the native compiler is available or not

    @since 5.1 *)

val architecture: string
(** Name of processor type for the native-code compiler *)

val model: string
(** Name of processor submodel for the native-code compiler *)

val system: string
(** Name of operating system for the native-code compiler *)

val target_os_type: string
(** Operating system targetted by the native-code compiler. One of
-  ["Unix"] (for all Unix versions, including Linux and macOS),
-  ["Win32"] (for MS-Windows, OCaml compiled with MSVC++ or MinGW-w64),
-  ["Cygwin"] (for MS-Windows, OCaml compiled with Cygwin). *)

val asm: string
(** The assembler (and flags) to use for assembling
    ocamlopt-generated code. *)

val asm_cfi_supported: bool
(** Whether assembler understands CFI directives *)

val asm_file0_supported: bool
(** Whether the assembler accepts the DWARF-5 [.file 0] and [.loc 0]
    directives, which define the line table's primary source file entry *)

val asm_size_type_directives: bool
(** Whether the [.size] and [.type] assembler directives can be used

    @since 5.4 *)

val asm_dwarf_version : int option
(** The DWARF Version emitted by the {!asm} command. At present this is only
    implemented for the clang internal assembler, all other assemblers have
    [None].

    @since 5.5 *)

val with_frame_pointers : bool
(** Whether assembler should maintain frame pointers *)

val with_address_sanitizer : bool
(** Whether code generation should expose native OCaml operations to
    AddressSanitizer. *)
>>>>>>> Compiler:HEAD

val default_keyword_edition : (int * int) option * string list

type name_mangling_scheme =
  | Flat
  | Structured

exception Invalid_name_mangling_scheme of string

val name_mangling_scheme : name_mangling_scheme
<<<<<<< Merlin:tt
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

val no_stack_checks : bool
(** [true] if stack checks are disabled. *)

val multidomain : bool
(** Whether creating multiple domains is allowed.
    Requires stack checks and poll insertion. *)

val tsan : bool
(** Whether ThreadSanitizer instrumentation is enabled *)

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
