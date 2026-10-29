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
<<<<<<< Merlin:dallsopp/relocatable-oxcaml-reloaded
        (* The current version number of the system *)
||||||| Compiler:last-imported
(** The current version number of the system *)

val bindir: string
(** The directory containing the binary programs *)

val standard_library_default: string
(** The configured value for the directory containing the standard libraries

    @since 5.5 *)

val standard_library: string
(** The effective directory containing the standard libraries *)

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
>>>>>>> Compiler:HEAD

val as_debug_prefix_map_flag : string
(** The flag to use for assembler debug prefix map ("" if none) *)

<<<<<<< Merlin:dallsopp/relocatable-oxcaml-reloaded
||||||| Compiler:last-imported
val as_compress_debug_sections_flag : string
(** The flag to use for assembler debug section compression ("" if none) *)

val cc_compress_debug_sections_flag : string
(** The flag to use for C compiler debug section compression ("" if none) *)

val objcopy_compress_debug_sections_flag : string
(** The flag to use for objcopy debug section compression ("" if none) *)

val objcopy : string
(** The objcopy command (and flags) to use for split debug enabled by
    [Clflags.dwarf_fission]. *)

val bytecode_cflags : string
(** The flags ocamlc should pass to the C compiler *)

val bytecode_cppflags : string
(** The flags ocamlc should pass to the C preprocessor *)

val native_cflags : string
(** The flags ocamlopt should pass to the C compiler *)

val native_cppflags : string
(** The flags ocamlopt should pass to the C preprocessor *)

val bytecomp_c_libraries: string
(** The C libraries to link with custom runtimes *)

val native_c_libraries: string
(** The C libraries to link with native-code programs *)

val compression_c_libraries: string
(** The C libraries needed with -lcomprmarsh (should appear before
    {!native_c_libraries} in a call to the C compiler)

    @since 5.4 *)

val native_ldflags : string
(* Flags to pass to the system linker *)

val with_nonexecstack_note : bool
(** Whether an explicit ".note.GNU-stack" section is to be added to indicate
    the stack should not be executable

    @since 5.4 *)

val native_pack_linker: string
(** The linker to use for packaging (ocamlopt -pack) and for partial
    links (ocamlopt -output-obj). *)

val mkdll: string
(** The linker command line to build dynamic libraries. *)

val mkexe: string
(** The linker command line to build executables. *)

val mkmaindll: string
(** The linker command line to build main programs as dlls. *)

val default_rpath: string
(** Option to add a directory to be searched for libraries at runtime
    (used by ocamlmklib) *)

val mksharedlibrpath: string
(** Option to add a directory to be searched for shared libraries at runtime
    (used by ocamlmklib) *)

val ar: string
(** Name of the ar command, or "" if not needed  (MSVC) *)

=======
val as_compress_debug_sections_flag : string
(** The flag to use for assembler debug section compression ("" if none) *)

val cc_compress_debug_sections_flag : string
(** The flag to use for C compiler debug section compression ("" if none) *)

val objcopy_compress_debug_sections_flag : string
(** The flag to use for objcopy debug section compression ("" if none) *)

val objcopy : string
(** The objcopy command (and flags) to use for split debug enabled by
    [Clflags.dwarf_fission]. *)

val as_is_cc : bool
(** Whether the assembler is actually an assembler, or whether we are really
    assembling files via the C compiler

    @since 5.5 *)

val bytecode_cflags : string
(** The flags ocamlc should pass to the C compiler *)

val bytecode_cppflags : string
(** The flags ocamlc should pass to the C preprocessor *)

val native_cflags : string
(** The flags ocamlopt should pass to the C compiler *)

val native_cppflags : string
(** The flags ocamlopt should pass to the C preprocessor *)

val bytecomp_c_libraries: string
(** The C libraries to link with custom runtimes *)

val native_c_libraries: string
(** The C libraries to link with native-code programs *)

val compression_c_libraries: string
(** The C libraries needed with -lcomprmarsh (should appear before
    {!native_c_libraries} in a call to the C compiler)

    @since 5.4 *)

val native_ldflags : string
(* Flags to pass to the system linker *)

val with_nonexecstack_note : bool
(** Whether an explicit ".note.GNU-stack" section is to be added to indicate
    the stack should not be executable

    @since 5.4 *)

val native_pack_linker: string
(** The linker to use for packaging (ocamlopt -pack) and for partial
    links (ocamlopt -output-obj). *)

val mkdll: string
(** The linker command line to build dynamic libraries. *)

val mkexe: string
(** The linker command line to build executables. *)

val mkmaindll: string
(** The linker command line to build main programs as dlls. *)

val default_rpath: string
(** Option to add a directory to be searched for libraries at runtime
    (used by ocamlmklib) *)

val mksharedlibrpath: string
(** Option to add a directory to be searched for shared libraries at runtime
    (used by ocamlmklib) *)

val ar: string
(** Name of the ar command, or "" if not needed  (MSVC) *)

>>>>>>> Compiler:HEAD
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
<<<<<<< Merlin:dallsopp/relocatable-oxcaml-reloaded
        (* Biggest tag that can be stored in the header of a regular block. *)
||||||| Compiler:last-imported
(** Biggest tag that can be stored in the header of a regular block. *)

val lazy_tag : int
(** Normally the same as Obj.lazy_tag.  Separate definition because
    of technical reasons for bootstrapping. *)

val max_young_wosize: int
(** Maximal size of arrays that are directly allocated in the
    minor heap *)

val stack_threshold: int
(** Size in words of safe area at bottom of VM stack,
    see runtime/caml/config.h *)

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
=======
(** Biggest tag that can be stored in the header of a regular block. *)

val lazy_tag : int
(** Normally the same as Obj.lazy_tag.  Separate definition because
    of technical reasons for bootstrapping. *)

val max_young_wosize: int
(** Maximal size of arrays that are directly allocated in the
    minor heap *)

val stack_threshold: int
(** Size in words of safe area at bottom of VM stack,
    see runtime/caml/config.h *)

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
-  ["Cygwin"] (for MS-Windows, OCaml compiled with Cygwin).

    @since 5.4 *)

val target_unix: bool
(** True if [target_os_type = "Unix"]

    @since 5.5 *)

val target_win32: bool
(** True if [target_os_type = "Win32"]

    @since 5.5 *)

val target_cygwin: bool
(** True if [target_os_type = "Cygwin"]

    @since 5.5 *)

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
>>>>>>> Compiler:HEAD

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
