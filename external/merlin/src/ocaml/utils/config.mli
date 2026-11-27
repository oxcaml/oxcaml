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
<<<<<<< Merlin:dallsopp/relocatable-oxcaml-1
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
