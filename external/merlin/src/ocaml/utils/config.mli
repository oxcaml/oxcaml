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
        (* The current version number of the system *)

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

<<<<<<< Merlin:mshinwell.dwarfsize.pr7-line-table-file-zero
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
