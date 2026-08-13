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

val reserved_header_bits : int
val syntax_quotations : bool
(** Whether quote [<[_]>] and splice [$] syntax is enabled. *)

val default_keyword_edition : (int * int) option * string list

type name_mangling_scheme =
  | Flat
  | Structured

exception Invalid_name_mangling_scheme of string

val name_mangling_scheme : name_mangling_scheme
<<<<<<< Merlin:stedolan/thin-libbie
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

val runtime5 : bool
(** Always [true], Previously:[false] when using the
    OCaml 4.14 runtime. *)

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

val ar_thin_archives_flags: string
(** Flags to pass to ar instead of "rc" to build thin archives. *)

val runtime5 : bool
(** Always [true], Previously:[false] when using the
    OCaml 4.14 runtime. *)

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
