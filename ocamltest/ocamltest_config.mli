(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Interface for ocamltest's configuration module *)

(** Architecture for the native compiler *)
val arch : string

(** Whether AFL support has been enabled in the compiler *)
val afl_instrument : bool

(** Path to the assembler *)
val asm : string

(** Command to use to invoke the C preprocessor *)
val cpp : string

(** Flags to pass to the C preprocessor *)
val cppflags : string

(** Command to use to invoke the C compiler *)
val cc : string

(** Flags to pass to the C compiler *)
val cflags : string

(** Type of C compiler (msvc, cc, etc.) *)
val ccomptype : string

(** Path to the diff tool *)
val diff : string

(** Flags to pass to the diff tool *)
val diff_flags : string

(** [true] if shared libraries are supported, [false] otherwise *)
val shared_libraries : bool

(** [Some true] for unix, [Some false] for win32unix, or [None] if neither is
    built. *)
val libunix : bool option

(** Indicates whether systhreads is available. *)
val systhreads : bool

(** Indicates where the systhreads library is installed under otherlibs/. *)
val systhreads_path : string

(** Indicates whether str is available. *)
val str : bool

(** Extension of object files *)
val objext : string

(** Extension of library files *)
val libext : string

(** Extension of assembly files *)
val asmext : string

(** The content of the SYSTEM Make variable *)
val system : string

(** Flags passed by default to ocamlc.byte and ocamlc.opt *)
val ocamlc_default_flags : string

(** Flags passed by default to ocamlopt.byte and ocamlopt.opt *)
val ocamlopt_default_flags : string

(** The absolute path of the directory containing the sources of OCaml *)
val ocamlsrcdir : string

(** Whether flambda has been enabled at configure time *)
val flambda : bool

(** Whether flambda2 has been enabled at configure time *)
val flambda2 : bool

val flat_float_array : bool
(* Whether the compiler was configured with --enable-flat-float-array *)

(** Whether ocamldoc has been enabled at configure time *)
val ocamldoc : bool

(** Whether ocamldebug has been enabled at configure time *)
val ocamldebug : bool

(** Whether the native compiler has been enabled at configure time *)
val native_compiler : bool

(** Whether support for native dynlink is available or not *)
val native_dynlink : bool

(** Flags to use when compiling a C object for a shared library *)
val shared_library_cflags : string

(** Extension of shared object files *)
val sharedobjext : string

(** Path of the CSharp compiler, empty if not available *)
val csc : string

(** Flags for the CSharp compiler *)
val csc_flags : string

(** Extension of executable files *)
val exe : string

val mkdll : string

val mkexe : string

val bytecc_libs : string

val nativecc_libs : string

val windows_unicode : bool

(** Whether the compiler was configured to generate
    each function in a separate section *)
val function_sections : bool

(** Whether the instrumented runtime is available *)
val instrumented_runtime : bool

(** Whether the target supports tracing probes *)
val probes : bool

(** Whether stack allocation is enabled *)
val stack_allocation : bool

(** Whether poll insertion is enabled *)
val poll_insertion : bool

(** Whether frame-pointers have been enabled at configure time *)
val frame_pointers : bool

(** Whether the runtime system supports naked pointers outside the heap *)
val naked_pointers : bool

(** Whether ThreadSanitizer support has been enabled at configure time *)
val tsan : bool
