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

(** From Lambda to assembly code *)

(** The type of converters straight from Lambda to Cmm. This is how Flambda 2
    operates. *)
type direct_to_cmm =
  ppf_dump:Format.formatter ->
  prefixname:string ->
  Lambda.program ->
  Cmm.phrase list

(** The one true way to get from Lambda to Cmm. *)
type pipeline = Direct_to_cmm of direct_to_cmm

(** Compile an implementation from Lambda using the given middle end. *)
val compile_implementation :
  (module Compiler_owee.Unix_intf.S) ->
  ?toplevel:(string -> bool) ->
  pipeline:pipeline ->
  sourcefile:string option ->
  prefixname:string ->
  ppf_dump:Format.formatter ->
  Lambda.program ->
  unit

(** [compile_implementation_linear] reads Linear IR from [progname] file
    produced by previous compilation stages (for example, using "ocamlopt
    -save-ir-after" and "ocamlfdo opt") and continues compilation from [Emit].

    Correctness: carefully consider any use of [Config], [Clflags],
    [Oxcaml_flags] and shared variables during or after [Emit]. A mismatch
    between between their values in different compilation stages might lead to a
    miscompilation or compilation failures during
    [compile_implementation_linear]. Mismatches can also be due to marshaling of
    Linear IR (for example, if physical equality is used).

    In particular, compiler configuration settings and compilation flags used by
    [ocamlopt] must match the ones used by [ocamlfdo]. Currently, [ocamlfdo]
    uses compiler-libs, so it must be compiled with the same configuration as
    [ocamlopt] from stages 1 and 2. For example, builds with frame pointers
    enabled require a compatible [ocamlfdo]. There is currently no way to pass
    compilation flags to [ocamlfdo] so transformations perfomed by [ocamlfdo]
    must not depend on such compilation flags. For example, SIMD is disabled in
    [ocamlfdo].

    Note that it is not safe to call functions that access variables whose
    values depend on previous compilation stages. For example, calling
    [Reg.create] may return a register that clashes with existing ones, because
    of the shared stamp counter in [Reg] that is not recorded in Linear IR
    files. *)
val compile_implementation_linear :
  (module Compiler_owee.Unix_intf.S) ->
  string ->
  progname:string ->
  ppf_dump:Format.formatter ->
  unit

val compile_phrase : ppf_dump:Format.formatter -> Cmm.phrase -> unit

(** Emits one word of data that refers to [symbol], for a symbol that exists to
    make a link fail: the system linker, or the loader of a shared object,
    refuses the object file being emitted unless something defines the symbol.

    To have that effect under a linker that discards unreferenced sections or
    symbols, the word must be emitted right after [Emit.begin_assembly], before
    any other data: it then belongs to the data that the startup file refers to
    for every object file whose code it registers (see
    [Asmlink.make_startup_file]). Nothing is emitted for the LLVM backend, whose
    object files are linked with those of the standard library, which have such
    references. *)
val emit_link_time_reference : Cmm.symbol -> unit

(** Defines [symbol] as one byte of data, for the references that
    [emit_link_time_reference] emits. With [~hidden:true] the definition is not
    exported from the shared object being linked, whatever assembles the file
    (the system assembler, the internal one or the LLVM backend): it only serves
    the object files of that shared object. *)
val emit_link_time_definition : hidden:bool -> Cmm.symbol -> unit

(** For the startup file of an executable or shared object that must not contain
    an object file with a reference to [symbol] ([emit_link_time_reference]):
    makes such a reference one that only a definition inside the same executable
    or shared object can satisfy ([Emitaux.emit_hidden_weak_reference]). Without
    it, the definition of another shared object of the process, or of the
    program that loads a plugin, satisfies the reference. The startup file must
    come before the other files of the link, as it does in the links that the
    compiler runs: not every linker goes back on a reference that it has already
    resolved in a shared object. Nothing is emitted for the LLVM backend, for an
    object file that the internal assembler writes, nor where the object format
    cannot say this (Mach-O). *)
val emit_link_time_exclusion : Cmm.symbol -> unit

(** Emits a reference ([emit_link_time_reference]) to the link-time guard of the
    codegen variant selected by the axis flags
    ([Codegen_variant.runtime_guard_symbol]): the symbol that the runtimes built
    for the same frame-pointer, stack-check and AddressSanitizer settings
    define, and no other runtime.

    Every object file of generated code must contain this reference, so that the
    system linker rejects a runtime, or generated code such as an archive of
    cached generic functions, built for other settings. The functions of this
    module that compile an implementation emit it; code that calls
    [Emit.begin_assembly] itself (the startup file, the cached generic
    functions) calls this function after it. *)
val emit_codegen_variant_guard_reference : unit -> unit

(** Emits the reference of the startup file, as the word
    [caml_startup_codegen_variant_guard]. The runtime that starts the program
    checks that this word holds the address of its own guard: the linker cannot
    refuse a program linked with the shared runtimes of two variants, of which
    one defines the guard and the other starts the program. *)
val emit_codegen_variant_guard_of_startup : unit -> unit

(** Emits a definition of the same symbol that is not exported from the shared
    object being linked, for the startup file of a plugin: the units of the
    plugin are then tied to the settings the plugin was linked for, while
    loading it only requires the symbols it shares with the host. Comparing
    those settings with the host's is left to the loader (see [Dynlink]), which
    has to open the plugin before it can read them. *)
val emit_codegen_variant_guard_definition : unit -> unit

(** Emits a reference to [caml_code_not_fully_polled] if the current unit is
    compiled without polls: the symbol that the startup code of a program
    defines when the program is not fully polled, and that of a plugin when one
    of its units has no polls.

    The polled bit of a unit is in its [.cmx] file, in its archive or in the
    header of its plugin, and its code is in another file. With this reference
    in every object file of OCaml code without polls, the linker refuses such an
    object file in a program whose startup code says that every unit has polls,
    however the object file came to be linked: beside a [.cmx] file of another
    compilation, from another archive than the one whose unit info was read, or
    given to the linker as it is. The dynamic linker does the same for a plugin
    that a fully polled program loads, when no definition is visible to the
    plugin. The functions of this module that compile an implementation emit it,
    except for the phrases of a toplevel and for the LLVM backend. *)
val emit_unpolled_code_reference : unit -> unit

(** Emits the definition of [caml_code_not_fully_polled], for the startup code
    of an image that has, by its unit infos, a unit without polls. That of a
    program is [exported], an ordinary global symbol. That of a plugin is not
    exported from the plugin, like its definition of the link-time guard: it
    serves the units of the plugin, and must not vouch for the object files of a
    plugin loaded after it. *)
val emit_unpolled_code_definition : exported:bool -> unit

(** For the startup code of an image that has units and, by its unit infos, none
    without polls: an object file without polls in that image then finds no
    definition of [caml_code_not_fully_polled], whatever the other images of the
    process define ([emit_link_time_exclusion]). So the link of a program that
    would be marked fully polled fails, and so does the link of a plugin whose
    header says that every unit has polls. *)
val emit_unpolled_code_exclusion : unit -> unit

type error =
  | Assembler_error of string
  | Binary_emitter_mismatch of string
  | Mismatched_for_pack of Compilation_unit.Prefix.t
  | Asm_generation of string * Emitaux.error

exception Error of error

val report_error : error Format_doc.format_printer

val report_error_doc : error Format_doc.printer

val compile_unit :
  (module Compiler_owee.Unix_intf.S) ->
  output_prefix:string ->
  asm_filename:string ->
  keep_asm:bool ->
  obj_filename:string ->
  may_reduce_heap:bool ->
  ppf_dump:Format.formatter ->
  (unit -> unit) ->
  unit
