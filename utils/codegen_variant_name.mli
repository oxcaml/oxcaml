(******************************************************************************
 *                                  OxCaml                                    *
 *                        Jacob Van Buren, Jane Street                        *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** The codegen-variant naming scheme: one token per code-generation axis,
    a variant name being the '-'-joined, canonically sorted names of the
    enabled tokens, with the all-off variant spelled "base" (names are
    absolute, independent of the configure default).  Examples: "base",
    "fp", "poll", "fp-sc", "asan-fp".

    This module is dependency-free (no [Clflags]/[Config]/[Misc]): it is the
    single source of the name scheme and of the per-token flag tables, and
    is shared BY FILE COPY with build tools that cannot link compilerlibs
    (stdlib/dune_rules/gen.ml, ocamltest, otherlibs/dynlink).  The
    Makefile-side tables in Makefile.codegen-variants re-encode the flag
    tables and must be kept in sync.

    Token reservation rule: a token name never contains '-' or '_', and
    never equals a runtime-variant suffix string ("d", "i", "pic", "nnp");
    see codegen_variant_name.ml and the consistency test. *)

type token =
  | Address_sanitizer
  | Frame_pointers
  | Poll_insertion
  | Stack_checks

(** Every supported token, in canonical (sorted-name) order. *)
val all_tokens : token list

val token_name : token -> string

val token_of_name : string -> token option

val equal_token : token -> token -> bool

(** {2 Per-token flag tables} *)

(** The ocamlopt flag selecting the token's value ("-frame-pointers" /
    "-no-frame-pointers"). *)
val ocamlopt_flag : token -> enabled:bool -> string

(** The OCAMLPARAM fragment selecting the token's value
    ("frame-pointers=1"). *)
val ocamlparam : token -> enabled:bool -> string

(** The flag stating that a C or assembler compilation gives its codegen
    variant on the command line ([-DCAML_CODEGEN_VARIANT_EXPLICIT]): the
    headers of a variant's tree then leave the axis macros to the command
    line instead of defining them for that variant. *)
val explicit_variant_flag : string

(** The flags below turn the axis of a token on.  An axis is off when
    nothing is said: there are no flags for that. *)

(** The -D<macro>=1 flag defining the token's axis macro
    (WITH_FRAME_POINTERS, STACK_CHECKS_ENABLED, WITH_ADDRESS_SANITIZER) for
    a C compilation; empty for tokens without a runtime macro.  Only
    meaningful together with [explicit_variant_flag]. *)
val cc_macro_flags : token -> string list

(** The C-compiler flags that make the generated machine code itself have
    the token's axis (frame-pointer preservation, sanitizer
    instrumentation), as opposed to the macro seen by the preprocessor. *)
val cc_behavioral_flags : token -> string list

(** C-compiler flags for building runtime objects with the token's axis:
    [cc_macro_flags] followed by [cc_behavioral_flags]. *)
val cc_flags : token -> string list

(** Like [cc_flags], for assembly (.S) preprocessing: [cc_macro_flags]. *)
val aspp_flags : token -> string list

(** Whether the token changes the runtime: runtime archives are built per
    projection onto the runtime axes, and variants differing only in
    non-runtime tokens share archives. *)
val is_runtime_axis : token -> bool

(** {2 Names (sets of enabled tokens)} *)

type t

(** The all-off variant, named "base". *)
val base : t

(** Sorts and dedupes. *)
val of_tokens : token list -> t

(** In canonical order. *)
val tokens : t -> token list

val mem : token -> t -> bool

val equal : t -> t -> bool

val to_string : t -> string

(** Accepts canonical names only: "base", or known tokens, strictly sorted,
    '-'-joined. *)
val of_string : string -> t option

(** The name restricted to its runtime axes. *)
val runtime_projection : t -> t

(** The runtime-archive name suffix for [t]: "" when its runtime
    projection is [base] (the unsuffixed archives are those of base),
    otherwise "_" ^ the projection's name with '-' mapped to '_'
    ("_fp", "_fp_sc").  Must agree with the archive names emitted by
    stdlib/dune_rules/gen.ml. *)
val runtime_archive_suffix : t -> string

(** The link-time guard of [t]: the symbol
    "caml_runtime_codegen_variant_is_" ^ the name of [t]'s runtime
    projection with '-' mapped to '_' (caml_runtime_codegen_variant_is_base,
    caml_runtime_codegen_variant_is_fp_sc).  A runtime defines the guard of
    the axes that it was built for (runtime/codegen_variant_guard.c), and
    every object file that the native compiler generates for [t] refers to this
    one, so that the system linker refuses to combine code with a runtime,
    or with other generated code, built for other runtime axes. *)
val runtime_guard_symbol : t -> string

(** Absolute per-axis ocamlopt flags for the named variant: one flag per
    supported token, enabled or disabled per membership in [t]. *)
val ocamlopt_flags : t -> string list

(** Absolute per-axis OCAMLPARAM fragments, one per supported token. *)
val ocamlparams : t -> string list

(** The C-compiler flags for the named variant: [explicit_variant_flag]
    followed by the [cc_flags] of its tokens. *)
val variant_cc_flags : t -> string list

(** Like [variant_cc_flags], for assembly (.S) preprocessing. *)
val variant_aspp_flags : t -> string list
