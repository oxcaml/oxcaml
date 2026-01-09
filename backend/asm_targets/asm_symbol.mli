(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2014-2022 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)

(* Returns true if the argument is a character that should be escaped *)
val should_be_escaped : char -> bool

(** Symbol visibility for linking. [Global] symbols are exported and can be
    referenced from other compilation units. [Local] symbols are file-scope and
    can only be referenced within the same file. *)
type visibility =
  | Global
  | Local

(** Comparison and hashing of symbols is based on their encoded form (as
    returned by [encode]), so two symbols that produce the same assembly output
    are considered equal regardless of how they were constructed. *)
include Identifiable.S

(** [create] creates a new symbol with the given visibility. *)
val create : visibility:visibility -> string -> t

(** [create_global] creates a global symbol. Shorthand for
    [create ~visibility:Global]. *)
val create_global : string -> t

(** [create_local] creates a local symbol. Shorthand for
    [create ~visibility:Local]. *)
val create_local : string -> t

(** [create_without_encoding] creates a symbol from an already-encoded string.
    Use this when you have a string that already includes the symbol prefix
    (e.g., "_camlFoo" on macOS). The string will be used as-is without further
    encoding. *)
val create_without_encoding : visibility:visibility -> string -> t

val encode : t -> string

val to_raw_string : t -> string

val visibility : t -> visibility

val is_global : t -> bool

val is_local : t -> bool

(** We predefine several non-user generated symbols. *)
module Predef : sig
  val caml_call_gc : t

  val caml_call_gc_sse : t

  val caml_call_gc_avx : t

  val caml_call_gc_avx512 : t

  val caml_c_call : t

  val caml_allocN : t

  val caml_allocN_sse : t

  val caml_allocN_avx : t

  val caml_allocN_avx512 : t

  val caml_alloc1 : t

  val caml_alloc1_sse : t

  val caml_alloc1_avx : t

  val caml_alloc1_avx512 : t

  val caml_alloc2 : t

  val caml_alloc2_sse : t

  val caml_alloc2_avx : t

  val caml_alloc2_avx512 : t

  val caml_alloc3 : t

  val caml_alloc3_sse : t

  val caml_alloc3_avx : t

  val caml_alloc3_avx512 : t

  val caml_ml_array_bound_error : t

  val caml_ml_array_align_error : t

  val caml_raise_exn : t

  val caml_negf_mask : t

  val caml_absf_mask : t

  val caml_negf32_mask : t

  val caml_absf32_mask : t

  val stapsdt_base : t

  val caml_probes_semaphore : name:string -> t
end
