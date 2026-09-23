(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

(** A cohort is a set of definitions, spread across compilation units, that are
    all equivalent (typically instantiations of one layout-polymorphic template
    at the same arguments). The linker keeps a single member of each cohort, so
    every member must be emitted under the same weak symbol; a cohort id is the
    globally unique key from which that symbol is derived.

    A cohort id pairs the compilation unit that defined the template with a name
    that is deterministic given the template and its arguments. *)

type t

(** Toplevels set this to [false]: every phrase is a compilation unit within a
    single process, and the process-wide symbol tables identify symbols by
    linkage name, so a canonical name shared across units would clash. Nothing
    is lost since there is no link step to deduplicate. *)
val enabled : bool ref

val create : Compilation_unit.t -> string -> t

val compilation_unit : t -> Compilation_unit.t

val name : t -> string

val equal : t -> t -> bool

val compare : t -> t -> int

val hash : t -> int

val print : Format.formatter -> t -> unit

module Map : Map.S with type key = t

(** The linkage name of a member's code. *)
val code_linkage_name : t -> Linkage_name.t

(** The linkage name of a member's statically-allocated closure block. *)
val closure_linkage_name : t -> Linkage_name.t
