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

(** Identifiers for layout-polymorphism templates.

    A template id is allocated when slambda evaluation registers a template
    (see [Slambdaeval]); the owner+stamp combination is globally unique, at
    least within one linked unit. Template ids are also used to give stable,
    cross-compilation-unit identities to the value slots of the environment
    block associated with a template (see [Lambda.Pset_of_closures]). *)
type t

val create :
  owner:Compilation_unit.t option -> name:Slambdaident.t option -> t

val owner : t -> Compilation_unit.t option

(** Unique within [owner] (a dedicated counter, monotonically increasing per
    compilation process). *)
val stamp : t -> int

val print : Format_doc.formatter -> t -> unit

val equal : t -> t -> bool

val hash : t -> int

module Tbl : Hashtbl.S with type key = t
