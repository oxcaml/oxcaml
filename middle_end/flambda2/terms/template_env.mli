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

(** The definition of a layout-polymorphic or static template, which binds the
    environment block [env] that the definition creates, unchanged.
    [template_key] names the template: it is a symbol, but has no value.
    [instantiations] are the code (without closures) of this unit's
    instantiations of the template, which only ever receive environments created
    by this definition.

    Simplify records the type of the environment here, as the type of
    [template_key], simplifies the code of the instantiations using it, and
    replaces the definition by [env]. The type describes the environments
    created by this definition only, so Simplify must simplify the definition
    once: it must never copy it before simplifying it (see
    [Template_definitions]). In classic mode, closure conversion doesn't create
    template definitions. *)
type t

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val create :
  template_key:Symbol.t -> instantiations:Code_id.t list -> env:Simple.t -> t

val template_key : t -> Symbol.t

val instantiations : t -> Code_id.t list

val env : t -> Simple.t

val print : Format.formatter -> t -> unit
