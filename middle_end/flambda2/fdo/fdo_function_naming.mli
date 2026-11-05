(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
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

(** The FDO identity of a function (see [Fdo_counter_identity]), computed from
    its Lambda definition.

    A function is named by its scope path and a discriminator.

    Functions with no (stable) names (like anonymous functions, wrappers, or
    lazy bodies) instead use the enclosing path and the first few tokens
    (parameters, identifiers, primitives, and constants). For example, like:
    ["Foo.(fun)@x,acc,+"]. *)

(** What naming a function depends on besides its own definition: the path of
    the enclosing function and the discriminators already handed out in the
    compilation unit. *)
module Context : sig
  type t

  (** The context of a compilation unit's toplevel code. *)
  val toplevel : Compilation_unit.t -> t
end

(** The identity of a compilation unit's toplevel code, which counts as a
    function. *)
val toplevel_function : Compilation_unit.t -> Fdo_counter.Function.t

(** The identity of a function defined in the given context, and the context of
    the functions defined in its body. *)
val of_function :
  Context.t -> Lambda.lfunction -> Fdo_counter.Function.t * Context.t

(** The first [limit] (5 by default) distinct tokens of a function. Only
    parameter and body identifiers, primitives, and constants are considered, in
    translation order. *)
val leading_tokens :
  ?limit:int -> params:Ident.t list -> Lambda.lambda -> string list
