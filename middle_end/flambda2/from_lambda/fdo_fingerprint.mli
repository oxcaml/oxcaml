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

(** The FDO identity of a function, computed from its Lambda definition (see
    [Fdo_counter_identity]).

    The [definition_id] names the function by its scope path and a
    discriminator.

    Functions with no (stable) names (like anonymous functions, wrappers, or
    lazy bodies) instead use the enclosing path and the first few tokens
    (parameters, identifiers, primitives, and constants). For example, like:
    ["Foo.(fun)@x,acc,+"]. *)

(** What naming a function depends on besides its own definition. Used in
    [Lambda_to_flambda_env] while translating from Lambda. Contains the path of
    the enclosing function and the discriminators already handed out in the
    compilation unit. *)
module Naming_context : sig
  type t

  (** The context of a compilation unit's toplevel code. *)
  val toplevel : Compilation_unit.t -> t
end

type t

(** The identity of a function defined in the given context, and the context of
    the functions defined in its body. *)
val of_function : Naming_context.t -> Lambda.lfunction -> t * Naming_context.t

val definition_id : t -> Fdo_counter.Definition_id.t

(** The counters of the function's entry edge: its entry counter. *)
val entry_annotation : t -> Fdo_annotation.t

(** The first [limit] (5 by default) distinct tokens of a function. Only
    parameter and body identifiers, primitives, and constants are considered, in
    translation order. *)
val leading_tokens :
  ?limit:int -> params:Ident.t list -> Lambda.lambda -> string list
