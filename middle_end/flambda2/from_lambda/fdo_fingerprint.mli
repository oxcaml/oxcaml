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
    ["Foo.(fun)@x,acc,+"].

    The body hash tells apart function bodies whose differences are likely to
    change code layout decisions, which any change to what the program computes
    may: it covers the control-flow constructs (in the order the body's counters
    are numbered in), constants, switch keys, identifiers by name, primitives
    with the payloads that select what they compute, call sites with their
    inlining attributes, and the function attributes. It leaves out what varies
    between compilations of the same code (source positions, identifier stamps,
    debug uids, static handler numbers) and the layouts, modes, kinds and shapes
    of terms and primitives. Counters inside the body are only comparable
    between builds with the same body hash, which requires the same compiler
    version. *)

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

(** The FDO function id. *)
val function_ : t -> Fdo_counter.Function.t

val body_hash : t -> Fdo_counter.Body_hash.t

(** The counters of the function's entry edge. *)
val entry_annotation : t -> Fdo_annotation.t

(** The first [limit] (5 by default) distinct tokens of a function. Only
    parameter and body identifiers, primitives, and constants are considered, in
    translation order. *)
val leading_tokens :
  ?limit:int -> params:Ident.t list -> Lambda.lambda -> string list

(** The body hash of a function. Exposed for testing. *)
val hash_function : Lambda.lfunction -> Fdo_counter.Body_hash.t
