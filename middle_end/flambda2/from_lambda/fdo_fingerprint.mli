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

    The definition id names the function by its scope path and an occurrence
    number among the functions with that path, so it survives edits of the body.
    An anonymous function (or a wrapper, or a lazy body) has no name to be
    stable under, and an occurrence among its scope's anonymous functions would
    change whenever one is added before it, so its first few tokens (parameters,
    identifiers, primitives, constants) are part of its name instead:
    ["Foo.(fun)@x,acc,+"]. *)

type t

(** [fresh_discriminator ~path] counts the functions already named [path];
    [enclosing_path] names the enclosing function, for functions without scopes.
*)
val of_function :
  fresh_discriminator:(path:string -> int) ->
  enclosing_path:string ->
  Lambda.lfunction ->
  t

val definition_id : t -> Fdo_counter.Definition_id.t

(** The counters of the function's entry edge: its entry counter. *)
val entry_annotation : t -> Fdo_annotation.t

(** The scope path the definition id is derived from, used as [enclosing_path]
    for the functions defined in the body. *)
val path : t -> string

(** The first (up to five) distinct tokens of a function: its parameters, then,
    in translation order, the identifiers its body binds or mentions, the values
    of other compilation units it uses (unit name and offset), its primitives
    (with their tag or offset: "Pfield0", "Pmakeblock1", "int_add", "caml_hash")
    and its integer constants. Exposed for testing. *)
val leading_tokens : params:Ident.t list -> Lambda.lambda -> string list
