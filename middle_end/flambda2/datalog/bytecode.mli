(******************************************************************************
 *                                  OxCaml                                    *
 *                        Basile Clément, OCamlPro                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
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

open Datalog_imports

type bindings_ref

type bindings

val print_bindings : Format.formatter -> bindings -> unit

val get_bindings : bindings_ref -> bindings

module Make (Iterator : Leapfrog.Iterator) : sig
  type t

  val print : Format.formatter -> t -> unit

  val run : t -> unit

  type assembler

  (** [for_in var iterator body] repeats the [body] for each value provided by
      the [iterator].

      {b Note}: This creates a new scope that can be exited (shortcutting any
      later iterations) using [break]. *)
  val for_in :
    'a Value.repr with_name ->
    'a Iterator.t list with_names ->
    ('a Or_null_receiver.t -> assembler) ->
    assembler

  (** [break n] breaks out of the [n] innermost loops (created with [for_in]).

      {b Note}: [break 0] is a no-op. *)
  val break : int -> assembler

  (** [if_in key iterators body] executes body if [key] is in the intersection
      of the [iterators]. *)
  val if_in :
    'a Or_null_receiver.t with_name ->
    'a Iterator.t list with_names ->
    assembler ->
    assembler

  val if_not_in :
    ('t, 'k, _) Trie.is_trie ->
    't Or_null_receiver.t with_name ->
    'k Or_null_receiver.hlist with_names ->
    assembler ->
    assembler

  val if_not_equal :
    'v Value.repr ->
    'v Or_null_receiver.t with_name ->
    'v Or_null_receiver.t with_name ->
    assembler ->
    assembler

  (** [if_ fn args body] executes [body] if [fn] holds for the current values of
      [args] and does nothing otherwise. *)
  val if_ :
    ('a Constant.hlist -> bool) with_name ->
    'a Or_null_receiver.hlist with_names ->
    assembler ->
    assembler

  val call_with_bindings :
    (bindings_ref -> 'b Constant.hlist -> unit) with_name ->
    'b Or_null_receiver.hlist with_names ->
    assembler

  val ( ++ ) : assembler -> assembler -> assembler

  val list : assembler list -> assembler

  val assemble : assembler -> t
end
