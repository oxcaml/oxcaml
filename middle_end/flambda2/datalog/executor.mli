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

type t

val print : Format.formatter -> t -> unit

val run : t -> unit

type _ builder

val build : nil builder -> t

val break : int -> 'a builder

val for_in :
  'a Value.repr with_name ->
  'a Trie.Iterator.t list with_names ->
  ('a Channel.or_null_receiver -> ('a -> 'b) builder) ->
  'b builder

val if_in :
  'a Channel.or_null_receiver with_name ->
  'a Trie.Iterator.t list with_names ->
  'b builder ->
  'b builder

val unless :
  ('t, 'k, 'v) Trie.is_trie ->
  't Channel.or_null_receiver with_name ->
  'k Or_null_receiver.hlist with_names ->
  'a builder ->
  'a builder

val unless_eq :
  'a Value.repr ->
  'a Or_null_receiver.t with_name ->
  'a Or_null_receiver.t with_name ->
  'b builder ->
  'b builder

val filter :
  ('a Constant.hlist -> bool) ->
  'a Or_null_receiver.hlist with_names ->
  'b builder ->
  'b builder

type bindings_ref

val call :
  (bindings_ref -> 'a Constant.hlist -> unit) with_name ->
  'a Or_null_receiver.hlist with_names ->
  'b builder ->
  'b builder

type bindings

val print_bindings : Format.formatter -> bindings -> unit

val get_bindings : bindings_ref -> bindings
