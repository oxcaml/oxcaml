(******************************************************************************
 *                                  OxCaml                                    *
 *                       Basile Clément, OCamlPro                             *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 OCamlPro                                                *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

open Heterogenous_list

type ('t, 'k, 'v) id

type (_, _, _) hlist =
  | [] : ('v, nil, 'v) hlist
  | ( :: ) : ('t, 'k, 's) id * ('s, 'ks, 'v) hlist -> ('t, 'k -> 'ks, 'v) hlist

val value_repr : ('t, 'k, 'v) id -> 'k Value.repr

val equal_key : ('t, 'k, 'v) id -> 'k -> 'k -> bool

val print_key : ('t, 'k, 'v) id -> Format.formatter -> 'k -> unit

val print_keys :
  ('t, 'k, 'v) hlist -> Format.formatter -> 'k Constant.hlist -> unit

val compare_key : ('t, 'k, 'v) id -> 'k -> 'k -> int

val compare_keys :
  ('t, 'k, 'v) hlist -> 'k Constant.hlist -> 'k Constant.hlist -> int

val is_trie : ('t, 'k, 'v) hlist -> ('t, 'k, 'v) Trie.is_trie

module Make (_ : sig
  val name : string

  val print : Format.formatter -> int -> unit
end) : sig
  type t = int

  val print : Format.formatter -> t -> unit

  module Set : Container_types.Set with type elt = t

  module Map :
    Container_types.Map_plus_iterator with type key = t with module Set = Set

  val datalog_column_id : ('a Map.t, t, 'a) id
end
