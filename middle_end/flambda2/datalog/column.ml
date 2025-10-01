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

type (_, _, _) repr =
  | Patricia_tree_repr : ('a Patricia_tree.map, int, 'a) repr

type ('t, 'k, 'v) id =
  { name : string;
    value_repr : 'k Value.repr;
    repr : ('t, 'k, 'v) repr
  }

let equal_key { value_repr; _ } = Value.equal_repr value_repr

let print_key { value_repr; _ } = Value.print_repr value_repr

let value_repr { value_repr; _ } = value_repr

type (_, _, _) hlist =
  | [] : ('v, nil, 'v) hlist
  | ( :: ) : ('t, 'k, 's) id * ('s, 'ks, 'v) hlist -> ('t, 'k -> 'ks, 'v) hlist

let rec print_keys :
    type t k v. (t, k, v) hlist -> Format.formatter -> k Constant.hlist -> unit
    =
 fun columns ppf keys ->
  match columns, keys with
  | [], [] -> ()
  | [column], [key] -> print_key column ppf key
  | column :: (_ :: _ as columns), key :: keys ->
    Format.fprintf ppf "%a,@ %a" (print_key column) key (print_keys columns)
      keys

(* We could expose the fact that we do not support relations without arguments
   in the types, but a runtime error here allows us to give a better error
   message. Plus, we might support constant relations (represented as an option)
   in the future. *)
let rec is_trie : type t k v. (t, k, v) hlist -> (t, k, v) Trie.is_trie =
  function
  | [] -> Misc.fatal_error "Cannot create relation with no arguments"
  | [{ repr = Patricia_tree_repr; _ }] -> Trie.patricia_tree_is_trie
  | { repr = Patricia_tree_repr; _ } :: (_ :: _ as columns) ->
    Trie.patricia_tree_of_trie (is_trie columns)

module Make (X : sig
  val name : string

  val print : Format.formatter -> int -> unit
end) =
struct
  type t = int

  let print = X.print

  module Tree = Patricia_tree.Make (X)
  module Set = Tree.Set
  module Map = Tree.Map

  let value_repr = Value.int_repr ~print

  let datalog_column_id =
    { name = X.name; value_repr; repr = Patricia_tree_repr }
end
