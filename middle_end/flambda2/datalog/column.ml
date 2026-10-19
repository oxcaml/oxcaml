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

[@@@ocaml.flambda_o4]

open Heterogenous_list

module Int = struct
  include Numbers.Int
  module Tree = Patricia_tree.Make (Numbers.Int)
  module Map = Tree.Map
  module Set = Tree.Set
end

type (_, _, _) repr =
  | Patricia_tree_repr : ('a Patricia_tree.map, int, 'a) repr

type ('t, 'k, 'v) id =
  { name : string;
    print_key : Format.formatter -> 'k -> unit;
    repr : ('t, 'k, 'v) repr
  }

let singleton : type t k v. (t, k, v) id -> k -> v -> t =
 fun { repr; _ } key value ->
  let Patricia_tree_repr = repr in
  Int.Map.singleton key value

let equal_key : type t k v. (t, k, v) id -> k -> k -> bool =
 fun { repr = Patricia_tree_repr; _ } -> Int.equal

let compare_key : type t k v. (t, k, v) id -> k -> k -> int =
 fun { repr = Patricia_tree_repr; _ } -> Int.compare

let print_key : type t k v. (t, k, v) id -> Format.formatter -> k -> unit =
 fun { print_key; _ } -> print_key

type (_, _, _) hlist =
  | [] : ('v, nil, 'v) hlist
  | ( :: ) : ('t, 'k, 's) id * ('s, 'ks, 'v) hlist -> ('t, 'k -> 'ks, 'v) hlist

let rec print_keys : type t k v.
    (t, k, v) hlist -> Format.formatter -> k Constant.hlist -> unit =
 fun columns ppf keys ->
  match columns, keys with
  | [], [] -> ()
  | [column], [key] -> print_key column ppf key
  | column :: (_ :: _ as columns), key :: keys ->
    Format.fprintf ppf "%a,@ %a" (print_key column) key (print_keys columns)
      keys

let rec compare_keys : type t k v.
    (t, k, v) hlist -> k Constant.hlist -> k Constant.hlist -> int =
 fun columns xs ys ->
  match columns, xs, ys with
  | [], [], [] -> 0
  | column :: columns, x :: xs, y :: ys ->
    let c = compare_key column x y in
    if c <> 0 then c else compare_keys columns xs ys

let rec singleton_hlist : type t k v.
    (t, k, v) hlist -> k Constant.hlist -> v -> t =
 fun columns args value ->
  match args, columns with
  | [], [] -> value
  | arg :: args, column :: columns ->
    singleton column arg (singleton_hlist columns args value)

let union_total : type t k v. (t, k, v) id -> (v -> v -> v) -> t -> t -> t =
 fun { repr; _ } f t1 t2 ->
  let Patricia_tree_repr = repr in
  Int.Map.union_total (fun _ v1 v2 -> f v1 v2) t1 t2
[@@inline]

let union_total_hlist (type v) columns (f : v -> v -> v) t1 t2 =
  (* Cautious changing this function -- it is used in hot loops in the datalog
     interpreter and its performance is very sensitive. It is written in such a
     way as to minimize allocations and wrappers, while allowing specialization
     over specific instances of [f] (important in the common case where [v] is
     [unit]). *)
  let rec union_total_hlist : type t k. (t, k, v) hlist -> t -> t -> t =
   fun columns t1 t2 ->
    match columns with
    | [] -> (f [@inlined hint]) t1 t2
    | column :: columns -> union_total column (union_total_hlist columns) t1 t2
  in
  union_total_hlist columns t1 t2
[@@inline]

let diff_or_null : type t k v.
    (t, k, v) id -> (v -> v -> v Or_null.t) -> t -> t -> t Or_null.t =
 fun { repr; _ } f t1 t2 ->
  let Patricia_tree_repr = repr in
  let t =
    Int.Map.diff_sharing
      (fun _ v1 v2 ->
        match f v1 v2 with Null -> None | This datum -> Some datum)
      t1 t2
  in
  (* Never build empty tries! *)
  if Int.Map.is_empty t then Or_null.null else Or_null.this t
[@@inline]

let diff_or_null_hlist (type v) columns (f : v -> v -> v Or_null.t) t1 t2 =
  (* Cautious changing this function -- see [union_total_hlist] *)
  let rec diff_or_null_hlist : type t k.
      (t, k, v) hlist -> t -> t -> t Or_null.t =
   fun columns t1 t2 ->
    match columns with
    | [] -> (f [@inlined hint]) t1 t2
    | column :: columns ->
      diff_or_null column (diff_or_null_hlist columns) t1 t2
  in
  diff_or_null_hlist columns t1 t2
[@@inline]

let iter : type t k v. (t, k, v) id -> (k -> v -> unit) -> t -> unit =
 fun { repr; _ } f t ->
  let Patricia_tree_repr = repr in
  Int.Map.iter f t

let rec iter_hlist : type t k v.
    (t, k, v) hlist -> (k Constant.hlist -> v -> unit) -> t -> unit =
 fun columns f t ->
  match columns with
  | [] -> f [] t
  | column :: columns ->
    iter column (fun k s -> iter_hlist columns (fun ks v -> f (k :: ks) v) s) t

let fold : type t k v a. (t, k, v) id -> (k -> v -> a -> a) -> t -> a -> a =
 fun { repr; _ } ->
  let Patricia_tree_repr = repr in
  Int.Map.fold

let rec fold_hlist : type t k v a.
    (t, k, v) hlist -> (k Constant.hlist -> v -> a -> a) -> t -> a -> a =
 fun columns f t acc ->
  match columns with
  | [] -> f [] t acc
  | column :: columns ->
    fold column
      (fun k s acc ->
        fold_hlist columns (fun ks v acc -> f (k :: ks) v acc) s acc)
      t acc

let empty : type t k v. (t, k, v) id -> t =
 fun { repr; _ } ->
  let Patricia_tree_repr = repr in
  Int.Map.empty

let is_empty : type t k v. (t, k, v) id -> t -> bool =
 fun { repr; _ } ->
  let Patricia_tree_repr = repr in
  Int.Map.is_empty

let is_empty_hlist : type t k v. (t, k, v) hlist -> t -> bool = function
  | [] -> fun _ -> false
  | column :: _ -> is_empty column

let find_or_null : type t k v. (t, k, v) id -> k -> t -> v Or_null.t =
 fun { repr; _ } ->
  let Patricia_tree_repr = repr in
  Int.Map.find_or_null

let rec find_or_null_hlist : type t k v.
    (t, k, v) hlist -> k Constant.hlist -> t -> v Or_null.t =
 fun columns ks t ->
  match ks, columns with
  | [], [] -> Or_null.this t
  | k :: ks, column :: columns -> (
    match find_or_null column k t with
    | Null -> Or_null.null
    | This s -> find_or_null_hlist columns ks s)

let union_total_shared : type t k v.
    (t, k, v) id -> (v -> v -> v) -> t -> t -> t =
 fun { repr; _ } f t1 t2 ->
  let Patricia_tree_repr = repr in
  Int.Map.union_total_shared (fun _ v1 v2 -> f v1 v2) t1 t2

let rec union_right_biased_hlist : type t k v. (t, k, v) hlist -> t -> t -> t =
 fun columns t1 t2 ->
  match columns with
  | [] -> t2
  | column :: columns ->
    union_total_shared column
      (fun s1 s2 -> union_right_biased_hlist columns s1 s2)
      t1 t2

let add_or_replace_hlist columns keys value t =
  union_right_biased_hlist columns t (singleton_hlist columns keys value)

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

  let datalog_column_id =
    { name = X.name; print_key = print; repr = Patricia_tree_repr }
end

module Iterator = struct
  include Leapfrog.Map (Int)

  let create : type s k v.
      (s, k, v) id ->
      s Channel.or_null_receiver ->
      v Channel.or_null_sender ->
      k t =
   fun { repr; _ } ->
    let Patricia_tree_repr = repr in
    create
end
