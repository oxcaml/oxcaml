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

val equal_key : ('t, 'k, 'v) id -> 'k -> 'k -> bool

val print_key : ('t, 'k, 'v) id -> Format.formatter -> 'k -> unit

val print_keys :
  ('t, 'k, 'v) hlist -> Format.formatter -> 'k Constant.hlist -> unit

val compare_key : ('t, 'k, 'v) id -> 'k -> 'k -> int

val compare_keys :
  ('t, 'k, 'v) hlist -> 'k Constant.hlist -> 'k Constant.hlist -> int

val empty : ('t, 'k, 'v) id -> 't

val is_empty : ('t, 'k, 'v) id -> 't -> bool

val is_empty_hlist : ('t, 'k, 'v) hlist -> 't -> bool

val singleton : ('t, 'k, 'v) id -> 'k -> 'v -> 't

val singleton_hlist : ('t, 'k, 'v) hlist -> 'k Constant.hlist -> 'v -> 't

val union_total : ('t, 'k, 'v) id -> ('v -> 'v -> 'v) -> 't -> 't -> 't

val union_total_hlist : ('t, 'k, 'v) hlist -> ('v -> 'v -> 'v) -> 't -> 't -> 't

(* The returned [diff] is guaranteed to be non-empty if not null. *)
val diff_or_null :
  ('t, 'k, 'v) id -> ('v -> 'v -> 'v Or_null.t) -> 't -> 't -> 't Or_null.t

val diff_or_null_hlist :
  ('t, 'k, 'v) hlist -> ('v -> 'v -> 'v Or_null.t) -> 't -> 't -> 't Or_null.t

val add_or_replace_hlist :
  ('t, 'k, 'v) hlist -> 'k Constant.hlist -> 'v -> 't -> 't

val iter : ('t, 'k, 'v) id -> ('k -> 'v -> unit) -> 't -> unit

val iter_hlist :
  ('t, 'k, 'v) hlist -> ('k Constant.hlist -> 'v -> unit) -> 't -> unit

val fold : ('t, 'k, 'v) id -> ('k -> 'v -> 'a -> 'a) -> 't -> 'a -> 'a

val fold_hlist :
  ('t, 'k, 'v) hlist -> ('k Constant.hlist -> 'v -> 'a -> 'a) -> 't -> 'a -> 'a

val find_or_null : ('t, 'k, 'v) id -> 'k -> 't -> 'v Or_null.t

val find_or_null_hlist :
  ('t, 'k, 'v) hlist -> 'k Constant.hlist -> 't -> 'v Or_null.t

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

module Iterator : sig
  include Leapfrog.Iterator

  (** [create column name input output] creates a column iterator.

      The [input] reference is used to initialize the first iterator when [init]
      is called.

      The [output] reference is set to the corresponding value when [accept] is
      called on the last iterator. *)
  val create :
    ('s, 'k, 'v) id ->
    's Channel.or_null_receiver ->
    'v Channel.or_null_sender ->
    'k t
end
