(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Joe Kerrigan, Jane Street, New York                    *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.flambda_o3]

type ('a : value_or_null, 'b : value_or_null) t =
  #('a * ('a, 'b) idx_atomic)

external get :
  ('a : value_or_null) ('b : value_or_null).
  ('a, 'b) t @ local -> 'b = "%unsafe_atomic_load_ptr"

external set :
  ('a : value_or_null) ('b : value_or_null).
  (('a, 'b) t[@local_opt]) -> 'b -> unit = "%unsafe_atomic_set_ptr"

external exchange :
  ('a : value_or_null) ('b : value_or_null).
  (('a, 'b) t[@local_opt]) -> 'b -> 'b = "%unsafe_atomic_exchange_ptr"

external compare_and_set :
  ('a : value_or_null) ('b : value_or_null).
  (('a, 'b) t[@local_opt]) -> 'b -> 'b -> bool = "%unsafe_atomic_cas_ptr"

external compare_exchange :
  ('a : value_or_null) ('b : value_or_null).
  (('a, 'b) t[@local_opt]) -> 'b -> 'b -> 'b
  = "%unsafe_atomic_compare_exchange_ptr"

external fetch_and_add :
  ('a : value_or_null). ('a, int) t @ local -> int -> int
  = "%unsafe_atomic_fetch_add_ptr"

external add :
  ('a : value_or_null). ('a, int) t @ local -> int -> unit
  = "%unsafe_atomic_add_ptr"

external sub :
  ('a : value_or_null). ('a, int) t @ local -> int -> unit
  = "%unsafe_atomic_sub_ptr"

external logand :
  ('a : value_or_null). ('a, int) t @ local -> int -> unit
  = "%unsafe_atomic_land_ptr"

external logor :
  ('a : value_or_null). ('a, int) t @ local -> int -> unit
  = "%unsafe_atomic_lor_ptr"

external logxor :
  ('a : value_or_null). ('a, int) t @ local -> int -> unit
  = "%unsafe_atomic_lxor_ptr"

let incr p = add p 1
let decr p = sub p 1
