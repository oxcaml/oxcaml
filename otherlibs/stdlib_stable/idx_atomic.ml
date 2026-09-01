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

type ('a : value_or_null, 'b : value_or_null) t : bits64 mod everything =
  ('a, 'b) idx_atomic

external get :
  ('a : value_or_null) ('b : value_or_null).
  'a @ local -> ('a, 'b) t -> 'b = "%atomic_load_idx"

external set :
  ('a : value_or_null) ('b : value_or_null).
  ('a[@local_opt]) -> ('a, 'b) t -> 'b -> unit = "%atomic_set_idx"

external exchange :
  ('a : value_or_null) ('b : value_or_null).
  ('a[@local_opt]) -> ('a, 'b) t -> 'b -> 'b
  = "%atomic_exchange_idx"

external compare_and_set :
  ('a : value_or_null) ('b : value_or_null).
  ('a[@local_opt]) -> ('a, 'b) t -> 'b -> 'b -> bool
  = "%atomic_cas_idx"

external compare_exchange :
  ('a : value_or_null) ('b : value_or_null).
  ('a[@local_opt]) -> ('a, 'b) t -> 'b -> 'b -> 'b
  = "%atomic_compare_exchange_idx"

external fetch_and_add :
  ('a : value_or_null). 'a @ local -> ('a, int) t -> int -> int
  = "%atomic_fetch_add_idx"

external add :
  ('a : value_or_null). 'a @ local -> ('a, int) t -> int -> unit
  = "%atomic_add_idx"

external sub :
  ('a : value_or_null). 'a @ local -> ('a, int) t -> int -> unit
  = "%atomic_sub_idx"

external logand :
  ('a : value_or_null). 'a @ local -> ('a, int) t -> int -> unit
  = "%atomic_land_idx"

external logor :
  ('a : value_or_null). 'a @ local -> ('a, int) t -> int -> unit
  = "%atomic_lor_idx"

external logxor :
  ('a : value_or_null). 'a @ local -> ('a, int) t -> int -> unit
  = "%atomic_lxor_idx"

let incr a i = add a i 1
let decr a i = sub a i 1
