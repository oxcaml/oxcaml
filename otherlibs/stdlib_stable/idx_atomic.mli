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

(** Atomic indices into blocks. *)

(** An alias for the type of atomic indices into blocks. *)
type ('a : value_or_null, 'b : value_or_null) t : bits64 mod everything =
  ('a, 'b) idx_atomic

(** [get a i] gets [a] at index [i] atomically.

    It can take [a] locally and return its result globally because atomic
    indices can only be created to elements with the [global] modality. *)
external get :
  ('a : value_or_null) ('b : value_or_null).
  'a @ local -> ('a, 'b) t -> 'b = "%atomic_load_idx"

(** [set a i v] sets [a] at index [i] to [v] atomically.

    It can take [a] locally and [v] globally because atomic indices can only be
    created to elements with the [global] modality. *)
external set :
  ('a : value_or_null) ('b : value_or_null).
  ('a[@local_opt]) -> ('a, 'b) t -> 'b -> unit = "%atomic_set_idx"

(** [exchange a i v] sets [a] at index [i] to [v] atomically and returns the
    previous value. *)
external exchange :
  ('a : value_or_null) ('b : value_or_null).
  ('a[@local_opt]) -> ('a, 'b) t -> 'b -> 'b = "%atomic_exchange_idx"

(** [compare_and_set a i seen v] sets [a] at index [i] to [v] only if the
    value of [a] at index [i] is physically equal to [seen]. The comparison
    and the set occur atomically.

    Returns [true] if the set occurred and [false] otherwise. *)
external compare_and_set :
  ('a : value_or_null) ('b : value_or_null).
  ('a[@local_opt]) -> ('a, 'b) t -> 'b -> 'b -> bool = "%atomic_cas_idx"

(** [compare_exchange a i seen v] sets [a] at index [i] to [v] only if the
    value of [a] at index [i] is physically equal to [seen]. The comparison
    and the set occur atomically.

    Returns the previous value of [a] at index [i], or the current
    (unchanged) value if the comparison failed. *)
external compare_exchange :
  ('a : value_or_null) ('b : value_or_null).
  ('a[@local_opt]) -> ('a, 'b) t -> 'b -> 'b -> 'b
  = "%atomic_compare_exchange_idx"

(** [fetch_and_add a i n] atomically increments [a] at index [i] by [n], and
    returns the current value (before the increment). *)
external fetch_and_add :
  ('a : value_or_null). 'a @ local -> ('a, int) t -> int -> int
  = "%atomic_fetch_add_idx"

(** [add a i n] atomically adds [n] onto [a] at index [i]. *)
external add :
  ('a : value_or_null). 'a @ local -> ('a, int) t -> int -> unit
  = "%atomic_add_idx"

(** [sub a i n] atomically subtracts [n] from [a] at index [i]. *)
external sub :
  ('a : value_or_null). 'a @ local -> ('a, int) t -> int -> unit
  = "%atomic_sub_idx"

(** [logand a i n] atomically bitwise-ands [n] onto [a] at index [i]. *)
external logand :
  ('a : value_or_null). 'a @ local -> ('a, int) t -> int -> unit
  = "%atomic_land_idx"

(** [logor a i n] atomically bitwise-ors [n] onto [a] at index [i]. *)
external logor :
  ('a : value_or_null). 'a @ local -> ('a, int) t -> int -> unit
  = "%atomic_lor_idx"

(** [logxor a i n] atomically bitwise-xors [n] onto [a] at index [i]. *)
external logxor :
  ('a : value_or_null). 'a @ local -> ('a, int) t -> int -> unit
  = "%atomic_lxor_idx"

(** [incr a i] atomically increments [a] at index [i] by [1]. *)
val incr : ('a : value_or_null). 'a @ local -> ('a, int) t -> unit

(** [decr a i] atomically decrements [a] at index [i] by [1]. *)
val decr : ('a : value_or_null). 'a @ local -> ('a, int) t -> unit
