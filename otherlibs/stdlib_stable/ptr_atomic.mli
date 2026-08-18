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

(*
   Like [idx_atomic], [ptr_atomic] uses kind [any] for its element type.
   See the comment in [./idx_atomic.mli] for more details.
*)

(** An alias for the type of atomic pointers into blocks. *)
type ('a : value_or_null, 'b : any) t =
  #('a * ('a, 'b) idx_atomic)

(** [get p] gets the value pointed to by [p] atomically.

    It can take [p] locally and return its result globally because atomic
    indices can only be created to elements with the [global] modality. *)
external get :
  ('a : value_or_null) ('b : value_or_null).
  ('a, 'b) t @ local -> 'b = "%unsafe_atomic_load_ptr"

(** [set p v] sets the value pointed to by [p] to [v] atomically.

    It can take [p] locally and [v] globally because atomic indices can only
    be created to elements with the [global] modality. *)
external set :
  ('a : value_or_null) ('b : value_or_null).
  (('a, 'b) t[@local_opt]) -> 'b -> unit = "%unsafe_atomic_set_ptr"

(** [exchange p v] sets the value pointed to by [p] to [v] atomically and
    returns the previous value. *)
external exchange :
  ('a : value_or_null) ('b : value_or_null).
  (('a, 'b) t[@local_opt]) -> 'b -> 'b = "%unsafe_atomic_exchange_ptr"

(** [compare_and_set p seen v] sets the value pointed to by [p] to [v] only
    if the value pointed to by [p] is physically equal to [seen]. The
    comparison and the set occur atomically.

    Returns [true] if the set occurred and [false] otherwise. *)
external compare_and_set :
  ('a : value_or_null) ('b : value_or_null).
  (('a, 'b) t[@local_opt]) -> 'b -> 'b -> bool = "%unsafe_atomic_cas_ptr"

(** [compare_exchange p seen v] sets the value pointed to by [p] to [v] only
    if the value pointed to by [p] is physically equal to [seen]. The
    comparison and the set occur atomically.

    Returns the previous value pointed to by [p], or the current (unchanged)
    value if the comparison failed. *)
external compare_exchange :
  ('a : value_or_null) ('b : value_or_null).
  (('a, 'b) t[@local_opt]) -> 'b -> 'b -> 'b
  = "%unsafe_atomic_compare_exchange_ptr"

(** [fetch_and_add p n] atomically increments the value pointed to by [p] by
    [n], and returns the current value (before the increment). *)
external fetch_and_add :
  ('a : value_or_null). ('a, int) t @ local -> int -> int
  = "%unsafe_atomic_fetch_add_ptr"

(** [add p n] atomically adds [n] onto the value pointed to by [p]. *)
external add :
  ('a : value_or_null). ('a, int) t @ local -> int -> unit
  = "%unsafe_atomic_add_ptr"

(** [sub p n] atomically subtracts [n] from the value pointed to by [p]. *)
external sub :
  ('a : value_or_null). ('a, int) t @ local -> int -> unit
  = "%unsafe_atomic_sub_ptr"

(** [logand p n] atomically bitwise-ands [n] onto the value pointed to by
    [p]. *)
external logand :
  ('a : value_or_null). ('a, int) t @ local -> int -> unit
  = "%unsafe_atomic_land_ptr"

(** [logor p n] atomically bitwise-ors [n] onto the value pointed to by
    [p]. *)
external logor :
  ('a : value_or_null). ('a, int) t @ local -> int -> unit
  = "%unsafe_atomic_lor_ptr"

(** [logxor p n] atomically bitwise-xors [n] onto the value pointed to by
    [p]. *)
external logxor :
  ('a : value_or_null). ('a, int) t @ local -> int -> unit
  = "%unsafe_atomic_lxor_ptr"

(** [incr p] atomically increments the value pointed to by [p] by [1]. *)
val incr : ('a : value_or_null). ('a, int) t @ local -> unit

(** [decr p] atomically decrements the value pointed to by [p] by [1]. *)
val decr : ('a : value_or_null). ('a, int) t @ local -> unit
