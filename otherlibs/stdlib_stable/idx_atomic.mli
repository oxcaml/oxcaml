(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Ryan Tjoa, Jane Street, New York                    *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Atomic indices into blocks. *)

(** An alias for the type of atomic indices into blocks. The element type
    parameter is [any] in the alias to match the predef, but the externals
    below restrict [\'b] to [value] because the underlying primitives use
    atomic word-sized loads and stores. *)
type ('a : value_or_null, 'b : any) t : bits64 mod everything
  = ('a, 'b) idx_atomic

(** [get a i] uses the index [i] to atomically load the element of [a]. *)
external get
  : ('a : value_or_null) ('b : value).
  ('a[@local_opt]) -> ('a, 'b) idx_atomic -> 'b
  = "%get_idx_atomic"

(** Like {!get}, but allows the base to be [contended]. *)
external get_contended
  : ('a : value_or_null) ('b : value).
  'a @ contended local -> ('a, 'b) idx_atomic -> 'b @ contended
  = "%get_idx_atomic"

(** [set a i v] uses the index [i] to atomically store [v] into [a]. *)
external set
  : ('a : value_or_null) ('b : value).
  ('a[@local_opt]) -> ('a, 'b) idx_atomic -> 'b -> unit
  = "%set_idx_atomic"

(** [exchange a i v] atomically replaces the element of [a] at index [i] with
    [v] and returns the previous value. *)
external exchange
  : ('a : value_or_null) ('b : value).
  ('a[@local_opt]) -> ('a, 'b) idx_atomic -> 'b -> 'b
  = "%atomic_exchange_idx"

(** [compare_and_set a i old new] atomically sets the element of [a] at
    index [i] to [new] if it currently equals [old]. Returns [true] iff the
    swap succeeded. *)
external compare_and_set
  : ('a : value_or_null) ('b : value).
  ('a[@local_opt]) -> ('a, 'b) idx_atomic -> 'b -> 'b -> bool
  = "%atomic_cas_idx"

(** Like {!compare_and_set} but returns the previous value (whether or not
    the swap happened). *)
external compare_exchange
  : ('a : value_or_null) ('b : value).
  ('a[@local_opt]) -> ('a, 'b) idx_atomic -> 'b -> 'b -> 'b
  = "%atomic_compare_exchange_idx"

(** [fetch_and_add a i delta] atomically increments the element of [a] at
    index [i] by [delta] and returns the previous value. *)
external fetch_and_add
  : ('a : value_or_null).
  ('a[@local_opt]) -> ('a, int) idx_atomic -> int -> int
  = "%atomic_fetch_add_idx"

(** [add a i delta] atomically increments the element of [a] at index [i] by
    [delta]. *)
external add
  : ('a : value_or_null).
  ('a[@local_opt]) -> ('a, int) idx_atomic -> int -> unit
  = "%atomic_add_idx"

(** [sub a i delta] atomically decrements the element of [a] at index [i] by
    [delta]. *)
external sub
  : ('a : value_or_null).
  ('a[@local_opt]) -> ('a, int) idx_atomic -> int -> unit
  = "%atomic_sub_idx"

(** [logand a i mask] atomically bitwise-AND-assigns the element of [a] at
    index [i] with [mask]. *)
external logand
  : ('a : value_or_null).
  ('a[@local_opt]) -> ('a, int) idx_atomic -> int -> unit
  = "%atomic_land_idx"

(** [logor a i mask] atomically bitwise-OR-assigns the element of [a] at
    index [i] with [mask]. *)
external logor
  : ('a : value_or_null).
  ('a[@local_opt]) -> ('a, int) idx_atomic -> int -> unit
  = "%atomic_lor_idx"

(** [logxor a i mask] atomically bitwise-XOR-assigns the element of [a] at
    index [i] with [mask]. *)
external logxor
  : ('a : value_or_null).
  ('a[@local_opt]) -> ('a, int) idx_atomic -> int -> unit
  = "%atomic_lxor_idx"
