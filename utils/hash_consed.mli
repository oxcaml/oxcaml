(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Simon Spies, Jane Street Europe                       *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   Permission is hereby granted, free of charge, to any person          *)
(*   obtaining a copy of this software and associated documentation       *)
(*   files (the "Software"), to deal in the Software without              *)
(*   restriction, including without limitation the rights to use, copy,   *)
(*   modify, merge, publish, distribute, sublicense, and/or sell copies   *)
(*   of the Software, and to permit persons to whom the Software is       *)
(*   furnished to do so, subject to the following conditions:             *)
(*                                                                        *)
(*   The above copyright notice and this permission notice shall be       *)
(*   included in all copies or substantial portions of the Software.      *)
(*                                                                        *)
(*   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,      *)
(*   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF   *)
(*   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                *)
(*   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS  *)
(*   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN   *)
(*   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN    *)
(*   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE     *)
(*   SOFTWARE.                                                            *)
(*                                                                        *)
(**************************************************************************)

(** Hash-consing utilities for value deduplication.

    This module provides hash-consing (also known as "interning" or
    "deduplication") which ensures that structurally equal values share the
    same memory representation. This enables:
    - O(1) equality checks via physical equality
    - O(1) hash lookups using precomputed hashes
    - Reduced memory usage when many duplicate values exist *)

(** A hash-consed value of type ['a] associated with a table of type ['tbl].

    The ['tbl] phantom type parameter prevents accidentally mixing values
    from different hash-consing tables, which would break the invariant that
    physical equality implies structural equality. *)
type ('a, 'tbl) t

(** [hash e] returns the precomputed hash of [e]. O(1). *)
val hash : ('a, 'tbl) t -> int

(** [equal e1 e2] returns [true] iff [e1] and [e2] are the same hash-consed
    element. This is O(1) because it uses physical equality, which is valid
    due to deduplication. *)
val equal : ('a, 'tbl) t -> ('a, 'tbl) t -> bool

(** [value e] returns the underlying value of [e]. *)
val value : ('a, 'tbl) t -> 'a

(** Functor to create a hash-consing table for a specific type.

    Each application of [Table] creates a fresh table with an incompatible
    [tbl] type, preventing values from different tables from being mixed. *)
module Table (H : sig
  type t

  (** Initial size for the internal hash table. *)
  val initial_size : int

  (** Hash function for values. *)
  val hash : t -> int

  (** Structural equality for values. *)
  val equal : t -> t -> bool
end) : sig
  (** Phantom type identifying this specific table. *)
  type tbl

  (** [create v] returns the canonical hash-consed element for [v]. If a
      structurally equal value already exists in the table, that element is
      returned. Otherwise, a new element is created and added to the table. *)
  val create : H.t -> (H.t, tbl) t
end

(** Simplified functor interface for hash-consing non-recursive types.

    This functor hides the table management and phantom type, providing a
    simpler interface. It is difficult to use for recursive types, since the
    functor argument (including [hash] and [equal]) must be provided before
    the result type is available. For recursive types, use [Table] instead. *)
module Dedup (H : sig
  type t

  val initial_size : int

  val hash : t -> int

  val equal : t -> t -> bool
end) : sig
  (** A deduplicated element wrapping a value of type [H.t]. *)
  type t

  (** [create v] returns the canonical element for [v]. *)
  val create : H.t -> t

  (** [hash e] returns the precomputed hash of [e]. O(1). *)
  val hash : t -> int

  (** [equal e1 e2] is O(1) physical equality. *)
  val equal : t -> t -> bool

  (** [value e] returns the underlying value. *)
  val value : t -> H.t
end
