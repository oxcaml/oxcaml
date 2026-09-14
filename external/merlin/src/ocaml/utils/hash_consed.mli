(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Simon Spies, Jane Street Europe                       *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
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

(** Hash-consing utilities.

    This module enables hash-consing via the [Dedup] functor. An application of
    [Dedup] yields an abstract element type [t] and an abstract type [table].
    Since the functor is generative, elements of distinct applications cannot be
    mixed up.

    Note, however, that a single application can be used to create several
    tables, and the types do not prevent mixing elements interned in different
    tables of the same application. Doing so breaks the invariant that
    structural equality of the underlying values implies physical equality of
    the interned elements: [equal] can spuriously return [false] for such
    elements. All elements that are compared with each other (e.g. as parts of
    keys in the same cache) must therefore be interned in the same table. *)

module type S = sig
  (** The underlying element type. *)
  type elt

  (** A deduplicated element. *)
  type t

  (** A hash-consing table. *)
  type table

  (** [create_table ~initial_size] creates a fresh, empty hash-consing table. *)
  val create_table : initial_size:int -> table

  (** [create table v] returns the canonical element for [v] in [table]. If a
      structurally equal value already exists in [table], that element is
      returned. Otherwise, a new element is created and added to [table]. *)
  val create : table -> elt -> t

  (** [value e] returns the underlying value of [e]. *)
  val value : t -> elt

  (** [hash e] returns the precomputed hash of [e]. O(1). *)
  val hash : t -> int

  (** [equal e1 e2] is physical equality of the hash-consed elements. This is
      O(1) and coincides with structural equality of the underlying values,
      provided [e1] and [e2] were interned in the same table (see the module
      comment above). *)
  val equal : t -> t -> bool

  (** [canonical table v] is the canonical, shared representative of [v]. *)
  val canonical : table -> elt -> elt

  (** [modify table e f] applies [f] to the underlying value and re-interns the
      result. *)
  val modify : table -> t -> (elt -> elt) -> t
end

module Dedup (H : Hashtbl.HashedType) () : S with type elt = H.t
