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

(** [Dedup] is a generative functor: each application [(Dedup(H)())] produces
    a module with a fresh, incompatible [t] type. This prevents accidentally
    mixing values from different deduplication domains that happen to share
    the same underlying representation. *)
module type Dedup = sig
  (** A deduplicated element wrapping a value with its precomputed hash. *)
  type t

  type value

  (** [create tbl v] returns a canonical element for [v]. If an element
      structurally equal to [v] already exists in [tbl], that element is
      returned. Otherwise, a new element is created, added to [tbl], and
      returned. *)
  val create : value -> t

  (** [equal e1 e2] is [true] iff [e1] and [e2] are physically equal.
      This is O(1) due to deduplication. *)
  val equal : t -> t -> bool

  (** [hash e] returns the precomputed hash of [e]. This is O(1). *)
  val hash : t -> int

  (** [value e] returns the underlying value of [e]. *)
  val value : t -> value
end

val deduplicate :
  initial_size:int ->
  (module Hashtbl.HashedType with type t = 'a) ->
  (module Dedup with type value = 'a)
