(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  type elt
  type t

  val empty : t
  val add : elt -> t -> t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val singleton : elt -> t
  val remove : elt -> t -> t
  val filter : (elt -> bool) -> t -> t
  val union : t -> t -> t
  val map : (elt -> elt) -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val iter_in_memory : (elt -> unit) -> t -> unit
  val cardinal : t -> int
  val elements : t -> elt list
  val fold : ('acc -> elt -> 'acc) -> 'acc -> t -> 'acc

  (** Like [fold], but subtrees that cannot be read because they live in an
      outdated store are skipped. [f] may also result in an outdated store
      error. Returns a (deduplicated) list of the outdated store errors, in
      addition to the accumulator. *)
  val fold_skipping_outdated :
    ('acc -> elt -> ('acc, Granular_marshal.Outdated_store.t) result) ->
    'acc ->
    t ->
    'acc * outdated:Granular_marshal.Outdated_store.Set.t

  val schema :
    Granular_marshal.iter -> (Granular_marshal.iter -> elt -> unit) -> t -> unit
end

module Make (Ord : Set.OrderedType) : S with type elt = Ord.t
