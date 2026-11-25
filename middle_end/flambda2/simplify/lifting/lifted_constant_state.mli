(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module LC = Lifted_constant

type t

val empty : t

val is_empty : t -> bool

val print : Format.formatter -> t -> unit

val add : t -> LC.t -> t

val add_list : t -> LC.t list -> t

val of_list : LC.t list -> t

val fold : t -> init:'a -> f:('a -> LC.t -> 'a) -> 'a

val diff : t -> t -> t
(** Difference between two states that are related (one should be the ancestor of another).

    CR gbury: that's actually more like [sub] than [diff] in practice *)

val all_defined_symbols : t -> Symbol.Set.t

module Sorted : sig
  type t

  val empty : t

  val print : Format.formatter -> t -> unit

  val add :
    find_code_metadata:(Code_id.t -> Code_metadata.t) ->
    used_value_slots:Name_occurrences.t ->
    t ->
    LC.t ->
    t

  val pop_lifted_constants_to_place :
    t -> bound_names:Name_occurrences.t -> LC.t list * t

  val pop_all_remaining_lifted_constants : t -> LC.t list

  val fold_in_any_order : t -> init:'a -> f:('a -> LC.t -> 'a) -> 'a
end

val sort :
  find_code_metadata:(Code_id.t -> Code_metadata.t) ->
  used_value_slots:Name_occurrences.t ->
  t ->
  Sorted.t
