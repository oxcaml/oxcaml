(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Unboxed_fields : sig
  type 'a u =
    | Not_unboxed of 'a
    | Unboxed of 'a t

  and 'a t = 'a u Field.Map.t

  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val fold_with_kind : (Flambda_kind.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val map : ('a -> 'b) -> 'a t -> 'b t

  val map_u : ('a -> 'b) -> 'a u -> 'b u

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val fold2_subset : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c

  val fold2_subset_u : ('a -> 'b -> 'c -> 'c) -> 'a u -> 'b u -> 'c -> 'c

  val fold2_subset_with_kind :
    (Flambda_kind.t -> 'a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
end

type unboxed = Variable.t Unboxed_fields.t

type changed_representation =
  | Block_representation of
      (int * Flambda_primitive.Block_access_kind.t) Unboxed_fields.t * int
  | Closure_representation of
      Value_slot.t Unboxed_fields.t
      * Function_slot.t Function_slot.Map.t
      * Function_slot.t

type result =
  { db : Datalog.database;
    unboxed_fields : unboxed Code_id_or_name.Map.t;
    changed_representation :
      (changed_representation * Code_id_or_name.t) Code_id_or_name.Map.t
  }

val pp_result : Format.formatter -> result -> unit

val unboxed_fields_ids_for_export :
  unboxed Code_id_or_name.Map.t -> Ids_for_export.t -> Ids_for_export.t

val unboxed_fields_fields_for_export :
  unboxed Code_id_or_name.Map.t -> Field.Set.t -> Field.Set.t

val unboxed_fields_apply_renaming :
  unboxed Code_id_or_name.Map.t ->
  Renaming.t ->
  rename_field:(Field.t -> Field.t) ->
  unboxed Code_id_or_name.Map.t

val changed_representation_ids_for_export :
  (changed_representation * Code_id_or_name.t) Code_id_or_name.Map.t ->
  Ids_for_export.t ->
  Ids_for_export.t

val changed_representation_fields_for_export :
  (changed_representation * Code_id_or_name.t) Code_id_or_name.Map.t ->
  Field.Set.t ->
  Field.Set.t

val changed_representation_apply_renaming :
  (changed_representation * Code_id_or_name.t) Code_id_or_name.Map.t ->
  Renaming.t ->
  rename_field:(Field.t -> Field.t) ->
  (changed_representation * Code_id_or_name.t) Code_id_or_name.Map.t

val cannot_change_calling_convention_table : Datalog_helpers.Maps.N.handle

val cannot_change_calling_convention : result -> Code_id.t -> bool

val perform_analysis :
  Datalog.database -> stats:Datalog.Schedule.stats -> result
