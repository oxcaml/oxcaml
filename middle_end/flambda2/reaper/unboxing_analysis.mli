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

  val fold2_subset : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c

  val fold2_subset_u : ('a -> 'b -> 'c -> 'c) -> 'a u -> 'b u -> 'c -> 'c

  val fold2_subset_with_kind :
    (Flambda_kind.t -> 'a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c

  val equal_shape : 'a t -> 'b t -> bool
end

type unboxed = Variable.t Unboxed_fields.t

type changed_representation =
  | Block_representation of
      (int * Flambda_primitive.Block_access_kind.t) Unboxed_fields.t * int
  | Closure_representation of
      Value_slot.t Unboxed_fields.t
      * Function_slot.t Function_slot.Map.t
      * Function_slot.t

type param_decision =
  | Keep of Variable.t * Flambda_kind.With_subkind.t
  | Delete
  | Unbox of Variable.t Unboxed_fields.t

val arity_of_decisions : param_decision list -> [`Complex] Flambda_arity.t

type my_closure_param_decision =
  | Keep_my_closure
  | Unbox_my_closure of Variable.t Unboxed_fields.t

val print_param_decision : Format.formatter -> param_decision -> unit

type result =
  { db : Datalog.database;
    unboxed_fields : unboxed Code_id_or_name.Map.t;
    changed_representation :
      (changed_representation * Code_id_or_name.t) Code_id_or_name.Map.t
  }

type calling_convention_change =
  | Not_changing_calling_convention
  | Changing_calling_convention of
      { my_closure_decision : my_closure_param_decision;
        params_decisions : param_decision list;
        return_decisions : param_decision list
      }

type code_changes

val get_calling_convention_change :
  code_changes -> Code_id.t -> calling_convention_change

(* Should only be called on code_ids from the current unit. *)
val get_code_metadata : code_changes -> Code_id.t -> Code_metadata.t

val pp_result : Format.formatter -> result -> unit

val perform_analysis :
  Datalog.database -> stats:Datalog.Schedule.stats -> result

val compute_code_changes :
  result ->
  rewrite_kind_with_subkind:
    (Name.t -> Flambda_kind.With_subkind.t -> Flambda_kind.With_subkind.t) ->
  rewrite_result_types:
    (my_closure:Variable.t ->
    params:(Variable.t * Points_to_analysis.keep_or_delete) list ->
    results:(Variable.t * Points_to_analysis.keep_or_delete) list ->
    Result_types.t ->
    Result_types.t Or_unknown_or_bottom.t) ->
  code_deps:Traverse_acc.code_dep Code_id.Map.t ->
  code_changes
