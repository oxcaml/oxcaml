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

type my_closure_param_decision =
  | Keep_my_closure
  | Unbox_my_closure of Variable.t Unboxed_fields.t

val print_param_decision : Format.formatter -> param_decision -> unit

(** The kinds of the parameters that remain after applying the given decisions,
    in order: a kept parameter keeps its kind, a deleted one contributes nothing
    and an unboxed one is replaced by the kinds of its fields. *)
val unarized_kinds_of_param_decisions :
  param_decision list -> Flambda_kind.With_subkind.t list

(** [unarized_kinds_of_param_decisions] as a single unboxed product. *)
val arity_of_param_decisions : param_decision list -> [`Complex] Flambda_arity.t

type result =
  { db : Datalog.database;
    unboxed_fields : unboxed Code_id_or_name.Map.t;
    changed_representation :
      (changed_representation * Code_id_or_name.t) Code_id_or_name.Map.t
  }

type calling_convention_changes

val my_closure_decision :
  calling_convention_changes -> Code_id.t -> my_closure_param_decision option

val function_params_to_keep :
  calling_convention_changes -> Code_id.t -> param_decision list option

val function_return_decision :
  calling_convention_changes -> Code_id.t -> param_decision list option

val pp_result : Format.formatter -> result -> unit

val cannot_change_calling_convention : result -> Code_id.t -> bool

val perform_analysis :
  Datalog.database -> stats:Datalog.Schedule.stats -> result

val compute_calling_convention_changes :
  result ->
  rewrite_kind_with_subkind:
    (Name.t -> Flambda_kind.With_subkind.t -> Flambda_kind.With_subkind.t) ->
  code_deps:Traverse_acc.code_dep Code_id.Map.t ->
  calling_convention_changes
