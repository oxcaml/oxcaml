(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import

type simplified_named =
  | Simple of Simple.t
  | Prim of Flambda_primitive.t * Debuginfo.t
  | Set_of_closures of Set_of_closures.t * Alloc_mode.For_allocations.t
  | Rec_info of Rec_info_expr.t

let to_named = function
  | Simple simple -> Named.create_simple simple
  | Prim (prim, dbg) -> Named.create_prim prim dbg
  | Set_of_closures (set, alloc_mode) ->
    Named.create_set_of_closures ~alloc_mode set
  | Rec_info rec_info_expr -> Named.create_rec_info rec_info_expr

type t =
  { named : simplified_named;
    cost_metrics : Cost_metrics.t;
    free_names : Name_occurrences.t
  }

let create ~machine_width (named : Named.t) =
  let (simplified_named : simplified_named), cost_metrics =
    match named with
    | Simple simple ->
      Simple simple, Cost_metrics.from_size (Code_size.simple simple)
    | Prim (prim, dbg) ->
      ( Prim (prim, dbg),
        Cost_metrics.from_size (Code_size.prim ~machine_width prim) )
    | Set_of_closures _ ->
      Misc.fatal_errorf
        "Cannot use [Simplified_named.create] on [Set_of_closures];@ use \
         [create_with_known_free_names] instead:@ %a"
        Named.print named
    | Static_consts _ ->
      Misc.fatal_errorf
        "Cannot create [Simplified_named] from [Static_consts];@ use the \
         lifted constant infrastructure instead:@ %a"
        Named.print named
    | Rec_info rec_info_expr -> Rec_info rec_info_expr, Cost_metrics.zero
  in
  { named = simplified_named;
    cost_metrics;
    free_names = Named.free_names named
  }

let create_with_known_free_names ~machine_width ~find_code_characteristics
    (named : Named.t) ~free_names =
  let (simplified_named : simplified_named), cost_metrics =
    match named with
    | Simple simple ->
      Simple simple, Cost_metrics.from_size (Code_size.simple simple)
    | Prim (prim, dbg) ->
      ( Prim (prim, dbg),
        Cost_metrics.from_size (Code_size.prim ~machine_width prim) )
    | Set_of_closures (set, alloc_mode) ->
      ( Set_of_closures (set, alloc_mode),
        Cost_metrics.set_of_closures ~find_code_characteristics set )
    | Static_consts _ ->
      Misc.fatal_errorf
        "Cannot create [Simplified_named] from [Static_consts];@ use the \
         lifted constant infrastructure instead:@ %a"
        Named.print named
    | Rec_info rec_info_expr -> Rec_info rec_info_expr, Cost_metrics.zero
  in
  { named = simplified_named; cost_metrics; free_names }

let filter_synthetic_value_slots t ~f =
  match t.named with
  | Simple _ | Prim _ | Rec_info _ -> t
  | Set_of_closures (set, alloc_mode) ->
    let synthetic_value_slots = Set_of_closures.synthetic_value_slots set in
    let synthetic_value_slots' =
      Value_slot.Map.filter (fun _ simple -> f simple) synthetic_value_slots
    in
    if
      Value_slot.Map.cardinal synthetic_value_slots'
      = Value_slot.Map.cardinal synthetic_value_slots
    then t
    else
      let set =
        Set_of_closures.with_value_slots set
          ~value_slots:(Set_of_closures.value_slots set)
          ~synthetic_value_slots:synthetic_value_slots'
      in
      let named = Set_of_closures (set, alloc_mode) in
      { t with named; free_names = Named.free_names (to_named named) }

let mark_unused_functions_as_deleted t ~live_code_ids ~find_code_metadata =
  match t.named with
  | Simple _ | Prim _ | Rec_info _ -> t
  | Set_of_closures (set, alloc_mode) ->
    if not (Set_of_closures.is_specialisation_site set)
    then
      Misc.fatal_errorf "Not a specialisation site:@ %a" Set_of_closures.print
        set;
    let changed = ref false in
    let function_decls =
      Function_slot.Lmap.map
        (fun (decl : Function_declarations.code_id_in_function_declaration) ->
          match decl with
          | Deleted _ -> decl
          | Code_id { code_id; only_full_applications = _ } ->
            if Code_id.Set.mem code_id live_code_ids
            then decl
            else (
              changed := true;
              let metadata = find_code_metadata code_id in
              Function_declarations.Deleted
                { function_slot_size = Code_metadata.function_slot_size metadata;
                  dbg = Code_metadata.dbg metadata
                }))
        (Function_declarations.funs_in_order
           (Set_of_closures.function_decls set))
    in
    if not !changed
    then t
    else
      let set =
        Set_of_closures.create
          ~is_specialisation_site:(Set_of_closures.is_specialisation_site set)
          ~value_slots:(Set_of_closures.value_slots set)
          ~synthetic_value_slots:(Set_of_closures.synthetic_value_slots set)
          (Function_declarations.create function_decls)
      in
      let named = Set_of_closures (set, alloc_mode) in
      (* The cost metrics of a specialisation site are zero. *)
      { t with named; free_names = Named.free_names (to_named named) }

let print ppf { named; _ } = Named.print ppf (to_named named)

let cost_metrics { cost_metrics; _ } = cost_metrics

let update_cost_metrics cost_metrics t = { t with cost_metrics }

type 'a or_rewritten =
  | Simplified of 'a
  | Rewritten of (body:Flambda.Expr.t -> Flambda.Expr.t)
