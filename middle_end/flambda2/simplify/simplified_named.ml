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

let for_speculative_inlining t =
  let t = filter_synthetic_value_slots t ~f:(fun _ -> false) in
  (* No term is rebuilt. Only direct calls, not the site's declarations, should
     root the code charged to this trial. *)
  { t with free_names = Name_occurrences.without_code_ids t.free_names }

let mark_unused_function_declarations_as_deleted function_decls ~live_code_ids
    ~find_code_metadata =
  Function_declarations.mark_as_deleted function_decls
    ~should_delete:(fun code_id -> not (Code_id.Set.mem code_id live_code_ids))
    ~function_slot_size_and_dbg:(fun code_id ->
      let metadata = find_code_metadata code_id in
      Code_metadata.function_slot_size metadata, Code_metadata.dbg metadata)

let mark_unused_functions_as_deleted t ~live_code_ids ~find_code_metadata =
  match t.named with
  | Simple _ | Prim _ | Rec_info _ -> t
  | Set_of_closures (set, alloc_mode) ->
    if not (Set_of_closures.is_specialisation_site set)
    then
      Misc.fatal_errorf "Not a specialisation site:@ %a" Set_of_closures.print
        set;
    let function_decls = Set_of_closures.function_decls set in
    let function_decls' =
      mark_unused_function_declarations_as_deleted function_decls ~live_code_ids
        ~find_code_metadata
    in
    if function_decls == function_decls'
    then t
    else
      let set =
        Set_of_closures.create
          ~is_specialisation_site:(Set_of_closures.is_specialisation_site set)
          ~value_slots:(Set_of_closures.value_slots set)
          ~synthetic_value_slots:(Set_of_closures.synthetic_value_slots set)
          function_decls'
      in
      let named = Set_of_closures (set, alloc_mode) in
      (* The cost metrics of a specialisation site are zero. *)
      { t with named; free_names = Named.free_names (to_named named) }

let rebuild_specialisation_site t ~live_code_ids ~names_available_for_hints
    ~find_code_metadata =
  match t.named with
  | Simple _ | Prim _ | Rec_info _ ->
    Misc.fatal_error "Not a specialisation site"
  | Set_of_closures (set, alloc_mode) ->
    if not (Set_of_closures.is_specialisation_site set)
    then
      Misc.fatal_errorf "Not a specialisation site:@ %a" Set_of_closures.print
        set;
    let synthetic_value_slots = Set_of_closures.synthetic_value_slots set in
    let synthetic_value_slots' =
      Value_slot.Map.filter
        (fun _ simple ->
          Name_occurrences.fold_names (Simple.free_names simple) ~init:true
            ~f:(fun available name ->
              available && Name.Set.mem name names_available_for_hints))
        synthetic_value_slots
    in
    let function_decls = Set_of_closures.function_decls set in
    let has_live_code =
      List.exists
        (fun code_id -> Code_id.Set.mem code_id live_code_ids)
        (Function_declarations.code_ids function_decls)
    in
    (* Keep dead siblings' slots and binders for imported layouts and phantom
       uses. Leave an entirely dead site for ordinary deletion or phantom
       handling. *)
    let function_decls' =
      if has_live_code
      then
        mark_unused_function_declarations_as_deleted function_decls
          ~live_code_ids ~find_code_metadata
      else function_decls
    in
    let keep =
      has_live_code && not (Value_slot.Map.is_empty synthetic_value_slots')
    in
    let t =
      if
        function_decls == function_decls'
        && Value_slot.Map.cardinal synthetic_value_slots
           = Value_slot.Map.cardinal synthetic_value_slots'
      then t
      else
        let set =
          Set_of_closures.create ~is_specialisation_site:true
            ~value_slots:(Set_of_closures.value_slots set)
            ~synthetic_value_slots:synthetic_value_slots' function_decls'
        in
        let named = Set_of_closures (set, alloc_mode) in
        (* The cost metrics of a specialisation site are zero. *)
        { t with named; free_names = Named.free_names (to_named named) }
    in
    t, keep

let print ppf { named; _ } = Named.print ppf (to_named named)

let cost_metrics { cost_metrics; _ } = cost_metrics

let update_cost_metrics cost_metrics t = { t with cost_metrics }

type 'a or_rewritten =
  | Simplified of 'a
  | Rewritten of (body:Flambda.Expr.t -> Flambda.Expr.t)
