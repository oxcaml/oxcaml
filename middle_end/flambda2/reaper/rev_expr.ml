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

type tail_expr =
  | Invalid of { message : string }
  | Apply_cont of Apply_cont_expr.t
  | Switch of Switch_expr.t
  | Apply of Apply_expr.t

type rev_expr_holed =
  | Hole
  | Let of
      { bound_pattern : Bound_pattern.t;
        defining_expr : rev_named;
        parent : rev_expr_holed
      }
  | Let_cont of
      { cont : Continuation.t;
        handler : cont_handler;
        parent : rev_expr_holed
      }
  | Let_cont_rec of
      { invariant_params : Bound_parameters.t;
        handlers : cont_handler Continuation.Lmap.t;
        parent : rev_expr_holed
      }

and rev_named =
  | Named of Flambda.named
  | Set_of_closures of rev_set_of_closures * Alloc_mode.For_allocations.t
  | Static_consts of rev_static_const_or_code list

and rev_static_const_or_code =
  | Code
  | Deleted_code
  | Static_const of rev_static_const

and rev_static_const =
  | Set_of_closures of rev_set_of_closures
  | Other of Static_const.t

and rev_code =
  { params_and_body : rev_params_and_body;
    free_names_of_params_and_body : Name_occurrences.t;
    code_metadata : Code_metadata.t
  }

and rev_params_and_body =
  { return_continuation : Continuation.t;
    exn_continuation : Continuation.t;
    params : Bound_parameters.t;
    body : rev_expr;
    my_closure : Variable.t;
    my_alloc_mode : Alloc_mode.For_applications.t;
    my_depth : Variable.t
  }

and rev_set_of_closures =
  { value_slots : Simple.t Value_slot.Map.t;
    function_decls : Function_declarations.t
  }

and cont_handler =
  { bound_parameters : Bound_parameters.t;
    is_exn_handler : bool;
    is_cold : bool;
    expr : rev_expr
  }

and rev_expr =
  { expr : tail_expr;
    holed_expr : rev_expr_holed
  }

type t = rev_expr

let ids_for_export_set_of_closures { value_slots; function_decls } =
  Value_slot.Map.fold
    (fun _value_slot simple ids -> Ids_for_export.add_simple ids simple)
    value_slots
    (Function_declarations.ids_for_export function_decls)

let ids_for_export_static_const_or_code (const : rev_static_const_or_code) =
  match const with
  | Code | Deleted_code -> Ids_for_export.empty
  | Static_const (Set_of_closures set_of_closures) ->
    ids_for_export_set_of_closures set_of_closures
  | Static_const (Other static_const) ->
    Static_const.ids_for_export static_const

let rec ids_for_export { expr; holed_expr } =
  let ids_for_export_tail_expr =
    match expr with
    | Invalid { message = _ } -> Ids_for_export.empty
    | Apply_cont apply_cont -> Apply_cont_expr.ids_for_export apply_cont
    | Switch switch -> Switch_expr.ids_for_export switch
    | Apply apply -> Apply_expr.ids_for_export apply
  in
  Ids_for_export.union ids_for_export_tail_expr
    (ids_for_export_holed holed_expr)

and ids_for_export_holed (holed : rev_expr_holed) =
  match holed with
  | Hole -> Ids_for_export.empty
  | Let { bound_pattern; defining_expr; parent } ->
    Ids_for_export.union_list
      [ Bound_pattern.ids_for_export bound_pattern;
        ids_for_export_named defining_expr;
        ids_for_export_holed parent ]
  | Let_cont { cont; handler; parent } ->
    Ids_for_export.add_continuation
      (Ids_for_export.union
         (ids_for_export_cont_handler handler)
         (ids_for_export_holed parent))
      cont
  | Let_cont_rec { invariant_params; handlers; parent } ->
    let ids = ids_for_export_holed parent in
    let ids =
      Continuation.Lmap.fold
        (fun cont handler ids ->
          let ids =
            Ids_for_export.union ids (ids_for_export_cont_handler handler)
          in
          Ids_for_export.add_continuation ids cont)
        handlers ids
    in
    Ids_for_export.union (Bound_parameters.ids_for_export invariant_params) ids

and ids_for_export_named (named : rev_named) =
  match named with
  | Named named -> Flambda.Named.ids_for_export named
  | Set_of_closures (set_of_closures, alloc_mode) ->
    Ids_for_export.union
      (ids_for_export_set_of_closures set_of_closures)
      (Alloc_mode.For_allocations.ids_for_export alloc_mode)
  | Static_consts consts ->
    List.fold_left
      (fun ids const ->
        Ids_for_export.union ids (ids_for_export_static_const_or_code const))
      Ids_for_export.empty consts

and ids_for_export_cont_handler
    { bound_parameters; is_exn_handler = _; is_cold = _; expr } =
  Ids_for_export.union
    (Bound_parameters.ids_for_export bound_parameters)
    (ids_for_export expr)

let ids_for_export_code
    { params_and_body =
        { return_continuation;
          exn_continuation;
          params;
          body;
          my_closure;
          my_alloc_mode;
          my_depth
        };
      free_names_of_params_and_body;
      code_metadata
    } =
  let ids = ids_for_export body in
  let ids = Ids_for_export.add_continuation ids return_continuation in
  let ids = Ids_for_export.add_continuation ids exn_continuation in
  let ids = Ids_for_export.add_variable ids my_closure in
  let ids = Ids_for_export.add_variable ids my_depth in
  (* [free_names_of_params_and_body] is allowed to be an over-approximation of
     the names occurring in [params_and_body], so we must collect its ids too *)
  Ids_for_export.union_list
    [ ids;
      Alloc_mode.For_applications.ids_for_export my_alloc_mode;
      Bound_parameters.ids_for_export params;
      Name_occurrences.ids_for_export free_names_of_params_and_body;
      Code_metadata.ids_for_export code_metadata ]

let apply_renaming_set_of_closures { value_slots; function_decls } renaming =
  { value_slots =
      Value_slot.Map.map (Renaming.apply_simple renaming) value_slots;
    function_decls =
      Function_declarations.apply_renaming function_decls renaming
  }

let apply_renaming_static_const_or_code (const : rev_static_const_or_code)
    renaming : rev_static_const_or_code =
  match const with
  | Code | Deleted_code -> const
  | Static_const (Set_of_closures set_of_closures) ->
    Static_const
      (Set_of_closures (apply_renaming_set_of_closures set_of_closures renaming))
  | Static_const (Other static_const) ->
    Static_const (Other (Static_const.apply_renaming static_const renaming))

let rec apply_renaming { expr; holed_expr } renaming =
  let expr =
    match expr with
    | Invalid _ -> expr
    | Apply_cont apply_cont ->
      Apply_cont (Apply_cont_expr.apply_renaming apply_cont renaming)
    | Switch switch -> Switch (Switch_expr.apply_renaming switch renaming)
    | Apply apply -> Apply (Apply_expr.apply_renaming apply renaming)
  in
  { expr; holed_expr = apply_renaming_holed holed_expr renaming }

and apply_renaming_holed (holed : rev_expr_holed) renaming : rev_expr_holed =
  match holed with
  | Hole -> Hole
  | Let { bound_pattern; defining_expr; parent } ->
    Let
      { bound_pattern = Bound_pattern.apply_renaming bound_pattern renaming;
        defining_expr = apply_renaming_named defining_expr renaming;
        parent = apply_renaming_holed parent renaming
      }
  | Let_cont { cont; handler; parent } ->
    Let_cont
      { cont = Renaming.apply_continuation renaming cont;
        handler = apply_renaming_cont_handler handler renaming;
        parent = apply_renaming_holed parent renaming
      }
  | Let_cont_rec { invariant_params; handlers; parent } ->
    let renamed_handlers_as_list =
      List.map
        (fun (cont, handler) ->
          ( Renaming.apply_continuation renaming cont,
            apply_renaming_cont_handler handler renaming ))
        (Continuation.Lmap.bindings handlers)
    in
    Let_cont_rec
      { invariant_params =
          Bound_parameters.apply_renaming invariant_params renaming;
        handlers = Continuation.Lmap.of_list renamed_handlers_as_list;
        parent = apply_renaming_holed parent renaming
      }

and apply_renaming_named (named : rev_named) renaming : rev_named =
  match named with
  | Named named -> Named (Flambda.Named.apply_renaming named renaming)
  | Set_of_closures (set_of_closures, alloc_mode) ->
    Set_of_closures
      ( apply_renaming_set_of_closures set_of_closures renaming,
        Alloc_mode.For_allocations.apply_renaming alloc_mode renaming )
  | Static_consts consts ->
    Static_consts
      (List.map
         (fun const -> apply_renaming_static_const_or_code const renaming)
         consts)

and apply_renaming_cont_handler
    { bound_parameters; is_exn_handler; is_cold; expr } renaming =
  { bound_parameters = Bound_parameters.apply_renaming bound_parameters renaming;
    is_exn_handler;
    is_cold;
    expr = apply_renaming expr renaming
  }

let apply_renaming_code
    { params_and_body =
        { return_continuation;
          exn_continuation;
          params;
          body;
          my_closure;
          my_alloc_mode;
          my_depth
        };
      free_names_of_params_and_body;
      code_metadata
    } renaming =
  { params_and_body =
      { return_continuation =
          Renaming.apply_continuation renaming return_continuation;
        exn_continuation = Renaming.apply_continuation renaming exn_continuation;
        params = Bound_parameters.apply_renaming params renaming;
        body = apply_renaming body renaming;
        my_closure = Renaming.apply_variable renaming my_closure;
        my_alloc_mode =
          Alloc_mode.For_applications.apply_renaming my_alloc_mode renaming;
        my_depth = Renaming.apply_variable renaming my_depth
      };
    free_names_of_params_and_body =
      Name_occurrences.apply_renaming free_names_of_params_and_body renaming;
    code_metadata = Code_metadata.apply_renaming code_metadata renaming
  }
