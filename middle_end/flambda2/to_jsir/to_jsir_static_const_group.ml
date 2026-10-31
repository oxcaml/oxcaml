(******************************************************************************
 *                                  OxCaml                                    *
 *                           Leo Lee, Jane Street                             *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open! Flambda.Import
open! Jsoo_imports.Import
module CIS = Code_id_or_symbol
module SCC = Strongly_connected_components.Make (Numbers.Int)

(** Definitions to emit after code bodies have been translated.

    [Code] creates a JSIR function value for code that does not need a
    set-specific closure environment. This is needed even when Flambda has
    removed the set of closures and retained only direct calls to the code, as
    JSIR calls still require a function value.

    [Closure] supplies captured values and creates the closures that need them.
    Functions already created by [Code] are reused. This also binds the set's
    symbols and function slots to their closures. *)
type definition =
  | Code of Code_id.t * Code.t
  | Closure of Symbol.t Function_slot.Lmap.t * Set_of_closures.t
  | Block of Symbol.t * Static_const.t

let definitions ~env consts bound_static =
  Static_const_group.match_against_bound_static consts bound_static ~init:[]
    ~code:(fun acc code_id code ->
      if (To_jsir_env.get_code_id_exn env code_id).closure_bound_with_code
      then Code (code_id, code) :: acc
      else acc)
    ~deleted_code:(fun acc _ -> acc)
    ~set_of_closures:(fun acc ~closure_symbols soc ->
      Closure (closure_symbols, soc) :: acc)
    ~block_like:(fun acc symbol const -> Block (symbol, const) :: acc)
  |> List.rev |> Array.of_list

(* Map each symbol or code ID to the index of the definition supplying its
   value. A code ID maps to its [Code] node if its closure is created
   separately, or to the [Closure] node that creates it otherwise. *)
let build_indices ~env definitions =
  let indices = ref CIS.Map.empty in
  let add name index = indices := CIS.Map.add name index !indices in
  let add_closure_code_id index _slot = function
    | Function_declarations.Deleted _ -> ()
    | Function_declarations.Code_id { code_id; only_full_applications = _ } ->
      if not (To_jsir_env.get_code_id_exn env code_id).closure_bound_with_code
      then add (CIS.create_code_id code_id) index
  in
  Array.iteri
    (fun index -> function
      | Code (code_id, _) -> add (CIS.create_code_id code_id) index
      | Block (symbol, _) -> add (CIS.create_symbol symbol) index
      | Closure (symbols, soc) ->
        Function_slot.Lmap.iter
          (fun _ symbol -> add (CIS.create_symbol symbol) index)
          symbols;
        Function_slot.Lmap.iter
          (add_closure_code_id index)
          (Function_declarations.funs_in_order
             (Set_of_closures.function_decls soc)))
    definitions;
  !indices

let add_dependency ~indices name deps =
  match CIS.Map.find_opt name indices with
  | None -> deps
  | Some index -> Numbers.Int.Set.add index deps

(* Collect dependencies from free symbols and code IDs. We recursively inspect
   the body of any nested code, since its free symbols are also captured by the
   enclosing closure. *)
let collect_dependencies_from_free_names ~indices ~code_by_id free_names deps =
  let rec visit_free_names free_names (visited, deps) =
    let deps =
      Symbol.Set.fold
        (fun symbol deps ->
          add_dependency ~indices (CIS.create_symbol symbol) deps)
        (Name_occurrences.symbols free_names)
        deps
    in
    Code_id.Set.fold visit_code_id
      (Name_occurrences.code_ids free_names)
      (visited, deps)
  and visit_code_id code_id (visited, deps) =
    match CIS.Map.find_opt (CIS.create_code_id code_id) indices with
    | Some index -> visited, Numbers.Int.Set.add index deps
    | None when Code_id.Set.mem code_id visited -> visited, deps
    | None -> (
      match Code_id.Map.find_opt code_id code_by_id with
      | None -> visited, deps
      | Some code ->
        visit_free_names
          (Code0.free_names_of_params_and_body code)
          (Code_id.Set.add code_id visited, deps))
  in
  visit_free_names free_names (Code_id.Set.empty, deps) |> snd

let collect_dependencies_from_value_slots ~env ~indices ~code_by_id value_slots
    deps =
  Value_slot.Map.fold
    (fun slot simple deps ->
      match To_jsir_env.get_value_slot env slot with
      | None -> deps (* not used *)
      | Some _ ->
        collect_dependencies_from_free_names ~indices ~code_by_id
          (Simple.free_names simple) deps)
    value_slots deps

let collect_dependencies_from_function_decl ~env ~indices ~code_by_id decl deps
    =
  match decl with
  | Function_declarations.Deleted _ -> deps
  | Function_declarations.Code_id { code_id; only_full_applications = _ } -> (
    if (To_jsir_env.get_code_id_exn env code_id).closure_bound_with_code
    then add_dependency ~indices (CIS.create_code_id code_id) deps
    else
      match Code_id.Map.find_opt code_id code_by_id with
      | None -> deps (* outside this group *)
      | Some code ->
        collect_dependencies_from_free_names ~indices ~code_by_id
          (Code0.free_names_of_params_and_body code)
          deps)

(* Build edges from each definition to the definitions it depends on. *)
let dependency_graph ~env ~code_by_id definitions =
  let indices = build_indices ~env definitions in
  Array.mapi
    (fun index definition ->
      let deps =
        match definition with
        | Code (_, code) ->
          collect_dependencies_from_free_names ~indices ~code_by_id
            (Code0.free_names_of_params_and_body code)
            Numbers.Int.Set.empty
        | Block (_, const) ->
          collect_dependencies_from_free_names ~indices ~code_by_id
            (Static_const.free_names const)
            Numbers.Int.Set.empty
        | Closure (_, soc) ->
          let deps =
            collect_dependencies_from_value_slots ~env ~indices ~code_by_id
              (Set_of_closures.value_slots soc)
              Numbers.Int.Set.empty
          in
          Function_slot.Lmap.fold
            (fun _slot decl deps ->
              collect_dependencies_from_function_decl ~env ~indices ~code_by_id
                decl deps)
            (Function_declarations.funs_in_order
               (Set_of_closures.function_decls soc))
            deps
      in
      index, deps)
    definitions
  |> Array.to_list

(* If [defer_initialise] is true, we first pre-allocate a mutable block, and
   patch the values later - this is used when there is a cyclic dependency.
   Otherwise, we allocate and initalise the block together. *)
let with_allocated_blocks ~env ~res ~defer_initialise definitions indices ~f =
  let blocks =
    List.filter_map
      (fun index ->
        match definitions.(index) with
        | Block (symbol, const) -> Some (symbol, const)
        | Code _ | Closure _ -> None)
      indices
  in
  let res =
    List.fold_left
      (fun res (symbol, const) ->
        if defer_initialise
        then To_jsir_static_const.preallocate_block_like ~env ~res symbol const
        else To_jsir_static_const.block_like ~env ~res symbol const |> snd)
      res blocks
  in
  let res = f res in
  if defer_initialise
  then
    List.fold_left
      (fun res (symbol, const) ->
        To_jsir_static_const.initialise_block_like ~env ~res symbol const)
      res blocks
  else res

let emit_closures ~env definitions indices res =
  let res, inits =
    List.fold_left
      (fun (res, inits) index ->
        match definitions.(index) with
        | Code _ | Block _ -> res, inits
        | Closure (closure_symbols, soc) ->
          let res, init =
            To_jsir_set_of_closures.prepare_static_set_of_closures ~env ~res
              ~closure_symbols soc
          in
          res, Numbers.Int.Map.add index init inits)
      (res, Numbers.Int.Map.empty)
      indices
  in
  let res =
    List.fold_left
      (fun res index ->
        match definitions.(index) with
        | Block _ -> res
        | Code (code_id, _) ->
          To_jsir_static_const.code_closure ~env ~res code_id
        | Closure _ ->
          List.fold_left
            (fun res { To_jsir_shared.bound_var; defining_expr } ->
              To_jsir_result.add_instr_exn res (Let (bound_var, defining_expr)))
            res (Numbers.Int.Map.find index inits).closures)
      res indices
  in
  Numbers.Int.Map.fold
    (fun _ (init : To_jsir_set_of_closures.initialisation) res ->
      To_jsir_result.add_var_aliases_exn res
        (init.aliases @ init.value_slot_bindings))
    inits res

let emit_definitions_for_component ~env ~res definitions component =
  let cyclic, indices =
    match component with
    | SCC.No_loop index -> false, [index]
    | SCC.Has_loop indices -> true, List.sort Int.compare indices
  in
  with_allocated_blocks ~env ~res ~defer_initialise:cyclic definitions indices
    ~f:(emit_closures ~env definitions indices)

let translate ~env ~res ~translate_body bound_static consts =
  (* Reserve all symbols before translating bodies, since static definitions may
     refer to one another in any order. *)
  let symbols = Bound_static.symbols_being_defined bound_static in
  let env =
    Symbol.Set.fold
      (fun symbol env ->
        To_jsir_env.add_symbol_without_registering env symbol
          (Jsir.Var.fresh ()))
      symbols env
  in
  let env, res =
    Static_const_group.match_against_bound_static consts bound_static
      ~init:(env, res)
      ~code:(fun (env, res) code_id code ->
        To_jsir_static_const.prepare_code ~env ~res ~code_id code)
      ~deleted_code:(fun acc _ -> acc)
      ~set_of_closures:(fun acc ~closure_symbols:_ _ -> acc)
      ~block_like:(fun acc _ _ -> acc)
  in
  let env, res =
    Static_const_group.match_against_bound_static consts bound_static
      ~init:(env, res)
      ~code:(fun (env, res) code_id code ->
        To_jsir_static_const.code ~env ~res ~translate_body ~code_id code)
      ~deleted_code:(fun acc _ -> acc)
      ~set_of_closures:(fun acc ~closure_symbols:_ _ -> acc)
      ~block_like:(fun acc _ _ -> acc)
  in
  let definitions = definitions ~env consts bound_static in
  let components =
    dependency_graph ~env
      ~code_by_id:(Static_const_group.pieces_of_code consts)
      definitions
    |> SCC.stable_connected_components_sorted_from_roots_to_leaf
  in
  (* Edges point to dependencies, so visit leaves first. *)
  let res =
    Array.fold_right
      (fun component res ->
        emit_definitions_for_component ~env ~res definitions component)
      components res
  in
  let res =
    Symbol.Set.fold
      (fun symbol res -> To_jsir_env.register_symbol_exn env ~res symbol)
      symbols res
  in
  env, res
