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

(** Static definitions to emit in dependency order after [Shared] code bodies
    are translated.

    [Code] binds a JSIR closure for [Shared] code. This is still needed when
    Flambda removes the [Set_of_closures] and keeps only direct calls, because
    JSIR calls always take a closure value.

    [Closure] translates and binds the [Instantiated] code of a set of closures,
    reusing any closures already created by [Code], and aliases the set's
    symbols to their closures. *)
type definition =
  | Code of Code_id.t * Code.t * To_jsir_env.closure
  | Closure of
      Symbol.t Function_slot.Lmap.t
      * Set_of_closures.t
      * To_jsir_set_of_closures.static
  | Block of Symbol.t * Static_const.t

(* Precondition: all code in the group must have been added to [env].

   Registering the slot for every set here ensures they are available before any
   code in the group is translated, since code may project from any closure
   symbol in the group. *)
let definitions ~env ~res consts bound_static =
  let env, res, definitions =
    Static_const_group.match_against_bound_static consts bound_static
      ~init:(env, res, [])
      ~code:(fun (env, res, acc) code_id code ->
        match To_jsir_env.get_code_exn env code_id with
        | Shared (Current_unit { closure; code = _ }) ->
          env, res, Code (code_id, code, closure) :: acc
        | Shared (Other_unit _) | Instantiated _ -> env, res, acc)
      ~deleted_code:(fun acc _ -> acc)
      ~set_of_closures:(fun (env, res, acc) ~closure_symbols soc ->
        let env, res, static =
          To_jsir_set_of_closures.prepare_static_set_of_closures ~env ~res
            ~closure_symbols soc
        in
        env, res, Closure (closure_symbols, soc, static) :: acc)
      ~block_like:(fun (env, res, acc) symbol const ->
        env, res, Block (symbol, const) :: acc)
  in
  env, res, List.rev definitions |> Array.of_list

(* Map each defined symbol or [Shared] code ID to its definition index.
   [Instantiated] code has no definition of its own: it is translated at each
   closure site, so its dependencies are collected transitively below. *)
let build_indices definitions =
  let indices = ref CIS.Map.empty in
  let add name index = indices := CIS.Map.add name index !indices in
  Array.iteri
    (fun index -> function
      | Code (code_id, _, _) -> add (CIS.create_code_id code_id) index
      | Block (symbol, _) -> add (CIS.create_symbol symbol) index
      | Closure (symbols, _, _) ->
        Function_slot.Lmap.iter
          (fun _ symbol -> add (CIS.create_symbol symbol) index)
          symbols)
    definitions;
  !indices

let add_dependency ~indices name deps =
  match CIS.Map.find_opt name indices with
  | None -> deps
  | Some index -> Numbers.Int.Set.add index deps

(* Collect dependencies from free symbols and code IDs. For any code ID without
   a [Code] node, recurse into its body: that code is translated where its
   closures are created, so its free symbols become dependencies of the
   enclosing definition. *)
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

let collect_dependencies_from_value_slots ~env ~indices ~code_by_id soc deps =
  let used_value_slots = To_jsir_set_of_closures.used_value_slots ~env soc in
  Value_slot.Map.fold
    (fun slot simple deps ->
      if Value_slot.Set.mem slot used_value_slots
      then
        collect_dependencies_from_free_names ~indices ~code_by_id
          (Simple.free_names simple) deps
      else deps)
    (Set_of_closures.value_slots soc)
    deps

let collect_dependencies_from_function_decl ~indices ~code_by_id decl deps =
  match decl with
  | Function_declarations.Deleted _ -> deps
  | Function_declarations.Code_id { code_id; only_full_applications = _ } ->
    collect_dependencies_from_free_names ~indices ~code_by_id
      (Name_occurrences.singleton_code_id code_id Name_mode.normal)
      deps

(* Build edges from each definition to the definitions it depends on. *)
let dependency_graph ~env ~code_by_id definitions =
  let indices = build_indices definitions in
  Array.mapi
    (fun index definition ->
      let deps =
        match definition with
        | Code (_, code, _) ->
          collect_dependencies_from_free_names ~indices ~code_by_id
            (Code0.free_names_of_params_and_body code)
            Numbers.Int.Set.empty
        | Block (_, const) ->
          collect_dependencies_from_free_names ~indices ~code_by_id
            (Static_const.free_names const)
            Numbers.Int.Set.empty
        | Closure (_, soc, _) ->
          let deps =
            collect_dependencies_from_value_slots ~env ~indices ~code_by_id soc
              Numbers.Int.Set.empty
          in
          Function_slot.Lmap.fold
            (fun _slot decl deps ->
              collect_dependencies_from_function_decl ~indices ~code_by_id decl
                deps)
            (Function_declarations.funs_in_order
               (Set_of_closures.function_decls soc))
            deps
      in
      index, deps)
    definitions
  |> Array.to_list

let symbols_defined_by definitions indices =
  List.fold_left
    (fun symbols index ->
      match definitions.(index) with
      | Code _ -> symbols
      | Block (symbol, _) -> Symbol.Set.add symbol symbols
      | Closure (closure_symbols, _, _) ->
        Function_slot.Lmap.fold
          (fun _ symbol symbols -> Symbol.Set.add symbol symbols)
          closure_symbols symbols)
    Symbol.Set.empty indices

(* If [defer_initialise] is true, we first pre-allocate a mutable block, and
   patch the fields that refer to the cycle later - this is used when there is a
   cyclic dependency. Otherwise, we allocate and initialise the block
   together. *)
let with_allocated_blocks ~env ~res ~defer_initialise definitions indices ~f =
  let blocks =
    List.filter_map
      (fun index ->
        match definitions.(index) with
        | Block (symbol, const) -> Some (symbol, const)
        | Code _ | Closure _ -> None)
      indices
  in
  let symbols_in_cycle = symbols_defined_by definitions indices in
  let res =
    List.fold_left
      (fun res (symbol, const) ->
        if defer_initialise
        then
          To_jsir_static_const.preallocate_block_like ~env ~res
            ~symbols_in_cycle symbol const
        else To_jsir_static_const.block_like ~env ~res symbol const)
      res blocks
  in
  let res = f res in
  if defer_initialise
  then
    List.fold_left
      (fun res (symbol, const) ->
        To_jsir_static_const.initialise_block_like ~env ~res ~symbols_in_cycle
          symbol const)
      res blocks
  else res

let emit_closures ~env ~translate_body definitions indices res =
  let res =
    List.fold_left
      (fun res index ->
        match definitions.(index) with
        | Code _ | Block _ -> res
        | Closure (_, _, static) ->
          To_jsir_set_of_closures.translate_instances ~env ~res ~translate_body
            static.instances)
      res indices
  in
  let res =
    List.fold_left
      (fun res index ->
        match definitions.(index) with
        | Block _ -> res
        | Code (_, _, closure) ->
          To_jsir_result.add_instr_exn res
            (Let (closure.var, To_jsir_static_const.closure_expr closure))
        | Closure (_, _, static) ->
          To_jsir_set_of_closures.bind_instances ~res static.instances)
      res indices
  in
  List.fold_left
    (fun res index ->
      match definitions.(index) with
      | Code _ | Block _ -> res
      | Closure (_, _, static) ->
        To_jsir_result.add_var_aliases_exn res static.aliases)
    res indices

let emit_definitions_for_component ~env ~res ~translate_body definitions
    component =
  let cyclic, indices =
    match component with
    | SCC.No_loop index -> false, [index]
    | SCC.Has_loop indices -> true, List.sort Int.compare indices
  in
  with_allocated_blocks ~env ~res ~defer_initialise:cyclic definitions indices
    ~f:(emit_closures ~env ~translate_body definitions indices)

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
  let env, res, definitions = definitions ~env ~res consts bound_static in
  let res =
    Array.fold_left
      (fun res definition ->
        match definition with
        | Code (_, code, closure) ->
          To_jsir_static_const.code ~env ~res ~translate_body ~closure code
        | Closure _ | Block _ -> res)
      res definitions
  in
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
        emit_definitions_for_component ~env ~res ~translate_body definitions
          component)
      components res
  in
  (* The registration of a closure symbol is given the location of its
     closure. *)
  let closure_locs =
    Static_const_group.match_against_bound_static consts bound_static
      ~init:Symbol.Map.empty
      ~code:(fun locs _ _ -> locs)
      ~deleted_code:(fun locs _ -> locs)
      ~set_of_closures:(fun locs ~closure_symbols soc ->
        To_jsir_set_of_closures.closure_symbol_locs ~env ~closure_symbols soc
        |> Symbol.Map.disjoint_union locs)
      ~block_like:(fun locs _ _ -> locs)
  in
  let res =
    Symbol.Set.fold
      (fun symbol res ->
        let loc = Symbol.Map.find_opt symbol closure_locs in
        To_jsir_env.register_symbol_exn env ~res ?loc symbol)
      symbols res
  in
  let res =
    Array.fold_left
      (fun res definition ->
        match definition with
        | Code (code_id, _, closure) ->
          To_jsir_result.register_code res ?loc:closure.loc code_id closure.var
        | Closure _ | Block _ -> res)
      res definitions
  in
  env, res
