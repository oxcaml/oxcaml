(******************************************************************************
 *                                  OxCaml                                    *
 *                           Leo Lee, Jane Street                             *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

open! Jsoo_imports.Import

type instance =
  { closure : To_jsir_env.closure;
    code : Code.t
  }

type static =
  { instances : instance list;
    aliases : To_jsir_result.var_binding list
  }

(* With [-g], closures are named after their functions. The Lambda to Flambda
   pass gives anonymous functions the name ["fn"], possibly followed by their
   location in square brackets, which we omit. *)
let name_closure slot fn_var =
  if Flambda_features.debug () && Jsir.Var.generated_name fn_var
  then
    let name = Function_slot.name slot in
    if not (String.equal name "fn" || String.starts_with ~prefix:"fn[" name)
    then Jsir.Var.set_name fn_var name

let value_slots_projected_by (code : To_jsir_env.code) =
  match code with
  | Shared _ -> Value_slot.Set.empty
  | Instantiated code ->
    Name_occurrences.value_slots_in_normal_projections (Code0.free_names code)

let used_value_slots ~env soc =
  Function_slot.Lmap.fold
    (fun _slot (decl : Function_declarations.code_id_in_function_declaration)
         used ->
      match decl with
      | Deleted _ -> used
      | Code_id { code_id; only_full_applications = _ } ->
        To_jsir_env.get_code_exn env code_id
        |> value_slots_projected_by |> Value_slot.Set.union used)
    (Function_declarations.funs_in_order (Set_of_closures.function_decls soc))
    Value_slot.Set.empty

let closure_for_code ~env ~res code_id :
    To_jsir_result.t * Jsir.Var.t * instance option =
  match To_jsir_env.get_code_exn env code_id with
  | Shared shared ->
    let var, res = To_jsir_env.get_shared_closure ~res code_id shared in
    res, var, None
  | Instantiated code ->
    let closure, res = To_jsir_static_const.new_closure ~res code in
    res, closure.var, Some { closure; code }

(* Allocate fresh closures for any [Instantiated] code in [soc] and register
   this set's slots on each new closure and on [symbols] (the static closure
   symbol variables, if any). Returns the instances for the caller to translate
   and bind, along with the closure variable for each function declaration in
   order ([None] for deleted code). *)
let prepare ~env ~res ~symbols soc =
  let decls =
    Set_of_closures.function_decls soc
    |> Function_declarations.funs_in_order |> Function_slot.Lmap.bindings
  in
  let res, closures =
    List.fold_left_map
      (fun res (slot, decl) ->
        match
          (decl : Function_declarations.code_id_in_function_declaration)
        with
        | Deleted _ -> res, None
        | Code_id { code_id; only_full_applications = _ } ->
          let res, var, instance = closure_for_code ~env ~res code_id in
          name_closure slot var;
          res, Some (slot, var, instance))
      res decls
  in
  let closure_vars =
    List.map (Option.map (fun (_slot, var, _instance) -> var)) closures
  in
  let closures = List.filter_map Fun.id closures in
  let function_slots =
    List.map (fun (slot, var, _instance) -> slot, var) closures
    |> Function_slot.Map.of_list
  in
  let instances =
    List.filter_map (fun (_slot, _var, instance) -> instance) closures
  in
  let used_value_slots = used_value_slots ~env soc in
  let value_slots, res =
    Value_slot.Map.fold
      (fun slot simple (value_slots, res) ->
        if Value_slot.Set.mem slot used_value_slots
        then
          let var, res = To_jsir_shared.simple ~env ~res simple in
          Value_slot.Map.add slot var value_slots, res
        else value_slots, res)
      (Set_of_closures.value_slots soc)
      (Value_slot.Map.empty, res)
  in
  let slots : To_jsir_env.slots = { function_slots; value_slots } in
  (* Do not attach slots to [Shared] closures, as they may belong to multiple
     sets of closures and their code never projects from them. *)
  let env =
    List.fold_left
      (fun env closure -> To_jsir_env.add_closure_slots env ~closure slots)
      env
      (symbols @ List.map (fun { closure; code = _ } -> closure.var) instances)
  in
  env, res, instances, closure_vars

let translate_instances ~env ~res ~translate_body instances =
  List.fold_left
    (fun res { closure; code } ->
      To_jsir_static_const.code ~env ~res ~translate_body ~closure code)
    res instances

let bind_instances ~res instances =
  List.fold_left
    (fun res { closure; code = _ } ->
      To_jsir_result.add_instr_exn res
        (Let (closure.var, To_jsir_static_const.closure_expr closure)))
    res instances

let dynamic_set_of_closures ~env ~res ~translate_body ~bound_vars soc =
  let env, res, instances, closure_vars = prepare ~env ~res ~symbols:[] soc in
  let env =
    List.fold_left2
      (fun env bound_var closure_var ->
        match closure_var with
        | None -> env
        | Some closure_var ->
          To_jsir_env.add_var env (Bound_var.var bound_var) closure_var)
      env bound_vars closure_vars
  in
  let res = translate_instances ~env ~res ~translate_body instances in
  env, bind_instances ~res instances

let closure_symbol_locs ~env ~closure_symbols soc =
  let funs = Set_of_closures.function_decls soc |> Function_declarations.funs in
  Function_slot.Lmap.fold
    (fun slot symbol locs ->
      match
        (Function_slot.Map.find slot funs
          : Function_declarations.code_id_in_function_declaration)
      with
      | Deleted _ -> locs
      | Code_id { code_id; only_full_applications = _ } -> (
        let loc =
          match To_jsir_env.get_code_exn env code_id with
          | Shared (Current_unit closure) -> closure.loc
          | Shared (Other_unit code_metadata) ->
            To_jsir_static_const.closure_loc code_metadata
          | Instantiated code ->
            To_jsir_static_const.closure_loc (Code0.code_metadata code)
        in
        match loc with
        | None -> locs
        | Some loc -> Symbol.Map.add symbol loc locs))
    closure_symbols Symbol.Map.empty

let prepare_static_set_of_closures ~env ~res ~closure_symbols soc =
  let symbols, res =
    List.fold_right
      (fun symbol (symbols, res) ->
        let var, res = To_jsir_env.get_symbol_exn env ~res symbol in
        var :: symbols, res)
      (Function_slot.Lmap.data closure_symbols)
      ([], res)
  in
  let env, res, instances, closure_vars = prepare ~env ~res ~symbols soc in
  let aliases =
    List.fold_left2
      (fun aliases symbol closure_var ->
        match closure_var with
        | None -> aliases
        | Some closure_var ->
          { To_jsir_result.bound_var = symbol; defining_var = closure_var }
          :: aliases)
      [] symbols closure_vars
  in
  env, res, { instances; aliases }
