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

type initialisation =
  { value_slot_bindings : To_jsir_result.var_binding list;
    closures : To_jsir_shared.expr_binding list;
    aliases : To_jsir_result.var_binding list
  }

let prepare ~env ~res ~bindings ~add_to_env soc =
  let fun_decls = Set_of_closures.function_decls soc in
  let decls =
    Function_declarations.funs_in_order fun_decls |> Function_slot.Lmap.bindings
  in
  (* Return value-slot bindings, closures and alias bindings separately, as
     static groups may need to emit several sets of recursive closures as one
     consecutive run (since JSOO allows closures to refer to one another only if
     they are consecutive). *)
  let value_slot_bindings, res =
    Value_slot.Map.fold
      (fun slot simple (bindings, res) ->
        match To_jsir_env.get_value_slot env slot with
        | Some var ->
          (* This value slot has been used in the function body, so we should
             bind the used variable to the appropriate [Simple.t] *)
          let simple_var, res = To_jsir_shared.simple ~env ~res simple in
          ( { To_jsir_result.bound_var = var; defining_var = simple_var }
            :: bindings,
            res )
        | None ->
          (* This value slot is not used, so we don't need to do anything *)
          bindings, res)
      (Set_of_closures.value_slots soc)
      ([], res)
  in
  let env, res, closures, aliases =
    List.fold_left2
      (fun (env, res, closures, aliases) binding (slot, decl) ->
        match
          (decl : Function_declarations.code_id_in_function_declaration)
        with
        | Deleted _ -> env, res, closures, aliases
        | Code_id { code_id; only_full_applications = _ } ->
          let ({ addr;
                 params;
                 closure = fn_var;
                 closure_bound_with_code;
                 hint;
                 loc
               }
                : To_jsir_env.code_id) =
            To_jsir_env.get_code_id_exn env code_id
          in
          let closures =
            if closure_bound_with_code
            then closures (* The code definition will later bind its closure. *)
            else
              let expr : Jsir.expr =
                Closure (params, (addr, []), (hint, loc))
              in
              { To_jsir_shared.bound_var = fn_var; defining_expr = expr }
              :: closures
          in
          (* If this function slot is used, its corresponding variable should've
             already been added to the environment when the code using it was
             translated. We should make sure that this matches up with our
             preemptively declared closure variable for this closure. *)
          let aliases =
            match To_jsir_env.get_function_slot env slot with
            | None -> aliases
            | Some fn_var' ->
              { To_jsir_result.bound_var = fn_var'; defining_var = fn_var }
              :: aliases
          in
          let env, res, aliases' = add_to_env ~env ~res binding fn_var in
          env, res, closures, aliases' @ aliases)
      (env, res, [], []) bindings decls
  in
  env, res, { value_slot_bindings; closures = List.rev closures; aliases }

let dynamic_set_of_closures ~env ~res ~bound_vars soc =
  let vars = List.map Bound_var.var bound_vars in
  let env, res, { value_slot_bindings; closures; aliases } =
    prepare ~env ~res ~bindings:vars
      ~add_to_env:(fun ~env ~res var fn_var ->
        To_jsir_env.add_var env var fn_var, res, [])
      soc
  in
  let res =
    List.fold_left
      (fun res { To_jsir_shared.bound_var; defining_expr } ->
        To_jsir_result.add_instr_exn res (Let (bound_var, defining_expr)))
      res closures
  in
  env, To_jsir_result.add_var_aliases_exn res (aliases @ value_slot_bindings)

let prepare_static_set_of_closures ~env ~res ~closure_symbols soc =
  let add_to_env ~env ~res symbol fn_var =
    let var, res = To_jsir_env.get_symbol_exn env ~res symbol in
    env, res, [{ To_jsir_result.bound_var = var; defining_var = fn_var }]
  in
  let symbols = Function_slot.Lmap.data closure_symbols in
  let _env, res, initialisation =
    prepare ~env ~res ~bindings:symbols ~add_to_env soc
  in
  res, initialisation
