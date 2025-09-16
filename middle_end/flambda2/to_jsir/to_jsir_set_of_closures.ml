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

let set_of_closures ~env ~res ~bindings ~add_to_env soc =
  let fun_decls = Set_of_closures.function_decls soc in
  let decls =
    Function_declarations.funs_in_order fun_decls |> Function_slot.Lmap.bindings
  in
  (* CR selee: It should be sufficient to have these variables bound before the
     call site of the closures being defined, but it seems that having the
     closure expression before the bindings interact badly with the JSOO
     simplifier when self-recursive functions become tail-call optimised.
     Swapping the order resolved the issue, but we should dig into the intended
     semantics of JSIR more. *)
  let env, res =
    Value_slot.Map.fold
      (fun slot simple (env, res) ->
        match To_jsir_env.get_value_slot env slot with
        | Some var ->
          let simple_var, res = To_jsir_shared.simple ~env ~res simple in
          (* This value slot has been used in the function body, so we should
             set the used variable to be the appropriate [Simple.t] *)
          env, To_jsir_result.add_instr_exn res (Assign (var, simple_var))
        | None ->
          (* This value slot is not used, so we don't need to do anything *)
          env, res)
      (Set_of_closures.value_slots soc)
      (env, res)
  in
  List.fold_left2
    (fun (env, res) binding (slot, decl) ->
      match (decl : Function_declarations.code_id_in_function_declaration) with
      | Deleted _ -> env, res
      | Code_id { code_id; only_full_applications = _ } ->
        let ({ addr; params; closure = fn_var } : To_jsir_env.code_id) =
          To_jsir_env.get_code_id_exn env code_id
        in
        let expr : Jsir.expr = Closure (params, (addr, []), None) in
        (* If this function slot is used, its corresponding variable should've
           already been added to the environment when the code using it was
           translated. We should make sure that this matches up with our
           preemptively declared closure variable for this closure. *)
        let res =
          match To_jsir_env.get_function_slot env slot with
          | None -> res
          | Some fn_var' ->
            To_jsir_result.add_instr_exn res (Assign (fn_var', fn_var))
        in
        let res = To_jsir_result.add_instr_exn res (Let (fn_var, expr)) in
        add_to_env ~env ~res binding fn_var)
    (env, res) bindings decls

let dynamic_set_of_closures ~env ~res ~bound_vars soc =
  let vars = List.map Bound_var.var bound_vars in
  set_of_closures ~env ~res ~bindings:vars
    ~add_to_env:(fun ~env ~res var fn_var ->
      To_jsir_env.add_var env var fn_var, res)
    soc

let static_set_of_closures ~env ~res ~closure_symbols soc =
  let add_to_env ~env ~res symbol fn_var =
    (* We may have already encountered the symbol when translating the code, so
       we should check first whether a variable already exists; if so, we should
       make sure that the variable also points to the function variable *)
    match To_jsir_env.get_symbol env ~res symbol with
    | Some (var, res) ->
      env, To_jsir_result.add_instr_exn res (Assign (var, fn_var))
    | None -> To_jsir_env.add_symbol env ~res symbol fn_var
  in
  let symbols = Function_slot.Lmap.data closure_symbols in
  set_of_closures ~env ~res ~bindings:symbols ~add_to_env soc
