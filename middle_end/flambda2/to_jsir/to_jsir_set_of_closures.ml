let set_of_closures ~env ~res ~bindings ~add_to_env soc =
  let fun_decls = Set_of_closures.function_decls soc in
  let decls =
    Function_declarations.funs_in_order fun_decls |> Function_slot.Lmap.bindings
  in
  let env, res =
    List.fold_left2
      (fun (env, res) binding (slot, decl) ->
        match
          (decl : Function_declarations.code_id_in_function_declaration)
        with
        | Deleted _ -> env, res
        | Code_id { code_id; only_full_applications = _ } ->
          (* CR selee: thread through debug information *)
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
  in
  Value_slot.Map.fold
    (fun slot simple (env, res) ->
      match To_jsir_env.get_value_slot env slot with
      | Some var ->
        let simple_var, res = To_jsir_shared.simple ~env ~res simple in
        (* This value slot has been used in the function body, so we should set
           the used variable to be the appropriate [Simple.t] *)
        env, To_jsir_result.add_instr_exn res (Assign (var, simple_var))
      | None ->
        (* This value slot is not used, so we don't need to do anything *)
        env, res)
    (Set_of_closures.value_slots soc)
    (env, res)

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
    match To_jsir_env.get_symbol env symbol with
    | Some var -> env, To_jsir_result.add_instr_exn res (Assign (var, fn_var))
    | None -> To_jsir_env.add_symbol env symbol fn_var, res
  in
  let symbols = Function_slot.Lmap.data closure_symbols in
  set_of_closures ~env ~res ~bindings:symbols ~add_to_env soc
