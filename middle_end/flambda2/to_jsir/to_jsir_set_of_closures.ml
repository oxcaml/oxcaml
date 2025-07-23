let set_of_closures ~env ~res ~bindings ~add_to_env soc =
  let fun_decls = Set_of_closures.function_decls soc in
  let decls =
    Function_declarations.funs_in_order fun_decls |> Function_slot.Lmap.to_seq
  in
  let env, res =
    Seq.fold_left2
      (fun (env, res) binding (slot, decl) ->
        match
          (decl : Function_declarations.code_id_in_function_declaration)
        with
        | Deleted _ -> env, res
        | Code_id { code_id; only_full_applications = _ } ->
          (* CR selee: thread through debug information *)
          let addr, params = To_jsir_env.get_code_id_exn env code_id in
          let expr : Jsir.expr = Closure (params, (addr, []), None) in
          (* If this function slot is used, it should've already been added when
             the code using it was defined *)
          (* CR selee: check *)
          let env, fn_var =
            match To_jsir_env.get_function_slot_exn env slot with
            | fn_var -> env, fn_var
            | exception Not_found ->
              (* This function slot is not used anywhere, so we are free to make
                 a new variable *)
              let var = Jsir.Var.fresh () in
              To_jsir_env.add_function_slot env slot var, var
          in
          let res = To_jsir_result.add_instr_exn res (Let (fn_var, expr)) in
          add_to_env ~env ~res binding fn_var)
      (env, res) bindings decls
  in
  Value_slot.Map.fold
    (fun slot simple (env, res) ->
      match To_jsir_env.get_value_slot_exn env slot with
      | var ->
        let simple_var, res = To_jsir_shared.simple ~env ~res simple in
        (* This value slot has been used in the function body, so we should set
           the used variable to be the appropriate [Simple.t] *)
        env, To_jsir_result.add_instr_exn res (Assign (var, simple_var))
      | exception Not_found ->
        (* This value slot is not used, so we don't need to do anything *)
        env, res)
    (Set_of_closures.value_slots soc)
    (env, res)

let dynamic_set_of_closures ~env ~res ~bound_vars soc =
  let vars = List.to_seq bound_vars |> Seq.map Bound_var.var in
  set_of_closures ~env ~res ~bindings:vars
    ~add_to_env:(fun ~env ~res var fn_var ->
      To_jsir_env.add_var env var fn_var, res)
    soc

let static_set_of_closures ~env ~res ~closure_symbols soc =
  let add_to_env ~env ~res symbol fn_var =
    (* We may have already encountered the symbol when translating the code, so
       we should check first whether a variable already exists; if so, we should
       make sure that the variable also points to the function variable *)
    match To_jsir_env.get_symbol_exn env symbol with
    | var -> env, To_jsir_result.add_instr_exn res (Assign (var, fn_var))
    | exception Not_found -> To_jsir_env.add_symbol env symbol fn_var, res
  in
  let symbols = Function_slot.Lmap.to_seq closure_symbols |> Seq.map snd in
  set_of_closures ~env ~res ~bindings:symbols ~add_to_env soc
