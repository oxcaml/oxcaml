let static_const_not_supported () =
  Misc.fatal_error "This static_const is not yet supported."

let const_or_var ~env ~res ~symbol ~to_jsir_const
    (const_or_var : 'a Or_variable.t) =
  match const_or_var with
  | Const c ->
    To_jsir_shared.bind_expr_to_symbol ~env ~res symbol
      (Constant (to_jsir_const c))
  | Var (var, _dbg) ->
    To_jsir_env.add_symbol_alias_of_var_exn env ~symbol ~alias_of:var, res

let const_or_var_to_var ~env ~res ~to_jsir_const
    (const_or_var : 'a Or_variable.t) =
  match const_or_var with
  | Const c ->
    let var = Jsir.Var.fresh () in
    ( var,
      env,
      To_jsir_result.add_instr_exn res (Let (var, Constant (to_jsir_const c))) )
  | Var (var, _dbg) -> To_jsir_env.get_var_exn env var, env, res

let const_or_vars_to_vars ~env ~res ~to_jsir_const const_or_vars =
  let vars, env, res =
    List.fold_right
      (fun v (vars, env, res) ->
        let var, env, res = const_or_var_to_var ~env ~res ~to_jsir_const v in
        var :: vars, env, res)
      const_or_vars ([], env, res)
  in
  Array.of_list vars, env, res

let float32_to_jsir_const float32 : Jsir.constant =
  Float32
    (Numeric_types.Float32_by_bit_pattern.to_bits float32 |> Int64.of_int32)

let float_to_jsir_const float : Jsir.constant =
  Float32 (Numeric_types.Float_by_bit_pattern.to_bits float)

let int32_to_jsir_const int32 : Jsir.constant = Int32 int32

let int64_to_jsir_const int64 : Jsir.constant = Int64 int64

let nativeint_to_jsir_const nativeint : Jsir.constant =
  Int32 (Targetint_32_64.to_int32 nativeint)

let block_like ~env ~res symbol (const : Static_const.t) =
  match const with
  | Set_of_closures _closures ->
    Misc.fatal_errorf
      "Cannot translate %a: expected a block-like static const, instead found \
       Set_of_closures"
      Static_const.print const
  | Block (tag, mut, _shape, fields) ->
    let expr, env, res =
      To_jsir_shared.block ~env ~res ~tag ~mut
        ~fields:(List.map Simple.With_debuginfo.simple fields)
    in
    To_jsir_shared.bind_expr_to_symbol ~env ~res symbol expr
  | Boxed_float32 value ->
    const_or_var ~env ~res ~symbol ~to_jsir_const:float32_to_jsir_const value
  | Boxed_float value ->
    const_or_var ~env ~res ~symbol ~to_jsir_const:float_to_jsir_const value
  | Boxed_int32 value ->
    const_or_var ~env ~res ~symbol ~to_jsir_const:int32_to_jsir_const value
  | Boxed_int64 value ->
    const_or_var ~env ~res ~symbol ~to_jsir_const:int64_to_jsir_const value
  | Boxed_nativeint value ->
    const_or_var ~env ~res ~symbol ~to_jsir_const:nativeint_to_jsir_const value
  | Boxed_vec128 _ | Boxed_vec256 _ | Boxed_vec512 _ ->
    (* Need SIMD *)
    static_const_not_supported ()
  | Immutable_float_block values | Immutable_float_array values ->
    let vars, env, res =
      const_or_vars_to_vars ~env ~res ~to_jsir_const:float_to_jsir_const values
    in
    To_jsir_shared.bind_expr_to_symbol ~env ~res symbol
      (Block (Tag.double_array_tag |> Tag.to_int, vars, Array, Immutable))
  | Immutable_float32_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_int32_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_int64_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_nativeint_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_vec128_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_vec256_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_vec512_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_value_array values ->
    ignore values;
    static_const_not_supported ()
  | Empty_array kind ->
    ignore kind;
    static_const_not_supported ()
  | Mutable_string { initial_value } ->
    ignore initial_value;
    static_const_not_supported ()
  | Immutable_string value ->
    To_jsir_shared.bind_expr_to_symbol ~env ~res symbol
      (Constant (String value))

let code ~env ~res ~translate_body ~code_id code =
  let free_names = Code0.free_names code in
  let function_slots = Name_occurrences.all_function_slots free_names in
  let value_slots = Name_occurrences.all_value_slots free_names in
  let symbols = Name_occurrences.symbols free_names in
  (* We create new variables that represent each function slot, value slot and
     symbol (for static mutually-recursive code blocks) if they don't exist
     already, and use them everywhere that the corresponding slot is used. We
     will make sure later (when translating [Set_of_closures]) that the closures
     or values representing these slots are bound to the correct variables. *)
  let env =
    Function_slot.Set.fold
      (fun slot env -> To_jsir_env.add_function_slot_if_not_found env slot)
      function_slots env
  in
  let env =
    Value_slot.Set.fold
      (fun slot env -> To_jsir_env.add_value_slot_if_not_found env slot)
      value_slots env
  in
  let env =
    Symbol.Set.fold
      (fun symbol env -> To_jsir_env.add_symbol_if_not_found env symbol)
      symbols env
  in
  let params_and_body = Code0.params_and_body code in
  Flambda.Function_params_and_body.pattern_match params_and_body
    ~f:(fun
         ~return_continuation
         ~exn_continuation
         bound_params
         ~body
         ~my_closure:_
         ~is_my_closure_used:_
         ~my_region:_
         ~my_ghost_region:_
         ~my_depth:_
         ~free_names_of_body:_
       ->
      let fn_params, env_with_params =
        To_jsir_shared.bound_parameters ~env bound_params
      in
      let res, addr = To_jsir_result.new_block res ~params:[] in
      let _env_with_params, res =
        (* Throw away the environment after translating the body *)
        translate_body
          ~env:
            (To_jsir_env.enter_function_body env_with_params
               ~return_continuation ~exn_continuation)
          ~res body
      in
      let env = To_jsir_env.add_code_id env code_id ~addr ~params:fn_params in
      env, res)
