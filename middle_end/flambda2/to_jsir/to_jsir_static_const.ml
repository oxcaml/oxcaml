let static_const_not_supported () =
  Misc.fatal_error "This static_const is not yet supported."

let block_like' ~env ~res (const : Static_const.t) :
    Jsir.expr * To_jsir_env.t * To_jsir_result.t =
  match const with
  | Set_of_closures _closures ->
    Misc.fatal_errorf
      "Cannot translate %a: expected a block-like static const, instead found \
       Set_of_closures"
      Static_const.print const
  | Block (tag, mut, _shape, fields) ->
    (* CR selee: is it ok to ignore shape? *)
    let tag = Tag.Scannable.to_int tag in
    let mutability : Jsir.mutability =
      match mut with
      | Mutable -> Maybe_mutable
      | Immutable -> Immutable
      | Immutable_unique ->
        (* CR selee: check *)
        Immutable
    in
    let fields, res =
      To_jsir_shared.simples ~env ~res
        (List.map Simple.With_debuginfo.simple fields)
    in
    Block (tag, Array.of_list fields, NotArray, mutability), env, res
  | Boxed_float32 value ->
    ignore value;
    static_const_not_supported ()
  | Boxed_float value ->
    ignore value;
    static_const_not_supported ()
  | Boxed_int32 value ->
    ignore value;
    static_const_not_supported ()
  | Boxed_int64 value ->
    ignore value;
    static_const_not_supported ()
  | Boxed_nativeint value ->
    ignore value;
    static_const_not_supported ()
  | Boxed_vec128 value ->
    ignore value;
    static_const_not_supported ()
  | Boxed_vec256 value ->
    ignore value;
    static_const_not_supported ()
  | Boxed_vec512 value ->
    ignore value;
    static_const_not_supported ()
  | Immutable_float_block values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_float_array values ->
    ignore values;
    static_const_not_supported ()
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
    ignore value;
    static_const_not_supported ()

let block_like ~env ~res symbol const =
  let expr, env, res = block_like' ~env ~res const in
  To_jsir_shared.bind_expr_to_symbol ~env ~res symbol expr

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
         ~my_closure
         ~is_my_closure_used:_
         ~my_region:_
         ~my_ghost_region:_
         ~my_depth:_
         ~free_names_of_body:_
       ->
      (* CR selee: A hack to get things to work, should figure out what
         [my_closure] is actually used for *)
      let var = Jsir.Var.fresh () in
      let env = To_jsir_env.add_var env my_closure var in
      let env_with_params, fn_params =
        (* The natural fold instead of [List.fold_left] to preserve order of
           parameters *)
        List.fold_right
          (fun bound_param (env, params) ->
            let var = Jsir.Var.fresh () in
            let env =
              To_jsir_env.add_var env (Bound_parameter.var bound_param) var
            in
            env, var :: params)
          (Bound_parameters.to_list bound_params)
          (env, [])
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
