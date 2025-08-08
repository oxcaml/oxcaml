open! Flambda.Import

(** Bind a fresh variable to the result of translating [simple] into JSIR, and
    map [fvar] to this new variable in the environment. *)
let create_let_simple ~env ~res fvar simple =
  Simple.pattern_match' simple
    ~var:(fun name ~coercion:_ ->
      let env = To_jsir_env.add_alias_of_var_exn env ~var:fvar ~alias_of:name in
      env, res)
    ~symbol:(fun symbol ~coercion:_ ->
      (* CR selee: come back *)
      let env =
        To_jsir_env.add_alias_of_symbol_exn env ~var:fvar ~alias_of:symbol
      in
      env, res)
    ~const:(fun const ->
      let expr = Jsir.Constant (To_jsir_shared.reg_width_const const) in
      To_jsir_shared.bind_expr_to_var ~env ~res fvar expr)

(** Bind a fresh variable to the result of translating [prim] into JSIR, and
    map [fvar] to this new variable in the environment. *)
let create_let_prim ~env ~res fvar prim dbg =
  let jvar, env, res = To_jsir_primitive.primitive ~env ~res prim dbg in
  To_jsir_env.add_var env fvar jvar, res

(** Apply the function pointed to by the variable in [f] with [args], bind the
    return value to a variable, and return the variable and the result. *)
let apply_fn ~res ~f ~args ~exact =
  let apply : Jsir.expr = Apply { f; args; exact } in
  let var = Jsir.Var.fresh () in
  let res = To_jsir_result.add_instr_exn res (Let (var, apply)) in
  var, res

let rec expr ~env ~res e =
  match Expr.descr e with
  | Let e' -> let_expr ~env ~res e'
  | Let_cont e' -> let_cont ~env ~res e'
  | Apply e' -> apply_expr ~env ~res e'
  | Apply_cont e' -> apply_cont ~env ~res e'
  | Switch e' -> switch ~env ~res e'
  | Invalid { message } -> invalid ~env ~res message

and let_expr ~env ~res e =
  Let.pattern_match' e
    ~f:(fun bound_pattern ~num_normal_occurrences_of_bound_vars ~body ->
      ignore num_normal_occurrences_of_bound_vars;
      match Bound_pattern.name_mode bound_pattern with
      | Normal ->
        let_expr_normal ~env ~res e ~bound_pattern
          ~num_normal_occurrences_of_bound_vars ~body
      | Phantom -> expr ~env ~res body
      | In_types ->
        Misc.fatal_errorf "Cannot bind In_types variables in terms:@ %a"
          Let.print e)

and let_expr_normal ~env ~res e ~(bound_pattern : Bound_pattern.t)
    ~num_normal_occurrences_of_bound_vars:_ ~body =
  let env, res =
    match bound_pattern, Let.defining_expr e with
    | Singleton v, Simple s ->
      let fvar = Bound_var.var v in
      create_let_simple ~env ~res fvar s
    | Singleton v, Prim (p, dbg) ->
      let fvar = Bound_var.var v in
      create_let_prim ~env ~res fvar p dbg
    | Set_of_closures bound_vars, Set_of_closures soc ->
      To_jsir_set_of_closures.dynamic_set_of_closures ~env ~res ~bound_vars soc
    | Static bound_static, Static_consts consts ->
      (* To translate closures, we require that code is translated before the
         corresponding closure, so that the translation environment is set
         correctly. Code usually does come before it is used in a closure, but
         for static lets, they may be defined within the same let binding.
         Hence, we need to run two passes: one only looking at the code, and one
         skipping the code.

         CR selee: This is slightly tragic, maybe there's a cleaner solution *)
      let env, res =
        Static_const_group.match_against_bound_static consts bound_static
          ~init:(env, res)
          ~code:(fun (env, res) code_id code ->
            To_jsir_static_const.code ~env ~res ~translate_body:expr ~code_id
              code)
          ~deleted_code:(fun (env, res) _code_id -> env, res)
          ~set_of_closures:(fun (env, res) ~closure_symbols:_ _soc -> env, res)
          ~block_like:(fun (env, res) _symbol _static_const -> env, res)
      in
      Static_const_group.match_against_bound_static consts bound_static
        ~init:(env, res)
        ~code:(fun (env, res) _code_id _code -> env, res)
        ~deleted_code:(fun (env, res) _code_id -> env, res)
        ~set_of_closures:(fun (env, res) ~closure_symbols soc ->
          To_jsir_set_of_closures.static_set_of_closures ~env ~res
            ~closure_symbols soc)
        ~block_like:(fun (env, res) symbol static_const ->
          To_jsir_static_const.block_like ~env ~res symbol static_const)
    | Singleton _, Rec_info _ -> expr ~env ~res body
    | Singleton _, (Set_of_closures _ | Static_consts _)
    | Set_of_closures _, (Simple _ | Prim _ | Static_consts _ | Rec_info _)
    | Static _, (Simple _ | Prim _ | Set_of_closures _ | Rec_info _) ->
      Misc.fatal_errorf "Mismatch between pattern and defining expression:@ %a"
        Let.print e
  in
  expr ~env ~res body

and let_cont ~env ~res (e : Flambda.Let_cont_expr.t) =
  match e with
  | Non_recursive
      { handler; num_free_occurrences = _; is_applied_with_traps = _ } ->
    Non_recursive_let_cont_handler.pattern_match handler ~f:(fun k ~body ->
        let handler = Non_recursive_let_cont_handler.handler handler in
        Continuation_handler.pattern_match handler
          ~f:(fun params ~handler:cont_body ->
            let params, env = To_jsir_shared.bound_parameters ~env params in
            let res, addr = To_jsir_result.new_block res ~params in
            let _env, res = expr ~env ~res cont_body in
            let env = To_jsir_env.add_continuation env k addr in
            expr ~env ~res body))
  | Recursive handlers ->
    Recursive_let_cont_handlers.pattern_match handlers
      ~f:(fun ~invariant_params ~body conts ->
        if Continuation_handlers.contains_exn_handler conts
        then
          Misc.fatal_errorf
            "Recursive continuation bindings cannot involve exception \
             handlers:@ %a"
            Let_cont.print e;
        let domain = Flambda.Continuation_handlers.domain conts in
        let env, res =
          (* See explanation in [To_jsir_static_const.code]: we first reserve
             addresses representing each continuation, so that mutually
             recursive continuations can refer to them. *)
          List.fold_left
            (fun (env, res) k ->
              let res, addr = To_jsir_result.reserve_address res in
              let env = To_jsir_env.add_continuation env k addr in
              env, res)
            (env, res) domain
        in
        let res =
          Continuation.Lmap.fold
            (fun k handler res ->
              Continuation_handler.pattern_match handler
                ~f:(fun params ~handler:cont_body ->
                  let params, env =
                    To_jsir_shared.bound_parameters ~env
                      (Bound_parameters.append invariant_params params)
                  in
                  let addr =
                    match To_jsir_env.get_continuation_exn env k with
                    | Return | Exception ->
                      Misc.fatal_errorf
                        "Continuation %a is unexpectedly a return or exception \
                         continuation of the current environment"
                        Continuation.print k
                    | Block addr -> addr
                  in
                  let res =
                    To_jsir_result.new_block_with_addr_exn res ~params ~addr
                  in
                  let _env, res = expr ~env ~res cont_body in
                  res))
            (Flambda.Continuation_handlers.to_map conts)
            res
        in
        expr ~env ~res body)

and apply_expr ~env ~res e =
  let continuation = Apply_expr.continuation e in
  let exn_continuation = Apply_expr.exn_continuation e in
  let call_kind = Apply_expr.call_kind e in
  if Exn_continuation.exn_handler exn_continuation
     <> To_jsir_env.exn_continuation env
  then failwith "unimplemented for now";
  let var, res =
    match Apply_expr.callee e, call_kind with
    | None, _ | _, Effect _ -> failwith "effects not implemented yet"
    | Some callee, (Function _ | Method _) ->
      let args, res = To_jsir_shared.simples ~env ~res (Apply_expr.args e) in
      let f, res = To_jsir_shared.simple ~env ~res callee in
      (* CR selee: assume exact = false for now, JSIR seems to assume false in
         the case that we don't know *)
      apply_fn ~res ~f ~args ~exact:false
    | Some callee, C_call _ ->
      let args, res = To_jsir_shared.simples ~env ~res (Apply_expr.args e) in
      let symbol =
        match Simple.must_be_symbol callee with
        | Some (symbol, _coercion) -> symbol
        | None ->
          Misc.fatal_errorf
            "Expected callee to be a symbol for C calls, instead found %a"
            Simple.print callee
      in
      let name = Symbol.linkage_name symbol |> Linkage_name.to_string in
      let expr : Jsir.expr =
        Prim (Extern name, List.map (fun arg : Jsir.prim_arg -> Pv arg) args)
      in
      let var = Jsir.Var.fresh () in
      let res = To_jsir_result.add_instr_exn res (Let (var, expr)) in
      var, res
  in
  match continuation with
  | Never_returns ->
    env, To_jsir_result.end_block_with_last_exn res (Return var)
  | Return cont -> (
    match To_jsir_env.get_continuation_exn env cont with
    | Exception ->
      failwith "unimplemented" (* CR selee: or should be disallowed? *)
    | Return -> env, To_jsir_result.end_block_with_last_exn res (Return var)
    | Block addr ->
      env, To_jsir_result.end_block_with_last_exn res (Branch (addr, [var])))

and apply_cont0 ~env ~res apply_cont : Jsir.last * To_jsir_result.t =
  let args, res =
    To_jsir_shared.simples ~env ~res (Apply_cont.args apply_cont)
  in
  match Apply_cont.trap_action apply_cont with
  | None -> (
    let continuation = Apply_cont.continuation apply_cont in
    let res_cont = To_jsir_env.get_continuation_exn env continuation in
    match (res_cont : To_jsir_env.continuation) with
    | Return -> (
      match Continuation.sort continuation with
      | Toplevel_return ->
        assert (List.length args = 1);
        (* CR selee: This is a hack, but I can't find a way to trigger any
           behaviour that isn't calling [caml_register_global] on the toplevel
           module. I suspect for single-file compilation this is always fine.
           Will come back and review later. *)
        let compilation_unit =
          To_jsir_env.module_symbol env |> Symbol.compilation_unit
        in
        let module_name =
          Compilation_unit.name compilation_unit
          |> Compilation_unit.Name.to_string
        in
        let var = Jsir.Var.fresh () in
        let res =
          To_jsir_result.add_instr_exn res
            (Jsir.Let
               ( var,
                 Prim
                   ( Extern "caml_register_global",
                     [ Pc (Int (Targetint.of_int_exn 0));
                       Pv (List.hd args);
                       Pc
                         (* CR selee: this assumes javascript, WASM needs just
                            String *)
                         (NativeString
                            (Jsir.Native_string.of_string module_name)) ] ) ))
        in
        Stop, res
      | Return ->
        if List.length args <> 1
        then Misc.fatal_error "Currently only one return argument is supported";
        Jsir.Return (List.hd args), res
      | Normal_or_exn | Define_root_symbol ->
        (* CR selee: seems odd, review later *)
        Misc.fatal_errorf "Unexpected continuation sort for continuation %a"
          Continuation.print continuation)
    | Exception -> failwith "unimplemented"
    | Block addr -> Jsir.Branch (addr, args), res)
  | Some (Push { exn_handler }) ->
    ignore exn_handler;
    failwith "unimplemented"
  | Some (Pop { exn_handler; raise_kind }) ->
    ignore (exn_handler, raise_kind);
    failwith "unimplemented"

and apply_cont ~env ~res apply_cont =
  let last, res = apply_cont0 ~env ~res apply_cont in
  env, To_jsir_result.end_block_with_last_exn res last

and switch ~env ~res e =
  let scrutinee, res =
    To_jsir_shared.simple ~env ~res (Switch_expr.scrutinee e)
  in
  let arms = Switch_expr.arms e in
  let domain = Targetint_31_63.Map.keys arms in
  let min = Targetint_31_63.Set.min_elt domain |> Targetint_31_63.to_int in
  let max = Targetint_31_63.Set.max_elt domain |> Targetint_31_63.to_int in
  (* Flambda2 allows the domain to be arbitrary non-negative subsets of
     targetint, whereas JSIR requires [0..n].

     We assume that the max value isn't too high, and just pad the out-of-domain
     cases to invalid blocks. *)
  assert (min >= 0);
  let res, arms =
    Array.fold_left_map
      (fun res i ->
        let apply_cont = Targetint_31_63.Map.find_opt i arms in
        let last, res =
          match apply_cont with
          | Some apply_cont -> apply_cont0 ~env ~res apply_cont
          | None ->
            let res, addr = To_jsir_result.invalid_switch_block res in
            (Branch (addr, []) : Jsir.last), res
        in
        match last with
        | Branch cont -> res, cont
        | Return _ | Raise _ | Stop | Cond _ | Switch _ | Pushtrap _ | Poptrap _
          ->
          let res, addr = To_jsir_result.new_block res ~params:[] in
          let res = To_jsir_result.end_block_with_last_exn res last in
          res, (addr, []))
      res
      (Array.init (max + 1) Targetint_31_63.of_int)
  in
  let last : Jsir.last =
    match Array.length arms with
    | 2 ->
      (* Special case: turn it into if/then/else *)
      Cond (scrutinee, arms.(1), arms.(0))
    | _ -> Switch (scrutinee, arms)
  in
  env, To_jsir_result.end_block_with_last_exn res last

and invalid ~env ~res _msg = env, res

let unit ~offsets:_ ~all_code:_ ~reachable_names:_ flambda_unit =
  let env =
    To_jsir_env.create
      ~module_symbol:(Flambda_unit.module_symbol flambda_unit)
      ~return_continuation:(Flambda_unit.return_continuation flambda_unit)
      ~exn_continuation:(Flambda_unit.exn_continuation flambda_unit)
  in
  let res = To_jsir_result.create () in
  let res, _addr = To_jsir_result.new_block res ~params:[] in
  let _env, res = expr ~env ~res (Flambda_unit.body flambda_unit) in
  let program = To_jsir_result.to_program_exn res in
  Jsir.invariant program;
  program
