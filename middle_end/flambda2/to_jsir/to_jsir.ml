open! Flambda2_terms.Flambda.Import

(** Bind a fresh JSIR variable to [expr], and map [fvar] to this new variable in the
    environment. *)
let bind_expr_to_var ~env ~res fvar expr =
  let jvar = Jsir.Var.fresh () in
  ( To_jsir_env.add_var env fvar jvar,
    To_jsir_result.add_instr res (Jsir.Let (jvar, expr)) )

(** Bind a fresh JSIR variable to [expr], and map [fvar] to this new variable in the
    environment. *)
let bind_expr_to_symbol ~env ~res symbol expr =
  let jvar = Jsir.Var.fresh () in
  ( To_jsir_env.add_symbol env symbol jvar,
    To_jsir_result.add_instr res (Jsir.Let (jvar, expr)) )

(** Bind a fresh variable to the result of translating [simple] into JSIR, and
    map [fvar] to this new variable in the environment. *)
let create_let_simple ~env ~res fvar simple =
  Flambda2_term_basics.Simple.pattern_match' simple
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
      bind_expr_to_var ~env ~res fvar expr)

(** Bind a fresh variable to the result of translating [prim] into JSIR, and
    map [fvar] to this new variable in the environment. *)
let create_let_prim ~env ~res fvar prim dbg =
  let expr, env, res = To_jsir_primitive.primitive ~env ~res prim dbg in
  bind_expr_to_var ~env ~res fvar expr

(** Bind a fresh variable to the result of translating [const] into JSIR, and
    map [symbol] to this new variable in the environment.*)
let create_let_block_like ~env ~res symbol const =
  let expr, env, res = To_jsir_static_const.block_like ~env ~res const in
  bind_expr_to_symbol ~env ~res symbol expr

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
      match
        Flambda2_bound_identifiers.Bound_pattern.name_mode bound_pattern
      with
      | Normal ->
        let_expr_normal ~env ~res e ~bound_pattern
          ~num_normal_occurrences_of_bound_vars ~body
      | Phantom -> expr ~env ~res body
      | In_types ->
        Misc.fatal_errorf "Cannot bind In_types variables in terms:@ %a"
          Let.print e)

and let_expr_normal ~env ~res e
    ~(bound_pattern : Flambda2_bound_identifiers.Bound_pattern.t)
    ~num_normal_occurrences_of_bound_vars ~body =
  (* CR selee: translate to [Let] *)
  ignore num_normal_occurrences_of_bound_vars;
  let env, res =
    match bound_pattern, Let.defining_expr e with
    | Singleton v, Simple s ->
      let fvar = Flambda2_bound_identifiers.Bound_var.var v in
      create_let_simple ~env ~res fvar s
    | Singleton v, Prim (p, dbg) ->
      let fvar = Flambda2_bound_identifiers.Bound_var.var v in
      create_let_prim ~env ~res fvar p dbg
    | Set_of_closures bound_vars, Set_of_closures soc ->
      ignore (bound_vars, soc);
      failwith "unimplemented"
    | Static bound_static, Static_consts consts ->
      Static_const_group.match_against_bound_static consts bound_static
        ~init:(env, res)
        ~code:(fun (env, res) code_id code ->
          ignore (env, res, code_id, code);
          failwith "unimplemented")
        ~deleted_code:(fun (env, res) code_id ->
          ignore (env, res, code_id);
          failwith "unimplemented")
        ~set_of_closures:(fun (env, res) ~closure_symbols soc ->
          ignore (env, res, closure_symbols, soc);
          failwith "unimplemented")
        ~block_like:(fun (env, res) symbol static_const ->
          create_let_block_like ~env ~res symbol static_const)
    | Singleton _, Rec_info _ -> expr ~env ~res body
    | Singleton _, (Set_of_closures _ | Static_consts _)
    | Set_of_closures _, (Simple _ | Prim _ | Static_consts _ | Rec_info _)
    | Static _, (Simple _ | Prim _ | Set_of_closures _ | Rec_info _) ->
      Misc.fatal_errorf "Mismatch between pattern and defining expression:@ %a"
        Let.print e
  in
  expr ~env ~res body

and let_cont ~env ~res e =
  (* CR selee: probably make a new block *)
  ignore (env, res, e);
  failwith "unimplemented"

and apply_expr ~env ~res e =
  (* CR selee: translate to [Apply] *)
  ignore (env, res, e);
  failwith "unimplemented"

and apply_cont ~env ~res apply_cont =
  let args, res =
    To_jsir_shared.simples ~env ~res (Apply_cont.args apply_cont)
  in
  let res =
    match Apply_cont.trap_action apply_cont with
    | None ->
      let continuation = Apply_cont.continuation apply_cont in
      let res_cont = To_jsir_env.get_continuation_exn env continuation in
      let (last : Jsir.last) =
        match (res_cont : To_jsir_env.continuation) with
        | Return -> (
          match Flambda2_identifiers.Continuation.sort continuation with
          | Toplevel_return -> Jsir.Stop
          | Return ->
            if List.length args <> 1
            then
              Misc.fatal_error "Currently only one return argument is supported";
            Jsir.Return (List.hd args)
          | Normal_or_exn | Define_root_symbol ->
            (* CR selee: seems odd, review later *)
            Misc.fatal_errorf "Unexpected continuation sort for continuation %a"
              Flambda2_identifiers.Continuation.print continuation)
        | Exception -> failwith "unimplemented"
        | Block addr -> Jsir.Branch (addr, args)
      in
      To_jsir_result.set_last res last
    | Some (Push { exn_handler }) ->
      ignore exn_handler;
      failwith "unimplemented"
    | Some (Pop { exn_handler; raise_kind }) ->
      ignore (exn_handler, raise_kind);
      failwith "unimplemented"
  in
  env, res

and switch ~env ~res e =
  (* CR selee: translate to [Switch], but beware that flambda allows arbitrary
     arms whereas Jsir requires 0..n (double check) *)
  ignore (env, res, e);
  failwith "unimplemented"

and invalid ~env ~res msg =
  (* CR selee: can probably ignore *)
  ignore (env, res, msg);
  failwith "unimplemented"

let unit ~offsets:_ ~all_code:_ ~reachable_names:_ flambda_unit =
  let env =
    To_jsir_env.create
      ~return_continuation:
        (Flambda2_terms.Flambda_unit.return_continuation flambda_unit)
      ~exn_continuation:
        (Flambda2_terms.Flambda_unit.exn_continuation flambda_unit)
  in
  let res = To_jsir_result.create () in
  let _env, res =
    expr ~env ~res (Flambda2_terms.Flambda_unit.body flambda_unit)
  in
  To_jsir_result.to_program_exn res
