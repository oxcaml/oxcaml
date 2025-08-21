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

open! Flambda.Import
open! Jsoo_imports.Import

(* CR selee: we should eventually get rid of these *)
let unsupported_multiple_return_variables vars =
  match vars with
  | [var] -> var
  | [] -> Misc.fatal_error "Found return statement with no arguments"
  | _ :: _ ->
    Misc.fatal_error "Multiple return variables are currently unsupported."

(** Bind a fresh variable to the result of translating [simple] into JSIR, and
    map [fvar] to this new variable in the environment. *)
let create_let_simple ~env ~res fvar simple =
  Simple.pattern_match' simple
    ~var:(fun name ~coercion:_ ->
      let env =
        To_jsir_env.add_var_alias_of_var_exn env ~var:fvar ~alias_of:name
      in
      env, res)
    ~symbol:(fun symbol ~coercion:_ ->
      To_jsir_env.add_var_alias_of_symbol_exn env ~res ~var:fvar
        ~alias_of:symbol)
    ~const:(fun const ->
      let expr = Jsir.Constant (To_jsir_shared.reg_width_const const) in
      To_jsir_shared.bind_expr_to_var ~env ~res fvar expr)

(** Bind a fresh variable to the result of translating [prim] into JSIR, and
    map [fvar] to this new variable in the environment. *)
let create_let_prim ~env ~res fvar prim =
  let jvar, env, res = To_jsir_primitive.primitive ~env ~res prim in
  match jvar with
  | None -> env, res
  | Some jvar -> To_jsir_env.add_var env fvar jvar, res

(** Apply the function pointed to by the variable in [f] with [args], bind the
    return value to a variable, and return the variable and the result. *)
let apply_fn ~res ~f ~args ~exact =
  let apply : Jsir.expr = Apply { f; args; exact } in
  let var = Jsir.Var.fresh () in
  let res = To_jsir_result.add_instr_exn res (Let (var, apply)) in
  var, res

(** For exception continuations with multiple arguments. See comment in [let_cont]. *)
let assign_extra_args ~env ~res exn_handler extra_args =
  let extra_param_vars =
    if Continuation.equal (To_jsir_env.exn_continuation env) exn_handler
    then
      (* This was passed in through a parameter, so has not been declared via a
         [Let_cont] and expects zero [extra_args]. *)
      []
    else
      let ({ addr = _; exn_param = _; extra_args = extra_params_vars }
            : To_jsir_env.exn_handler) =
        To_jsir_env.get_exn_handler_exn env exn_handler
      in
      extra_params_vars
  in
  match List.length extra_param_vars <> List.length extra_args with
  | true ->
    Misc.fatal_errorf "Exception handler %a expected %d extra_args, got %d"
      Continuation.print exn_handler
      (List.length extra_param_vars)
      (List.length extra_args)
  | false ->
    List.fold_left2
      (fun res param_var arg ->
        match arg with
        | None -> res
        | Some arg ->
          To_jsir_result.add_instr_exn res
            (Set_field (param_var, 0, Non_float, arg)))
      res extra_param_vars extra_args

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
      To_jsir_result.with_debuginfo_exn res dbg ~f:(fun res ->
          create_let_prim ~env ~res fvar p)
    | Set_of_closures bound_vars, Set_of_closures soc ->
      To_jsir_set_of_closures.dynamic_set_of_closures ~env ~res ~bound_vars soc
    | Static bound_static, Static_consts consts ->
      (* Definitions within this group can reference each others' symbols
         (potentially not in the order they were declared in), so we should
         first add the symbols to the environment before doing any translation.
         Crucially, we should not register any symbols at this stage, since they
         will not yet be defined. *)
      let symbols = Bound_static.symbols_being_defined bound_static in
      let env =
        Symbol.Set.fold
          (fun symbol env ->
            To_jsir_env.add_symbol_without_registering env symbol
              (Jsir.Var.fresh ()))
          symbols env
      in
      (* To translate closures, we require that all the code is inserted into
         the environment before any of the actual translation happens. Code
         usually does come before it is used in a closure, but for static lets,
         they may be defined within the same let binding. Moreover, code may
         reference each other's code IDs, not necessarily in the order they are
         declared in. Hence, we run two passes: one inserting code into the
         environment, and one doing the actual translation. *)
      let env, res =
        Static_const_group.match_against_bound_static consts bound_static
          ~init:(env, res)
          ~code:(fun (env, res) code_id code ->
            To_jsir_static_const.prepare_code ~env ~res ~code_id code)
          ~deleted_code:(fun (env, res) _code_id -> env, res)
          ~set_of_closures:(fun (env, res) ~closure_symbols:_ _soc -> env, res)
          ~block_like:(fun (env, res) _symbol _static_const -> env, res)
      in
      let env, res =
        Static_const_group.match_against_bound_static consts bound_static
          ~init:(env, res)
          ~code:(fun (env, res) code_id code ->
            To_jsir_static_const.code ~env ~res ~translate_body:expr ~code_id
              code)
          ~deleted_code:(fun (env, res) _code_id -> env, res)
          ~set_of_closures:(fun (env, res) ~closure_symbols soc ->
            To_jsir_set_of_closures.static_set_of_closures ~env ~res
              ~closure_symbols soc)
          ~block_like:(fun (env, res) symbol static_const ->
            To_jsir_static_const.block_like ~env ~res symbol static_const)
      in
      (* Now that the symbols are defined, we must remember to register all the
         symbols to the global symbol table. *)
      let res =
        Symbol.Set.fold
          (fun symbol res -> To_jsir_env.register_symbol_exn env ~res symbol)
          symbols res
      in
      env, res
    | Singleton _, Rec_info _ -> expr ~env ~res body
    | Singleton _, (Set_of_closures _ | Static_consts _)
    | Set_of_closures _, (Simple _ | Prim _ | Static_consts _ | Rec_info _)
    | Static _, (Simple _ | Prim _ | Set_of_closures _ | Rec_info _) ->
      Misc.fatal_errorf "Mismatch between pattern and defining expression:@ %a"
        Let.print e
  in
  expr ~env ~res body

and let_cont ~env ~res (e : Flambda.Let_cont_expr.t) =
  (* Flambda exception handlers have [extra_args], which are used to pass in
     specific instances of mutable values (that become decomposed into many
     immutable values) to the exception handler.

     Since JSIR Raise only has one argument, we use mutable variables to pass in
     the extra_args, and make sure that they are set properly before the
     Apply_cont containing the Exn_handler containing the extra_args is
     applied. *)
  let make_mutables ~res n =
    let extra_args_boxed = List.init n (fun _ -> Jsir.Var.fresh ()) in
    let res =
      List.fold_left
        (fun res var ->
          let null = Jsir.Var.fresh () in
          let res =
            To_jsir_result.add_instr_exn res (Let (null, Constant Null))
          in
          To_jsir_result.add_instr_exn res
            (Let (var, Block (0, [| null |], NotArray, Maybe_mutable))))
        res extra_args_boxed
    in
    extra_args_boxed, res
  in
  (* A note on evaluation order: let_cont k = [handler] in [body] should be more
     interpreted like [body] where k = [handler], and control flow flows from
     [body] to [handler]. While variables aren't captured in continuations and
     must be passed explicitly, symbols defined in [body] are scoped in
     [handler].

     Hence, we translate [body] before [handler], but make sure that the name
     [k] exists in the environment before translating [body] so that we can use
     it. *)
  match e with
  | Non_recursive
      { handler; num_free_occurrences = _; is_applied_with_traps = _ } ->
    Non_recursive_let_cont_handler.pattern_match handler ~f:(fun k ~body ->
        let handler = Non_recursive_let_cont_handler.handler handler in
        Continuation_handler.pattern_match handler
          ~f:(fun params ~handler:cont_body ->
            let params, env = To_jsir_shared.bound_parameters ~env params in
            let res, addr = To_jsir_result.reserve_address res in
            let env, res =
              match Continuation_handler.is_exn_handler handler with
              | false ->
                let env = To_jsir_env.add_continuation env k addr in
                (* CR selee: we keep the environment because we need the symbols
                   defined there, but really there should be a way to flush
                   variables out of the environment to respect flambda scoping
                   invariants *)
                let env, res = expr ~env ~res body in
                let res =
                  To_jsir_result.new_block_with_addr_exn res ~params ~addr
                in
                env, res
              | true ->
                let exn_param, extra_args =
                  match params with
                  | [] ->
                    Misc.fatal_errorf
                      "No parameters given for exception continuation %a"
                      Continuation.print k
                  | hd :: tl -> hd, tl
                in
                let extra_args_boxed, res =
                  make_mutables ~res (List.length extra_args)
                in
                let env =
                  To_jsir_env.add_exn_handler env k ~addr ~exn_param
                    ~extra_args:extra_args_boxed
                in
                let env, res = expr ~env ~res body in
                let res =
                  To_jsir_result.new_block_with_addr_exn res ~params:[] ~addr
                in
                let res =
                  List.fold_left2
                    (fun res boxed actual ->
                      To_jsir_result.add_instr_exn res
                        (Let (actual, Field (boxed, 0, Non_float))))
                    res extra_args_boxed extra_args
                in
                env, res
            in
            expr ~env ~res cont_body))
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
        let env, res = expr ~env ~res body in
        let res =
          Continuation.Lmap.fold
            (fun k handler res ->
              Continuation_handler.pattern_match handler
                ~f:(fun params ~handler:cont_body ->
                  let params, env =
                    To_jsir_shared.bound_parameters ~env
                      (Bound_parameters.append invariant_params params)
                  in
                  let addr = To_jsir_env.get_continuation_exn env k in
                  let res =
                    To_jsir_result.new_block_with_addr_exn res ~params ~addr
                  in
                  let _env, res = expr ~env ~res cont_body in
                  res))
            (Flambda.Continuation_handlers.to_map conts)
            res
        in
        env, res)

and apply_expr ~env ~res e =
  (* Pass in any extra arguments for the exception continuation in mutable
     variables. A slightly sad hack, but necessary since [Raise] only has one
     parameter. *)
  let exn_continuation = Apply_expr.exn_continuation e in
  let extra_args = Exn_continuation.extra_args exn_continuation in
  let extra_args, res =
    List.fold_right
      (fun (arg, kind) (args, res) ->
        match Flambda_kind.With_subkind.kind kind with
        | Region | Rec_info -> None :: args, res
        | Value | Naked_number _ ->
          let arg, res = To_jsir_shared.simple ~env ~res arg in
          Some arg :: args, res)
      extra_args ([], res)
  in
  let res =
    assign_extra_args ~env ~res
      (Exn_continuation.exn_handler exn_continuation)
      extra_args
  in
  let args = Apply_expr.args e in
  let return_var, res =
    match Apply_expr.callee e, Apply_expr.call_kind e with
    | None, _ | _, Effect _ -> failwith "effects not implemented yet"
    | Some callee, Function _ ->
      let args, res = To_jsir_shared.simples ~env ~res args in
      let f, res = To_jsir_shared.simple ~env ~res callee in
      (* CR selee: assume exact = false for now, JSIR seems to assume false in
         the case that we don't know *)
      apply_fn ~res ~f ~args ~exact:false
    | Some callee, Method { obj; kind; alloc_mode = _ } ->
      let args, res = To_jsir_shared.simples ~env ~res args in
      let obj, res = To_jsir_shared.simple ~env ~res obj in
      let field, res = To_jsir_shared.simple ~env ~res callee in
      let res, f =
        match kind with
        | Public -> To_jsir_result.get_public_method res ~obj ~field
        | Self ->
          let methods = Jsir.Var.fresh () in
          let res =
            To_jsir_result.add_instr_exn res
              (Let (methods, Field (obj, 0, Non_float)))
          in
          let f = Jsir.Var.fresh () in
          let res =
            To_jsir_result.add_instr_exn res
              (Let (f, Prim (Array_get, [Pv methods; Pv field])))
          in
          res, f
        | Cached ->
          (* [meth_kind = Cached] generation in Lambda is disabled for
             non-native backends *)
          Misc.fatal_errorf "Found cached method invocation for Apply_expr %a"
            Apply_expr.print e
      in
      apply_fn ~res ~f ~args:(obj :: args) ~exact:false
    | Some callee, C_call _ ->
      let symbol =
        match Simple.must_be_symbol callee with
        | Some (symbol, _coercion) -> symbol
        | None ->
          Misc.fatal_errorf
            "Expected callee to be a symbol for C calls, instead found %a"
            Simple.print callee
      in
      To_jsir_primitive.extern ~env ~res symbol args
  in
  match Apply_expr.continuation e with
  | Never_returns ->
    env, To_jsir_result.end_block_with_last_exn res (Return return_var)
  | Return cont ->
    if Continuation.equal (To_jsir_env.return_continuation env) cont
    then env, To_jsir_result.end_block_with_last_exn res (Return return_var)
    else
      let addr = To_jsir_env.get_continuation_exn env cont in
      ( env,
        To_jsir_result.end_block_with_last_exn res (Branch (addr, [return_var]))
      )

and apply_cont0 ~env ~res apply_cont =
  let args, res =
    To_jsir_shared.simples ~env ~res (Apply_cont.args apply_cont)
  in
  let continuation = Apply_cont.continuation apply_cont in
  let get_last ~raise_kind_and_exn_handler : Jsir.last * To_jsir_result.t =
    match Continuation.sort continuation with
    | Toplevel_return ->
      let module_symbol =
        match args with
        | [arg] -> arg
        | [] -> Misc.fatal_error "Found a Toplevel_return with no arguments"
        | _ :: _ ->
          Misc.fatal_error "Found a Toplevel_return with multiple arguments"
      in
      (* CR selee: This is a hack, but I can't find a way to trigger any
         behaviour that isn't calling [caml_register_global] on the toplevel
         module. I suspect for single-file compilation this is always fine. Will
         come back and review later. *)
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
                     Pv module_symbol;
                     Pc
                       (* CR selee: this assumes javascript, WASM needs just
                          String *)
                       (NativeString (Jsir.Native_string.of_string module_name))
                   ] ) ))
      in
      Stop, res
    | Return ->
      let arg = unsupported_multiple_return_variables args in
      Return arg, res
    | Normal_or_exn -> (
      match raise_kind_and_exn_handler with
      | None ->
        let addr = To_jsir_env.get_continuation_exn env continuation in
        Jsir.Branch (addr, args), res
      | Some (raise_kind, exn_handler) ->
        let raise_kind =
          match (raise_kind : Trap_action.Raise_kind.t) with
          | Regular -> `Normal
          | Reraise -> `Reraise
          | No_trace -> `Notrace
        in
        let exn, extra_args =
          match args with
          | hd :: tl -> hd, tl
          | [] ->
            Misc.fatal_errorf
              "Attempting to call the exception continuation %a with no \
               arguments arguments (should be at least one)"
              Continuation.print continuation
        in
        let res =
          assign_extra_args ~env ~res exn_handler
            (List.map Option.some extra_args)
        in
        Raise (exn, raise_kind), res)
    | Define_root_symbol -> failwith "unimplemented"
  in
  match Apply_cont.trap_action apply_cont with
  | None -> get_last ~raise_kind_and_exn_handler:None
  | Some (Push { exn_handler }) -> (
    let last, res = get_last ~raise_kind_and_exn_handler:None in
    let ({ addr = handler_addr; exn_param = handler_var; extra_args = _ }
          : To_jsir_env.exn_handler) =
      To_jsir_env.get_exn_handler_exn env exn_handler
    in
    match last with
    | Branch cont -> Jsir.Pushtrap (cont, handler_var, (handler_addr, [])), res
    | Return _ | Raise _ | Stop | Cond _ | Switch _ | Pushtrap _ | Poptrap _ ->
      let res, addr = To_jsir_result.new_block res ~params:[] in
      let res = To_jsir_result.end_block_with_last_exn res last in
      Jsir.Pushtrap ((addr, []), handler_var, (handler_addr, [])), res)
  | Some (Pop { exn_handler; raise_kind }) -> (
    let raise_kind_and_exn_handler =
      Option.map (fun r -> r, exn_handler) raise_kind
    in
    let last, res = get_last ~raise_kind_and_exn_handler in
    if Option.is_some raise_kind
    then last, res
    else
      (* [Jsir.Poptrap] always jumps to a block after popping the exception
         handler. If we want to do anything than just [Branch], we should jump
         to a new block that actually does the thing we want. *)
      match last with
      | Branch cont -> Jsir.Poptrap cont, res
      | Return _ | Raise _ | Stop | Cond _ | Switch _ | Pushtrap _ | Poptrap _
        ->
        let res, addr = To_jsir_result.new_block res ~params:[] in
        let res = To_jsir_result.end_block_with_last_exn res last in
        Jsir.Poptrap (addr, []), res)

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
        match (last : Jsir.last) with
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

and invalid ~env ~res _msg =
  let res =
    To_jsir_result.add_instr_exn res
      (Let (Jsir.Var.fresh (), Prim (Extern "caml_invalid_primitive", [])))
  in
  env, To_jsir_result.end_block_with_last_exn res Stop

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
