(* Random generation of the IR, configurable through [Config]. *)
module Bin_op = Ir.Bin_op
module Expr = Ir.Expr
module Function = Ir.Function
module Name = Ir.Name
module Number = Ir.Number
module NumberTy = Ir.NumberTy
module Statement = Ir.Statement
module Ty = Ir.Ty

module State = struct
  type t =
    { random_state : Random.State.t;
      mutable fresh_counter : int;
      mutable function_counter : int;
      mutable top_level_functions : Function.t list
    }

  let create random_state =
    { random_state;
      fresh_counter = 0;
      function_counter = 0;
      top_level_functions = []
    }

  let fresh t =
    let name = Name.of_string (Format.sprintf "x_%d" t.fresh_counter) in
    t.fresh_counter <- t.fresh_counter + 1;
    name

  let can_create_function t ~max = t.function_counter < max

  let reserve_function t =
    let name = Name.of_string (Format.sprintf "f_%d" t.function_counter) in
    t.function_counter <- t.function_counter + 1;
    name
end

module Env = struct
  type t = { bindings : (Name.t * Ty.t) list }

  let empty = { bindings = [] }

  let extend t (name, ty) = { bindings = (name, ty) :: t.bindings }
end

(* CR-someday hwasilewski: Move all constants, including probabilities, into
   Config. *)
(* CR-someday hwasilewski: Make [Config] controlled by swarm testing. *)
module Config = struct
  let max_function_count = 5

  let fun_min_param_count = 1

  let fun_max_param_count = 5

  let max_block_depth = 3

  let max_expression_complexity = 10

  let main_var_count = 5
end

(* CR-someday hwasilewski: We should consider changing this to be a monad, which
   would allow our generators to backtrack and fail. This would mimic
   Quickcheck-style generators more closely. *)
module Gen = struct
  type 'a t = (unit -> 'a) option

  let unavailable : 'a t = None

  let create (f : unit -> 'a) = Some f

  let return x = create (fun () -> x)

  let when_ condition f = if condition then create f else unavailable

  let map t ~f =
    match t with
    | None -> unavailable
    | Some generate -> create (fun () -> f (generate ()))

  let weighted random_state choices =
    let choices =
      List.filter_map
        (fun (weight, choice) ->
          if weight < 0
          then invalid_arg "Gen.weighted: negative weight"
          else if weight = 0
          then None
          else Option.map (fun generate -> weight, generate) choice)
        choices
    in
    match choices with
    | [] -> unavailable
    | _ ->
      create (fun () ->
          let total =
            List.fold_left (fun total (weight, _) -> total + weight) 0 choices
          in
          let rec select index = function
            | [] -> assert false
            | (weight, generate) :: choices ->
              if index < weight
              then generate ()
              else select (index - weight) choices
          in
          select (Random.State.int random_state total) choices)

  let _uniform random_state choices =
    weighted random_state (List.map (fun choice -> 1, choice) choices)

  let run_exn = function
    | None -> invalid_arg "Gen.run_exn: no available generator"
    | Some generate -> generate ()
end

let random_element (st : State.t) list =
  List.nth list (Random.State.int st.random_state (List.length list))

let random_number_ty st = random_element st NumberTy.all

let random_int_in_range (st : State.t) ~min ~max =
  Random.State.int_in_range st.random_state ~min ~max

let with_expression_complexity f = f ~complexity:(ref 0)

let can_recurse ~complexity =
  !complexity + 2 <= Config.max_expression_complexity

let record_complexity expr ~complexity =
  incr complexity;
  expr

let rec gen_number (st : State.t) (env : Env.t) (nty : NumberTy.t) ~complexity =
  let gen_const_int base =
    Gen.create (fun () ->
        let small ~min ~max =
          Gen.create (fun () ->
              Random.State.int_in_range st.random_state ~min ~max
              |> Int64.of_int)
        in
        let bits =
          Gen.run_exn
            (Gen.weighted st.random_state
               [ 1, small ~min:(-1) ~max:1;
                 1, small ~min:(-10) ~max:10;
                 2, Gen.create (fun () -> Random.State.bits64 st.random_state)
               ])
        in
        record_complexity
          (Expr.Const (Number.of_integral_bits base bits))
          ~complexity)
  in
  let gen_const_float =
    Gen.create (fun () ->
        (* CR-soon hwasilewski: Add a skewed distribution of floats similar to
           [gen_const_int]. *)
        record_complexity
          (Expr.Const
             (Number.Float
                (Int64.float_of_bits (Random.State.bits64 st.random_state))))
          ~complexity)
  in
  let gen_const_float32 =
    Gen.create (fun () ->
        record_complexity
          (Expr.Const
             (Number.Float32
                (Int32.float_of_bits (Random.State.bits32 st.random_state))))
          ~complexity)
  in
  let gen_const (nty : NumberTy.t) =
    let boxed =
      match nty.base with
      | Float -> gen_const_float
      | Float32 -> gen_const_float32
      | (Int | Nativeint | Int64 | Int32 | Int16 | Int8) as base ->
        gen_const_int base
    in
    if nty.unboxed
    then
      Gen.map boxed ~f:(fun expr ->
          Expr.Convert { expr; from = NumberTy.boxed nty.base; to_ = nty })
    else boxed
  in
  let gen_var nty =
    let vars =
      List.filter (fun (_, vty) -> Ty.equal (Ty.Number nty) vty) env.bindings
    in
    Gen.when_
      (not (List.is_empty vars))
      (fun () ->
        let name, _ty = random_element st vars in
        record_complexity (Expr.Var name) ~complexity)
  in
  let gen_ty nty =
    Gen.run_exn
      (Gen.weighted st.random_state
         [1, Gen.create (fun () -> random_number_ty st); 3, Gen.return nty])
  in
  let gen_binop nty =
    Gen.when_ (can_recurse ~complexity) (fun () ->
        let inner_ty = gen_ty nty in
        let binop = random_element st (Bin_op.ops_for_ty (Ty.Number nty)) in
        let lhs = gen_number st env inner_ty ~complexity in
        let rhs = gen_number st env inner_ty ~complexity in
        Expr.Convert
          { from = inner_ty;
            to_ = nty;
            expr = Expr.Bin_op { ty = Ty.Number inner_ty; op = binop; lhs; rhs }
          })
  in
  let standard =
    Gen.weighted st.random_state
      [2, gen_var nty; 1, gen_const nty; 4, gen_binop nty]
  in
  Gen.run_exn
    (Gen.weighted st.random_state
       [8, standard; 2, gen_fun_call st env nty ~complexity])

and gen_fun_call (st : State.t) caller_env return_ty ~complexity =
  if not (can_recurse ~complexity)
  then Gen.unavailable
  else
    let gen_arguments params =
      List.map
        (fun (_, ty) ->
          match ty with
          | Ty.Number nty -> gen_number st caller_env nty ~complexity
          (* CR-soon hwasilewski: add bool arguments *)
          | Ty.Bool -> assert false)
        params
    in
    let call_existing_function () =
      let function_ = random_element st st.top_level_functions in
      let args = gen_arguments function_.params in
      record_complexity
        (Expr.Convert
           { expr = Expr.Call_toplevel { fun_name = function_.name; args };
             from = function_.return_ty;
             to_ = return_ty
           })
        ~complexity
    in
    let existing_function =
      Gen.when_
        (not (List.is_empty st.top_level_functions))
        call_existing_function
    in
    let new_function =
      Gen.when_ (State.can_create_function st ~max:Config.max_function_count)
        (fun () ->
          let name = State.reserve_function st in
          let parameter_types =
            List.init
              (random_int_in_range st ~min:Config.fun_min_param_count
                 ~max:Config.fun_max_param_count) (fun _ -> random_number_ty st)
          in
          let callee_env, params =
            List.fold_left_map
              (fun env nty ->
                let name = State.fresh st in
                Env.extend env (name, Ty.Number nty), (name, Ty.Number nty))
              Env.empty parameter_types
          in
          let args = gen_arguments params in
          let _callee_env, body = gen_fun_body st callee_env 0 in
          let result =
            with_expression_complexity (fun ~complexity ->
                gen_number st callee_env return_ty ~complexity)
          in
          let function_ = { Function.name; params; body; return_ty; result } in
          (* Creating a function after its full body was successfully generated
             ensures that all the functions called by [function_] were already
             added to [st.top_level_functions]. This means that functions are
             topologically sorted by construction. *)
          st.top_level_functions <- function_ :: st.top_level_functions;
          record_complexity
            (Expr.Call_toplevel { fun_name = name; args })
            ~complexity)
    in
    Gen.weighted st.random_state [1, existing_function; 1, new_function]

and gen_bool (st : State.t) env ~complexity =
  let gen_binop op arg_ty gen_arg =
    Gen.create (fun () ->
        let lhs = gen_arg ~complexity in
        let rhs = gen_arg ~complexity in
        Expr.Bin_op { ty = arg_ty; op; lhs; rhs })
  in
  let nty = random_number_ty st in
  let gen_number_arg ~complexity = gen_number st env nty ~complexity in
  let gen_bool_arg ~complexity = gen_bool st env ~complexity in
  let gen_bool_binop op =
    if can_recurse ~complexity
    then gen_binop op Ty.Bool gen_bool_arg
    else Gen.unavailable
  in
  Gen.run_exn
    (Gen.weighted st.random_state
       [ 1, gen_binop Bin_op.Eq (Ty.Number nty) gen_number_arg;
         1, gen_bool_binop Bin_op.Eq;
         1, gen_bool_binop Bin_op.And;
         1, gen_bool_binop Bin_op.Or ])

and gen_decl st env =
  let name = State.fresh st in
  let nty = random_number_ty st in
  let expr =
    with_expression_complexity (fun ~complexity ->
        gen_number st env nty ~complexity)
  in
  let env = Env.extend env (name, Ty.Number nty) in
  env, (name, Ty.Number nty, expr)

and gen_fun_body (st : State.t) (env : Env.t) depth =
  let stmt_count = 1 + Random.State.int st.random_state 4 in
  let rec gen env remaining =
    if remaining = 0
    then env, Statement.Seq []
    else
      let continue env statement =
        let env, rest = gen env (remaining - 1) in
        env, Statement.sequence statement rest
      in
      let gen_assign =
        Gen.when_
          (not (List.is_empty env.Env.bindings))
          (fun () ->
            let name, ty = random_element st env.Env.bindings in
            (* CR-soon hwasilewski: Add boolean variable generation. *)
            match ty with
            | Ty.Number nty ->
              let expr =
                with_expression_complexity (fun ~complexity ->
                    gen_number st env nty ~complexity)
              in
              continue env (Statement.Assign (name, expr))
            | Bool ->
              invalid_arg
                "gen_fun_body.gen_assign: unexpected variable of type bool")
      in
      let gen_if =
        Gen.create (fun () ->
            let condition =
              with_expression_complexity (fun ~complexity ->
                  gen_bool st env ~complexity)
            in
            let _env_l, left = gen_fun_body st env (depth + 1) in
            let _env_r, right = gen_fun_body st env (depth + 1) in
            continue env (Statement.If (condition, left, right)))
      in
      let gen_local_decl =
        Gen.when_ (remaining > 1) (fun () ->
            let env, (name, _ty, expr) = gen_decl st env in
            let env, body = gen env (remaining - 1) in
            env, Statement.Let_mutable (name, expr, body))
      in
      let gen_bounded_loop =
        Gen.create (fun () ->
            let name = State.fresh st in
            let times = 1 + Random.State.int st.random_state 3 in
            let _, inner = gen_fun_body st env (depth + 1) in
            continue env (Statement.Bounded_loop (name, times, inner)))
      in
      let allowed =
        if depth >= Config.max_block_depth
        then [1, gen_assign]
        else [4, gen_assign; 1, gen_if; 1, gen_bounded_loop; 1, gen_local_decl]
      in
      Gen.run_exn (Gen.weighted st.random_state allowed)
  in
  gen env stmt_count

(* CR-soon hwasilewski: Make the generated outputs less pessimistic for the
   register allocator, which we do not want to spend as much time on. *)
let gen_program (st : State.t) env =
  let rec gen_vars env count =
    if count = 0
    then env, []
    else
      let env, decl = gen_decl st env in
      let env, decls = gen_vars env (count - 1) in
      env, decl :: decls
  in
  let env, main_decls = gen_vars env Config.main_var_count in
  let _env, main_statement = gen_fun_body st env 0 in
  Program.create
    ~functions:(List.rev st.top_level_functions)
    ~main_decls ~main_statement

let generate random =
  let state = State.create random in
  gen_program state Env.empty
