(* Random generation of the IR, configurable through [Config]. *)
module Bin_op = Ir.Bin_op
module Expr = Ir.Expr
module Function = Ir.Function
module Inline = Ir.Inline
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

module Binding = struct
  type t =
    { name : Name.t;
      ty : Ty.t;
      is_mutable : bool
    }
end

module Loop_variable = struct
  type t =
    { name : Name.t;
      lower_bound : int;
      upper_bound : int
    }
end

module Env = struct
  type t =
    { bindings : Binding.t list;
      loop_variables : Loop_variable.t list
    }

  let empty = { bindings = []; loop_variables = [] }

  let extend t binding = { t with bindings = binding :: t.bindings }

  let extend_loop t ({ Loop_variable.name; _ } as loop_variable) =
    let t =
      extend t
        { Binding.name;
          ty = Ty.Number (NumberTy.boxed Int);
          is_mutable = false
        }
    in
    { t with loop_variables = loop_variable :: t.loop_variables }
end

(* CR-someday hwasilewski: Move all constants, including probabilities, into
   Config. *)
(* CR-soon hwasilewski: Make [Config] controlled by swarm testing. *)
module Config = struct
  let max_function_count = 10

  let fun_min_param_count = 0

  let fun_max_param_count = 5

  let max_block_depth = 4

  let max_loop_stride = 8

  let max_loop_offset = 16

  let max_expression_complexity = 20

  let toplevel_var_count = 5

  let max_array_dimensions = 3

  let max_array_axis_size = 10

  let max_array_elements = 256

  (* Percentages. *)
  let array_probability = 25

  let array_literal_probability = 50

  let bounded_index_probability = 75

  let opaque_initializer_probability = 50

  let opaque_leaf_probability = 5

  let opaque_loop_bound_probability = 50
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
          then Misc.fatal_errorf "Gen.weighted: negative weight"
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
    | None -> Misc.fatal_errorf "Gen.run_exn: no available generator"
    | Some generate -> generate ()
end

let random_element (st : State.t) list =
  List.nth list (Random.State.int st.random_state (List.length list))

let random_number_ty st = random_element st NumberTy.all

let random_int_in_range (st : State.t) ~min ~max =
  Random.State.int_in_range st.random_state ~min ~max

let with_expression_complexity f = f ~complexity:(ref 0)

let with_probability (st : State.t) ~probability =
  Random.State.int st.random_state 100 < probability

let maybe_opaque st ~probability expr =
  if with_probability st ~probability then Expr.Opaque expr else expr

let can_recurse ~complexity =
  !complexity + 2 <= Config.max_expression_complexity

let record_complexity expr ~complexity =
  incr complexity;
  expr

(* CR-soon hwasilewski: Add general expressions for shift counts. *)
let gen_shift_count (st : State.t) (nty : NumberTy.t) ~complexity =
  let width =
    match nty.base with
    | Int -> Sys.int_size
    | Nativeint -> Sys.word_size
    | Int64 -> 64
    | Int32 -> 32
    | Int16 -> 16
    | Int8 -> 8
    | Float | Float32 ->
      Misc.fatal_errorf "gen_shift_count: expected an integral type"
  in
  let count =
    if Random.State.bool st.random_state
    then random_element st [0; 1; width - 2; width - 1]
    else Random.State.int st.random_state width
  in
  record_complexity (Expr.Const (Number.Int count)) ~complexity

let gen_array_dimensions st =
  let dimensions =
    random_int_in_range st ~min:1 ~max:Config.max_array_dimensions
  in
  let rec sizes remaining budget =
    if remaining = 0
    then []
    else
      let size =
        random_int_in_range st ~min:1
          ~max:(min Config.max_array_axis_size budget)
      in
      size :: sizes (remaining - 1) (budget / size)
  in
  sizes dimensions Config.max_array_elements

let gen_array_index st (env : Env.t) size =
  let variables =
    List.filter
      (fun { Loop_variable.lower_bound; upper_bound; _ } ->
        upper_bound - lower_bound < size)
      env.loop_variables
  in
  if
    (not (List.is_empty variables))
    && with_probability st ~probability:Config.bounded_index_probability
  then
    let { Loop_variable.name; lower_bound; upper_bound } =
      random_element st variables
    in
    let offset =
      random_int_in_range st ~min:(-lower_bound) ~max:(size - 1 - upper_bound)
    in
    if offset = 0
    then Expr.Var name
    else
      Expr.Bin_op
        { ty = Ty.Number (NumberTy.boxed Int);
          op = Bin_op.Add;
          lhs = Expr.Var name;
          rhs = Expr.Const (Number.Int offset)
        }
  else Expr.Const (Number.Int (random_int_in_range st ~min:0 ~max:(size - 1)))

let gen_array_indices st env dimensions =
  List.map (gen_array_index st env) dimensions

let gen_numeric_var (st : State.t) (env : Env.t) nty ~complexity =
  let vars =
    List.filter_map
      (fun { Binding.name; ty; _ } ->
        match ty with
        | Ty.Number nty -> Some (nty, fun () -> Expr.Var name)
        | Ty.Array (nty, dimensions) ->
          Some
            ( nty,
              fun () ->
                Expr.Array_get (name, gen_array_indices st env dimensions) )
        | Ty.Bool -> None)
      env.bindings
  in
  Gen.when_
    (not (List.is_empty vars))
    (fun () ->
      let inner_ty, generate = random_element st vars in
      maybe_opaque st ~probability:Config.opaque_leaf_probability
        (record_complexity
           (Expr.Convert { from = inner_ty; to_ = nty; expr = generate () })
           ~complexity))

let gen_float_bits (st : State.t) ~fraction_bits ~exponent_bits ~bits_of_float
    ~random_bits =
  let sign_bit = Int64.shift_left 1L (fraction_bits + exponent_bits) in
  let min_normal = Int64.shift_left 1L fraction_bits in
  let infinity =
    Int64.shift_left (Int64.of_int ((1 lsl exponent_bits) - 1)) fraction_bits
  in
  let with_sign bits =
    if Random.State.bool st.random_state
    then Int64.logor sign_bit bits
    else bits
  in
  let small =
    Gen.create (fun () ->
        let numerator = Random.State.int st.random_state 17 in
        let exponent = -(Random.State.int st.random_state 5) in
        let value = Float.ldexp (float_of_int numerator) exponent in
        with_sign (bits_of_float value))
  in
  let boundary =
    Gen.create (fun () ->
        let powers =
          List.map
            (fun exponent -> bits_of_float (Float.ldexp 1. exponent))
            [0; 7; 15; 31; Sys.int_size - 1; 63; fraction_bits + 1]
        in
        let bits =
          random_element st (1L :: min_normal :: Int64.pred infinity :: powers)
        in
        let offset = random_int_in_range st ~min:(-1) ~max:1 in
        with_sign (Int64.add bits (Int64.of_int offset)))
  in
  let special =
    Gen.create (fun () ->
        let payload =
          if Random.State.bool st.random_state
          then 0L
          else
            Int64.succ
              (Random.State.int64 st.random_state (Int64.pred min_normal))
        in
        with_sign (Int64.logor infinity payload))
  in
  Gen.run_exn
    (Gen.weighted st.random_state
       [10, Gen.create random_bits; 5, small; 4, boundary; 1, special])

let gen_numeric_const (st : State.t) (nty : NumberTy.t) ~complexity =
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
                 (* CR-soon hwasilewski: Add max_int and min_int. *) ])
        in
        record_complexity
          (Expr.Const (Number.of_integral_bits base bits))
          ~complexity)
  in
  let gen_const_float =
    Gen.create (fun () ->
        let bits =
          gen_float_bits st ~fraction_bits:52 ~exponent_bits:11
            ~bits_of_float:Int64.bits_of_float
            ~random_bits:(fun () -> Random.State.bits64 st.random_state)
        in
        record_complexity (Expr.Const (Number.Float bits)) ~complexity)
  in
  let gen_const_float32 =
    Gen.create (fun () ->
        let bits =
          gen_float_bits st ~fraction_bits:23 ~exponent_bits:8
            ~bits_of_float:(fun x -> Int64.of_int32 (Int32.bits_of_float x))
            ~random_bits:(fun () ->
              Int64.of_int32 (Random.State.bits32 st.random_state))
          |> Int64.to_int32
        in
        record_complexity (Expr.Const (Number.Float32 bits)) ~complexity)
  in
  let gen_const (nty : NumberTy.t) =
    let boxed =
      match nty.base with
      | Float -> gen_const_float
      | Float32 -> gen_const_float32
      | (Int | Nativeint | Int64 | Int32 | Int16 | Int8) as base ->
        gen_const_int base
    in
    let const =
      if nty.unboxed
      then
        Gen.map boxed ~f:(fun expr ->
            Expr.Convert { expr; from = NumberTy.boxed nty.base; to_ = nty })
      else boxed
    in
    Gen.map const
      ~f:(maybe_opaque st ~probability:Config.opaque_leaf_probability)
  in
  gen_const nty

let gen_array st env nty dimensions =
  if with_probability st ~probability:Config.array_literal_probability
  then
    let count = List.fold_left ( * ) 1 dimensions in
    let pool =
      List.init
        (random_int_in_range st ~min:1 ~max:(max 1 (count / 2)))
        (fun _ ->
          with_expression_complexity (fun ~complexity ->
              Gen.run_exn (gen_numeric_const st nty ~complexity)))
    in
    let rec literal = function
      | [] -> random_element st pool
      | size :: rest ->
        Expr.Array_literal (List.init size (fun _ -> literal rest))
    in
    literal dimensions
  else
    let init =
      with_expression_complexity (fun ~complexity ->
          match gen_numeric_var st env nty ~complexity with
          | Some generate -> generate ()
          | None -> Gen.run_exn (gen_numeric_const st nty ~complexity))
    in
    Expr.Array_make { dimensions; init }

let rec gen_number (st : State.t) (env : Env.t) (nty : NumberTy.t) ~complexity =
  let gen_ty nty =
    Gen.run_exn
      (Gen.weighted st.random_state
         [1, Gen.create (fun () -> random_number_ty st); 3, Gen.return nty])
  in
  let gen_binop nty =
    Gen.when_ (can_recurse ~complexity) (fun () ->
        let inner_ty = gen_ty nty in
        let binop = random_element st (Bin_op.ops_for_ty (Ty.Number inner_ty)) in
        let lhs = gen_number st env inner_ty ~complexity in
        let rhs =
          match binop with
          | Bin_op.Shift_left
          | Bin_op.Shift_right
          | Bin_op.Shift_right_logical ->
            gen_shift_count st inner_ty ~complexity
          | _ -> gen_number st env inner_ty ~complexity
        in
        Expr.Convert
          { from = inner_ty;
            to_ = nty;
            expr = Expr.Bin_op { ty = Ty.Number inner_ty; op = binop; lhs; rhs }
          })
  in
  let leaf =
    Gen.weighted st.random_state
      [ 2, gen_numeric_var st env nty ~complexity;
        1, gen_numeric_const st nty ~complexity ]
  in
  Gen.run_exn
    (Gen.weighted st.random_state
       [5, leaf; 3, gen_binop nty; 1, gen_fun_call st env nty ~complexity])

and gen_fun_call (st : State.t) caller_env return_ty ~complexity =
  if
    (not (can_recurse ~complexity))
    || not (State.can_create_function st ~max:Config.max_function_count)
  then Gen.unavailable
  else
    let gen_arguments params =
      List.map
        (fun (_, ty) ->
          match ty with
          | Ty.Number nty -> gen_number st caller_env nty ~complexity
          (* CR-soon hwasilewski: add bool arguments *)
          | Ty.Bool -> assert false
          | Ty.Array _ ->
            Misc.fatal_errorf "gen_arguments: unexpected array parameter")
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
      Gen.create (fun () ->
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
                ( Env.extend env
                    { Binding.name; ty = Ty.Number nty; is_mutable = true },
                  (name, Ty.Number nty) ))
              Env.empty parameter_types
          in
          let inline : Inline.t =
            match Random.State.int st.random_state 3 with
            | 0 -> Never
            | 1 -> Always
            | _ -> Default
          in
          let args = gen_arguments params in
          let _callee_env, body = gen_fun_body st callee_env 0 in
          let result =
            with_expression_complexity (fun ~complexity ->
                match gen_numeric_var st callee_env return_ty ~complexity with
                | Some generate -> generate ()
                | None -> gen_number st callee_env return_ty ~complexity)
          in
          let function_ =
            { Function.name; params; inline; body; return_ty; result }
          in
          (* Creating a function after its full body was successfully generated
             ensures that all the functions called by [function_] were already
             added to [st.top_level_functions]. This means that functions are
             topologically sorted by construction. *)
          st.top_level_functions <- function_ :: st.top_level_functions;
          record_complexity
            (Expr.Call_toplevel { fun_name = name; args })
            ~complexity)
    in
    Gen.weighted st.random_state [2, existing_function; 1, new_function]

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
  let comparison = random_element st Bin_op.[Eq; Lt; Le; Gt; Ge] in
  Gen.run_exn
    (Gen.weighted st.random_state
       [ 5, gen_binop comparison (Ty.Number nty) gen_number_arg;
         1, gen_bool_binop Bin_op.Eq;
         1, gen_bool_binop Bin_op.And;
         1, gen_bool_binop Bin_op.Or ])

and gen_decl st env =
  let name = State.fresh st in
  let nty = random_number_ty st in
  let ty, expr =
    if with_probability st ~probability:Config.array_probability
    then
      let dimensions = gen_array_dimensions st in
      Ty.Array (nty, dimensions), gen_array st env nty dimensions
    else
      ( Ty.Number nty,
        with_expression_complexity (fun ~complexity ->
            gen_number st env nty ~complexity) )
  in
  let expr =
    maybe_opaque st ~probability:Config.opaque_initializer_probability expr
  in
  let env = Env.extend env { Binding.name; ty; is_mutable = true } in
  env, (name, ty, expr)

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
      let mutable_bindings =
        List.filter
          (fun (binding : Binding.t) ->
            match binding.Binding.ty with
            | Ty.Array _ -> true
            | Ty.Number _ | Ty.Bool -> binding.is_mutable)
          env.Env.bindings
      in
      let gen_assign =
        Gen.when_
          (not (List.is_empty mutable_bindings))
          (fun () ->
            let { Binding.name; ty; _ } = random_element st mutable_bindings in
            let nty, assign =
              (* CR-soon hwasilewski: Add boolean variable generation. *)
              match ty with
              | Ty.Number nty ->
                nty, (fun expr -> Statement.Assign (name, expr))
              | Ty.Array (nty, dimensions) ->
                let indices = gen_array_indices st env dimensions in
                nty, (fun expr -> Statement.Array_set (name, indices, expr))
              | Bool ->
                Misc.fatal_errorf
                  "gen_fun_body.gen_assign: unexpected variable of type bool"
            in
            let expr =
              with_expression_complexity (fun ~complexity ->
                  gen_number st env nty ~complexity)
            in
            continue env (assign expr))
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
            let scale =
              random_int_in_range st ~min:1 ~max:Config.max_loop_stride
            in
            let scale =
              if Random.State.bool st.random_state then scale else -scale
            in
            let offset =
              random_int_in_range st ~min:(-Config.max_loop_offset)
                ~max:Config.max_loop_offset
            in
            let initial_value = (scale * times) + offset in
            let bound_value = scale + offset in
            let init =
              maybe_opaque st ~probability:Config.opaque_loop_bound_probability
                (Expr.Const (Number.Int initial_value))
            in
            let loop_env =
              Env.extend_loop env
                { Loop_variable.name;
                  lower_bound = min initial_value bound_value;
                  upper_bound = max initial_value bound_value
                }
            in
            let _, inner = gen_fun_body st loop_env (depth + 1) in
            continue env
              (Statement.Bounded_loop
                 { var = name;
                   init;
                   bound = Expr.Const (Number.Int bound_value);
                   stride = -scale;
                   body = inner
                 }))
      in
      let gen_empty =
        Gen.when_ (List.is_empty mutable_bindings) (fun () ->
            env, Statement.Seq [])
      in
      let allowed =
        if depth >= Config.max_block_depth
        then [1, gen_assign; 1, gen_empty]
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
  let env, toplevel_decls = gen_vars env Config.toplevel_var_count in
  let _env, toplevel_statement = gen_fun_body st env 0 in
  Program.create
    ~functions:(List.rev st.top_level_functions)
    ~toplevel_decls ~toplevel_statement

let generate random =
  let state = State.create random in
  gen_program state Env.empty
