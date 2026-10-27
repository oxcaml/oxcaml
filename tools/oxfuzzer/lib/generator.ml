(* Random generation of the IR, configurable through [Config]. *)
module Bin_op = Ir.Bin_op
module Binding = Ir.Binding
module Exn = Ir.Exn
module Expr = Ir.Expr
module Function = Ir.Function
module Handler = Ir.Handler
module Inline = Ir.Inline
module Name = Ir.Name
module Number = Ir.Number
module NumberTy = Ir.NumberTy
module Place = Ir.Place
module Statement = Ir.Statement
module Sum = Ir.Sum
module Ty = Ir.Ty

module State = struct
  type t =
    { random_state : Random.State.t;
      swarm : Config.Swarm.t;
      number_types : NumberTy.t list;
      names : Fresh.t;
      function_names : Fresh.t;
      mutable top_level_functions : Function.t list;
      mutable record_types : Ty.record list;
      mutable exceptions : Exn.t list;
      mutable variant_types : Ty.variant list;
      mutable matches_remaining : int;
      mutable tries_remaining : int
    }

  let create random_state =
    let swarm = Config.Swarm.create random_state in
    { random_state;
      swarm;
      number_types = Config.Swarm.number_types swarm;
      names = Fresh.create ~prefix:"x";
      function_names = Fresh.create ~prefix:"f";
      top_level_functions = [];
      record_types = [];
      exceptions = [];
      variant_types = [];
      matches_remaining = Config.max_match_count;
      tries_remaining = Config.max_try_count
    }

  let fresh t = Fresh.next t.names

  let can_create_function t ~max = Fresh.count t.function_names < max

  let reserve_function t = Fresh.next t.function_names
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
      loop_variables : Loop_variable.t list;
      catchable_exceptions : Exn.t list
    }

  let empty = { bindings = []; loop_variables = []; catchable_exceptions = [] }

  let can_raise t exn = List.exists (Exn.equal exn) t.catchable_exceptions

  let extend_catchable_exceptions t exns =
    let new_exns = List.filter (fun exn -> not (can_raise t exn)) exns in
    { t with catchable_exceptions = new_exns @ t.catchable_exceptions }

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

(* CR-someday xclerc: We should consider changing this to be a monad, which
   would allow our generators to backtrack and fail. This would mimic
   Quickcheck-style generators more closely. *)
module Gen = struct
  type 'a t = (unit -> 'a) option

  let unavailable : 'a t = None

  let create (f : unit -> 'a) = Some f

  let return x = create (fun () -> x)

  let when_ condition f = if condition then create f else unavailable

  let only_if condition gen = if condition then gen else unavailable

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

let random_number_ty ?(value_only = false) (st : State.t) =
  let number_types =
    if value_only
    then List.filter (fun (nty : NumberTy.t) -> not nty.unboxed) st.number_types
    else st.number_types
  in
  random_element st number_types

let random_exceptions (st : State.t) exceptions ~count =
  let rec pick chosen remaining count =
    if count = 0 || List.is_empty remaining
    then List.rev chosen
    else
      let exn = random_element st remaining in
      pick (exn :: chosen)
        (List.filter (fun other -> not (Exn.equal exn other)) remaining)
        (count - 1)
  in
  pick [] exceptions count

let random_int_in_range (st : State.t) ~min ~max =
  Random.State.int_in_range st.random_state ~min ~max

let with_probability (st : State.t) ~probability =
  Random.State.int st.random_state 100 < probability

let maybe_opaque st ~enabled ~probability expr =
  if enabled && with_probability st ~probability then Expr.Opaque expr else expr

(* CR-soon xclerc: Add general expressions for shift counts. *)
let gen_shift_count (st : State.t) (nty : NumberTy.t) =
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
  Expr.Const (Number.Int count)

let gen_array_dimensions (st : State.t) =
  let dimensions =
    if st.swarm.multidimensional_arrays
    then random_int_in_range st ~min:1 ~max:Config.max_array_dimensions
    else 1
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

(* With [value_only], only types of layout value are generated. Arrays are
   values whatever their element type. *)
let gen_scalar_or_array_type ?(value_only = false) (st : State.t) =
  if
    st.swarm.arrays && with_probability st ~probability:Config.array_probability
  then Ty.Array (random_number_ty st, gen_array_dimensions st)
  else Ty.Number (random_number_ty ~value_only st)

let gen_type_with_unboxed_tuples (st : State.t) ~leaf =
  let rec gen depth =
    if
      st.swarm.unboxed_tuples
      && depth < Config.max_unboxed_tuple_depth
      && with_probability st ~probability:Config.unboxed_tuple_probability
    then
      Ty.Unboxed_tuple
        (List.init
           (random_int_in_range st ~min:2 ~max:Config.max_unboxed_tuple_arity)
           (fun _ -> gen (depth + 1)))
    else leaf ()
  in
  gen 0

let gen_record_types (st : State.t) =
  let representations =
    List.filter
      (function
        | `Boxed -> st.swarm.boxed_records
        | `Unboxed -> st.swarm.unboxed_records)
      [`Boxed; `Unboxed]
  in
  let count =
    if List.is_empty representations
    then 0
    else random_int_in_range st ~min:1 ~max:Config.max_record_types
  in
  let rec gen id records =
    if id = count
    then List.rev records
    else
      let fields =
        List.init (random_int_in_range st ~min:1 ~max:Config.max_record_fields)
          (fun index ->
            { Ty.index;
              ty =
                gen_type_with_unboxed_tuples st ~leaf:(fun () ->
                    if
                      (not (List.is_empty records))
                      && with_probability st
                           ~probability:Config.nested_record_probability
                    then Ty.Record (random_element st records)
                    else gen_scalar_or_array_type st);
              is_mutable =
                st.swarm.mutable_record_fields
                && with_probability st
                     ~probability:Config.mutable_field_probability
            })
      in
      let variants =
        List.map
          (function
            | `Boxed -> { Ty.id; fields; unboxed = false }
            | `Unboxed -> { Ty.id; fields; unboxed = true })
          representations
      in
      gen (id + 1) (List.rev_append variants records)
  in
  gen 0 []

let gen_record_or_scalar_type ?(value_only = false) (st : State.t) =
  let record_types =
    if value_only
    then
      List.filter
        (fun (record : Ty.record) -> not record.unboxed)
        st.record_types
    else st.record_types
  in
  if
    (not (List.is_empty record_types))
    && with_probability st ~probability:Config.record_probability
  then Ty.Record (random_element st record_types)
  else gen_scalar_or_array_type ~value_only st

let gen_exception (st : State.t) id =
  let payload =
    (* [raise] only accepts values, so the payload must be one too. *)
    if with_probability st ~probability:Config.exception_payload_probability
    then Some (gen_record_or_scalar_type ~value_only:true st)
    else None
  in
  { Exn.id; payload }

let gen_exceptions (st : State.t) =
  if not st.swarm.exceptions
  then []
  else
    List.init
      (random_int_in_range st ~min:1 ~max:Config.max_exception_count)
      (gen_exception st)

let gen_constructor_type (st : State.t) constructor_index =
  let args =
    List.init (random_int_in_range st ~min:0 ~max:Config.max_constructor_args)
      (fun _ ->
        gen_type_with_unboxed_tuples st ~leaf:(fun () ->
            gen_record_or_scalar_type st))
  in
  { Ty.constructor_index; args }

let gen_variant_type (st : State.t) variant_id =
  let constructors =
    List.init
      (random_int_in_range st ~min:1 ~max:Config.max_variant_constructors)
      (gen_constructor_type st)
  in
  { Ty.variant_id; constructors }

(* CR xclerc for xclerc: randomly add [@@unboxed] if legal. *)
let gen_variant_types (st : State.t) =
  if not st.swarm.variants
  then []
  else
    List.init
      (random_int_in_range st ~min:1 ~max:Config.max_variant_types)
      (gen_variant_type st)

(* [or_null] needs a non-null [value] payload. Arrays and boxed aggregates
   qualify even when their contents have unboxed layouts. *)
let gen_or_null_payload_type (st : State.t) =
  if
    st.swarm.arrays && with_probability st ~probability:Config.array_probability
  then Ty.Array (random_number_ty st, gen_array_dimensions st)
  else
    let numbers =
      List.filter_map
        (fun (nty : NumberTy.t) ->
          if nty.unboxed then None else Some (Ty.Number nty))
        st.number_types
    in
    let records =
      List.filter_map
        (fun (record : Ty.record) ->
          if record.unboxed then None else Some (Ty.Record record))
        st.record_types
    in
    let variants =
      List.map (fun variant -> Ty.Variant variant) st.variant_types
    in
    random_element st (numbers @ records @ variants)

let gen_sum_type (st : State.t) =
  Gen.run_exn
    (Gen.weighted st.random_state
       [ ( 1,
           Gen.when_
             (not (List.is_empty st.variant_types))
             (fun () -> Sum.Variant (random_element st st.variant_types)) );
         ( 1,
           Gen.when_ st.swarm.or_null (fun () ->
               Sum.Or_null (gen_or_null_payload_type st)) ) ])

(* CR-soon xclerc: Add boolean variable generation. *)
let gen_type (st : State.t) =
  gen_type_with_unboxed_tuples st ~leaf:(fun () ->
      if
        st.swarm.or_null
        && with_probability st ~probability:Config.or_null_probability
      then Ty.Or_null (gen_or_null_payload_type st)
      else if
        (not (List.is_empty st.variant_types))
        && with_probability st ~probability:Config.variant_probability
      then Ty.Variant (random_element st st.variant_types)
      else gen_record_or_scalar_type st)

let can_convert (st : State.t) from to_ =
  match from, to_ with
  | Ty.Number _, Ty.Number _ -> true
  | Ty.Record left, Ty.Record right ->
    Int.equal left.id right.id
    && (Bool.equal left.unboxed right.unboxed
       || st.swarm.record_representation_conversions)
  | _ -> Ty.equal from to_

let convert_value (st : State.t) from to_ expr =
  if Ty.equal from to_
  then expr
  else
    match from, to_ with
    | Ty.Number from, Ty.Number to_ -> Expr.Convert { expr; from; to_ }
    | Ty.Record from, Ty.Record to_ when Int.equal from.id to_.id ->
      Expr.Record_convert
        { from; to_unboxed = to_.unboxed; source_name = State.fresh st; expr }
    | _ -> Misc.fatal_errorf "convert_value: incompatible types"

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
    then Expr.Read (Place.Variable name)
    else
      Expr.Bin_op
        { ty = Ty.Number (NumberTy.boxed Int);
          op = Bin_op.Add;
          lhs = Expr.Read (Place.Variable name);
          rhs = Expr.Const (Number.Int offset)
        }
  else Expr.Const (Number.Int (random_int_in_range st ~min:0 ~max:(size - 1)))

let gen_array_indices st env dimensions =
  List.map (gen_array_index st env) dimensions

let places (st : State.t) (env : Env.t) ~for_write =
  let rec collect ty is_mutable make =
    let here = if for_write && not is_mutable then [] else [ty, make] in
    let children =
      match ty with
      | Ty.Record record ->
        List.concat_map
          (fun (field : Ty.field) ->
            collect field.ty ((not record.unboxed) && field.is_mutable)
              (fun () -> Place.Field (make (), record, field)))
          record.fields
      | Ty.Array (nty, dimensions) ->
        if for_write && not st.swarm.array_writes
        then []
        else
          [ ( Ty.Number nty,
              fun () ->
                Place.Element (make (), gen_array_indices st env dimensions) )
          ]
      | Ty.Unboxed_tuple components ->
        List.mapi
          (fun index ty ->
            collect ty false (fun () ->
                let tuple = make () in
                let name = State.fresh st in
                Place.Unboxed_tuple_component
                  { tuple; arity = List.length components; index; name }))
          components
        |> List.concat
      | Ty.Number _ | Ty.Bool | Ty.Variant _ | Ty.Or_null _ -> []
    in
    here @ children
  in
  List.concat_map
    (fun (binding : Binding.t) ->
      collect binding.ty binding.is_mutable (fun () ->
          Place.Variable binding.name))
    env.bindings

let gen_existing st env ty =
  let choices =
    List.filter
      (fun (from, _) -> can_convert st from ty)
      (places st env ~for_write:false)
  in
  Gen.when_
    (not (List.is_empty choices))
    (fun () ->
      let from, make = random_element st choices in
      convert_value st from ty (Expr.Read (make ())))

let gen_numeric_var (st : State.t) env nty =
  Gen.map
    (gen_existing st env (Ty.Number nty))
    ~f:
      (maybe_opaque st ~enabled:st.swarm.opaque_leaves
         ~probability:Config.opaque_leaf_probability)

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
        let exponent = -Random.State.int st.random_state 5 in
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

let gen_numeric_const (st : State.t) (nty : NumberTy.t) =
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
               [ 5, small ~min:(-1) ~max:1;
                 5, small ~min:(-10) ~max:10;
                 10, Gen.create (fun () -> Random.State.bits64 st.random_state)
                 (* CR-soon xclerc: Add max_int and min_int. *) ])
        in
        Expr.Const (Number.of_integral_bits base bits))
  in
  let gen_const_float =
    Gen.create (fun () ->
        let bits =
          gen_float_bits st ~fraction_bits:52 ~exponent_bits:11
            ~bits_of_float:Int64.bits_of_float ~random_bits:(fun () ->
              Random.State.bits64 st.random_state)
        in
        Expr.Const (Number.Float bits))
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
        Expr.Const (Number.Float32 bits))
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
      ~f:
        (maybe_opaque st ~enabled:st.swarm.opaque_leaves
           ~probability:Config.opaque_leaf_probability)
  in
  gen_const nty

(* CR-someday xclerc: Let arrays have arbitrary types, including records. *)
let gen_array st env nty dimensions =
  if with_probability st ~probability:Config.array_literal_probability
  then
    let count = List.fold_left ( * ) 1 dimensions in
    let pool =
      List.init
        (random_int_in_range st ~min:1 ~max:(max 1 (count / 2)))
        (fun _ -> Gen.run_exn (gen_numeric_const st nty))
    in
    let rec literal = function
      | [] -> random_element st pool
      | size :: rest ->
        Expr.Array_literal (List.init size (fun _ -> literal rest))
    in
    literal dimensions
  else
    let init =
      match gen_numeric_var st env nty with
      | Some generate -> generate ()
      | None -> Gen.run_exn (gen_numeric_const st nty)
    in
    Expr.Array_make { dimensions; init_name = State.fresh st; init }

let can_gen_try (st : State.t) =
  st.swarm.exceptions && st.tries_remaining > 0
  && not (List.is_empty st.exceptions)

let gen_handler (st : State.t) (env : Env.t) ~gen_body (exn : Exn.t) =
  let payload =
    match exn.payload with
    | Some ty
      when with_probability st ~probability:Config.payload_binding_probability
      ->
      Some { Binding.name = State.fresh st; ty; is_mutable = false }
    | Some _ | None -> None
  in
  let env = Option.fold payload ~none:env ~some:(Env.extend env) in
  { Handler.pattern = Exn { exn; payload }; body = gen_body env }

let gen_handlers (st : State.t) (env : Env.t) ~gen_body =
  let count =
    random_int_in_range st ~min:1
      ~max:(min Config.max_handler_count (List.length st.exceptions))
  in
  let exceptions = random_exceptions st st.exceptions ~count in
  let catch_all =
    with_probability st ~probability:Config.catch_all_probability
  in
  let body_env =
    Env.extend_catchable_exceptions env
      (if catch_all then st.exceptions else exceptions)
  in
  let handlers = List.map (gen_handler st env ~gen_body) exceptions in
  let handlers =
    if catch_all
    then handlers @ [{ Handler.pattern = Catch_all; body = gen_body env }]
    else handlers
  in
  body_env, handlers

let rec gen_number (st : State.t) (env : Env.t) (nty : NumberTy.t) =
  let gen_ty nty =
    Gen.run_exn
      (Gen.weighted st.random_state
         [1, Gen.create (fun () -> random_number_ty st); 3, Gen.return nty])
  in
  let gen_binop nty =
    Gen.create (fun () ->
        let inner_ty = gen_ty nty in
        let binop =
          Bin_op.ops_for_ty (Ty.Number inner_ty)
          |> List.filter (fun (op : Bin_op.t) ->
              match op with
              | Bit_and | Bit_or | Bit_xor | Shift_left | Shift_right
              | Shift_right_logical ->
                st.swarm.bitwise_operations
              | _ -> true)
          |> random_element st
        in
        let lhs = gen_number st env inner_ty in
        let rhs =
          match binop with
          | Bin_op.Shift_left | Bin_op.Shift_right | Bin_op.Shift_right_logical
            ->
            gen_shift_count st inner_ty
          | _ -> gen_number st env inner_ty
        in
        Expr.Convert
          { from = inner_ty;
            to_ = nty;
            expr = Expr.Bin_op { ty = Ty.Number inner_ty; op = binop; lhs; rhs }
          })
  in
  let leaf =
    Gen.weighted st.random_state
      [2, gen_numeric_var st env nty; 1, gen_numeric_const st nty]
  in
  Gen.run_exn
    (Gen.weighted st.random_state
       [ 5, leaf;
         3, gen_binop nty;
         1, gen_fun_call st env (Ty.Number nty);
         1, gen_try_expr st env (Ty.Number nty);
         1, gen_match st env (Ty.Number nty) ])

(* CR-someday xclerc: These function calls can have side effects, because of
   mutable records. Currently we rely on a de facto right-to-left evaluation
   order, which holds in many cases but is not specified. We should either do
   some sort of effect analysis in the style of Efftester, make sure we pull
   function calls out of expressions into let-bindings for example. Or we can
   just decide that we treat an evaluation order violation as a bug. *)

(* Generates a function call, possibly by creating a new function. This amounts
   to lazy function generation. Note that once [Config.max_function_count] have
   been generated, we do not allow calls to existing functions as well. This
   serves to limit the exponential blowup of nesting loops and functions and
   follows the design of CSmith. *)
and gen_fun_call (st : State.t) (caller_env : Env.t) return_ty =
  if
    (not st.swarm.function_calls)
    || not (State.can_create_function st ~max:Config.max_function_count)
  then Gen.unavailable
  else
    let gen_arguments params =
      List.map
        (fun (binding : Binding.t) -> gen_value st caller_env binding.ty)
        params
    in
    let functions =
      List.filter
        (fun (function_ : Function.t) ->
          can_convert st function_.return_ty return_ty
          && List.for_all (Env.can_raise caller_env) function_.raises)
        st.top_level_functions
    in
    let existing_function =
      Gen.when_
        (not (List.is_empty functions))
        (fun () ->
          let function_ = random_element st functions in
          let args = gen_arguments function_.params in
          convert_value st function_.return_ty return_ty
            (Expr.Call_toplevel { fun_name = function_.name; args }))
    in
    let new_function =
      Gen.create (fun () ->
          let name = State.reserve_function st in
          let params =
            List.init
              (random_int_in_range st ~min:Config.fun_min_param_count
                 ~max:Config.fun_max_param_count) (fun _ ->
                { Binding.name = State.fresh st;
                  ty = gen_type st;
                  is_mutable =
                    st.swarm.mutable_bindings
                    && with_probability st
                         ~probability:Config.mutable_binding_probability
                })
          in
          let raises =
            List.filter
              (fun _ ->
                with_probability st
                  ~probability:Config.callee_raises_probability)
              caller_env.catchable_exceptions
          in
          let callee_env =
            List.fold_left Env.extend
              { Env.empty with catchable_exceptions = raises }
              params
          in
          let inline : Inline.t =
            let open Inline in
            Gen.run_exn
              (Gen.weighted st.random_state
                 [ 1, Gen.when_ st.swarm.never_inline (fun () -> Never);
                   1, Gen.when_ st.swarm.always_inline (fun () -> Always);
                   1, Gen.return Default ])
          in
          let args = gen_arguments params in
          let _callee_env, body = gen_fun_body st callee_env 0 in
          (* CR-someday xclerc: Make sure we use a result that uses as many
             generated variables as possible. Otherwise, most of the function
             might become dead code, which is a waste of CPU time for
             fuzzing. *)
          let result =
            match gen_existing st callee_env return_ty with
            | Some generate -> generate ()
            | None -> gen_value st callee_env return_ty
          in
          let function_ =
            { Function.name; params; inline; body; return_ty; result; raises }
          in
          (* Creating a function after its full body was successfully generated
             ensures that all the functions called by [function_] were already
             added to [st.top_level_functions]. This means that functions are
             topologically sorted by construction. *)
          st.top_level_functions <- function_ :: st.top_level_functions;
          Expr.Call_toplevel { fun_name = name; args })
    in
    Gen.weighted st.random_state [2, existing_function; 1, new_function]

and gen_bool (st : State.t) env =
  let gen_binop op arg_ty gen_arg =
    Gen.create (fun () ->
        let lhs = gen_arg () in
        let rhs = gen_arg () in
        Expr.Bin_op { ty = arg_ty; op; lhs; rhs })
  in
  let nty = random_number_ty st in
  let gen_number_arg () = gen_number st env nty in
  let gen_bool_arg () = gen_bool st env in
  let gen_bool_binop op = gen_binop op Ty.Bool gen_bool_arg in
  let comparison = random_element st Bin_op.[Eq; Lt; Le; Gt; Ge] in
  Gen.run_exn
    (Gen.weighted st.random_state
       [ 5, gen_binop comparison (Ty.Number nty) gen_number_arg;
         1, gen_bool_binop Bin_op.Eq;
         1, gen_bool_binop Bin_op.And;
         1, gen_bool_binop Bin_op.Or;
         1, gen_try_expr st env Ty.Bool;
         1, gen_match st env Ty.Bool ])

and gen_raise (st : State.t) (env : Env.t) : Expr.raise =
  let exn = random_element st env.catchable_exceptions in
  let payload = Option.map (fun ty -> gen_value st env ty) exn.payload in
  let notrace =
    with_probability st ~probability:Config.raise_notrace_probability
  in
  { exn; payload; notrace }

and gen_raise_expr (st : State.t) (env : Env.t) ty =
  (* [raise] can only be given a value type. *)
  let is_value =
    match ty with
    | Ty.Number { unboxed; _ } | Ty.Record { unboxed; _ } -> not unboxed
    | Ty.Array _ | Ty.Bool | Ty.Variant _ | Ty.Or_null _ -> true
    | Ty.Unboxed_tuple _ -> false
  in
  Gen.when_
    (is_value && not (List.is_empty env.catchable_exceptions))
    (fun () -> Expr.Raise (gen_raise st env))

and gen_try_expr (st : State.t) (env : Env.t) ty =
  Gen.when_ (can_gen_try st) (fun () ->
      st.tries_remaining <- st.tries_remaining - 1;
      let gen_body env =
        Gen.run_exn
          (Gen.weighted st.random_state
             [ 4, Gen.create (fun () -> gen_value st env ty);
               1, gen_raise_expr st env ty ])
      in
      let body_env, handlers = gen_handlers st env ~gen_body in
      let body = gen_body body_env in
      Expr.Try { ty; body; handlers })

and gen_value (st : State.t) env ty =
  match ty with
  | Ty.Number nty -> gen_number st env nty
  | Ty.Bool -> gen_bool st env
  | Ty.Array (nty, dimensions) ->
    Gen.run_exn
      (Gen.weighted st.random_state
         [ 3, gen_existing st env ty;
           2, Gen.create (fun () -> gen_array st env nty dimensions);
           1, gen_fun_call st env ty;
           1, gen_try_expr st env ty;
           1, gen_match st env ty ])
  | Ty.Unboxed_tuple components ->
    let construct =
      Gen.create (fun () ->
          Expr.Unboxed_tuple
            (List.map
               (fun ty ->
                 let name = State.fresh st in
                 name, gen_value st env ty)
               components))
    in
    Gen.run_exn
      (Gen.weighted st.random_state
         [ 3, gen_existing st env ty;
           2, construct;
           1, gen_fun_call st env ty;
           1, gen_try_expr st env ty;
           1, gen_match st env ty ])
  | Ty.Record record ->
    let existing = gen_existing st env ty in
    let construct =
      Gen.create (fun () ->
          Expr.Record
            ( record,
              List.map
                (fun (field : Ty.field) -> gen_value st env field.ty)
                record.fields ))
    in
    let update =
      Gen.map existing ~f:(fun base ->
          let field = random_element st record.fields in
          Expr.Record_update (record, base, field, gen_value st env field.ty))
      |> Gen.only_if st.swarm.record_updates
    in
    Gen.run_exn
      (Gen.weighted st.random_state
         [ 3, existing;
           2, construct;
           1, update;
           1, gen_fun_call st env ty;
           1, gen_try_expr st env ty;
           1, gen_match st env ty ])
  | Ty.Variant variant -> gen_sum_value st env (Sum.Variant variant)
  | Ty.Or_null payload -> gen_sum_value st env (Sum.Or_null payload)

and gen_sum_value (st : State.t) env sum =
  Gen.run_exn
    (Gen.weighted st.random_state
       [ 5, Gen.create (fun () -> gen_sum st env sum);
         1, gen_match st env (Sum.ty sum) ])

and gen_constructor st env sum (constructor : Ty.constructor) =
  let args =
    List.map
      (fun ty ->
        let name = State.fresh st in
        name, gen_value st env ty)
      constructor.args
  in
  Expr.Construct { sum; constructor; args }

and gen_sum (st : State.t) env sum =
  let ty = Sum.ty sum in
  Gen.run_exn
    (Gen.weighted st.random_state
       [ 3, gen_existing st env ty;
         ( 2,
           Gen.create (fun () ->
               gen_constructor st env sum
                 (random_element st (Sum.constructors sum))) );
         1, gen_fun_call st env ty ])

and gen_match_expr st env sum scrutinee ~branch =
  let cases =
    List.map
      (fun (constructor : Ty.constructor) ->
        let bindings =
          List.map
            (fun ty ->
              { Binding.name = State.fresh st; ty; is_mutable = false })
            constructor.args
        in
        let branch_env = List.fold_left Env.extend env bindings in
        { Expr.constructor; bindings; body = branch branch_env constructor })
      (Sum.constructors sum)
  in
  Expr.Match { sum; scrutinee; cases }

and gen_match (st : State.t) env result_ty =
  Gen.when_
    (st.swarm.matches && st.matches_remaining > 0
    && (st.swarm.or_null || not (List.is_empty st.variant_types)))
    (fun () ->
      (* A global budget bounds the extra branching added to expression
         generation. *)
      st.matches_remaining <- st.matches_remaining - 1;
      let sum = gen_sum_type st in
      let nested =
        st.matches_remaining > 0
        && with_probability st ~probability:Config.match_in_match_probability
      in
      let scrutinee, reachable_constructors =
        if nested
        then (
          st.matches_remaining <- st.matches_remaining - 1;
          let input_sum = gen_sum_type st in
          let constructors = Sum.constructors sum in
          let count = List.length constructors in
          let offset = Random.State.int st.random_state count in
          let output_constructor (input_constructor : Ty.constructor) =
            List.nth constructors
              ((input_constructor.constructor_index + offset) mod count)
          in
          let reachable_constructors =
            List.map output_constructor (Sum.constructors input_sum)
          in
          let input = Expr.Opaque (gen_sum st env input_sum) in
          (* Hide the input tag, but preserve the produced tags so that the
             outer match can be specialized at the producer's branch exits. *)
          let scrutinee =
            gen_match_expr st env input_sum input
              ~branch:(fun env constructor ->
                gen_constructor st env sum (output_constructor constructor))
          in
          scrutinee, reachable_constructors)
        else Expr.Opaque (gen_sum st env sum), Sum.constructors sum
      in
      gen_match_expr st env sum scrutinee
        ~branch:(fun env (constructor : Ty.constructor) ->
          let reachable =
            List.exists
              (fun (reachable : Ty.constructor) ->
                Int.equal reachable.constructor_index
                  constructor.constructor_index)
              reachable_constructors
          in
          match st.swarm.assertions, reachable with
          | Unreachable_only, false -> Expr.Assert_false
          | (Disabled | May_raise), _ | Unreachable_only, true ->
            gen_value st env result_ty))

and gen_decl (st : State.t) env =
  let binding =
    { Binding.name = State.fresh st;
      ty = gen_type st;
      is_mutable =
        st.swarm.mutable_bindings
        && with_probability st ~probability:Config.mutable_binding_probability
    }
  in
  let expr =
    maybe_opaque st ~enabled:st.swarm.opaque_initializers
      ~probability:Config.opaque_initializer_probability
      (gen_value st env binding.ty)
  in
  Env.extend env binding, (binding, expr)

and gen_fun_body (st : State.t) (env : Env.t) depth =
  let stmt_count = 1 + Random.State.int st.random_state 4 in
  let rec gen (env : Env.t) remaining =
    if remaining = 0
    then env, Statement.Seq []
    else
      let continue env statement =
        let env, rest = gen env (remaining - 1) in
        env, Statement.sequence statement rest
      in
      let targets = places st env ~for_write:true in
      let gen_assign =
        Gen.when_
          (not (List.is_empty targets))
          (fun () ->
            let ty, make = random_element st targets in
            let target = make () in
            let expr = gen_value st env ty in
            continue env (Statement.Assign (target, expr)))
      in
      let gen_if =
        Gen.when_ st.swarm.conditionals (fun () ->
            let condition = gen_bool st env in
            let _env_l, left = gen_fun_body st env (depth + 1) in
            let _env_r, right = gen_fun_body st env (depth + 1) in
            continue env (Statement.If (condition, left, right)))
      in
      let gen_local_decl =
        Gen.when_ (remaining > 1) (fun () ->
            let env, (binding, expr) = gen_decl st env in
            let env, body = gen env (remaining - 1) in
            env, Statement.Let (binding, expr, body))
      in
      (* CR-someday xclerc: Skew the generation of loop bounds, so that they fit
         inside the bounds of an array dimensions with higher probability.
         (Similar to CSmith) *)
      let gen_bounded_loop =
        Gen.when_ st.swarm.bounded_loops (fun () ->
            let name = State.fresh st in
            (* We iterate 1-3 times, more iterations have diminishing returns of
               bugs found vs CPU time. *)
            let times = 1 + Random.State.int st.random_state 3 in
            (* Apply [i -> scale * i + offset] to [times; ...; 1], giving a
               stride of [-scale]. *)
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
              maybe_opaque st ~enabled:st.swarm.opaque_loop_bounds
                ~probability:Config.opaque_loop_bound_probability
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
      let gen_try_stmt =
        Gen.when_ (can_gen_try st) (fun () ->
            st.tries_remaining <- st.tries_remaining - 1;
            let gen_body env = snd (gen_fun_body st env (depth + 1)) in
            let body_env, handlers = gen_handlers st env ~gen_body in
            let body = gen_body body_env in
            continue env (Statement.Try { body; handlers }))
      in
      (* A [raise] ends the block, as any statement after it would be dead
         code. *)
      let gen_raise_stmt =
        Gen.when_
          (not (List.is_empty env.catchable_exceptions))
          (fun () -> env, Statement.Raise (gen_raise st env))
      in
      let gen_empty =
        Gen.when_ (List.is_empty targets) (fun () -> env, Statement.Seq [])
      in
      let allowed =
        if depth >= Config.max_block_depth
        then [4, gen_assign; 4, gen_empty; 1, gen_raise_stmt]
        else
          [ 4, gen_assign;
            1, gen_if;
            1, gen_bounded_loop;
            1, gen_local_decl;
            1, gen_try_stmt;
            1, gen_raise_stmt ]
      in
      match Gen.weighted st.random_state allowed with
      | Some generate -> generate ()
      | None -> env, Statement.Seq []
  in
  gen env stmt_count

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
  Program.create ~swarm:st.swarm ~record_types:st.record_types
    ~exceptions:st.exceptions ~variant_types:st.variant_types
    ~functions:(List.rev st.top_level_functions)
    ~toplevel_decls ~toplevel_statement

let generate random =
  let state = State.create random in
  state.record_types <- gen_record_types state;
  state.exceptions <- gen_exceptions state;
  state.variant_types <- gen_variant_types state;
  gen_program state Env.empty
