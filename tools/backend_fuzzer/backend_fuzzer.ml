(* CR hwasilewski: split the file into multiple files *)

open Ast_helper
open Asttypes

let loc name = { Location.txt = name; loc = Location.none }

let lid name = loc (Longident.Lident name)

let ldot module_name name = loc (Longident.Ldot (lid module_name, loc name))

let ident name = Exp.ident (lid name)

let qualified_ident module_name name = Exp.ident (ldot module_name name)

let int n = Exp.constant (Const.int n)

let unit_ = Exp.construct (lid "()") None

let apply id args = Exp.apply id (List.map (fun a -> Nolabel, a) args)

let op name args = apply (ident name) args

let value_param pattern : Parsetree.function_param =
  { pparam_loc = Location.none;
    pparam_desc = Pparam_val (Nolabel, None, pattern)
  }

let function_ params body =
  Exp.function_ params
    { mode_annotations = [];
      ret_mode_annotations = [];
      ret_type_constraint = None
    }
    (Pfunction_body body)

module Name = struct
  type t = string

  let of_string string =
    let invalid () =
      invalid_arg (Format.sprintf "Name.of_string: invalid name %S" string)
    in
    match Misc.Utf8_lexeme.normalize string with
    | Error _ -> invalid ()
    | Ok name ->
      if
        String.equal name "" || String.equal name "_"
        || Misc.Utf8_lexeme.is_capitalized name
        || not (Misc.Utf8_lexeme.is_valid_identifier name)
      then invalid ()
      else name

  let to_string s = s
end

module NumberTy = struct
  module Base = struct
    type t =
      | Float
      | Float32
      | Int
      | Nativeint
      | Int64
      | Int32
      | Int16
      | Int8

    let all = [Float; Float32; Int; Nativeint; Int64; Int32; Int16; Int8]

    let equal left right =
      match left, right with
      | Float, Float
      | Float32, Float32
      | Int, Int
      | Nativeint, Nativeint
      | Int64, Int64
      | Int32, Int32
      | Int16, Int16
      | Int8, Int8 ->
        true
      | (Float | Float32 | Int | Nativeint | Int64 | Int32 | Int16 | Int8), _ ->
        false

    let to_module = function
      | Float -> "Float"
      | Float32 -> "Float32"
      | Int -> "Int"
      | Nativeint -> "Nativeint"
      | Int64 -> "Int64"
      | Int32 -> "Int32"
      | Int16 -> "Int16"
      | Int8 -> "Int8"

    let is_float = function
      | Float | Float32 -> true
      | Int | Nativeint | Int64 | Int32 | Int16 | Int8 -> false
  end

  type t =
    { base : Base.t;
      unboxed : bool
    }

  let boxed base = { base; unboxed = false }

  let unboxed base = { base; unboxed = true }

  let equal left right =
    Base.equal left.base right.base && Bool.equal left.unboxed right.unboxed

  let to_module t = Base.to_module t.base ^ if t.unboxed then "_u" else ""

  let to_string ?(no_hash = false) t =
    String.lowercase_ascii (Base.to_module t.base)
    ^ if t.unboxed then if no_hash then "_u" else "#" else ""

  let converter_name ~from ~to_ =
    Format.sprintf "%s_of_%s"
      (to_string ~no_hash:true to_)
      (to_string ~no_hash:true from)

  let is_float t = Base.is_float t.base

  let all = List.concat_map (fun base -> [boxed base; unboxed base]) Base.all
end

module Number = struct
  type t =
    | Float of float
    | Float32 of float
    | Int of int
    | Nativeint of Nativeint.t
    | Int64 of int64
    | Int32 of int32
    | Int16 of int
    | Int8 of int

  (* CR-someday hwasilewski: This feels like a hacky workaround, maybe change
     the generation to be more direct for each numeric type. *)

  (* This function truncates an int64 to an int of a given [width], meaning that
     its last [width] bits are kept, the rest is zeroed and the sign is
     preserved. *)
  let truncate_signed ~width x =
    let shift = 64 - width in
    Int64.(to_int (shift_right (shift_left x shift) shift))

  let of_integral_bits (base : NumberTy.Base.t) bits =
    match base with
    | Int -> Int (Int64.to_int bits)
    | Nativeint -> Nativeint (Int64.to_nativeint bits)
    | Int64 -> Int64 bits
    | Int32 -> Int32 (Int64.to_int32 bits)
    | Int16 -> Int16 (truncate_signed ~width:16 bits)
    | Int8 -> Int8 (truncate_signed ~width:8 bits)
    | Float | Float32 -> invalid_arg "Number.of_integral_bits"

  (* [nan] and the infinities have no literal form, so they are emitted as
     identifiers rather than constants. *)
  let float_code m ?suffix x =
    if Float.is_nan x
    then qualified_ident m "nan"
    else if x = Float.infinity
    then qualified_ident m "infinity"
    else if x = Float.neg_infinity
    then qualified_ident m "neg_infinity"
    else Exp.constant (Const.float ?suffix (Printf.sprintf "%h" x))

  let to_code = function
    | Float n -> float_code "Float" n
    | Float32 n -> float_code "Float32" ~suffix:'s' n
    | Int n -> Exp.constant (Const.int n)
    | Nativeint n -> Exp.constant (Const.nativeint n)
    | Int64 n -> Exp.constant (Const.int64 n)
    | Int32 n -> Exp.constant (Const.int32 n)
    | Int16 n -> Exp.constant (Const.int ~suffix:'S' n)
    | Int8 n -> Exp.constant (Const.int ~suffix:'s' n)
end

module Ty = struct
  type t =
    | Number of NumberTy.t
    | Bool

  let equal left right =
    match left, right with
    | Number l, Number r -> NumberTy.equal l r
    | Bool, Bool -> true
    | (Number _ | Bool), _ -> false
end

module Bin_op = struct
  (* CR-soon hwasilewski: Add more operators. *)
  type t =
    | Add
    | Sub
    | Mul
    | Eq
    | And
    | Or

  let num_binops = [Add; Sub; Mul]

  let ops_for_ty (ty : Ty.t) =
    match ty with Number _ -> num_binops | Bool -> assert false

  let to_code ty op =
    let name = function
      | Add -> "add"
      | Sub -> "sub"
      | Mul -> "mul"
      | Eq -> "equal"
      | And | Or -> assert false
    in
    match ty, op with
    | _, And -> ident "&&"
    | _, Or -> ident "||"
    | Ty.Number nty, op -> qualified_ident (NumberTy.to_module nty) (name op)
    | Ty.Bool, op -> qualified_ident "Bool" (name op)
end

module Expr = struct
  type t =
    | Const of Number.t
    | Var of Name.t
    | Bin_op of
        { ty : Ty.t;
          op : Bin_op.t;
          lhs : t;
          rhs : t
        }
    | Convert of
        { expr : t;
          from : NumberTy.t;
          to_ : NumberTy.t
        }
    | Call_toplevel of
        { fun_name : Name.t;
          args : t list
        }

  let convert_num expr ~(from : NumberTy.t) ~(to_ : NumberTy.t) =
    if NumberTy.equal from to_
    then expr
    else apply (ident (NumberTy.converter_name ~from ~to_)) [expr]

  let rec to_code : t -> Parsetree.expression = function
    | Const n -> Number.to_code n
    | Var name ->
      Exp.ident { Location.txt = Longident.Lident name; loc = Location.none }
    | Bin_op { ty; op; lhs; rhs } ->
      Exp.apply (Bin_op.to_code ty op)
        [Nolabel, to_code lhs; Nolabel, to_code rhs]
    | Convert { expr; from; to_ } -> convert_num (to_code expr) ~from ~to_
    | Call_toplevel { fun_name; args } ->
      apply (ident (Name.to_string fun_name)) (List.map to_code args)
end

module Statement = struct
  type t =
    | Assign of Name.t * Expr.t
    | Seq of t list
    | If of Expr.t * t * t
    | Let_mutable of Name.t * Expr.t * t
    | Bounded_loop of Name.t * int * t

  let let_mutable name expr body =
    Exp.let_ Mutable Nonrecursive
      [Vb.mk (Pat.var (Name.to_string name |> loc)) expr]
      body

  let sequence statement = function
    | Seq statements -> Seq (statement :: statements)
    | rest -> Seq [statement; rest]

  let rec to_code : t -> Parsetree.expression = function
    | Assign (name, expr) ->
      Exp.setinstvar (Name.to_string name |> loc) (Expr.to_code expr)
    | Bounded_loop (loop_var, times, stmt) ->
      let var = ident (Name.to_string loop_var) in
      let_mutable loop_var
        (Exp.apply
           (qualified_ident "Sys" "opaque_identity")
           [Nolabel, int times])
        (Exp.while_
           (op ">" [var; int 0])
           (Exp.sequence (to_code stmt)
              (Exp.setinstvar
                 (Name.to_string loop_var |> loc)
                 (op "-" [var; int 1]))))
    | Let_mutable (name, expr, body) ->
      let_mutable name (Expr.to_code expr) (to_code body)
    | Seq statements ->
      List.fold_right Exp.sequence (List.map to_code statements) unit_
    | If (condition, if_true, if_false) ->
      Exp.ifthenelse (Expr.to_code condition) (to_code if_true)
        (Some (to_code if_false))
end

module Function = struct
  type t =
    { name : Name.t;
      params : (Name.t * Ty.t) list;
      body : Statement.t;
      return_ty : NumberTy.t;
      result : Expr.t
    }

  let to_code { name; params; body; return_ty = _; result } =
    let names = List.map (fun (name, _ty) -> name) params in
    let to_param (name, _ty) = value_param (Pat.var (loc name)) in
    let function_params =
      match params with
      | [] -> [value_param (Pat.construct (lid "()") None)]
      | _ -> List.map to_param params
    in
    let body = Exp.sequence (Statement.to_code body) (Expr.to_code result) in
    let body =
      List.fold_right
        (fun name body ->
          Statement.let_mutable name
            (apply (qualified_ident "Sys" "opaque_identity") [ident name])
            body)
        names body
    in
    Str.value Nonrecursive
      [ Vb.mk
          (Pat.var (loc name))
          (* CR-soon hwasilewski: With some probability add either the inline
             never or inline always attribute. *)
          (function_ function_params body) ]
end

module Program = struct
  type t =
    { functions : Function.t list;
      main_decls : (Name.t * Ty.t * Expr.t) list;
      main_statement : Statement.t
    }

  let is_float_to_integral ~from ~to_ =
    NumberTy.is_float from && not (NumberTy.is_float to_)

  let unsafe_converter_name ~from ~to_ =
    "unsafe_" ^ NumberTy.converter_name ~from ~to_

  let conversions =
    List.concat_map
      (fun from ->
        List.map
          (fun to_ ->
            if NumberTy.equal from to_
            then ""
            else
              let from_name = NumberTy.to_string from in
              let to_name = NumberTy.to_string to_ in
              let name = Format.sprintf "%s_of_%s" to_name from_name in
              let function_name =
                if is_float_to_integral ~from ~to_
                then unsafe_converter_name ~from ~to_
                else NumberTy.converter_name ~from ~to_
              in
              Format.sprintf {|external %s : %s -> %s = "%%%s"@.|} function_name
                from_name to_name name)
          NumberTy.all)
      NumberTy.all
    |> String.concat ""

  let integral_bound_name base =
    String.lowercase_ascii (NumberTy.Base.to_module base)

  let integral_size (base : NumberTy.Base.t) =
    match base with
    | Int -> "Sys.int_size"
    | Nativeint -> "Nativeint.size"
    | Int64 -> "64"
    | Int32 -> "32"
    | Int16 -> "16"
    | Int8 -> "8"
    | Float | Float32 -> invalid_arg "Program.integral_size"

  (* Float-to-integer primitives are unspecified for NaN and out-of-range
     inputs. These wrappers map NaN to zero and saturate infinities and
     overflow. *)
  let integral_bounds =
    List.filter_map
      (fun base ->
        if NumberTy.Base.is_float base
        then None
        else
          let name = integral_bound_name base in
          Some
            (Format.sprintf
               "let %s_upper_bound = Float.ldexp 1.0 (%s - 1)@.let \
                %s_lower_bound = -. %s_upper_bound@."
               name (integral_size base) name name))
      NumberTy.Base.all
    |> String.concat ""

  let integral_value (to_ : NumberTy.t) value =
    let value = qualified_ident (NumberTy.Base.to_module to_.base) value in
    if to_.unboxed
    then
      apply
        (ident (NumberTy.converter_name ~from:(NumberTy.boxed to_.base) ~to_))
        [value]
    else value

  let float_for_comparison from expr =
    let boxed_float = NumberTy.boxed Float in
    if NumberTy.equal from boxed_float
    then expr
    else apply (ident (NumberTy.converter_name ~from ~to_:boxed_float)) [expr]

  let float_to_integral_conversions =
    let wrapper (from : NumberTy.t) (to_ : NumberTy.t) =
      let x = ident "x" in
      let x_for_comparison = ident "x_for_comparison" in
      let bound_name = integral_bound_name to_.base in
      let compare_to_bound comparison suffix =
        op comparison
          [ apply
              (qualified_ident "Float" "compare")
              [x_for_comparison; ident (bound_name ^ suffix)];
            int 0 ]
      in
      let body =
        Exp.ifthenelse
          (apply (qualified_ident "Float" "is_nan") [x_for_comparison])
          (integral_value to_ "zero")
          (Some
             (Exp.ifthenelse
                (compare_to_bound ">=" "_upper_bound")
                (integral_value to_ "max_int")
                (Some
                   (Exp.ifthenelse
                      (compare_to_bound "<" "_lower_bound")
                      (integral_value to_ "min_int")
                      (Some
                         (apply (ident (unsafe_converter_name ~from ~to_)) [x]))))))
      in
      let body =
        Exp.let_ Immutable Nonrecursive
          [ Vb.mk
              (Pat.var (loc "x_for_comparison"))
              (float_for_comparison from x) ]
          body
      in
      Str.value Nonrecursive
        [ Vb.mk
            (Pat.var (loc (NumberTy.converter_name ~from ~to_)))
            (function_ [value_param (Pat.var (loc "x"))] body) ]
    in
    List.concat_map
      (fun from ->
        List.filter_map
          (fun to_ ->
            if is_float_to_integral ~from ~to_
            then Some (wrapper from to_)
            else None)
          NumberTy.all)
      NumberTy.all

  (* We check floats for equality by testing if their bits are equal. This is
     correct unless they are NaN, which is why before comparing, we run
     [canonicalize_nan] to cast NaNs to a common representation. *)
  let preamble =
    Format.sprintf
      {|
%s
%s
let canonicalize_nan x =
  if Float.is_nan x then Float.nan else x
|}
      conversions integral_bounds

  let canon_nan_name = "canonicalize_nan"

  (* Produce an expression, which prints the value of the numeric expression [e]
     of type [nty] *)
  let print_number nty e =
    let fmt, arg =
      if NumberTy.is_float nty
      then
        ( "%h ",
          apply (ident canon_nan_name)
            [Expr.convert_num ~from:nty ~to_:(NumberTy.boxed Float) e] )
      else "%Ld ", Expr.convert_num ~from:nty ~to_:(NumberTy.boxed Int64) e
    in
    apply
      (qualified_ident "Printf" "printf")
      [Exp.constant (Const.string fmt); arg]

  let to_code { functions; main_decls = decls; main_statement = statement } =
    let print_decl (name, ty, _expr) =
      match ty with
      | Ty.Number nty -> print_number nty (ident name)
      | _ -> assert false
    in
    let body =
      Exp.sequence
        (Statement.to_code statement)
        (List.fold_left
           (fun acc decl -> Exp.sequence (print_decl decl) acc)
           unit_ decls)
    in
    let body =
      List.fold_right
        (fun (name, _ty, expr) acc ->
          Statement.let_mutable name
            (Exp.apply
               (qualified_ident "Sys" "opaque_identity")
               [Nolabel, Expr.to_code expr])
            acc)
        decls body
    in
    let main =
      Str.value Nonrecursive
        [ Vb.mk
            (Pat.var (loc "main"))
            (function_ [value_param (Pat.construct (lid "()") None)] body) ]
    in
    let run = Str.eval (Exp.apply (ident "main") [Nolabel, unit_]) in
    let code =
      Pprintast.string_of_structure
        (float_to_integral_conversions
        @ List.map Function.to_code functions
        @ [main; run])
    in
    Format.sprintf "%s\n%s\n" preamble code
end

module State = struct
  type t =
    { rs : Random.State.t;
      mutable fresh_counter : int;
      mutable function_counter : int;
      mutable toplevel_functions : Function.t list
    }

  let create rs =
    { rs; fresh_counter = 0; function_counter = 0; toplevel_functions = [] }

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
  type t = { tenv : (Name.t * Ty.t) list }

  let empty = { tenv = [] }

  let extend t (name, ty) = { tenv = (name, ty) :: t.tenv }
end

module Config = struct
  let fun_count = 5

  let fun_min_param_count = 1

  let fun_max_param_count = 5

  let fun_depth = 3

  let expression_complexity = 10

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

  let weighted rs choices =
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
          select (Random.State.int rs total) choices)

  let _uniform rs choices =
    weighted rs (List.map (fun choice -> 1, choice) choices)

  let run_exn = function
    | None -> invalid_arg "Gen.run_exn: no available generator"
    | Some generate -> generate ()
end

let random_element (st : State.t) list =
  List.nth list (Random.State.int st.rs (List.length list))

let random_number_ty st = random_element st NumberTy.all

let range (st : State.t) ~min ~max = Random.State.int_in_range st.rs ~min ~max

let with_expression_complexity f = f ~complexity:(ref 0)

let can_recurse ~complexity = !complexity + 2 <= Config.expression_complexity

let record_leaf expr ~complexity =
  incr complexity;
  expr

let rec gen_int (st : State.t) (env : Env.t) (nty : NumberTy.t) ~complexity =
  let gen_const_int base =
    Gen.create (fun () ->
        let small ~min ~max =
          Gen.create (fun () ->
              Random.State.int_in_range st.rs ~min ~max |> Int64.of_int)
        in
        let bits =
          Gen.run_exn
            (Gen.weighted st.rs
               [ 1, small ~min:(-1) ~max:1;
                 1, small ~min:(-10) ~max:10;
                 2, Gen.create (fun () -> Random.State.bits64 st.rs) ])
        in
        record_leaf (Expr.Const (Number.of_integral_bits base bits)) ~complexity)
  in
  let gen_const_float =
    Gen.create (fun () ->
        (* CR-soon hwasilewski: Add a skewed distribution of floats similar to
           [gen_const_int]. *)
        record_leaf
          (Expr.Const
             (Number.Float (Int64.float_of_bits (Random.State.bits64 st.rs))))
          ~complexity)
  in
  let gen_const_float32 =
    Gen.create (fun () ->
        record_leaf
          (Expr.Const
             (Number.Float32 (Int32.float_of_bits (Random.State.bits32 st.rs))))
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
      List.filter (fun (_, vty) -> Ty.equal (Ty.Number nty) vty) env.tenv
    in
    Gen.when_
      (not (List.is_empty vars))
      (fun () ->
        let name, _ty = random_element st vars in
        record_leaf (Expr.Var name) ~complexity)
  in
  let gen_ty nty =
    Gen.run_exn
      (Gen.weighted st.rs
         [1, Gen.create (fun () -> random_number_ty st); 3, Gen.return nty])
  in
  let gen_binop nty =
    Gen.when_ (can_recurse ~complexity) (fun () ->
        let inner_ty = gen_ty nty in
        let binop = random_element st (Bin_op.ops_for_ty (Ty.Number nty)) in
        let lhs = gen_int st env inner_ty ~complexity in
        let rhs = gen_int st env inner_ty ~complexity in
        Expr.Convert
          { from = inner_ty;
            to_ = nty;
            expr = Expr.Bin_op { ty = Ty.Number inner_ty; op = binop; lhs; rhs }
          })
  in
  let standard =
    Gen.weighted st.rs [2, gen_var nty; 1, gen_const nty; 4, gen_binop nty]
  in
  Gen.run_exn
    (Gen.weighted st.rs [8, standard; 2, gen_fun_call st env nty ~complexity])

and gen_fun_call (st : State.t) caller_env return_ty ~complexity =
  if not (can_recurse ~complexity)
  then Gen.unavailable
  else
    let gen_arguments params =
      List.map
        (fun (_, ty) ->
          match ty with
          | Ty.Number nty -> gen_int st caller_env nty ~complexity
          (* CR-soon hwasilewski: add bool arguments *)
          | Ty.Bool -> assert false)
        params
    in
    let call_existing_function () =
      let function_ = random_element st st.toplevel_functions in
      let args = gen_arguments function_.params in
      record_leaf
        (Expr.Convert
           { expr = Expr.Call_toplevel { fun_name = function_.name; args };
             from = function_.return_ty;
             to_ = return_ty
           })
        ~complexity
    in
    let existing_function =
      Gen.when_
        (not (List.is_empty st.toplevel_functions))
        call_existing_function
    in
    let new_function =
      Gen.when_ (State.can_create_function st ~max:Config.fun_count) (fun () ->
          let name = State.reserve_function st in
          let parameter_types =
            List.init
              (range st ~min:Config.fun_min_param_count
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
                gen_int st callee_env return_ty ~complexity)
          in
          let function_ = { Function.name; params; body; return_ty; result } in
          st.toplevel_functions <- function_ :: st.toplevel_functions;
          record_leaf (Expr.Call_toplevel { fun_name = name; args }) ~complexity)
    in
    Gen.weighted st.rs [1, existing_function; 1, new_function]

and gen_bool (st : State.t) env ~complexity =
  let gen_binop op arg_ty gen_arg =
    Gen.create (fun () ->
        let lhs = gen_arg ~complexity in
        let rhs = gen_arg ~complexity in
        Expr.Bin_op { ty = arg_ty; op; lhs; rhs })
  in
  let nty = random_number_ty st in
  let gen_number_arg ~complexity = gen_int st env nty ~complexity in
  let gen_bool_arg ~complexity = gen_bool st env ~complexity in
  let gen_bool_binop op =
    if can_recurse ~complexity
    then gen_binop op Ty.Bool gen_bool_arg
    else Gen.unavailable
  in
  Gen.run_exn
    (Gen.weighted st.rs
       [ 1, gen_binop Bin_op.Eq (Ty.Number nty) gen_number_arg;
         1, gen_bool_binop Bin_op.Eq;
         1, gen_bool_binop Bin_op.And;
         1, gen_bool_binop Bin_op.Or ])

and gen_decl st env =
  let name = State.fresh st in
  let nty = random_number_ty st in
  let expr =
    with_expression_complexity (fun ~complexity ->
        gen_int st env nty ~complexity)
  in
  let env = Env.extend env (name, Ty.Number nty) in
  env, (name, Ty.Number nty, expr)

and gen_fun_body (st : State.t) (env : Env.t) depth =
  let stmt_count = 1 + Random.State.int st.rs 4 in
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
          (not (List.is_empty env.Env.tenv))
          (fun () ->
            let name, ty = random_element st env.Env.tenv in
            (* CR-soon hwasilewski: Add boolean variable generation. *)
            match ty with
            | Ty.Number nty ->
              let expr =
                with_expression_complexity (fun ~complexity ->
                    gen_int st env nty ~complexity)
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
            let times = 1 + Random.State.int st.rs 3 in
            let _, inner = gen_fun_body st env (depth + 1) in
            continue env (Statement.Bounded_loop (name, times, inner)))
      in
      let allowed =
        if depth >= Config.fun_depth
        then [1, gen_assign]
        else [4, gen_assign; 1, gen_if; 1, gen_bounded_loop; 1, gen_local_decl]
      in
      Gen.run_exn (Gen.weighted st.rs allowed)
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
  { Program.functions = List.rev st.toplevel_functions;
    main_decls;
    main_statement
  }

let () =
  let master = Random.State.make_self_init () in
  let st = State.create master in
  Printf.printf "%s" (Program.to_code (gen_program st Env.empty))
