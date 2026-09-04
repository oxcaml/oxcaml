(* The generator's internal representation of a subset of OxCaml. *)
open Ast_helper
open Asttypes
open Parsetree_helpers

(** Names of generated value bindings, including variables and functions. *)
module Name = struct
  type t = string

  (** Check the string is a valid lowercase value identifier. *)
  let of_string string =
    let invalid () =
      Misc.fatal_errorf "Name.of_string: invalid name %S" string
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

    let is_floating_point = function
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

  let is_floating_point t = Base.is_floating_point t.base

  let all = List.concat_map (fun base -> [boxed base; unboxed base]) Base.all
end

module Number = struct
  type t =
    | Float of int64
    | Float32 of int32
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
    | Float | Float32 ->
      Misc.fatal_errorf "Number.of_integral_bits: expected an integral type"

  let to_code = function
    | Float bits ->
      apply
        (qualified_ident "Int64" "float_of_bits")
        [Exp.constant (Const.int64 bits)]
    | Float32 bits ->
      apply
        (qualified_ident "Float32" "of_bits")
        [Exp.constant (Const.int32 bits)]
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
    | Lt
    | Le
    | Gt
    | Ge
    | And
    | Or

  let num_binops = [Add; Sub; Mul]

  let ops_for_ty (ty : Ty.t) =
    match ty with
    | Number _ -> num_binops
    | Bool ->
      Misc.fatal_errorf
        "Bin_op.ops_for_ty: only numeric types allowed, but got Bool"

  let to_code ty binop lhs rhs =
    let module_name =
      match ty with
      | Ty.Number nty -> NumberTy.to_module nty
      | Ty.Bool -> "Bool"
    in
    let call name = apply (qualified_ident module_name name) [lhs; rhs] in
    match binop with
    | Add -> call "add"
    | Sub -> call "sub"
    | Mul -> call "mul"
    | Eq -> call "equal"
    | Lt -> op "<" [call "compare"; int 0]
    | Le -> op "<=" [call "compare"; int 0]
    | Gt -> op ">" [call "compare"; int 0]
    | Ge -> op ">=" [call "compare"; int 0]
    | And -> op "&&" [lhs; rhs]
    | Or -> op "||" [lhs; rhs]
end

module Expr = struct
  (* CR-soon hwasilewski: Currently this type is constructed manually, but that
     is a mistake, as we can create for example a [Bin_op] with invalid types.
     To mitigate, we should make [t] private and add smart constructors. *)
  type t =
    | Const of Number.t
    | Var of Name.t
    | Opaque of t
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
    | Opaque expr ->
      apply (qualified_ident "Sys" "opaque_identity") [to_code expr]
    | Bin_op { ty; op; lhs; rhs } ->
      Bin_op.to_code ty op (to_code lhs) (to_code rhs)
    | Convert { expr; from; to_ } -> convert_num (to_code expr) ~from ~to_
    | Call_toplevel { fun_name; args } ->
      let args = match args with [] -> [unit_] | _ -> List.map to_code args in
      apply (ident (Name.to_string fun_name)) args
end

module Statement = struct
  type t =
    | Assign of Name.t * Expr.t
    | Seq of t list
    | If of Expr.t * t * t
    | Let_mutable of Name.t * Expr.t * t
    | Bounded_loop of Name.t * Expr.t * t

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
      let_mutable loop_var (Expr.to_code times)
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

module Inline = struct
  type t =
    | Never
    | Always
    | Default

  let to_attributes inline =
    let attribute payload =
      [Attr.mk (loc "inline") (PStr [Str.eval (ident payload)])]
    in
    match inline with
    | Default -> []
    | Never -> attribute "never"
    | Always -> attribute "always"
end

module Function = struct
  type t =
    { name : Name.t;
      params : (Name.t * Ty.t) list;
      inline : Inline.t;
      body : Statement.t;
      return_ty : NumberTy.t;
      result : Expr.t
    }

  let to_code { name; params; inline; body; return_ty = _; result } =
    let to_param (name, _ty) = value_param (Pat.var (loc name)) in
    let function_params =
      match params with
      | [] -> [value_param (Pat.construct (lid "()") None)]
      | _ -> List.map to_param params
    in
    let body = Exp.sequence (Statement.to_code body) (Expr.to_code result) in
    let body =
      List.fold_right
        (fun (name, _ty) body -> Statement.let_mutable name (ident name) body)
        params body
    in
    Str.value Nonrecursive
      [ Vb.mk
          ~attrs:(Inline.to_attributes inline)
          (Pat.var (loc name))
          (function_ function_params body) ]
end
