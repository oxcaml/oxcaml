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
    | Array of NumberTy.t * int list
    | Record of record
    | Bool

  and record =
    { id : int;
      fields : field list;
      unboxed : bool
    }

  and field =
    { index : int;
      ty : t;
      is_mutable : bool
    }

  let equal left right =
    match left, right with
    | Number l, Number r -> NumberTy.equal l r
    | Array (l, ls), Array (r, rs) ->
      NumberTy.equal l r && List.equal Int.equal ls rs
    | Record l, Record r -> l.id = r.id && Bool.equal l.unboxed r.unboxed
    | Bool, Bool -> true
    | (Number _ | Array _ | Record _ | Bool), _ -> false

  let record_name { id; unboxed; _ } =
    Format.sprintf "record_%d_%s" id (if unboxed then "u" else "b")

  let field_name record { index; _ } =
    Format.sprintf "%s_field_%d" (record_name record) index

  let to_code = function
    | Number nty -> Typ.constr (lid (NumberTy.to_string nty)) []
    | Array (nty, dimensions) ->
      List.fold_left
        (fun ty _ -> Typ.constr (lid "array") [ty])
        (Typ.constr (lid (NumberTy.to_string nty)) []) dimensions
    | Record record -> Typ.constr (lid (record_name record)) []
    | Bool -> Typ.constr (lid "bool") []
end

module Binding = struct
  type t =
    { name : Name.t;
      ty : Ty.t;
      is_mutable : bool
    }
end

module Bin_op = struct
  (* CR-soon hwasilewski: Add more operators. *)
  type t =
    | Add
    | Sub
    | Mul
    | Bit_and
    | Bit_or
    | Bit_xor
    | Shift_left
    | Shift_right
    | Shift_right_logical
    | Eq
    | Lt
    | Le
    | Gt
    | Ge
    | And
    | Or

  let num_binops = [Add; Sub; Mul]

  let integral_binops =
    num_binops
    @ [Bit_and; Bit_or; Bit_xor; Shift_left; Shift_right; Shift_right_logical]

  let ops_for_ty (ty : Ty.t) =
    match ty with
    | Number nty ->
      if NumberTy.is_floating_point nty then num_binops else integral_binops
    | Bool | Array _ | Record _ ->
      Misc.fatal_errorf "Bin_op.ops_for_ty: expected a numeric type"

  let to_code ty binop lhs rhs =
    let module_name =
      match ty with
      | Ty.Number nty -> NumberTy.to_module nty
      | Ty.Bool -> "Bool"
      | Ty.Array _ | Ty.Record _ ->
        Misc.fatal_errorf "Bin_op.to_code: unexpected aggregate"
    in
    let call name = apply (qualified_ident module_name name) [lhs; rhs] in
    match binop with
    | Add -> call "add"
    | Sub -> call "sub"
    | Mul -> call "mul"
    | Bit_and -> call "logand"
    | Bit_or -> call "logor"
    | Bit_xor -> call "logxor"
    | Shift_left -> call "shift_left"
    | Shift_right -> call "shift_right"
    | Shift_right_logical -> call "shift_right_logical"
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
    | Read of place
    | Array_literal of t list
    | Array_make of
        { dimensions : int list;
          init_name : Name.t;
          init : t
        }
    | Record of Ty.record * t list
    | Record_update of Ty.record * t * Ty.field * t
    | Record_convert of
        { from : Ty.record;
          to_unboxed : bool;
          source_name : Name.t;
          expr : t
        }
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

  and place =
    | Variable of Name.t
    | Field of place * Ty.record * Ty.field
    | Element of place * t list

  let convert_num expr ~(from : NumberTy.t) ~(to_ : NumberTy.t) =
    if NumberTy.equal from to_
    then expr
    else apply (ident (NumberTy.converter_name ~from ~to_)) [expr]

  let bind name expr body =
    let name = Name.to_string name in
    Exp.let_ Immutable Nonrecursive
      [Vb.mk (Pat.var (loc name)) expr]
      (body (ident name))

  let constrain ty expr = Exp.constraint_ expr (Some (Ty.to_code ty)) []

  let field_code (record : Ty.record) field receiver =
    let project = if record.unboxed then Exp.unboxed_field else Exp.field in
    project (constrain (Ty.Record record) receiver)
      (lid (Ty.field_name record field))

  let record_code (record : Ty.record) fields source =
    let make =
      if record.unboxed then Exp.record_unboxed_product else Exp.record
    in
    constrain (Ty.Record record) (make fields source)

  (* CR hwasilewski: Add a comment explaining our reliance on
     de facto evaluation order rather than explicitly sequencing effects. *)
  let rec to_code : t -> Parsetree.expression = function
    | Const n -> Number.to_code n
    | Read place -> place_to_code place
    | Array_literal elements ->
      Exp.array Mutable (List.map to_code elements)
    | Array_make { dimensions; init_name; init } ->
      bind init_name (to_code init) (fun initial_value ->
          let rec make = function
            | [] -> Misc.fatal_errorf "Array_make: no dimensions"
            | [size] -> apply (ident "array_make") [int size; initial_value]
            | size :: rest ->
              apply
                (qualified_ident "Array" "init")
                [int size; function_ [value_param (Pat.any ())] (make rest)]
          in
          make dimensions)
    | Record (record, values) ->
      if List.length record.fields <> List.length values
      then Misc.fatal_errorf "Record: incorrect number of fields";
      let fields =
        List.map2
          (fun field value ->
            lid (Ty.field_name record field), to_code value)
          record.fields values
      in
      record_code record fields None
    | Record_update (record, source, field, value) ->
      record_code record
        [lid (Ty.field_name record field), to_code value]
        (Some (constrain (Ty.Record record) (to_code source)))
    | Record_convert { from; to_unboxed; source_name; expr } ->
      let target = { from with Ty.unboxed = to_unboxed } in
      bind source_name (constrain (Ty.Record from) (to_code expr))
        (fun source ->
          let fields =
            List.map
              (fun field ->
                ( lid (Ty.field_name target field),
                  field_code from field source ))
              from.fields
          in
          record_code target fields None)
    | Opaque expr ->
      apply (qualified_ident "Sys" "opaque_identity") [to_code expr]
    | Bin_op { ty; op; lhs; rhs } ->
      Bin_op.to_code ty op (to_code lhs) (to_code rhs)
    | Convert { expr; from; to_ } -> convert_num (to_code expr) ~from ~to_
    | Call_toplevel { fun_name; args } ->
      let args =
        match args with [] -> [unit_] | _ -> List.map to_code args
      in
      apply (ident (Name.to_string fun_name)) args

  and place_to_code = function
    | Variable name -> ident (Name.to_string name)
    | Field (parent, record, field) ->
      field_code record field (place_to_code parent)
    | Element (parent, indices) ->
      List.fold_left
        (fun array index ->
          apply (ident "array_get") [array; to_code index])
        (place_to_code parent) indices

  let assignment_to_code place value =
    match place with
    | Variable name ->
      Exp.setinstvar (loc (Name.to_string name)) (to_code value)
    | Field (parent, record, field) ->
      if record.unboxed || not field.is_mutable
      then Misc.fatal_errorf "Assign: field is not mutable";
      Exp.setfield (constrain (Ty.Record record) (place_to_code parent))
        (lid (Ty.field_name record field)) (to_code value)
    | Element (parent, indices) ->
      let rec set array = function
        | [] -> Misc.fatal_errorf "Assign: no array indices"
        | [index] ->
          apply (ident "array_set")
            [array; to_code index; to_code value]
        | index :: rest ->
          set (apply (ident "array_get") [array; to_code index]) rest
      in
      set (place_to_code parent) indices

end

module Place = struct
  type t = Expr.place =
    | Variable of Name.t
    | Field of t * Ty.record * Ty.field
    | Element of t * Expr.t list

  let to_code = Expr.place_to_code
end

module Statement = struct
  type t =
    | Assign of Place.t * Expr.t
    | Seq of t list
    | If of Expr.t * t * t
    | Let of Binding.t * Expr.t * t
    | Bounded_loop of
        { var : Name.t;
          init : Expr.t;
          bound : Expr.t;
          stride : int;
          body : t
        }

  let let_binding ({ name; ty; is_mutable } : Binding.t) expr body =
    let mutability = if is_mutable then Mutable else Immutable in
    Exp.let_ mutability Nonrecursive
      [ Vb.mk
          (Pat.constraint_ (Pat.var (loc (Name.to_string name)))
             (Some (Ty.to_code ty)) [])
          expr ]
      body

  let sequence statement = function
    | Seq statements -> Seq (statement :: statements)
    | rest -> Seq [statement; rest]

  let rec to_code : t -> Parsetree.expression = function
    | Assign (place, expr) -> Expr.assignment_to_code place expr
    | Bounded_loop { var; init; bound; stride; body } ->
      if stride = 0 then Misc.fatal_errorf "Bounded_loop: zero stride";
      let name = Name.to_string var in
      let bound_name = name ^ "_bound" in
      let comparison = if stride > 0 then "<=" else ">=" in
      let_binding
        { Binding.name = var;
          ty = Ty.Number (NumberTy.boxed Int);
          is_mutable = true
        }
        (Expr.to_code init)
        (Exp.let_ Immutable Nonrecursive
           [Vb.mk (Pat.var (loc bound_name)) (Expr.to_code bound)]
           (Exp.while_
              (op comparison [ident name; ident bound_name])
              (Exp.sequence (to_code body)
                 (Exp.setinstvar (loc name)
                    (op "+" [ident name; int stride])))))
    | Let (binding, expr, body) ->
      let_binding binding (Expr.to_code expr) (to_code body)
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
      params : Binding.t list;
      inline : Inline.t;
      body : Statement.t;
      return_ty : Ty.t;
      result : Expr.t
    }

  let to_code { name; params; inline; body; return_ty; result } =
    let to_param ({ name; ty; _ } : Binding.t) =
      value_param
        (Pat.constraint_ (Pat.var (loc (Name.to_string name)))
           (Some (Ty.to_code ty)) [])
    in
    let function_params =
      match params with
      | [] -> [value_param (Pat.construct (lid "()") None)]
      | _ -> List.map to_param params
    in
    let body =
      Exp.sequence (Statement.to_code body)
        (Exp.constraint_ (Expr.to_code result) (Some (Ty.to_code return_ty)) [])
    in
    let body =
      List.fold_right
        (fun (binding : Binding.t) body ->
          Statement.let_binding binding (ident (Name.to_string binding.name))
            body)
        params body
    in
    Str.value Nonrecursive
      [ Vb.mk
          ~attrs:(Inline.to_attributes inline)
          (Pat.var (loc (Name.to_string name)))
          (function_ function_params body) ]
end
