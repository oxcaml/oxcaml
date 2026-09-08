(* Final program lowering from the generator IR into OxCaml code. *)
open Ast_helper
open Asttypes
open Parsetree_helpers
module Binding = Ir.Binding
module Expr = Ir.Expr
module Function = Ir.Function
module Name = Ir.Name
module NumberTy = Ir.NumberTy
module Statement = Ir.Statement
module Ty = Ir.Ty

type t =
  { record_types : Ty.record list;
    functions : Function.t list;
    toplevel_decls : (Binding.t * Expr.t) list;
    toplevel_statement : Statement.t
  }

let create ~record_types ~functions ~toplevel_decls ~toplevel_statement =
  { record_types; functions; toplevel_decls; toplevel_statement }

let is_floating_point_to_integral ~from ~to_ =
  NumberTy.is_floating_point from && not (NumberTy.is_floating_point to_)

(* CR-someday hwasilewski: Optimize the prelude for faster compilation. Some
   possibilities are precompiling the prelude into a .cmxa file and only linking
   it during fuzzing, possibly only emitting the subset of functions that are
   needed. *)
let unsafe_converter_name ~from ~to_ =
  "unsafe_" ^ NumberTy.converter_name ~from ~to_

let conversions =
  let conversion from to_ =
    let from_name = NumberTy.to_string from in
    let to_name = NumberTy.to_string to_ in
    let primitive_name = Format.sprintf "%%%s_of_%s" to_name from_name in
    let function_name =
      if is_floating_point_to_integral ~from ~to_
      then unsafe_converter_name ~from ~to_
      else NumberTy.converter_name ~from ~to_
    in
    let type_expression name = Typ.constr (lid name) [] in
    let conversion_type =
      Typ.arrow Nolabel
        (type_expression from_name)
        (type_expression to_name) [] []
    in
    Str.primitive
      (Val.mk (loc function_name) conversion_type ~prim:[primitive_name])
  in
  List.concat_map
    (fun from ->
      List.filter_map
        (fun to_ ->
          if NumberTy.equal from to_ then None else Some (conversion from to_))
        NumberTy.all)
    NumberTy.all

let integral_bound_name base =
  String.lowercase_ascii (NumberTy.Base.to_module base)

let integral_size (base : NumberTy.Base.t) =
  match base with
  | Int -> qualified_ident "Sys" "int_size"
  | Nativeint -> qualified_ident "Nativeint" "size"
  | Int64 -> int 64
  | Int32 -> int 32
  | Int16 -> int 16
  | Int8 -> int 8
  | Float | Float32 ->
    Misc.fatal_errorf "Program.integral_size: expected an integral type"

(* Float-to-integer primitives are unspecified for NaN and out-of-range inputs.
   These wrappers map NaN to zero and saturate infinities and overflow. *)
let integral_bounds =
  let value name expr =
    Str.value Nonrecursive [Vb.mk (Pat.var (loc name)) expr]
  in
  let bounds base =
    let name = integral_bound_name base in
    let upper_name = name ^ "_upper_bound" in
    let lower_name = name ^ "_lower_bound" in
    let exponent = op "-" [integral_size base; int 1] in
    let upper =
      apply
        (qualified_ident "Float" "ldexp")
        [Exp.constant (Const.float "1.0"); exponent]
    in
    let lower = op "~-." [ident upper_name] in
    [value upper_name upper; value lower_name lower]
  in
  List.concat_map
    (fun base ->
      if NumberTy.Base.is_floating_point base then [] else bounds base)
    NumberTy.Base.all

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
                    (Some (apply (ident (unsafe_converter_name ~from ~to_)) [x]))))))
    in
    let body =
      Exp.let_ Immutable Nonrecursive
        [Vb.mk (Pat.var (loc "x_for_comparison")) (float_for_comparison from x)]
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
          if is_floating_point_to_integral ~from ~to_
          then Some (wrapper from to_)
          else None)
        NumberTy.all)
    NumberTy.all

(* We check floats for equality by testing if their bits are equal. This is
   correct unless they are NaN, which is why before comparing, we run
   [canonicalize_nan] to cast NaNs to a common representation. *)
let canon_nan_name = "canonicalize_nan"

let canonicalize_nan =
  let x = ident "x" in
  let body =
    Exp.ifthenelse
      (apply (qualified_ident "Float" "is_nan") [x])
      (qualified_ident "Float" "nan")
      (Some x)
  in
  Str.value Nonrecursive
    [ Vb.mk
        (Pat.var (loc canon_nan_name))
        (function_ [value_param (Pat.var (loc "x"))] body) ]

(* Produce an expression, which prints the value of the numeric expression [e]
   of type [nty] *)
let print_number nty e =
  let fmt, arg =
    if NumberTy.is_floating_point nty
    then
      ( "%h ",
        apply (ident canon_nan_name)
          [Expr.convert_num ~from:nty ~to_:(NumberTy.boxed Float) e] )
    else "%Ld ", Expr.convert_num ~from:nty ~to_:(NumberTy.boxed Int64) e
  in
  apply
    (qualified_ident "Printf" "printf")
    [Exp.constant (Const.string fmt); arg]

(* Keep in sync with [LIBRARIES] in oxfuzzer.py. *)
let libraries = ["stdlib_upstream_compatible"; "stdlib_stable"]

let array_primitives =
  let open Parsetree in
  let any =
    { pjka_loc = Location.none;
      pjka_desc = Pjk_abbreviation (lid "any")
    }
  in
  let separable =
    { pjka_loc = Location.none;
      pjka_desc = Pjk_mod (any, [loc (Mode "separable")])
    }
  in
  let a = Typ.var "a" None in
  let array = Typ.constr (lid "array") [a] in
  let int = Typ.constr (lid "int") [] in
  let unit = Typ.constr (lid "unit") [] in
  let external_ name primitive args result =
    let typ =
      List.fold_right
        (fun arg result -> Typ.arrow Nolabel arg result [] [])
        args result
    in
    Str.primitive
      (Val.mk
         ~attrs:[Attr.mk (loc "layout_poly") (PStr [])]
         ~prim:[primitive] (loc name)
         (Typ.poly [loc "a", Some separable] typ))
  in
  [ external_ "array_make" "%makearray_dynamic" [int; a] array;
    external_ "array_get" "%array_safe_get" [array; int] a;
    external_ "array_set" "%array_safe_set" [array; int; a] unit;
    external_ "array_length" "%array_length" [array] int
  ]

let record_declarations records =
  let declaration (record : Ty.record) =
    let fields =
      List.map
        (fun (field : Ty.field) ->
          let mut =
            if field.is_mutable && not record.unboxed
            then Mutable
            else Immutable
          in
          Type.field ~mut (loc (Ty.field_name record field))
            (Ty.to_code field.ty))
        record.fields
    in
    let kind =
      if record.unboxed
      then Parsetree.Ptype_record_unboxed_product fields
      else Parsetree.Ptype_record fields
    in
    Str.type_ Recursive [Type.mk ~kind (loc (Ty.record_name record))]
  in
  List.concat_map
    (fun (record : Ty.record) ->
      [ declaration { record with unboxed = false };
        declaration { record with unboxed = true } ])
    records

let rec print_value path ty expr =
  match ty with
  | Ty.Number nty -> print_number nty expr
  | Ty.Array (nty, dimensions) ->
    let rec print_elements depth dimensions array =
      match dimensions with
      | [] -> print_number nty array
      | _ :: rest ->
        let index = path ^ "_index_" ^ string_of_int depth in
        Exp.for_ (Pat.var (loc index)) (int 0)
          (op "-" [apply (ident "array_length") [array]; int 1])
          Upto
          (print_elements (depth + 1) rest
             (apply (ident "array_get") [array; ident index]))
    in
    print_elements 0 dimensions expr
  | Ty.Record record ->
    List.fold_right
      (fun (field : Ty.field) acc ->
        let project = if record.unboxed then Exp.unboxed_field else Exp.field in
        let value = project expr (lid (Ty.field_name record field)) in
        let path = path ^ "_field_" ^ string_of_int field.index in
        Exp.sequence (print_value path field.ty value) acc)
      record.fields unit_
  | Ty.Bool ->
    apply
      (qualified_ident "Printf" "printf")
      [Exp.constant (Const.string "%b "); expr]

let to_code
    { record_types;
      functions;
      toplevel_decls = decls;
      toplevel_statement = statement
    } =
  let opens =
    List.map
      (fun lib ->
        Str.open_ (Opn.mk (Mod.ident (lid (String.capitalize_ascii lib)))))
      libraries
  in
  let print_decl ((binding : Binding.t), _expr) =
    let name = Name.to_string binding.name in
    print_value (name ^ "_print") binding.ty (ident name)
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
      (fun (binding, expr) acc ->
        Statement.let_binding binding (Expr.to_code expr) acc)
      decls body
  in
  let main =
    Str.value Nonrecursive
      [ Vb.mk
          (Pat.var (loc "main"))
          (function_ [value_param (Pat.construct (lid "()") None)] body) ]
  in
  let run = Str.eval (Exp.apply (ident "main") [Nolabel, unit_]) in
  let structure =
    opens @ record_declarations record_types @ array_primitives @ conversions
    @ integral_bounds @ [canonicalize_nan] @ float_to_integral_conversions
    @ List.map Function.to_code functions
    @ [main; run]
  in
  Pprintast.string_of_structure structure ^ "\n"
