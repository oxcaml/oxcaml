(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Jane Street Group LLC                              *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type constant =
  | Const_int of int
  | Const_char of char
  | Const_untagged_char of int
  | Const_string of string
  | Const_float of string
  | Const_float32 of string
  | Const_unboxed_float of string
  | Const_unboxed_float32 of string
  | Const_int8 of int
  | Const_int16 of int
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint
  | Const_untagged_int of int
  | Const_untagged_int8 of int
  | Const_untagged_int16 of int
  | Const_unboxed_int32 of int32
  | Const_unboxed_int64 of int64
  | Const_unboxed_nativeint of nativeint

type arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string

type sort = Jkind_types.Sort.Const.t

type constructor =
  | Constructor of
      { type_path : Path.t;
        name : string
      }

type label =
  { type_path : Path.t;
    name : string
  }

type expression =
  | Sexp_var of Ident.t
  | Sexp_global of Path.t
  | Sexp_constant of constant
  | Sexp_apply of expression * (arg_label * expression * sort) list
  | Sexp_tuple of (string option * expression * sort) list
  | Sexp_unboxed_tuple of (string option * expression * sort) list
  | Sexp_unboxed_unit
  | Sexp_unboxed_bool of bool
  | Sexp_construct of constructor * (sort * expression) list
  | Sexp_variant of string * expression option
  | Sexp_record of record
  | Sexp_record_unboxed_product of record
  | Sexp_field of field
  | Sexp_unboxed_field of field
  | Sexp_ifthenelse of expression * expression * expression option
  | Sexp_sequence of expression * sort * expression
  | Sexp_assert of expression
  | Sexp_lazy of expression

and record =
  { fields : (label * sort * field_definition) list;
    extended_expression : (expression * sort) option
  }

and field_definition =
  | Kept
  | Overridden of expression

and field =
  { record : expression;
    sort : sort;
    label : label
  }

(* Traversals *)

let map_constructor ~type_path (Constructor { type_path = p; name }) =
  Constructor { type_path = type_path p; name }

let rec map_paths ~value_path ~type_path e =
  let map e = map_paths ~value_path ~type_path e in
  let map_labelled (lbl, e, sort) = lbl, map e, sort in
  match e with
  | Sexp_var _ | Sexp_constant _ | Sexp_unboxed_unit | Sexp_unboxed_bool _ -> e
  | Sexp_global path -> Sexp_global (value_path path)
  | Sexp_apply (funct, args) ->
    Sexp_apply (map funct, List.map map_labelled args)
  | Sexp_tuple es -> Sexp_tuple (List.map map_labelled es)
  | Sexp_unboxed_tuple es -> Sexp_unboxed_tuple (List.map map_labelled es)
  | Sexp_construct (cstr, args) ->
    Sexp_construct
      ( map_constructor ~type_path cstr,
        List.map (fun (sort, e) -> sort, map e) args )
  | Sexp_variant (lbl, arg) -> Sexp_variant (lbl, Option.map map arg)
  | Sexp_record r -> Sexp_record (map_record ~value_path ~type_path r)
  | Sexp_record_unboxed_product r ->
    Sexp_record_unboxed_product (map_record ~value_path ~type_path r)
  | Sexp_field f -> Sexp_field (map_field ~value_path ~type_path f)
  | Sexp_unboxed_field f ->
    Sexp_unboxed_field (map_field ~value_path ~type_path f)
  | Sexp_ifthenelse (c, e1, e2) ->
    Sexp_ifthenelse (map c, map e1, Option.map map e2)
  | Sexp_sequence (e1, sort, e2) -> Sexp_sequence (map e1, sort, map e2)
  | Sexp_assert e -> Sexp_assert (map e)
  | Sexp_lazy e -> Sexp_lazy (map e)

and map_label ~type_path { type_path = p; name } =
  { type_path = type_path p; name }

and map_record ~value_path ~type_path { fields; extended_expression } =
  let map e = map_paths ~value_path ~type_path e in
  let field (lbl, sort, def) =
    let def =
      match def with Kept -> Kept | Overridden e -> Overridden (map e)
    in
    map_label ~type_path lbl, sort, def
  in
  { fields = List.map field fields;
    extended_expression =
      Option.map (fun (e, s) -> map e, s) extended_expression
  }

and map_field ~value_path ~type_path { record; sort; label } =
  { record = map_paths ~value_path ~type_path record;
    sort;
    label = map_label ~type_path label
  }

type namespace =
  | Value
  | Type

let constructor_path (Constructor { type_path; _ }) = Type, type_path

let rec paths_rev acc = function
  | Sexp_var _ | Sexp_constant _ | Sexp_unboxed_unit | Sexp_unboxed_bool _ ->
    acc
  | Sexp_global path -> (Value, path) :: acc
  | Sexp_apply (funct, args) ->
    List.fold_left
      (fun acc (_, e, _) -> paths_rev acc e)
      (paths_rev acc funct) args
  | Sexp_tuple es | Sexp_unboxed_tuple es ->
    List.fold_left (fun acc (_, e, _) -> paths_rev acc e) acc es
  | Sexp_construct (cstr, args) ->
    List.fold_left
      (fun acc (_, e) -> paths_rev acc e)
      (constructor_path cstr :: acc)
      args
  | Sexp_variant (_, arg) -> Option.fold ~none:acc ~some:(paths_rev acc) arg
  | Sexp_record { fields; extended_expression }
  | Sexp_record_unboxed_product { fields; extended_expression } ->
    let acc =
      match fields with
      | ({ type_path; _ }, _, _) :: _ -> (Type, type_path) :: acc
      | [] -> acc
    in
    let acc =
      Option.fold ~none:acc
        ~some:(fun (e, _) -> paths_rev acc e)
        extended_expression
    in
    List.fold_left
      (fun acc (_, _, def) ->
        match def with Kept -> acc | Overridden e -> paths_rev acc e)
      acc fields
  | Sexp_field { record; label; _ } | Sexp_unboxed_field { record; label; _ } ->
    (Type, label.type_path) :: paths_rev acc record
  | Sexp_ifthenelse (c, e1, e2) ->
    let acc = paths_rev (paths_rev acc c) e1 in
    Option.fold ~none:acc ~some:(paths_rev acc) e2
  | Sexp_sequence (e1, _, e2) -> paths_rev (paths_rev acc e1) e2
  | Sexp_assert e | Sexp_lazy e -> paths_rev acc e

let paths e = List.rev (paths_rev [] e)

(* Equality. [vars] maps the variables bound so far in the first
   expression to the corresponding ones of the second. *)

let lookup vars id =
  Option.map snd (List.find_opt (fun (id', _) -> Ident.same id id') vars)

let same_var vars id1 id2 =
  match lookup vars id1 with Some id2' -> Ident.same id2 id2' | None -> false

(* Floats are compared by their representation, which distinguishes [0.]
   from [-0.]. Literals cannot denote NaNs. *)
let same_float f1 f2 =
  match float_of_string f1, float_of_string f2 with
  | x1, x2 -> Int64.equal (Int64.bits_of_float x1) (Int64.bits_of_float x2)
  | exception Failure _ -> String.equal f1 f2

let same_constant c1 c2 =
  match c1, c2 with
  | Const_float f1, Const_float f2
  | Const_float32 f1, Const_float32 f2
  | Const_unboxed_float f1, Const_unboxed_float f2
  | Const_unboxed_float32 f1, Const_unboxed_float32 f2 ->
    same_float f1 f2
  | _ -> c1 = c2

let same_arg_label l1 l2 =
  match l1, l2 with
  | Nolabel, Nolabel -> true
  | Labelled s1, Labelled s2 | Optional s1, Optional s2 -> String.equal s1 s2
  | (Nolabel | Labelled _ | Optional _), _ -> false

let same_sort = Jkind_types.Sort.Const.equal

let same_constructor ~same_path (Constructor c1) (Constructor c2) =
  String.equal c1.name c2.name && same_path Type c1.type_path c2.type_path

let same_label ~same_path l1 l2 =
  String.equal l1.name l2.name && same_path Type l1.type_path l2.type_path

let rec alpha_equal ~same_path ~vars e1 e2 =
  let equal e1 e2 = alpha_equal ~same_path ~vars e1 e2 in
  let equal_labelled (l1, e1, s1) (l2, e2, s2) =
    Option.equal String.equal l1 l2 && equal e1 e2 && same_sort s1 s2
  in
  match e1, e2 with
  | Sexp_var id1, Sexp_var id2 -> same_var vars id1 id2
  | Sexp_global p1, Sexp_global p2 -> same_path Value p1 p2
  | Sexp_constant c1, Sexp_constant c2 -> same_constant c1 c2
  | Sexp_apply (funct1, args1), Sexp_apply (funct2, args2) ->
    equal funct1 funct2
    && List.equal
         (fun (l1, e1, s1) (l2, e2, s2) ->
           same_arg_label l1 l2 && equal e1 e2 && same_sort s1 s2)
         args1 args2
  | Sexp_tuple es1, Sexp_tuple es2
  | Sexp_unboxed_tuple es1, Sexp_unboxed_tuple es2 ->
    List.equal equal_labelled es1 es2
  | Sexp_unboxed_unit, Sexp_unboxed_unit -> true
  | Sexp_unboxed_bool b1, Sexp_unboxed_bool b2 -> Bool.equal b1 b2
  | Sexp_construct (c1, args1), Sexp_construct (c2, args2) ->
    same_constructor ~same_path c1 c2
    && List.equal
         (fun (s1, e1) (s2, e2) -> same_sort s1 s2 && equal e1 e2)
         args1 args2
  | Sexp_variant (l1, a1), Sexp_variant (l2, a2) ->
    String.equal l1 l2 && Option.equal equal a1 a2
  | Sexp_record r1, Sexp_record r2
  | Sexp_record_unboxed_product r1, Sexp_record_unboxed_product r2 ->
    let same_def d1 d2 =
      match d1, d2 with
      | Kept, Kept -> true
      | Overridden e1, Overridden e2 -> equal e1 e2
      | (Kept | Overridden _), _ -> false
    in
    List.equal
      (fun (l1, s1, d1) (l2, s2, d2) ->
        same_label ~same_path l1 l2 && same_sort s1 s2 && same_def d1 d2)
      r1.fields r2.fields
    && Option.equal
         (fun (e1, s1) (e2, s2) -> equal e1 e2 && same_sort s1 s2)
         r1.extended_expression r2.extended_expression
  | Sexp_field f1, Sexp_field f2 | Sexp_unboxed_field f1, Sexp_unboxed_field f2
    ->
    equal f1.record f2.record && same_sort f1.sort f2.sort
    && same_label ~same_path f1.label f2.label
  | Sexp_ifthenelse (c1, t1, e1), Sexp_ifthenelse (c2, t2, e2) ->
    equal c1 c2 && equal t1 t2 && Option.equal equal e1 e2
  | Sexp_sequence (a1, s1, b1), Sexp_sequence (a2, s2, b2) ->
    equal a1 a2 && same_sort s1 s2 && equal b1 b2
  | Sexp_assert e1, Sexp_assert e2 | Sexp_lazy e1, Sexp_lazy e2 -> equal e1 e2
  | ( ( Sexp_var _ | Sexp_global _ | Sexp_constant _ | Sexp_apply _
      | Sexp_tuple _ | Sexp_unboxed_tuple _ | Sexp_unboxed_unit
      | Sexp_unboxed_bool _ | Sexp_construct _ | Sexp_variant _ | Sexp_record _
      | Sexp_record_unboxed_product _ | Sexp_field _ | Sexp_unboxed_field _
      | Sexp_ifthenelse _ | Sexp_sequence _ | Sexp_assert _ | Sexp_lazy _ ),
      _ ) ->
    false
