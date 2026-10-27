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
  | ( ( Sexp_var _ | Sexp_global _ | Sexp_constant _ | Sexp_apply _
      | Sexp_tuple _ | Sexp_unboxed_tuple _ | Sexp_unboxed_unit
      | Sexp_unboxed_bool _ | Sexp_construct _ | Sexp_variant _ ),
      _ ) ->
    false
