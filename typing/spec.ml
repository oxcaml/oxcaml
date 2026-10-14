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

open Asttypes

type constant =
    Const_int of int
  | Const_char of char
  | Const_untagged_char of int
  | Const_string of string * Location.t * string option
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

type sort = Jkind_types.Sort.Const.t

type locality = Mode.Locality.Const.t

type constructor =
  | Constructor of { type_path : Path.t; name : string }
  | Extension_constructor of Path.t

type label = { type_path : Path.t; name : string }

type 'ty expression =
  { sexp_desc : 'ty expression_desc;
    sexp_type : 'ty;
    sexp_loc : Location.t;
  }

and 'ty expression_desc =
  | Sexp_var of Ident.t
  | Sexp_global of Path.t
  | Sexp_constant of constant
  | Sexp_apply of
      { funct : 'ty expression;
        args : (arg_label * 'ty argument) list;
        ret_mode : locality;
      }
  | Sexp_tuple of (string option * 'ty expression) list * locality
  | Sexp_unboxed_tuple of (string option * 'ty expression * sort) list
  | Sexp_unboxed_unit
  | Sexp_unboxed_bool of bool
  | Sexp_construct of
      { constructor : constructor;
        args : ('ty expression * sort) list;
        locality : locality option;  (** [None] if no allocation is needed *)
      }
  | Sexp_variant of string * ('ty expression * locality) option
  | Sexp_record of
      { fields : (label * sort * 'ty field_definition) list;
          (** all the fields of the record type, in declaration order *)
        extended_expression : ('ty expression * sort) option;
        locality : locality option;
      }
  | Sexp_field of
      { record : 'ty expression;
        sort : sort;  (** sort of the record *)
        label : label;
      }
  | Sexp_record_unboxed_product of
      { fields : (label * sort * 'ty field_definition) list;
        extended_expression : ('ty expression * sort) option;
      }
  | Sexp_unboxed_field of
      { record : 'ty expression;
        sort : sort;
        label : label;
      }
  | Sexp_extension_constructor of Path.t

and 'ty argument =
  | Arg of 'ty expression * sort
  | Omitted

and 'ty field_definition =
  | Kept of 'ty
  | Overridden of 'ty expression


(* Traversals *)

type ('a, 'b) mapper =
  { ty : 'a -> 'b;
    value_path : Path.t -> Path.t;
    type_path : Path.t -> Path.t;
    extension_path : Path.t -> Path.t;
  }

let rec map m e =
  { sexp_desc = map_desc m e.sexp_desc;
    sexp_type = m.ty e.sexp_type;
    sexp_loc = e.sexp_loc;
  }

and map_desc m = function
  | Sexp_var id -> Sexp_var id
  | Sexp_global p -> Sexp_global (m.value_path p)
  | Sexp_constant c -> Sexp_constant c
  | Sexp_apply { funct; args; ret_mode } ->
      Sexp_apply
        { funct = map m funct;
          args = List.map (fun (lbl, arg) -> (lbl, map_argument m arg)) args;
          ret_mode }
  | Sexp_tuple (es, loc) ->
      Sexp_tuple (List.map (fun (lbl, e) -> (lbl, map m e)) es, loc)
  | Sexp_unboxed_tuple es ->
      Sexp_unboxed_tuple
        (List.map (fun (lbl, e, sort) -> (lbl, map m e, sort)) es)
  | Sexp_unboxed_unit -> Sexp_unboxed_unit
  | Sexp_unboxed_bool b -> Sexp_unboxed_bool b
  | Sexp_construct { constructor; args; locality } ->
      Sexp_construct
        { constructor = map_constructor m constructor;
          args = List.map (fun (e, sort) -> (map m e, sort)) args;
          locality }
  | Sexp_variant (lbl, arg) ->
      Sexp_variant (lbl, Option.map (fun (e, loc) -> (map m e, loc)) arg)
  | Sexp_record { fields; extended_expression; locality } ->
      Sexp_record
        { fields = List.map (map_field m) fields;
          extended_expression =
            Option.map (fun (e, sort) -> (map m e, sort)) extended_expression;
          locality }
  | Sexp_field { record; sort; label } ->
      Sexp_field
        { record = map m record; sort; label = map_label m label }
  | Sexp_record_unboxed_product { fields; extended_expression } ->
      Sexp_record_unboxed_product
        { fields = List.map (map_field m) fields;
          extended_expression =
            Option.map (fun (e, sort) -> (map m e, sort)) extended_expression }
  | Sexp_unboxed_field { record; sort; label } ->
      Sexp_unboxed_field
        { record = map m record; sort; label = map_label m label }
  | Sexp_extension_constructor p ->
      Sexp_extension_constructor (m.extension_path p)

and map_argument m = function
  | Arg (e, sort) -> Arg (map m e, sort)
  | Omitted -> Omitted

and map_field m (lbl, sort, def) =
  let def =
    match def with
    | Kept t -> Kept (m.ty t)
    | Overridden e -> Overridden (map m e)
  in
  (map_label m lbl, sort, def)

and map_constructor m = function
  | Constructor { type_path = p; name } ->
      Constructor { type_path = m.type_path p; name }
  | Extension_constructor p -> Extension_constructor (m.extension_path p)

and map_label m { type_path = p; name } =
  { type_path = m.type_path p; name }

type namespace = Value | Type | Extension

let paths e =
  let paths = ref [] in
  let path ns p = paths := (ns, p) :: !paths; p in
  ignore
    (map
       { ty = Fun.id;
         value_path = path Value;
         type_path = path Type;
         extension_path = path Extension }
       e
     : _ expression);
  List.rev !paths

let map ~ty ~value_path ~type_path e =
  map { ty; value_path; type_path; extension_path = type_path } e

let map_types ty e = map ~ty ~value_path:Fun.id ~type_path:Fun.id e

let map_paths ~value_path ~type_path e =
  map ~ty:Fun.id ~value_path ~type_path e

let iter_types f e =
  ignore
    (map ~ty:(fun t -> f t) ~value_path:Fun.id ~type_path:Fun.id e
     : unit expression)

(* Alpha-equivalence *)

(* [vars] maps the variables bound so far in the first expression to the
   corresponding ones of the second, most recent binding first. *)

let lookup vars id =
  Option.map snd (List.find_opt (fun (id', _) -> Ident.same id id') vars)

let same_var vars id1 id2 =
  match lookup vars id1 with
  | Some id2' -> Ident.same id2 id2'
  | None -> false

let same_constant (c1 : constant) (c2 : constant) =
  (* By representation, which distinguishes [0.] from [-0.]. Literals
     cannot denote NaNs. *)
  let same_float f1 f2 =
    match float_of_string f1, float_of_string f2 with
    | x1, x2 -> Int64.equal (Int64.bits_of_float x1) (Int64.bits_of_float x2)
    | exception Failure _ -> String.equal f1 f2
  in
  match c1, c2 with
  | Const_string (s1, _, _), Const_string (s2, _, _) ->
      (* The quotation delimiters do not matter. *)
      String.equal s1 s2
  | Const_float f1, Const_float f2
  | Const_float32 f1, Const_float32 f2
  | Const_unboxed_float f1, Const_unboxed_float f2
  | Const_unboxed_float32 f1, Const_unboxed_float32 f2 ->
      same_float f1 f2
  | _ -> c1 = c2

let same_constructor sp c1 c2 =
  match c1, c2 with
  | Constructor { type_path = p1; name = n1 },
    Constructor { type_path = p2; name = n2 } ->
      sp Type p1 p2 && String.equal n1 n2
  | Extension_constructor p1, Extension_constructor p2 -> sp Extension p1 p2
  | Constructor _, Extension_constructor _
  | Extension_constructor _, Constructor _ ->
      false

let same_label sp (l1 : label) (l2 : label) =
  sp Type l1.type_path l2.type_path && String.equal l1.name l2.name

let same_arg_label (l1 : arg_label) (l2 : arg_label) =
  match l1, l2 with
  | Nolabel, Nolabel -> true
  | Labelled s1, Labelled s2 | Optional s1, Optional s2 -> String.equal s1 s2
  | (Nolabel | Labelled _ | Optional _), _ -> false

let rec for_all2 f l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | x1 :: l1, x2 :: l2 -> f x1 x2 && for_all2 f l1 l2
  | [], _ :: _ | _ :: _, [] -> false

let rec alpha_equal_exp sp vars e1 e2 =
  match e1.sexp_desc, e2.sexp_desc with
  | Sexp_var id1, Sexp_var id2 -> same_var vars id1 id2
  | Sexp_global p1, Sexp_global p2 -> sp Value p1 p2
  | Sexp_constant c1, Sexp_constant c2 -> same_constant c1 c2
  | Sexp_apply a1, Sexp_apply a2 ->
      alpha_equal_exp sp vars a1.funct a2.funct
      && for_all2
           (fun (l1, arg1) (l2, arg2) ->
              same_arg_label l1 l2
              && (match arg1, arg2 with
                  | Arg (e1, _), Arg (e2, _) -> alpha_equal_exp sp vars e1 e2
                  | Omitted, Omitted -> true
                  | Arg _, Omitted | Omitted, Arg _ -> false))
           a1.args a2.args
  | Sexp_tuple (es1, _), Sexp_tuple (es2, _) ->
      for_all2
        (fun (l1, e1) (l2, e2) ->
           Option.equal String.equal l1 l2 && alpha_equal_exp sp vars e1 e2)
        es1 es2
  | Sexp_unboxed_tuple es1, Sexp_unboxed_tuple es2 ->
      for_all2
        (fun (l1, e1, _) (l2, e2, _) ->
           Option.equal String.equal l1 l2 && alpha_equal_exp sp vars e1 e2)
        es1 es2
  | Sexp_unboxed_unit, Sexp_unboxed_unit -> true
  | Sexp_unboxed_bool b1, Sexp_unboxed_bool b2 -> Bool.equal b1 b2
  | Sexp_construct c1, Sexp_construct c2 ->
      same_constructor sp c1.constructor c2.constructor
      && for_all2
           (fun (e1, _) (e2, _) -> alpha_equal_exp sp vars e1 e2)
           c1.args c2.args
  | Sexp_variant (l1, arg1), Sexp_variant (l2, arg2) ->
      String.equal l1 l2
      && (match arg1, arg2 with
          | None, None -> true
          | Some (e1, _), Some (e2, _) -> alpha_equal_exp sp vars e1 e2
          | None, Some _ | Some _, None -> false)
  | Sexp_record r1, Sexp_record r2 ->
      alpha_equal_fields sp vars r1.fields r2.fields
      && alpha_equal_extension sp vars
           r1.extended_expression r2.extended_expression
  | Sexp_record_unboxed_product r1, Sexp_record_unboxed_product r2 ->
      alpha_equal_fields sp vars r1.fields r2.fields
      && alpha_equal_extension sp vars
           r1.extended_expression r2.extended_expression
  | Sexp_field f1, Sexp_field f2 ->
      same_label sp f1.label f2.label
      && alpha_equal_exp sp vars f1.record f2.record
  | Sexp_unboxed_field f1, Sexp_unboxed_field f2 ->
      same_label sp f1.label f2.label
      && alpha_equal_exp sp vars f1.record f2.record
  | Sexp_extension_constructor p1, Sexp_extension_constructor p2 ->
      sp Extension p1 p2
  | ( Sexp_var _ | Sexp_global _ | Sexp_constant _ | Sexp_apply _
    | Sexp_tuple _ | Sexp_unboxed_tuple _ | Sexp_unboxed_unit
    | Sexp_unboxed_bool _ | Sexp_construct _ | Sexp_variant _
    | Sexp_record _ | Sexp_field _ | Sexp_record_unboxed_product _
    | Sexp_unboxed_field _ | Sexp_extension_constructor _ ), _ ->
      false

and alpha_equal_fields sp vars fields1 fields2 =
  for_all2
    (fun (l1, _, d1) (l2, _, d2) ->
       same_label sp l1 l2
       && (match d1, d2 with
           | Kept _, Kept _ -> true
           | Overridden e1, Overridden e2 -> alpha_equal_exp sp vars e1 e2
           | Kept _, Overridden _ | Overridden _, Kept _ -> false))
    fields1 fields2

and alpha_equal_extension sp vars ext1 ext2 =
  match ext1, ext2 with
  | None, None -> true
  | Some (e1, _), Some (e2, _) -> alpha_equal_exp sp vars e1 e2
  | None, Some _ | Some _, None -> false

let alpha_equal ~same_path ~vars e1 e2 =
  alpha_equal_exp same_path vars e1 e2
