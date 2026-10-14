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

type partial = Partial | Total

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
  | Sexp_let of rec_flag * 'ty value_binding list * 'ty expression
  | Sexp_function of
      { params : 'ty function_param list;
        body : 'ty function_body;
        ret_sort : sort;
        ret_mode : locality;
        locality : locality;  (** allocation mode of the closure *)
      }
  | Sexp_apply of
      { funct : 'ty expression;
        args : (arg_label * 'ty argument) list;
        ret_mode : locality;
      }
  | Sexp_match of
      { scrutinee : 'ty expression;
        sort : sort;
        cases : 'ty case list;
        partial : partial;
      }
  | Sexp_try of 'ty expression * 'ty case list
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
  | Sexp_ifthenelse of 'ty expression * 'ty expression * 'ty expression option
  | Sexp_sequence of 'ty expression * sort * 'ty expression
  | Sexp_assert of 'ty expression
  | Sexp_lazy of 'ty expression
  | Sexp_extension_constructor of Path.t

and 'ty argument =
  | Arg of 'ty expression * sort
  | Omitted

and 'ty field_definition =
  | Kept of 'ty
  | Overridden of 'ty expression

and 'ty value_binding =
  { svb_pat : 'ty pattern;
    svb_expr : 'ty expression;
    svb_rec_kind : Value_rec_types.recursive_binding_kind;
    svb_sort : sort;
  }

and 'ty function_param =
  { sfp_arg_label : arg_label;
    sfp_param : Ident.t;
    sfp_kind : 'ty function_param_kind;
    sfp_sort : sort;
    sfp_mode : locality;
    sfp_partial : partial;
  }

and 'ty function_param_kind =
  | Sparam_pat of 'ty pattern
  | Sparam_optional_default of 'ty pattern * 'ty expression * sort

and 'ty function_body =
  | Sfunction_body of 'ty expression
  | Sfunction_cases of
      { cases : 'ty case list;
        param : Ident.t;
        arg_sort : sort;
        arg_mode : locality;
        ret_type : 'ty;
        partial : partial;
      }

and 'ty case =
  { sc_lhs : 'ty pattern;
    sc_guard : 'ty expression option;
    sc_rhs : 'ty expression;
  }

and 'ty pattern =
  { spat_desc : 'ty pattern_desc;
    spat_type : 'ty;
    spat_loc : Location.t;
  }

and 'ty pattern_desc =
  | Spat_any
  | Spat_var of Ident.t * sort
  | Spat_alias of 'ty pattern * Ident.t * sort
  | Spat_constant of constant
  | Spat_tuple of (string option * 'ty pattern) list
  | Spat_unboxed_tuple of (string option * 'ty pattern * sort) list
  | Spat_unboxed_unit
  | Spat_unboxed_bool of bool
  | Spat_construct of constructor * (sort * 'ty pattern) list
  | Spat_variant of string * 'ty pattern option
  | Spat_record of (label * 'ty pattern) list * closed_flag
  | Spat_record_unboxed_product of (label * 'ty pattern) list * closed_flag
  | Spat_or of 'ty pattern * 'ty pattern
  | Spat_lazy of 'ty pattern
  | Spat_exception of 'ty pattern
      (** Only at the top of a [match] case. *)


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
  | Sexp_let (rf, vbs, body) ->
      Sexp_let (rf, List.map (map_value_binding m) vbs,
                map m body)
  | Sexp_function { params; body; ret_sort; ret_mode; locality } ->
      Sexp_function
        { params = List.map (map_function_param m) params;
          body = map_function_body m body;
          ret_sort; ret_mode; locality }
  | Sexp_apply { funct; args; ret_mode } ->
      Sexp_apply
        { funct = map m funct;
          args = List.map (fun (lbl, arg) -> (lbl, map_argument m arg)) args;
          ret_mode }
  | Sexp_match { scrutinee; sort; cases; partial } ->
      Sexp_match
        { scrutinee = map m scrutinee;
          sort;
          cases = List.map (map_case m) cases;
          partial }
  | Sexp_try (e, cases) ->
      Sexp_try (map m e, List.map (map_case m) cases)
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
  | Sexp_ifthenelse (c, t, e) ->
      Sexp_ifthenelse
        (map m c, map m t, Option.map (map m) e)
  | Sexp_sequence (e1, sort, e2) ->
      Sexp_sequence (map m e1, sort, map m e2)
  | Sexp_assert e -> Sexp_assert (map m e)
  | Sexp_lazy e -> Sexp_lazy (map m e)
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

and map_value_binding m vb =
  { svb_pat = map_pattern m vb.svb_pat;
    svb_expr = map m vb.svb_expr;
    svb_rec_kind = vb.svb_rec_kind;
    svb_sort = vb.svb_sort;
  }

and map_function_param m fp =
  { fp with
    sfp_kind =
      (match fp.sfp_kind with
       | Sparam_pat p -> Sparam_pat (map_pattern m p)
       | Sparam_optional_default (p, e, sort) ->
           Sparam_optional_default
             (map_pattern m p, map m e, sort)) }

and map_function_body m = function
  | Sfunction_body e -> Sfunction_body (map m e)
  | Sfunction_cases { cases; param; arg_sort; arg_mode; ret_type; partial } ->
      Sfunction_cases
        { cases = List.map (map_case m) cases;
          param; arg_sort; arg_mode;
          ret_type = m.ty ret_type;
          partial }

and map_case m c =
  { sc_lhs = map_pattern m c.sc_lhs;
    sc_guard = Option.map (map m) c.sc_guard;
    sc_rhs = map m c.sc_rhs;
  }

and map_constructor m = function
  | Constructor { type_path = p; name } ->
      Constructor { type_path = m.type_path p; name }
  | Extension_constructor p -> Extension_constructor (m.extension_path p)

and map_label m { type_path = p; name } =
  { type_path = m.type_path p; name }

and map_pattern m p =
  { spat_desc = map_pattern_desc m p.spat_desc;
    spat_type = m.ty p.spat_type;
    spat_loc = p.spat_loc;
  }

and map_fields m fields =
  List.map (fun (lbl, p) -> (map_label m lbl, map_pattern m p)) fields

and map_pattern_desc m = function
  | Spat_any -> Spat_any
  | Spat_var (id, sort) -> Spat_var (id, sort)
  | Spat_alias (p, id, sort) -> Spat_alias (map_pattern m p, id, sort)
  | Spat_constant c -> Spat_constant c
  | Spat_tuple ps ->
      Spat_tuple (List.map (fun (lbl, p) -> (lbl, map_pattern m p)) ps)
  | Spat_unboxed_tuple ps ->
      Spat_unboxed_tuple
        (List.map
           (fun (lbl, p, sort) -> (lbl, map_pattern m p, sort))
           ps)
  | Spat_unboxed_unit -> Spat_unboxed_unit
  | Spat_unboxed_bool b -> Spat_unboxed_bool b
  | Spat_construct (cstr, args) ->
      Spat_construct
        (map_constructor m cstr,
         List.map (fun (sort, p) -> (sort, map_pattern m p)) args)
  | Spat_variant (lbl, arg) ->
      Spat_variant (lbl, Option.map (map_pattern m) arg)
  | Spat_record (fields, closed) ->
      Spat_record (map_fields m fields, closed)
  | Spat_record_unboxed_product (fields, closed) ->
      Spat_record_unboxed_product (map_fields m fields, closed)
  | Spat_or (p1, p2) ->
      Spat_or (map_pattern m p1, map_pattern m p2)
  | Spat_lazy p -> Spat_lazy (map_pattern m p)
  | Spat_exception p -> Spat_exception (map_pattern m p)

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

let rec pattern_variables_acc acc p =
  match p.spat_desc with
  | Spat_any | Spat_constant _ | Spat_unboxed_unit | Spat_unboxed_bool _
  | Spat_construct (_, []) | Spat_variant (_, None) ->
      acc
  | Spat_var (id, _) -> id :: acc
  | Spat_alias (p, id, _) -> pattern_variables_acc (id :: acc) p
  | Spat_tuple ps ->
      List.fold_left (fun acc (_, p) -> pattern_variables_acc acc p) acc ps
  | Spat_unboxed_tuple ps ->
      List.fold_left (fun acc (_, p, _) -> pattern_variables_acc acc p) acc ps
  | Spat_construct (_, args) ->
      List.fold_left (fun acc (_, p) -> pattern_variables_acc acc p) acc args
  | Spat_variant (_, Some p) | Spat_lazy p | Spat_exception p ->
      pattern_variables_acc acc p
  | Spat_record (fields, _) | Spat_record_unboxed_product (fields, _) ->
      List.fold_left (fun acc (_, p) -> pattern_variables_acc acc p) acc fields
  | Spat_or (p1, _) ->
      (* Both sides bind the same variables. *)
      pattern_variables_acc acc p1

let pattern_variables p = List.rev (pattern_variables_acc [] p)

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

let same_rec_flag (r1 : rec_flag) (r2 : rec_flag) =
  match r1, r2 with
  | Recursive, Recursive | Nonrecursive, Nonrecursive -> true
  | (Recursive | Nonrecursive), _ -> false

let same_closed_flag (c1 : closed_flag) (c2 : closed_flag) =
  match c1, c2 with
  | Closed, Closed | Open, Open -> true
  | (Closed | Open), _ -> false

let rec for_all2 f l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | x1 :: l1, x2 :: l2 -> f x1 x2 && for_all2 f l1 l2
  | [], _ :: _ | _ :: _, [] -> false

(* Patterns: returns the extended [vars] if the patterns match, [None]
   otherwise. *)
let rec alpha_equal_pattern sp vars p1 p2 =
  match p1.spat_desc, p2.spat_desc with
  | Spat_any, Spat_any -> Some vars
  | Spat_var (id1, _), Spat_var (id2, _) -> Some ((id1, id2) :: vars)
  | Spat_alias (p1, id1, _), Spat_alias (p2, id2, _) ->
      Option.map
        (fun vars -> (id1, id2) :: vars)
        (alpha_equal_pattern sp vars p1 p2)
  | Spat_constant c1, Spat_constant c2 ->
      if same_constant c1 c2 then Some vars else None
  | Spat_tuple ps1, Spat_tuple ps2 ->
      if for_all2 (fun (l1, _) (l2, _) -> Option.equal String.equal l1 l2)
           ps1 ps2
      then alpha_equal_patterns sp vars (List.map snd ps1) (List.map snd ps2)
      else None
  | Spat_unboxed_tuple ps1, Spat_unboxed_tuple ps2 ->
      if for_all2
           (fun (l1, _, _) (l2, _, _) -> Option.equal String.equal l1 l2)
           ps1 ps2
      then
        alpha_equal_patterns sp vars
          (List.map (fun (_, p, _) -> p) ps1)
          (List.map (fun (_, p, _) -> p) ps2)
      else None
  | Spat_unboxed_unit, Spat_unboxed_unit -> Some vars
  | Spat_unboxed_bool b1, Spat_unboxed_bool b2 ->
      if Bool.equal b1 b2 then Some vars else None
  | Spat_construct (c1, args1), Spat_construct (c2, args2) ->
      if same_constructor sp c1 c2 then
        alpha_equal_patterns sp vars (List.map snd args1) (List.map snd args2)
      else None
  | Spat_variant (l1, None), Spat_variant (l2, None) ->
      if String.equal l1 l2 then Some vars else None
  | Spat_variant (l1, Some p1), Spat_variant (l2, Some p2) ->
      if String.equal l1 l2 then alpha_equal_pattern sp vars p1 p2 else None
  | Spat_record (fs1, c1), Spat_record (fs2, c2)
  | Spat_record_unboxed_product (fs1, c1),
    Spat_record_unboxed_product (fs2, c2) ->
      if same_closed_flag c1 c2
         && for_all2 (fun (l1, _) (l2, _) -> same_label sp l1 l2) fs1 fs2
      then alpha_equal_patterns sp vars (List.map snd fs1) (List.map snd fs2)
      else None
  | Spat_or (p1, q1), Spat_or (p2, q2) ->
      (* Both sides bind the same variables, which must be renamed
         consistently. *)
      (match alpha_equal_pattern sp vars p1 p2 with
       | None -> None
       | Some vars' ->
           (match alpha_equal_pattern sp vars q1 q2 with
            | None -> None
            | Some vars'' ->
                let left = pattern_variables p1 in
                if List.for_all
                     (fun id ->
                        match lookup vars' id, lookup vars'' id
                        with
                        | Some a, Some b -> Ident.same a b
                        | _ -> false)
                     left
                then Some vars'
                else None))
  | Spat_lazy p1, Spat_lazy p2 -> alpha_equal_pattern sp vars p1 p2
  | Spat_exception p1, Spat_exception p2 -> alpha_equal_pattern sp vars p1 p2
  | ( Spat_any | Spat_var _ | Spat_alias _ | Spat_constant _ | Spat_tuple _
    | Spat_unboxed_tuple _ | Spat_unboxed_unit | Spat_unboxed_bool _
    | Spat_construct _ | Spat_variant _ | Spat_record _
    | Spat_record_unboxed_product _ | Spat_or _ | Spat_lazy _
    | Spat_exception _ ), _ ->
      None

and alpha_equal_patterns sp vars ps1 ps2 =
  match ps1, ps2 with
  | [], [] -> Some vars
  | p1 :: ps1, p2 :: ps2 ->
      (match alpha_equal_pattern sp vars p1 p2 with
       | None -> None
       | Some vars -> alpha_equal_patterns sp vars ps1 ps2)
  | [], _ :: _ | _ :: _, [] -> None

let rec alpha_equal_exp sp vars e1 e2 =
  match e1.sexp_desc, e2.sexp_desc with
  | Sexp_var id1, Sexp_var id2 -> same_var vars id1 id2
  | Sexp_global p1, Sexp_global p2 -> sp Value p1 p2
  | Sexp_constant c1, Sexp_constant c2 -> same_constant c1 c2
  | Sexp_let (rf1, vbs1, body1), Sexp_let (rf2, vbs2, body2) ->
      same_rec_flag rf1 rf2
      && List.compare_lengths vbs1 vbs2 = 0
      &&
      let vars_body =
        List.fold_left2
          (fun acc vb1 vb2 ->
             match acc with
             | None -> None
             | Some vars -> alpha_equal_pattern sp vars vb1.svb_pat vb2.svb_pat)
          (Some vars) vbs1 vbs2
      in
      (match vars_body with
       | None -> false
       | Some vars_body ->
           let vars_rhs =
             match rf1 with Recursive -> vars_body | Nonrecursive -> vars
           in
           for_all2
             (fun vb1 vb2 ->
                alpha_equal_exp sp vars_rhs vb1.svb_expr vb2.svb_expr)
             vbs1 vbs2
           && alpha_equal_exp sp vars_body body1 body2)
  | Sexp_function f1, Sexp_function f2 ->
      alpha_equal_params sp vars f1.params f2.params (fun vars ->
        alpha_equal_body sp vars f1.body f2.body)
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
  | Sexp_match m1, Sexp_match m2 ->
      alpha_equal_exp sp vars m1.scrutinee m2.scrutinee
      && alpha_equal_cases sp vars m1.cases m2.cases
  | Sexp_try (e1, cases1), Sexp_try (e2, cases2) ->
      alpha_equal_exp sp vars e1 e2 && alpha_equal_cases sp vars cases1 cases2
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
  | Sexp_ifthenelse (c1, t1, e1), Sexp_ifthenelse (c2, t2, e2) ->
      alpha_equal_exp sp vars c1 c2
      && alpha_equal_exp sp vars t1 t2
      && Option.equal (alpha_equal_exp sp vars) e1 e2
  | Sexp_sequence (a1, _, b1), Sexp_sequence (a2, _, b2) ->
      alpha_equal_exp sp vars a1 a2 && alpha_equal_exp sp vars b1 b2
  | Sexp_assert e1, Sexp_assert e2 -> alpha_equal_exp sp vars e1 e2
  | Sexp_lazy e1, Sexp_lazy e2 -> alpha_equal_exp sp vars e1 e2
  | Sexp_extension_constructor p1, Sexp_extension_constructor p2 ->
      sp Extension p1 p2
  | ( Sexp_var _ | Sexp_global _ | Sexp_constant _ | Sexp_let _
    | Sexp_function _ | Sexp_apply _ | Sexp_match _ | Sexp_try _
    | Sexp_tuple _ | Sexp_unboxed_tuple _ | Sexp_unboxed_unit
    | Sexp_unboxed_bool _ | Sexp_construct _ | Sexp_variant _ | Sexp_record _
    | Sexp_field _ | Sexp_record_unboxed_product _ | Sexp_unboxed_field _
    | Sexp_ifthenelse _ | Sexp_sequence _ | Sexp_assert _ | Sexp_lazy _
    | Sexp_extension_constructor _ ), _ ->
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

and alpha_equal_cases sp vars cases1 cases2 =
  for_all2
    (fun c1 c2 ->
       match alpha_equal_pattern sp vars c1.sc_lhs c2.sc_lhs with
       | None -> false
       | Some vars ->
           Option.equal (alpha_equal_exp sp vars) c1.sc_guard c2.sc_guard
           && alpha_equal_exp sp vars c1.sc_rhs c2.sc_rhs)
    cases1 cases2

and alpha_equal_params sp vars params1 params2 k =
  match params1, params2 with
  | [], [] -> k vars
  | fp1 :: params1, fp2 :: params2 ->
      same_arg_label fp1.sfp_arg_label fp2.sfp_arg_label
      &&
      let vars = (fp1.sfp_param, fp2.sfp_param) :: vars in
      (match fp1.sfp_kind, fp2.sfp_kind with
       | Sparam_pat p1, Sparam_pat p2 ->
           (match alpha_equal_pattern sp vars p1 p2 with
            | None -> false
            | Some vars -> alpha_equal_params sp vars params1 params2 k)
       | Sparam_optional_default (p1, d1, _),
         Sparam_optional_default (p2, d2, _) ->
           (* The default is evaluated in the scope of the preceding
              parameters only. *)
           alpha_equal_exp sp vars d1 d2
           && (match alpha_equal_pattern sp vars p1 p2 with
               | None -> false
               | Some vars -> alpha_equal_params sp vars params1 params2 k)
       | Sparam_pat _, Sparam_optional_default _
       | Sparam_optional_default _, Sparam_pat _ ->
           false)
  | [], _ :: _ | _ :: _, [] -> false

and alpha_equal_body sp vars body1 body2 =
  match body1, body2 with
  | Sfunction_body e1, Sfunction_body e2 -> alpha_equal_exp sp vars e1 e2
  | Sfunction_cases c1, Sfunction_cases c2 ->
      let vars = (c1.param, c2.param) :: vars in
      alpha_equal_cases sp vars c1.cases c2.cases
  | Sfunction_body _, Sfunction_cases _ | Sfunction_cases _, Sfunction_body _
    ->
      false

let alpha_equal ~same_path ~vars e1 e2 =
  alpha_equal_exp same_path vars e1 e2
