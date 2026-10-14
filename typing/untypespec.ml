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
open Spec
open Ast_helper

let mknoloc = Location.mknoloc

let rec lident_of_path =
  let noloc_lident_of_path p = mknoloc (lident_of_path p) in
  function
  | Path.Pident id -> Longident.Lident (Ident.name id)
  | Path.Papply (p1, p2) ->
      Longident.Lapply (noloc_lident_of_path p1, noloc_lident_of_path p2)
  | Path.Pdot (p, s) | Path.Pextra_ty (p, Pcstr_ty s) ->
      Longident.Ldot (noloc_lident_of_path p, mknoloc s)
  | Path.Pextra_ty (p, _) -> lident_of_path p

let constant : Spec.constant -> Parsetree.constant = function
  | Const_char c -> Const.char c
  | Const_untagged_char c ->
      Const.mk (Pconst_untagged_char (Char.chr (c land 0xff)))
  | Const_string (s,loc,d) -> Const.string ?quotation_delimiter:d ~loc s
  | Const_int i -> Const.integer (Int.to_string i)
  | Const_int8 i -> Const.integer ~suffix:'s' (Int.to_string i)
  | Const_int16 i -> Const.integer ~suffix:'S' (Int.to_string i)
  | Const_int32 i -> Const.integer ~suffix:'l' (Int32.to_string i)
  | Const_int64 i -> Const.integer ~suffix:'L' (Int64.to_string i)
  | Const_nativeint i -> Const.integer ~suffix:'n' (Nativeint.to_string i)
  | Const_float f -> Const.float f
  | Const_float32 f -> Const.float ~suffix:'s' f
  | Const_unboxed_float f -> Const.mk (Pconst_unboxed_float (f, None))
  | Const_unboxed_float32 f -> Const.mk (Pconst_unboxed_float (f, Some 's'))
  | Const_untagged_int i ->
    Const.mk (Pconst_unboxed_integer (Int.to_string i, 'm'))
  | Const_untagged_int8 i ->
    Const.mk (Pconst_unboxed_integer (Int.to_string i, 's'))
  | Const_untagged_int16 i ->
    Const.mk (Pconst_unboxed_integer (Int.to_string i, 'S'))
  | Const_unboxed_int32 i ->
    Const.mk (Pconst_unboxed_integer (Int32.to_string i, 'l'))
  | Const_unboxed_int64 i ->
    Const.mk (Pconst_unboxed_integer (Int64.to_string i, 'L'))
  | Const_unboxed_nativeint i ->
    Const.mk (Pconst_unboxed_integer (Nativeint.to_string i, 'n'))

let head_type_annotation ~lident_of_path ty : Parsetree.core_type option =
  match Types.get_desc ty with
  | Tconstr (Pident id, _, _) when Ident.is_predef id -> None
  | Tconstr (Pextra_ty _, _, _) ->
      (* The types of inline records cannot be written. *)
      None
  | Tconstr (p, args, _) ->
      Some
        (Typ.constr (mknoloc (lident_of_path p))
           (List.map (fun _ -> Typ.any None) args))
  | _ -> None

type 'ty env =
  { lident_of_path : Path.t -> Longident.t;
    annotate : 'ty -> Parsetree.core_type option;
  }

let mk ~loc txt = Location.mkloc txt loc

(* The long identifier of a constructor or label: the one of its type, with
   the last component replaced by the name. The labels of inline records
   cannot be qualified and are disambiguated by their constructor; nor can
   the constructors and labels of a type through a functor application,
   which the type annotation disambiguates (see [annotate]). *)
let in_namespace_of env ~loc (type_path : Path.t) name =
  let lid : Longident.t =
    match type_path with
    | Pextra_ty (_, Pcstr_ty _) -> Lident name
    | _ when Path.contains_apply type_path -> Lident name
    | _ ->
        match env.lident_of_path type_path with
        | Lident _ | Lapply _ -> Lident name
        | Ldot (p, _) -> Ldot (p, mk ~loc name)
  in
  mk ~loc lid

let constructor env ~loc = function
  | Constructor { type_path; name } -> in_namespace_of env ~loc type_path name
  | Extension_constructor path -> mk ~loc (env.lident_of_path path)

let label env ~loc ({ type_path; name } : Spec.label) =
  in_namespace_of env ~loc type_path name

let ident ~loc id = mk ~loc (Ident.name id)

let annotate_pat env ty (p : Parsetree.pattern) =
  match env.annotate ty with
  | None -> p
  | Some cty -> Pat.constraint_ ~loc:p.ppat_loc p (Some cty) []

let annotate_exp env ty (e : Parsetree.expression) =
  match env.annotate ty with
  | None -> e
  | Some cty -> Exp.constraint_ ~loc:e.pexp_loc e (Some cty) []

let rec pattern env (p : _ Spec.pattern) =
  let loc = p.spat_loc in
  match p.spat_desc with
  | Spat_any -> Pat.any ~loc ()
  | Spat_var (id, _) -> Pat.var ~loc (ident ~loc id)
  | Spat_alias (p, id, _) -> Pat.alias ~loc (pattern env p) (ident ~loc id)
  | Spat_constant c -> Pat.constant ~loc (constant c)
  | Spat_tuple ps ->
      Pat.tuple ~loc (List.map (fun (l, p) -> (l, pattern env p)) ps) Closed
  | Spat_unboxed_tuple ps ->
      Pat.unboxed_tuple ~loc
        (List.map (fun (l, p, _) -> (l, pattern env p)) ps)
        Closed
  | Spat_unboxed_unit -> Pat.unboxed_unit ~loc ()
  | Spat_unboxed_bool b -> Pat.unboxed_bool ~loc b
  | Spat_construct (cstr, args) ->
      let args =
        match args with
        | [] -> None
        | [ (_, p) ] -> Some ([], pattern env p)
        | args ->
            Some
              ([],
               Pat.tuple ~loc
                 (List.map (fun (_, p) -> (None, pattern env p)) args)
                 Closed)
      in
      annotate_pat env p.spat_type
        (Pat.construct ~loc (constructor env ~loc cstr) args)
  | Spat_variant (lbl, arg) ->
      Pat.variant ~loc lbl (Option.map (pattern env) arg)
  | Spat_record (fields, closed) ->
      annotate_pat env p.spat_type
        (Pat.record ~loc (pattern_fields env ~loc fields) closed)
  | Spat_record_unboxed_product (fields, closed) ->
      annotate_pat env p.spat_type
        (Pat.record_unboxed_product ~loc (pattern_fields env ~loc fields)
           closed)
  | Spat_or (p1, p2) -> Pat.or_ ~loc (pattern env p1) (pattern env p2)
  | Spat_lazy p -> Pat.lazy_ ~loc (pattern env p)
  | Spat_exception p -> Pat.exception_ ~loc (pattern env p)

and pattern_fields env ~loc fields =
  List.map (fun (lbl, p) -> (label env ~loc lbl, pattern env p)) fields

let no_constraint : Parsetree.function_constraint =
  { mode_annotations = []; ret_type_constraint = None;
    ret_mode_annotations = [] }

let rec expression env (e : _ Spec.expression) =
  let loc = e.sexp_loc in
  match e.sexp_desc with
  | Sexp_var id -> Exp.ident ~loc (mk ~loc (Longident.Lident (Ident.name id)))
  | Sexp_global path -> Exp.ident ~loc (mk ~loc (env.lident_of_path path))
  | Sexp_constant c -> Exp.constant ~loc (constant c)
  | Sexp_let (rf, vbs, body) ->
      Exp.let_ ~loc Immutable rf
        (List.map (value_binding env) vbs)
        (expression env body)
  | Sexp_function { params; body; _ } ->
      Exp.function_ ~loc
        (List.map (function_param env) params)
        no_constraint
        (function_body env body)
  | Sexp_apply { funct; args; _ } ->
      let argument (lbl, arg) =
        match arg with
        | Arg (e, _) -> Some (lbl, expression env e)
        | Omitted -> None
      in
      Exp.apply ~loc (expression env funct) (List.filter_map argument args)
  | Sexp_match { scrutinee; cases; _ } ->
      Exp.match_ ~loc (expression env scrutinee) (List.map (case env) cases)
  | Sexp_try (e, cases) ->
      Exp.try_ ~loc (expression env e) (List.map (case env) cases)
  | Sexp_tuple (es, _) ->
      Exp.tuple ~loc (List.map (fun (l, e) -> (l, expression env e)) es)
  | Sexp_unboxed_tuple es ->
      Exp.unboxed_tuple ~loc
        (List.map (fun (l, e, _) -> (l, expression env e)) es)
  | Sexp_unboxed_unit -> Exp.unboxed_unit ~loc ()
  | Sexp_unboxed_bool b -> Exp.unboxed_bool ~loc b
  | Sexp_construct { constructor = cstr; args; _ } ->
      let args =
        match args with
        | [] -> None
        | [ (e, _) ] -> Some (expression env e)
        | args ->
            Some
              (Exp.tuple ~loc
                 (List.map (fun (e, _) -> (None, expression env e)) args))
      in
      annotate_exp env e.sexp_type
        (Exp.construct ~loc (constructor env ~loc cstr) args)
  | Sexp_variant (lbl, arg) ->
      Exp.variant ~loc lbl (Option.map (fun (e, _) -> expression env e) arg)
  | Sexp_record { fields; extended_expression; _ } ->
      annotate_exp env e.sexp_type
        (Exp.record ~loc (record_fields env ~loc fields)
           (Option.map (fun (e, _) -> expression env e) extended_expression))
  | Sexp_record_unboxed_product { fields; extended_expression } ->
      annotate_exp env e.sexp_type
        (Exp.record_unboxed_product ~loc (record_fields env ~loc fields)
           (Option.map (fun (e, _) -> expression env e) extended_expression))
  | Sexp_field { record; label = lbl; _ } ->
      Exp.field ~loc (annotated_record env record) (label env ~loc lbl)
  | Sexp_unboxed_field { record; label = lbl; _ } ->
      Exp.unboxed_field ~loc (annotated_record env record)
        (label env ~loc lbl)
  | Sexp_ifthenelse (c, t, e) ->
      Exp.ifthenelse ~loc (expression env c) (expression env t)
        (Option.map (expression env) e)
  | Sexp_sequence (e1, _, e2) ->
      Exp.sequence ~loc (expression env e1) (expression env e2)
  | Sexp_assert e -> Exp.assert_ ~loc (expression env e)
  | Sexp_lazy e -> Exp.lazy_ ~loc (expression env e)
  | Sexp_extension_constructor path ->
      let constructor =
        Exp.construct ~loc (mk ~loc (env.lident_of_path path)) None
      in
      Exp.extension ~loc
        (mk ~loc "ocaml.extension_constructor",
         PStr [Str.eval ~loc constructor])

(* The overridden fields, for [{ e with ... }] *)
and record_fields env ~loc fields =
  List.filter_map
    (fun (lbl, _, def) ->
       match def with
       | Kept _ -> None
       | Overridden e -> Some (label env ~loc lbl, expression env e))
    fields

(* The record of a field access, annotated with its type unless its
   construction already is *)
and annotated_record env record =
  match record.sexp_desc with
  | Sexp_record _ | Sexp_record_unboxed_product _ | Sexp_construct _ ->
      expression env record
  | _ -> annotate_exp env record.sexp_type (expression env record)

and value_binding env vb =
  Vb.mk ~loc:vb.svb_expr.sexp_loc (pattern env vb.svb_pat)
    (expression env vb.svb_expr)

and function_param env fp : Parsetree.function_param =
  let pat, default =
    match fp.sfp_kind with
    | Sparam_pat p -> pattern env p, None
    | Sparam_optional_default (p, e, _) ->
        pattern env p, Some (expression env e)
  in
  { pparam_desc = Pparam_val (fp.sfp_arg_label, default, pat);
    pparam_loc = pat.ppat_loc }

and function_body env : _ -> Parsetree.function_body = function
  | Sfunction_body e -> Pfunction_body (expression env e)
  | Sfunction_cases { cases; _ } ->
      let cases = List.map (case env) cases in
      let loc =
        match cases with
        | [] -> Location.none
        | (c : Parsetree.case) :: _ -> c.pc_lhs.ppat_loc
      in
      Pfunction_cases (cases, loc, [])

and case env c =
  Exp.case (pattern env c.sc_lhs)
    ?guard:(Option.map (expression env) c.sc_guard)
    (expression env c.sc_rhs)

let expression ~lident_of_path ~annotate e =
  expression { lident_of_path; annotate } e
