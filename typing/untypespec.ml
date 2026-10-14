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

type 'ty env =
  { lident_of_path : Path.t -> Longident.t;
  }

let mk ~loc txt = Location.mkloc txt loc

let rec expression env (e : _ Spec.expression) =
  let loc = e.sexp_loc in
  match e.sexp_desc with
  | Sexp_var id -> Exp.ident ~loc (mk ~loc (Longident.Lident (Ident.name id)))
  | Sexp_global path -> Exp.ident ~loc (mk ~loc (env.lident_of_path path))
  | Sexp_constant c -> Exp.constant ~loc (constant c)
  | Sexp_apply { funct; args; _ } ->
      let argument (lbl, arg) =
        match arg with
        | Arg (e, _) -> Some (lbl, expression env e)
        | Omitted -> None
      in
      Exp.apply ~loc (expression env funct) (List.filter_map argument args)

let expression ~lident_of_path e = expression { lident_of_path } e
