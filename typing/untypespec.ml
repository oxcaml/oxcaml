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
  | Const_int i -> Const.integer (Int.to_string i)

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
