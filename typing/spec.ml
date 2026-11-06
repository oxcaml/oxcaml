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

type constant = Const_int of int

type arg_label =
  | Nolabel
  | Labelled of string

type sort = Jkind_types.Sort.Const.t

type expression =
  | Sexp_var of Ident.t
  | Sexp_global of Path.t
  | Sexp_constant of constant
  | Sexp_apply of expression * (arg_label * expression * sort) list

(* Traversals *)

let rec map_paths ~value_path e =
  match e with
  | Sexp_var _ | Sexp_constant _ -> e
  | Sexp_global path -> Sexp_global (value_path path)
  | Sexp_apply (funct, args) ->
    Sexp_apply
      ( map_paths ~value_path funct,
        List.map (fun (lbl, e, sort) -> lbl, map_paths ~value_path e, sort) args
      )

let rec rename_vars ~rename e =
  match e with
  | Sexp_var id -> Sexp_var (rename id)
  | Sexp_global _ | Sexp_constant _ -> e
  | Sexp_apply (funct, args) ->
    Sexp_apply
      ( rename_vars ~rename funct,
        List.map (fun (lbl, e, sort) -> lbl, rename_vars ~rename e, sort) args
      )

type namespace = Value

let rec paths_rev acc = function
  | Sexp_var _ | Sexp_constant _ -> acc
  | Sexp_global path -> (Value, path) :: acc
  | Sexp_apply (funct, args) ->
    List.fold_left
      (fun acc (_, e, _) -> paths_rev acc e)
      (paths_rev acc funct) args

let paths e = List.rev (paths_rev [] e)
