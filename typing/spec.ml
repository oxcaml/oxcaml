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
  | Optional of string

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

type namespace = Value

let rec paths_rev acc = function
  | Sexp_var _ | Sexp_constant _ -> acc
  | Sexp_global path -> (Value, path) :: acc
  | Sexp_apply (funct, args) ->
    List.fold_left
      (fun acc (_, e, _) -> paths_rev acc e)
      (paths_rev acc funct) args

let paths e = List.rev (paths_rev [] e)

(* Equality. [vars] maps the variables bound so far in the first
   expression to the corresponding ones of the second. *)

let lookup vars id =
  Option.map snd (List.find_opt (fun (id', _) -> Ident.same id id') vars)

let same_var vars id1 id2 =
  match lookup vars id1 with Some id2' -> Ident.same id2 id2' | None -> false

let same_constant c1 c2 =
  match c1, c2 with Const_int n1, Const_int n2 -> Int.equal n1 n2

let same_arg_label l1 l2 =
  match l1, l2 with
  | Nolabel, Nolabel -> true
  | Labelled s1, Labelled s2 | Optional s1, Optional s2 -> String.equal s1 s2
  | (Nolabel | Labelled _ | Optional _), _ -> false

let same_sort = Jkind_types.Sort.Const.equal

let rec alpha_equal ~same_path ~vars e1 e2 =
  match e1, e2 with
  | Sexp_var id1, Sexp_var id2 -> same_var vars id1 id2
  | Sexp_global p1, Sexp_global p2 -> same_path Value p1 p2
  | Sexp_constant c1, Sexp_constant c2 -> same_constant c1 c2
  | Sexp_apply (funct1, args1), Sexp_apply (funct2, args2) ->
    alpha_equal ~same_path ~vars funct1 funct2
    && List.equal
         (fun (l1, e1, s1) (l2, e2, s2) ->
           same_arg_label l1 l2
           && alpha_equal ~same_path ~vars e1 e2
           && same_sort s1 s2)
         args1 args2
  | (Sexp_var _ | Sexp_global _ | Sexp_constant _ | Sexp_apply _), _ -> false
