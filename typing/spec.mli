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

(** Specification expressions: the clauses of laws, stored in signatures (see
    [Types.law_declaration]). Local variables are [Ident.t]s and globals are
    paths. The main operation is equality up to the renaming of local variables.
*)

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
      (** No argument is omitted: the arguments of a partial application are a
          prefix of the parameters. *)

(** {1 Traversals} *)

val map_paths : value_path:(Path.t -> Path.t) -> expression -> expression

(** The namespaces of the global paths of an expression: values. *)
type namespace = Value

(** The global paths of an expression with their namespaces, in traversal order.
*)
val paths : expression -> (namespace * Path.t) list

(** Equality up to the renaming of local variables. [same_path] compares global
    paths. [vars] relates the free variables of the first expression to those of
    the second. *)
val alpha_equal :
  same_path:(namespace -> Path.t -> Path.t -> bool) ->
  vars:(Ident.t * Ident.t) list ->
  expression ->
  expression ->
  bool
