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

(** Constants, as in [Typedtree], with floats as written and strings without
    their delimiters. *)
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

(** A constructor is identified by the path of its type and its name. *)
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
      (** No argument is omitted: the arguments of a partial application are a
          prefix of the parameters. *)
  | Sexp_tuple of (string option * expression * sort) list
  | Sexp_unboxed_tuple of (string option * expression * sort) list
  | Sexp_unboxed_unit
  | Sexp_unboxed_bool of bool
  | Sexp_construct of constructor * (sort * expression) list
  | Sexp_variant of string * expression option

(** {1 Traversals} *)

(** [map_paths ~value_path ~type_path e] applies [value_path] to the paths of
    the globals of [e] and [type_path] to the paths of the types of its
    constructors. *)
val map_paths :
  value_path:(Path.t -> Path.t) ->
  type_path:(Path.t -> Path.t) ->
  expression ->
  expression

(** The namespaces of the global paths of an expression: values and the types of
    constructors. *)
type namespace =
  | Value
  | Type

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
