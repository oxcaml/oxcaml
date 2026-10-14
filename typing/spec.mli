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

(** Specification expressions: the clauses of laws, stored in signatures
    (see [Types.law_description]). A subset of the expressions of
    {!Typedtree} without mutation, with types as a parameter (instantiated
    with [type_expr] by [Types]), local variables as [Ident.t]s and globals
    as paths, and sorts and modes as constants. *)

open Asttypes

(** Constants, as in [Typedtree]. *)
type constant =
    Const_int of int

type sort = Jkind_types.Sort.Const.t

type locality = Mode.Locality.Const.t

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

and 'ty argument =
  | Arg of 'ty expression * sort
  | Omitted

(** {1 Traversals} *)

(** [map ~ty ~value_path e] applies [ty] to every type of [e] and
    [value_path] to the paths of globals. *)
val map :
  ty:('a -> 'b) ->
  value_path:(Path.t -> Path.t) ->
  'a expression -> 'b expression

val map_types : ('a -> 'b) -> 'a expression -> 'b expression

val map_paths :
  value_path:(Path.t -> Path.t) -> 'a expression -> 'a expression

val iter_types : ('a -> unit) -> 'a expression -> unit

(** The namespaces of the global paths of an expression: values. *)
type namespace = Value

(** The global paths of an expression with their namespaces, in traversal
    order. *)
val paths : 'a expression -> (namespace * Path.t) list

(** Equality up to the renaming of local variables. Types, sorts and modes
    are not compared. [same_path] compares global paths. [vars] relates
    the free variables of the first expression to those of the second. *)
val alpha_equal :
  same_path:(namespace -> Path.t -> Path.t -> bool) ->
  vars:(Ident.t * Ident.t) list -> 'a expression -> 'a expression -> bool
