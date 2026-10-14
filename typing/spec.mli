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
  | Sexp_extension_constructor of Path.t

and 'ty argument =
  | Arg of 'ty expression * sort
  | Omitted

(** {1 Traversals} *)

(** [map ~ty ~value_path ~type_path e] applies [ty] to every type of [e],
    [value_path] to the paths of globals and [type_path] to the paths of
    constructors, extension constructors and labels. *)
val map :
  ty:('a -> 'b) ->
  value_path:(Path.t -> Path.t) ->
  type_path:(Path.t -> Path.t) ->
  'a expression -> 'b expression

val map_types : ('a -> 'b) -> 'a expression -> 'b expression

val map_paths :
  value_path:(Path.t -> Path.t) ->
  type_path:(Path.t -> Path.t) ->
  'a expression -> 'a expression

val iter_types : ('a -> unit) -> 'a expression -> unit

(** The namespaces of the global paths of an expression: values, the types
    of constructors and labels, and extension constructors. *)
type namespace = Value | Type | Extension

(** The global paths of an expression with their namespaces, in traversal
    order. *)
val paths : 'a expression -> (namespace * Path.t) list

(** Equality up to the renaming of local variables. Types, sorts and modes
    are not compared. [same_path] compares global paths. [vars] relates
    the free variables of the first expression to those of the second. *)
val alpha_equal :
  same_path:(namespace -> Path.t -> Path.t -> bool) ->
  vars:(Ident.t * Ident.t) list -> 'a expression -> 'a expression -> bool
