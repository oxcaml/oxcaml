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

(** The local variables bound by a pattern. *)
val pattern_variables : 'a pattern -> Ident.t list

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
