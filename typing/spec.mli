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

(** Specification expressions. Local variables are [Ident.t] and globals are
    [Path.t]. *)

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
      (** The arguments are in the order of the function type's parameters
          (e.g., for [f : x:int -> y:int -> int], [f ~y:2 ~x:1] has already been
          rewritten to [f ~x:1 ~y:2]). Partial applications are allowed, but
          only if they form a prefix of the type's parameters, so no argument
          can be [Omitted] in the sense of the typedtree (e.g., [f ~x:1] is
          allowed, but [f ~y:2] is disallowed). *)
