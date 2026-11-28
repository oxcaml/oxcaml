(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Jeremy Yallop, University of Cambridge                   *)
(*                                                                        *)
(*   Copyright 2017 Jeremy Yallop                                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** [is_valid_recursive_bindings bindings] checks a group of
    mutually-recursive bindings, given as the identifiers bound by each
    binding and its right-hand side. It returns, for each binding, its kind
    if it is valid. *)
val is_valid_recursive_bindings :
  (Ident.t list * Typedtree.expression) list ->
  Value_rec_types.recursive_binding_kind option list

val is_valid_class_expr : Ident.t list -> Typedtree.class_expr -> bool
