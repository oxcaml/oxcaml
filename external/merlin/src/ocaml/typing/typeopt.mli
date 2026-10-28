(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

val is_function_type :
      Env.t -> Types.type_expr -> (Types.type_expr * Types.type_expr) option
val is_base_type : Env.t -> Types.type_expr -> Path.t -> bool

val maybe_pointer_type : Env.t -> Types.type_expr
  -> Lambda.immediate_or_pointer * Lambda.nullable
val maybe_pointer : Typedtree.expression
  -> Lambda.immediate_or_pointer * Lambda.nullable

(* CR layouts-scannable: These functions will call [Ctype.type_sort] and extract
   the layout in order to compute the array_kind. If (representable) layout info
   is stored (e.g. in the typedtree) instead of sorts, those layouts can be
   threaded through these functions to avoid the possibly expensive calls. *)
val array_type_kind :
  elt_ty:(Types.type_expr option)
  -> Env.t -> Location.t -> Types.type_expr -> Lambda.array_kind
(*
val array_type_mut : Env.t -> Types.type_expr -> Lambda.mutable_flag
val array_kind_of_elt :
  Env.t -> Location.t -> Types.type_expr -> Lambda.array_kind
*)
val array_kind : Typedtree.expression -> Lambda.array_kind
val array_pattern_kind : Typedtree.pattern -> Lambda.array_kind

(*
val refine_mixed_block_element :
  Env.t -> Location.t -> Types.type_expr -> 'a Lambda.mixed_block_element
  -> 'a Lambda.mixed_block_element
*)
val classify_lazy_argument : Typedtree.expression ->
                             [ `Constant_or_function
                             | `Float_that_cannot_be_shortcut
                             | `Identifier of [`Forward_value | `Other]
                             | `Other]

(*
(* Raises an error if the mixed block's scannable prefix would be too long.
   The shape must not contain any splice variables. *)
val assert_mixed_product_support_for_lambda_shape :
  Location.t -> Typedecl.Mixed_product_kind.t
  -> 'a Lambda.mixed_block_element array -> unit
*)
