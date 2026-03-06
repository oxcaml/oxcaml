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

val array_type_kind :
  elt_sort:(Jkind.Sort.Const.t option) -> elt_ty:(Types.type_expr option)
  -> Env.t -> Location.t -> Types.type_expr -> Lambda.array_kind
(*
val array_type_mut : Env.t -> Types.type_expr -> Lambda.mutable_flag
val array_kind_of_elt :
  elt_sort:(Jkind.Sort.Const.t option)
  -> Env.t -> Location.t -> Types.type_expr -> Lambda.array_kind
*)
val array_kind :
  Typedtree.expression -> Jkind.Sort.Const.t -> Lambda.array_kind
(*
val array_pattern_kind :
  Typedtree.pattern -> Jkind.Sort.Const.t -> Lambda.array_kind

(* If [kind] or [layout] is unknown, attempt to specialize it by examining the
   type parameters of the bigarray. If [kind] or [length] is not unknown, returns
   it unmodified. *)
val bigarray_specialize_kind_and_layout :
  Env.t -> kind:Lambda.bigarray_kind -> layout:Lambda.bigarray_layout ->
  Types.type_expr -> Lambda.bigarray_kind * Lambda.bigarray_layout

val value_kind : Env.t -> Types.type_expr -> Lambda.value_kind

val transl_mixed_block_element :
  Env.t -> Location.t -> Types.type_expr -> Types.mixed_block_element
  -> unit Lambda.mixed_block_element

*)

val classify_lazy_argument : Typedtree.expression ->
                             [ `Constant_or_function
                             | `Float_that_cannot_be_shortcut
                             | `Identifier of [`Forward_value | `Other]
                             | `Other]

