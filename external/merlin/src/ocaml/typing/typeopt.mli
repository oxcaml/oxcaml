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

<<<<<<< Merlin:liam-merlin-5.4.0-ox4
||||||| Compiler:8fea84a50042cd6c3e05c8ef54e4b6970b72c783
(* These translate a type system sort to a lambda layout.  The function [layout]
   gives a more precise result---this should only be used when the kind is
   needed for compilation but the precise Lambda.layout isn't needed for
=======
(* Find the layout of an ident in the given environment. Returns [None] if the
   ident refers to a primitive. *)
val layout_of_ident : Env.t -> Ident.t -> Lambda.layout option

(* These translate a type system sort to a lambda layout.  The function [layout]
   gives a more precise result---this should only be used when the kind is
   needed for compilation but the precise Lambda.layout isn't needed for
>>>>>>> Compiler:d0ba5f3571676f89e2f535e9c3eb3a554c13f3aa
val classify_lazy_argument : Typedtree.expression ->
                             [ `Constant_or_function
                             | `Float_that_cannot_be_shortcut
                             | `Identifier of [`Forward_value | `Other]
                             | `Other]
<<<<<<< Merlin:liam-merlin-5.4.0-ox4
||||||| Compiler:8fea84a50042cd6c3e05c8ef54e4b6970b72c783
val layout_of_sort : Location.t -> Jkind.Sort.Const.t -> Lambda.layout
val layout_of_non_void_sort : Jkind.Sort.Const.t -> Lambda.layout

(* Given a function type and the sort of its return type, compute the layout of
   its return type. *)
val function_return_layout :
=======
val layout_of_sort : Location.t -> Jkind.Sort.Const.t -> Lambda.layout
val layout_of_non_void_sort : Jkind.Sort.Const.t -> Lambda.layout

(* Like [layout], but falls back to the sort when the type does not determine a
   value kind (e.g. has jkind [any]) *)
val layout_or_sort :
  Env.t -> Location.t -> Jkind.Sort.Const.t -> Types.type_expr -> Lambda.layout

(* Given a function type and the sort of its return type, compute the layout of
   its return type. *)
val function_return_layout :
>>>>>>> Compiler:d0ba5f3571676f89e2f535e9c3eb3a554c13f3aa
