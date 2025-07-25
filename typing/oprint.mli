(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Projet Cristal, INRIA Rocquencourt                   *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format
open Outcometree

val out_ident : (formatter -> out_ident -> unit) ref
val out_value : (formatter -> out_value -> unit) ref
val out_label : (formatter -> string * out_mutability * out_type
  * out_modality list -> unit) ref
val out_modality : (formatter -> out_modality -> unit) ref
val out_modes_new : (formatter -> out_mode_new list -> unit) ref
val out_jkind_const : (formatter -> out_jkind_const -> unit) ref
val out_jkind : (formatter -> out_jkind -> unit) ref
val out_type : (formatter -> out_type -> unit) ref
val out_type_args : (formatter -> out_type list -> unit) ref
val out_constr : (formatter -> out_constructor -> unit) ref
val out_constr_args :
  (formatter -> ((out_type * out_modality list) list) -> unit) ref
val out_class_type : (formatter -> out_class_type -> unit) ref
val out_module_type : (formatter -> out_module_type -> unit) ref
val out_sig_item : (formatter -> out_sig_item -> unit) ref
val out_signature : (formatter -> out_sig_item list -> unit) ref
val out_functor_parameters :
  (formatter ->
   (string option * Outcometree.out_module_type) option list -> unit)
    ref
val out_type_extension : (formatter -> out_type_extension -> unit) ref
val out_phrase : (formatter -> out_phrase -> unit) ref

val parenthesized_ident : string -> bool
