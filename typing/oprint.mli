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

open Outcometree

type 'a printer = 'a Format_doc.printer ref
type 'a toplevel_printer = (Format.formatter -> 'a -> unit) ref

<<<<<<< HEAD
val out_ident : out_ident printer
val out_value : out_value toplevel_printer
val out_label : (string * out_mutability * out_type * out_modality list) printer
val out_modality : out_modality printer
val out_modes : out_mode list printer
val out_jkind_const : out_jkind_const printer
val out_jkind : out_jkind printer
val out_type : out_type printer
val out_type_args : out_type list printer
val out_constr : out_constructor printer
val out_constr_args : (out_type * out_modality list) list printer
val out_class_type : out_class_type printer
val out_module_type : out_module_type printer
val out_sig_item : out_sig_item printer
val out_signature : out_sig_item list printer
||||||| 23e84b8c4d
val out_ident : (formatter -> out_ident -> unit) ref
val out_value : (formatter -> out_value -> unit) ref
val out_label : (formatter -> string * bool * out_type -> unit) ref
val out_type : (formatter -> out_type -> unit) ref
val out_type_args : (formatter -> out_type list -> unit) ref
val out_constr : (formatter -> out_constructor -> unit) ref
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
=======
val out_ident: out_ident printer
val out_value : out_value toplevel_printer
val out_label : out_label printer
val out_type : out_type printer
val out_type_args : out_type list printer
val out_constr : out_constructor printer
val out_class_type : out_class_type printer
val out_module_type : out_module_type printer
val out_sig_item : out_sig_item printer
val out_signature :out_sig_item list printer
val out_functor_parameters :
  (string option * Outcometree.out_module_type) option list printer
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
val out_type_extension : out_type_extension printer
val out_phrase : out_phrase toplevel_printer

val parenthesized_ident : string -> bool
