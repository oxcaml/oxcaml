(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2010 Institut National de Recherche en Informatique et     *)
(*     en Automatique                                                     *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compilation environments for compilation units *)

open Cmx_format

val reset : Unit_info.t -> unit
        (* Reset the environment and record the name of the unit being
           compiled (including any associated -for-pack prefix). *)

val reset_info_tables: unit -> unit

val current_unit_infos: unit -> unit unit_infos_gen
        (* Return the infos for the unit being compiled *)

val get_global_export_info : Compilation_unit.t
  -> Flambda2_cmx.Flambda_cmx_format.t option
        (* Means of getting the export info found in the
           .cmx file of the given unit. *)

val get_unit_export_info
  : Compilation_unit.t -> Flambda2_cmx.Flambda_cmx_format.t option

val set_export_info : Flambda2_cmx.Flambda_cmx_format.t -> unit
        (* Set the export information for the current unit. *)

<<<<<<< oxcaml
val need_curry_fun:
  Lambda.function_kind -> Cmm.machtype list -> Cmm.machtype -> unit
val need_apply_fun:
  Cmm.machtype list -> Cmm.machtype -> Cmx_format.alloc_mode -> unit
val need_send_fun:
  Cmm.machtype list -> Cmm.machtype -> Cmx_format.alloc_mode -> unit
||||||| upstream-base
val current_unit_symbol: unit -> Symbol.t
        (* flambda-only *)

val symbol_separator: char
        (* Return the module separator used when building symbol names. *)

val make_symbol: ?unitname:string -> string option -> string
        (* [make_symbol ~unitname:u None] returns the asm symbol that
           corresponds to the compilation unit [u] (default: the current unit).
           [make_symbol ~unitname:u (Some id)] returns the asm symbol that
           corresponds to symbol [id] in the compilation unit [u]
           (or the current unit). *)

val symbol_in_current_unit: string -> bool
        (* Return true if the given asm symbol belongs to the
           current compilation unit, false otherwise. *)

val is_predefined_exception: Symbol.t -> bool
        (* flambda-only *)

val unit_for_global: Ident.t -> Compilation_unit.t
        (* flambda-only *)

val symbol_for_global: Ident.t -> string
        (* Return the asm symbol that refers to the given global identifier
           flambda-only *)
val symbol_for_global': Ident.t -> Symbol.t
        (* flambda-only *)
val global_approx: Ident.t -> Clambda.value_approximation
        (* Return the approximation for the given global identifier
           clambda-only *)
val set_global_approx: Clambda.value_approximation -> unit
        (* Record the approximation of the unit being compiled
           clambda-only *)
val record_global_approx_toplevel: unit -> unit
        (* Record the current approximation for the current toplevel phrase
           clambda-only *)

val set_export_info: Export_info.t -> unit
        (* Record the information of the unit being compiled
           flambda-only *)
val approx_env: unit -> Export_info.t
        (* Returns all the information loaded from external compilation units
           flambda-only *)
val approx_for_global: Compilation_unit.t -> Export_info.t option
        (* Loads the exported information declaring the compilation_unit
           flambda-only *)

val need_curry_fun: int -> unit
val need_apply_fun: int -> unit
val need_send_fun: int -> unit
=======
val current_unit_symbol: unit -> Symbol.t
        (* flambda-only *)

val symbol_separator: char
        (* Return the module separator used when building symbol names. *)

val escape_prefix: string
        (* Return the escape prefix for hexadecimal escape sequences
           in symbol names. *)

val make_symbol: ?unitname:string -> string option -> string
        (* [make_symbol ~unitname:u None] returns the asm symbol that
           corresponds to the compilation unit [u] (default: the current unit).
           [make_symbol ~unitname:u (Some id)] returns the asm symbol that
           corresponds to symbol [id] in the compilation unit [u]
           (or the current unit). *)

val is_predefined_exception: Symbol.t -> bool
        (* flambda-only *)

val unit_for_global: Ident.t -> Compilation_unit.t
        (* flambda-only *)

val symbol_for_global: Ident.t -> string
        (* Return the asm symbol that refers to the given global identifier
           flambda-only *)
val symbol_for_global': Ident.t -> Symbol.t
        (* flambda-only *)
val global_approx: Ident.t -> Clambda.value_approximation
        (* Return the approximation for the given global identifier
           clambda-only *)
val set_global_approx: Clambda.value_approximation -> unit
        (* Record the approximation of the unit being compiled
           clambda-only *)
val record_global_approx_toplevel: unit -> unit
        (* Record the current approximation for the current toplevel phrase
           clambda-only *)

val set_export_info: Export_info.t -> unit
        (* Record the information of the unit being compiled
           flambda-only *)
val approx_env: unit -> Export_info.t
        (* Returns all the information loaded from external compilation units
           flambda-only *)
val approx_for_global: Compilation_unit.t -> Export_info.t option
        (* Loads the exported information declaring the compilation_unit
           flambda-only *)

val need_curry_fun: int -> unit
val need_apply_fun: int -> unit
val need_send_fun: int -> unit
>>>>>>> upstream-incoming
        (* Record the need of a currying (resp. application,
           message sending) function with the given arity *)

val cached_zero_alloc_info : Zero_alloc_info.t
        (* Return cached information about functions
           (from other complication units) that satisfy certain properties. *)

val cache_zero_alloc_info : Zero_alloc_info.t -> unit
        (* [cache_zero_alloc_info c] adds [c] to [cached_zero_alloc_info] *)

val new_const_symbol : unit -> string

val read_unit_info: string -> unit_infos * Digest.t
        (* Read infos and MD5 from a [.cmx] file. *)
val write_unit_info: unit_infos -> string -> unit
        (* Save the given infos in the given file *)
val save_unit_info:
  string -> main_module_block_format:Lambda.main_module_block_format ->
  arg_descr:Lambda.arg_descr option ->
  unit
        (* Save the infos for the current unit in the given file *)
val cache_unit_info: unit_infos -> unit
        (* Enter the given infos in the cache.  The infos will be
           honored by [symbol_for_global] and [global_approx]
           without looking at the corresponding .cmx file. *)

val require_global: Compilation_unit.t -> unit
        (* Enforce a link dependency of the current compilation
           unit to the required module *)

val read_library_info: string -> library_infos

val record_external_symbols : unit -> unit

(* CR mshinwell: see comment in .ml
val ensure_sharing_between_cmi_and_cmx_imports :
  (_ * (Compilation_unit.t * _) option) list ->
  (Compilation_unit.t * 'a) list ->
  (Compilation_unit.t * 'a) list
*)

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of Compilation_unit.t * Compilation_unit.t * string

exception Error of error

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer
