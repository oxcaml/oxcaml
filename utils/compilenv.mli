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

(** Compilation environments for compilation units.

    Export info fields use [Obj.t option] as an opaque representation;
    the middle-end module [Compilenv_flambda] provides typed access
    via [Flambda2_cmx.Flambda_cmx_format.t]. *)

open Cmx_format

val reset : Unit_info.t -> unit
        (* Reset the environment and record the name of the unit being
           compiled (including any associated -for-pack prefix). *)

val reset_info_tables: unit -> unit

val current_unit_infos: unit -> (unit, Obj.t option) unit_infos_gen
        (* Return the infos for the unit being compiled *)

val need_curry_fun:
  Lambda.function_kind -> machtype list -> machtype -> unit
val need_apply_fun:
  machtype list -> machtype -> alloc_mode -> unit
val need_send_fun:
  machtype list -> machtype -> alloc_mode -> unit
        (* Record the need of a currying (resp. application,
           message sending) function with the given arity *)

val cached_zero_alloc_info : Zero_alloc_info.t
        (* Return cached information about functions
           (from other complication units) that satisfy certain properties. *)

val cache_zero_alloc_info : Zero_alloc_info.t -> unit
        (* [cache_zero_alloc_info c] adds [c] to [cached_zero_alloc_info] *)

val new_const_symbol : unit -> string

val read_unit_info:
  string ->
  (Lambda.main_module_block_format, Obj.t option) unit_infos_gen
  * File_sections.t * Digest.t
        (* Read infos and MD5 from a [.cmx] file.
           Export info is returned as an opaque [Obj.t option] along
           with the [File_sections.t] needed to interpret it. *)

val write_unit_info:
  (Lambda.main_module_block_format, Obj.t option) unit_infos_gen ->
  export_info_sections:File_sections.t ->
  string -> unit
        (* Save the given infos in the given file *)

val save_unit_info:
  string -> main_module_block_format:Lambda.main_module_block_format ->
  arg_descr:Lambda.arg_descr option ->
  static_data:(Slambda_types.value Slambda_types.Or_missing.t
    * Slambda_types.Templates.templates) ->
  unit
        (* Save the infos for the current unit in the given file *)

val cache_unit_info:
  (Lambda.main_module_block_format, Obj.t option) unit_infos_gen -> unit
        (* Enter the given infos in the cache.  The infos will be
           honored by [symbol_for_global] and [global_approx]
           without looking at the corresponding .cmx file. *)

val ensure_unit_loaded : Compilation_unit.t -> unit
        (* Load the .cmx file for the given compilation unit if not
           already cached, and record the import dependency. *)

val try_load_unit : Compilation_unit.t -> unit
        (* Like [ensure_unit_loaded] but does not emit warning 58 if the
           .cmx file is not found. Used by the slambda evaluator where a
           missing .cmx just means there is no static data available. *)

val get_cached_static_data:
  Compilation_unit.t -> Slambda_types.value Slambda_types.Or_missing.t * Slambda_types.Templates.templates

val get_cached_export_info : Compilation_unit.t -> Obj.t option
        (* Return the cached export info for a loaded compilation unit,
           or [None] if the unit is not cached or has no export info. *)

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
