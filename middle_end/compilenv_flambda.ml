(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Flambda2-specific compilation environment operations. The base
   [Compilenv] stores export info as an opaque [raw_export_info]; this
   module is the sole bridge between that representation and the typed
   [Flambda_cmx_format.t]. *)

open Cmx_format

module Flambda_cmx_format = Flambda2_cmx.Flambda_cmx_format

type unit_infos =
  (Lambda.main_module_block_format, Flambda_cmx_format.t option) unit_infos_gen

type unit_infos_raw =
  Flambda_cmx_format.raw option Cmx_format.unit_infos_raw

let pack (t : Flambda_cmx_format.t) : Compilenv.raw_export_info =
  let raw, sections = Flambda_cmx_format.to_raw t in
  Compilenv.Raw_export_info.pack ~raw:(Obj.repr raw) ~sections

let unpack (r : Compilenv.raw_export_info) : Flambda_cmx_format.t =
  let raw, sections = Compilenv.Raw_export_info.unpack r in
  Flambda_cmx_format.from_raw ~sections (Obj.obj raw)

let to_base_unit_infos (ui : unit_infos) : Compilenv.unit_infos =
  { ui with ui_export_info = Option.map pack ui.ui_export_info }

let of_base_unit_infos (ui : Compilenv.unit_infos) : unit_infos =
  { ui with ui_export_info = Option.map unpack ui.ui_export_info }

let read_unit_info filename =
  let ui, crc = Compilenv.read_unit_info filename in
  (of_base_unit_infos ui, crc)

let write_unit_info ui filename =
  Compilenv.write_unit_info (to_base_unit_infos ui) filename

let cache_unit_info ui =
  Compilenv.cache_unit_info (to_base_unit_infos ui)

let get_unit_export_info comp_unit =
  Option.map unpack (Compilenv.get_unit_export_info comp_unit)

let which_cmx_file comp_unit =
  Compilation_unit.which_cmx_file comp_unit
    ~accessed_by:(Compilation_unit.get_current_exn ())

let get_global_export_info comp_unit =
  get_unit_export_info (which_cmx_file comp_unit)

let set_export_info info =
  let cu = Compilenv.current_unit_infos () in
  cu.ui_export_info <- Some (pack info)

let current_export_info () =
  Option.map unpack (Compilenv.current_unit_infos ()).ui_export_info
