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

(* Flambda2-specific compilation environment operations.

   Export info is stored in [Compilenv] as [Obj.t option] where each
   [Obj.t] is a packed [(Obj.t * File_sections.t)] pair: the raw
   Flambda export info and its associated file sections. *)

open Cmx_format

type unit_infos =
  (Lambda.main_module_block_format,
   Flambda2_cmx.Flambda_cmx_format.t option)
  unit_infos_gen

type unit_infos_raw =
  Flambda2_cmx.Flambda_cmx_format.raw option Cmx_format.unit_infos_raw

module CU = Compilation_unit

(* Unpack export info stored by base compilenv: the Obj.t is a pair
   (raw_export_info_obj, File_sections.t). *)
let unpack_export_info (packed : Obj.t) :
  Flambda2_cmx.Flambda_cmx_format.t =
  let (raw_obj, sections) =
    (Obj.obj packed : Obj.t * File_sections.t)
  in
  Flambda2_cmx.Flambda_cmx_format.from_raw ~sections
    (Obj.obj raw_obj)

(* Pack export info for storage in base compilenv *)
let pack_export_info (info : Flambda2_cmx.Flambda_cmx_format.t) : Obj.t =
  let raw, sections = Flambda2_cmx.Flambda_cmx_format.to_raw info in
  Obj.repr ((Obj.repr raw : Obj.t), sections)

let read_unit_info filename =
  let (ui, sections, crc) = Compilenv.read_unit_info filename in
  let typed_ui =
    { ui with
      ui_export_info =
        Option.map
          (fun raw ->
            Flambda2_cmx.Flambda_cmx_format.from_raw ~sections
              (Obj.obj raw))
          ui.ui_export_info }
  in
  (typed_ui, crc)

let write_unit_info info filename =
  let export_info, sections =
    match info.ui_export_info with
    | None -> None, File_sections.empty
    | Some ei ->
      let raw, sections = Flambda2_cmx.Flambda_cmx_format.to_raw ei in
      Some (Obj.repr raw), sections
  in
  Compilenv.write_unit_info
    { info with ui_export_info = export_info }
    ~export_info_sections:sections filename

let cache_unit_info info =
  let base_ui =
    match info.ui_export_info with
    | None -> { info with ui_export_info = None }
    | Some ei -> { info with ui_export_info = Some (pack_export_info ei) }
  in
  Compilenv.cache_unit_info base_ui

let equal_args arg1 arg2 =
  let ({ param = name1; value = value1 } : CU.argument) = arg1 in
  let ({ param = name2; value = value2 } : CU.argument) = arg2 in
  CU.Name.equal name1 name2 && CU.equal value1 value2

let equal_up_to_pack_prefix cu1 cu2 =
  CU.Name.equal (CU.name cu1) (CU.name cu2)
  && List.equal equal_args
       (CU.instance_arguments cu1) (CU.instance_arguments cu2)

let get_unit_export_info comp_unit =
  (* If this fails, it likely means that someone didn't call
     [CU.which_cmx_file]. *)
  assert (CU.can_access_cmx_file comp_unit
            ~accessed_by:(Compilenv.current_unit_infos ()).ui_unit);
  Compilenv.ensure_unit_loaded comp_unit;
  Option.map unpack_export_info
    (Compilenv.get_cached_export_info comp_unit)

let which_cmx_file comp_unit =
  CU.which_cmx_file comp_unit ~accessed_by:(CU.get_current_exn ())

let get_global_export_info comp_unit =
  get_unit_export_info (which_cmx_file comp_unit)

let set_export_info export_info =
  let ui = Compilenv.current_unit_infos () in
  ui.ui_export_info <- Some (pack_export_info export_info)
