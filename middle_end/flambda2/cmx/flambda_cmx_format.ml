(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Contents of middle-end-specific portion of .cmx files when using Flambda. *)

type table_data =
  { symbols : Symbol.importer;
    variables : Variable.importer;
    simples : Simple.importer;
    consts : Reg_width_const.importer;
    code_ids : Code_id.importer;
    continuations : Continuation.importer
  }

type t0 =
  { original_compilation_unit : Compilation_unit.t;
    final_typing_env : Flambda2_types.Typing_env.Serializable.t;
    all_code : Exported_code.raw;
    exported_offsets : Exported_offsets.t;
    used_value_slots : Value_slot.Set.t;
    table_data : table_data
  }

type raw = File_sections.Idx.t

type t = t0 list * File_sections.t

let from_raw ~sections raw =
  let t : t0 list = Obj.obj (File_sections.get sections raw) in
  t, sections

let to_raw ~sections (t : t0 list) =
  File_sections.Builder.add sections (Obj.repr t)

let create_raw ~final_typing_env ~all_code ~exported_offsets ~used_value_slots
    ~sections =
  let typing_env_exported_ids =
    Flambda2_types.Typing_env.Serializable.ids_for_export final_typing_env
  in
  let all_code_exported_ids = Exported_code.ids_for_export all_code in
  let exported_ids =
    Ids_for_export.union typing_env_exported_ids all_code_exported_ids
  in
  let symbols = Symbol.export exported_ids.symbols in
  let variables = Variable.export exported_ids.variables in
  let simples = Simple.export exported_ids.simples in
  let consts = Reg_width_const.export exported_ids.consts in
  let code_ids = Code_id.export exported_ids.code_ids in
  let continuations = Continuation.export exported_ids.continuations in
  let table_data =
    { symbols; variables; simples; consts; code_ids; continuations }
  in
  let all_code =
    Exported_code.to_raw
      ~add_section:(File_sections.Builder.add sections)
      all_code
  in
  let t =
    [ { original_compilation_unit = Current_unit.get_cu_exn ();
        final_typing_env;
        all_code;
        exported_offsets;
        used_value_slots;
        table_data
      } ]
  in
  to_raw ~sections t

let import_typing_env_and_code0 ~sections t =
  let symbols = t.table_data.symbols in
  let variables = t.table_data.variables in
  let simples = t.table_data.simples in
  let consts = t.table_data.consts in
  let code_ids = t.table_data.code_ids in
  let continuations = t.table_data.continuations in
  let used_value_slots = t.used_value_slots in
  let original_compilation_unit = t.original_compilation_unit in
  let import_map =
    Profile.record_call ~accumulate:true "create_import_map" (fun () ->
        Renaming.create_import_map ~symbols ~variables ~simples ~consts
          ~code_ids ~continuations ~used_value_slots ~original_compilation_unit)
  in
  let renaming = Renaming.from_import_map import_map in
  let typing_env =
    Profile.record_call ~accumulate:true "typing_env_apply_renaming" (fun () ->
        Flambda2_types.Typing_env.Serializable.import_names t.final_typing_env
          import_map)
  in
  let all_code =
    Profile.record_call ~accumulate:true "exported_code_from_raw" (fun () ->
        Exported_code.from_raw ~sections t.all_code)
  in
  let all_code =
    Profile.record_call ~accumulate:true "exported_code_apply_renaming"
      (fun () -> Exported_code.apply_renaming code_ids renaming all_code)
  in
  typing_env, all_code

let import_typing_env_and_code (t, sections) =
  match t with
  | [] -> Misc.fatal_error "Flambda cmx info should never be empty"
  | [t0] -> import_typing_env_and_code0 ~sections t0
  | t0 :: rem ->
    List.fold_left
      (fun (typing_env, code) t0 ->
        let typing_env0, code0 = import_typing_env_and_code0 ~sections t0 in
        let typing_env =
          Profile.record_call ~accumulate:true "typing_env_merge" (fun () ->
              Flambda2_types.Typing_env.Serializable.merge typing_env
                typing_env0)
        in
        let code =
          Profile.record_call ~accumulate:true "exported_code_merge" (fun () ->
              Exported_code.merge code code0)
        in
        typing_env, code)
      (import_typing_env_and_code0 ~sections t0)
      rem

let exported_offsets (t, _) =
  List.fold_left
    (fun offsets t0 -> Exported_offsets.merge offsets t0.exported_offsets)
    Exported_offsets.empty t

let with_exported_offsets (t, sections) exported_offsets =
  match t with
  | [t0] -> [{ t0 with exported_offsets }], sections
  | [] | _ :: _ :: _ ->
    Misc.fatal_error "Cannot set exported offsets on multiple units"

let pack ~sections (units : t option list) =
  (* CR vlaviron: turn this into a proper user error *)
  match units with
  | None :: _ ->
    if List.for_all Option.is_none units
    then None
    else
      Misc.fatal_error
        "Some pack units do not have their export info set.\n\
         Flambda doesn't support packing opaque and normal units together."
  | _ ->
    let t =
      List.fold_right
        (fun unit_opt pack_data ->
          let unit_data_old_idxs, unit_sections =
            match unit_opt with
            | Some unit -> unit
            | None ->
              Misc.fatal_error
                "Some pack units do not have their export info set.\n\
                 Flambda doesn't support packing opaque and normal units \
                 together."
          in
          let idx_map = Hashtbl.create (File_sections.length unit_sections) in
          let idx_mapper old_idx =
            match Hashtbl.find_opt idx_map old_idx with
            | Some new_idx -> new_idx
            | None ->
              let new_idx =
                File_sections.Builder.add sections
                  (File_sections.get unit_sections old_idx)
              in
              Hashtbl.add idx_map old_idx new_idx;
              new_idx
          in
          let unit_data_new_idxs =
            List.map
              (fun t0 ->
                { t0 with
                  all_code = Exported_code.map_raw_index idx_mapper t0.all_code
                })
              unit_data_old_idxs
          in
          unit_data_new_idxs @ pack_data)
        units []
    in
    Some (to_raw ~sections t)

let print0 ~sections ~print_typing_env ~print_code ~print_offsets ppf t =
  Format.fprintf ppf "@[<hov>Original unit:@ %a@]@;"
    (Format_doc.compat Compilation_unit.print)
    t.original_compilation_unit;
  let unit_info =
    Unit_info.make_dummy ~input_name:"<none>" t.original_compilation_unit
  in
  Env.set_current_unit unit_info;
  let typing_env, code = import_typing_env_and_code0 ~sections t in
  if print_typing_env
  then
    Format.fprintf ppf "@[<hov>Typing env:@ %a@]@;"
      Flambda2_types.Typing_env.Serializable.print typing_env;
  if print_code
  then Format.fprintf ppf "@[<hov>Code:@ %a@]@;" Exported_code.print_view code;
  if print_offsets
  then
    Format.fprintf ppf "@[<hov>Offsets:@ %a@]@;" Exported_offsets.print
      t.exported_offsets

let print ~print_typing_env ~print_code ~print_offsets ppf (t, sections) =
  let rec print_rest ppf = function
    | [] -> ()
    | t0 :: t ->
      Format.fprintf ppf "@ (%a)"
        (print0 ~sections ~print_typing_env ~print_code ~print_offsets)
        t0;
      print_rest ppf t
  in
  match t with
  | [] -> assert false
  | [t0] -> print0 ~sections ~print_typing_env ~print_code ~print_offsets ppf t0
  | t0 :: t ->
    Format.fprintf ppf "Packed units:@ @[<v>(%a)%a@]"
      (print0 ~sections ~print_typing_env ~print_code ~print_offsets)
      t0 print_rest t
