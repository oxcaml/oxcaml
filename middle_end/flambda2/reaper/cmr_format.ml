(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                   Miriam Vellacott, Jane Street Europe                 *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  { unit_metadata : Flambda_unit.Metadata.t;
    final_typing_env : Typing_env.t option;
    all_code : Exported_code.t;
    deps : Global_flow_graph.graph;
    rebuild_data : Reaper.Staged.Traverse_rebuild.t
  }

module All_code_with_sections = struct
  type t =
    { all_code : Exported_code.raw;
      sections : string array
    }

  let create ~used_value_slots ~canonicalise all_code =
    let all_code =
      (* CR mvellacott: [Exported_code.prepare_for_export] only uses
         [reachable_names] to prune code IDs, so it is sufficient to pass all
         present code IDs. However an implementation change could break this, it
         would be better to have some [prepare_all_for_export] function. *)
      let all_names =
        Code_id.Set.fold
          (fun code_id names ->
            Name_occurrences.add_code_id names code_id Name_mode.normal)
          (Exported_code.ids_for_export all_code).code_ids
          Name_occurrences.empty
      in
      Exported_code.prepare_for_export all_code ~reachable_names:all_names
        ~used_value_slots ~canonicalise
    in
    let ids_for_export = Exported_code.ids_for_export all_code in
    let sections_builder = File_sections.Builder.create 0 in
    let all_code =
      Exported_code.to_raw
        ~add_section:(File_sections.Builder.add sections_builder)
        all_code
    in
    let sections, _toc, _total_length =
      File_sections.serialize (File_sections.Builder.build sections_builder)
    in
    { all_code; sections }, ids_for_export

  let deserialise { all_code; sections } =
    let sections =
      File_sections.from_array
        (Array.map
           (fun section : Obj.t -> Marshal.from_string section 0)
           sections)
    in
    Exported_code.from_raw ~sections all_code
end

module Deps_with_fields = struct
  (** Fields are hashconsed per-process, so the graph is stored with views of
      them in the style of [table_data]. *)
  type t =
    { deps : Global_flow_graph.graph;
      fields : (Field.t * Field.view) list
    }

  let create deps =
    let fields =
      Field.Set.fold
        (fun field fields -> (field, Field.view field) :: fields)
        (Global_flow_graph.fields_for_export deps)
        []
    in
    { deps; fields }

  let deserialise { deps; fields } renaming =
    let field_map =
      List.fold_left
        (fun map (field, view) -> Field.Map.add field (Field.create view) map)
        Field.Map.empty fields
    in
    let rename_field field =
      match Field.Map.find_opt field field_map with
      | Some field -> field
      | None ->
        Misc.fatal_errorf
          "Field %a in the stored dependency graph has no view stored"
          Field.print field
    in
    Global_flow_graph.apply_renaming deps renaming ~rename_field
end

module Serialisable : sig
  type cmr_format = t

  type t

  val create : used_value_slots:Value_slot.Set.t -> cmr_format -> t

  val deserialise :
    machine_width:Target_system.Machine_width.t ->
    resolver:(Compilation_unit.t -> Typing_env.Serializable.t option) ->
    t ->
    cmr_format

  val compilation_unit : t -> Compilation_unit.t

  val deserialise_deps : t -> Global_flow_graph.graph
end = struct
  type cmr_format = t

  type t =
    { original_compilation_unit : Compilation_unit.t;
      table_data : Flambda_cmx_format.table_data;
      used_value_slots : Value_slot.Set.t;
      unit_metadata : Flambda_unit.Metadata.t;
      final_typing_env : Typing_env.Serializable.t option;
      all_code : All_code_with_sections.t;
      deps : Deps_with_fields.t;
      rebuild_data : Reaper.Staged.Traverse_rebuild.t
    }

  let create ~used_value_slots
      ({ unit_metadata; final_typing_env; all_code; deps; rebuild_data } :
        cmr_format) : t =
    (* The resuming invocation reads imported code metadata from the
       dependencies' .cmx files so only this unit's own code needs storing. *)
    let all_code =
      Exported_code.filter all_code ~f:(fun code_id ->
          Compilation_unit.is_current (Code_id.get_compilation_unit code_id))
    in
    let final_typing_env, canonicalise =
      match final_typing_env with
      | None -> None, Fun.id
      | Some typing_env ->
        let env, canonicalise =
          Typing_env.Pre_serializable.create typing_env ~used_value_slots
        in
        Some (Typing_env.Serializable.create_without_pruning env), canonicalise
    in
    (* Code metadata is stored twice ([all_code] and [rebuild_data]); both must
       have their types canonicalised. [unit_metadata] doesn't have types, so
       doesn't need canonicalising. *)
    let all_code, all_code_ids =
      All_code_with_sections.create ~used_value_slots ~canonicalise all_code
    in
    (* Apply the canonicalisation and unused value slot removal that
       [Pre_serializable.create] applied to the typing env to the [rebuild_data]
       types so that they are consistent. *)
    let rebuild_data =
      Reaper.Staged.Traverse_rebuild.map_result_types rebuild_data ~f:(fun ty ->
          Flambda2_types.remove_unused_value_slots_and_shortcut_aliases ty
            ~used_value_slots ~canonicalise)
    in
    (* Must happen after any identifiers change, in particular, after
       canonicalisation. *)
    let exported_ids =
      Ids_for_export.union_list
        [ Flambda_unit.Metadata.ids_for_export unit_metadata;
          all_code_ids;
          Global_flow_graph.ids_for_export deps;
          Reaper.Staged.Traverse_rebuild.ids_for_export rebuild_data;
          Option.fold ~none:Ids_for_export.empty
            ~some:Typing_env.Serializable.ids_for_export final_typing_env ]
    in
    { original_compilation_unit = Compilation_unit.get_current_exn ();
      table_data = Flambda_cmx_format.create_table_data exported_ids;
      used_value_slots;
      unit_metadata;
      final_typing_env;
      all_code;
      deps = Deps_with_fields.create deps;
      rebuild_data
    }

  let deserialise ~machine_width ~resolver
      { original_compilation_unit;
        table_data;
        used_value_slots;
        unit_metadata;
        final_typing_env;
        all_code;
        deps;
        rebuild_data
      } : cmr_format =
    (* Insert hashconsed objects from the paused process into this process'
       tables, and create a mapping from the IDs in the old process to the ones
       in this process. [code_ids] contains a copy of this mapping for code IDs,
       needed because [Exported_code.apply_renaming] takes that as a separate
       argument. [used_value_slots] here was computed by [finalize_offsets] in
       the paused process, see [Slot_offsets.result]. *)
    let renaming, code_ids =
      Flambda_cmx_format.import_renaming ~table_data ~used_value_slots
        ~original_compilation_unit
    in
    let final_typing_env =
      Option.map
        (fun typing_env ->
          Typing_env.Serializable.apply_renaming typing_env renaming
          |> Typing_env.Serializable.to_typing_env ~machine_width ~resolver)
        final_typing_env
    in
    let unit_metadata =
      Flambda_unit.Metadata.apply_renaming unit_metadata renaming
    in
    let all_code =
      All_code_with_sections.deserialise all_code
      |> Exported_code.apply_renaming code_ids renaming
    in
    let deps = Deps_with_fields.deserialise deps renaming in
    let rebuild_data =
      Reaper.Staged.Traverse_rebuild.apply_renaming rebuild_data renaming
    in
    { unit_metadata; final_typing_env; all_code; deps; rebuild_data }

  let compilation_unit t = t.original_compilation_unit

  let deserialise_deps
      { original_compilation_unit;
        table_data;
        used_value_slots;
        unit_metadata = _;
        final_typing_env = _;
        all_code = _;
        deps;
        rebuild_data = _
      } =
    (* [code_ids] is part of [renaming] that [Exported_code.apply_renaming]
       requires as a separate argument. We're not deserialising any
       [Exported_code], so we drop it here. *)
    let renaming, _code_ids =
      Flambda_cmx_format.import_renaming ~table_data ~used_value_slots
        ~original_compilation_unit
    in
    Deps_with_fields.deserialise deps renaming
end

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

let save ~filename ~used_value_slots t =
  let serialisable = Serialisable.create ~used_value_slots t in
  (* We need to store ID stamp counters so that stamp-based identifiers in the
     resumed process don't conflict with the ones created in this process. *)
  let id_stamp_counters = Id_stamp_counters.save () in
  let oc = open_out_bin filename in
  Misc.try_finally
    (fun () ->
      output_string oc Config.cmr_magic_number;
      output_value oc (serialisable, id_stamp_counters))
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> raise (Error (Marshal_failed filename)))

let load filename =
  let ic = open_in_bin filename in
  Misc.try_finally
    (fun () ->
      let magic = Config.cmr_magic_number in
      let format_code = String.sub magic 0 9 in
      let buffer = really_input_string ic (String.length magic) in
      if String.equal buffer magic
      then
        try (input_value ic : Serialisable.t * Id_stamp_counters.t) with
        | End_of_file | Failure _ -> raise (Error (Corrupted filename))
        | Error e -> raise (Error e)
      else if String.starts_with ~prefix:format_code buffer
      then raise (Error (Wrong_version filename))
      else raise (Error (Wrong_format filename)))
    ~always:(fun () -> close_in ic)

open Format_doc

let report_error ppf = function
  | Wrong_format filename ->
    fprintf ppf "Expected Cmr format. Incompatible file %a"
      Location.Doc.quoted_filename filename
  | Wrong_version filename ->
    fprintf ppf "%a@ is not compatible with this version of OCaml"
      Location.Doc.quoted_filename filename
  | Corrupted filename ->
    fprintf ppf "Corrupted format@ %a" Location.Doc.quoted_filename filename
  | Marshal_failed filename ->
    fprintf ppf "Failed to marshal Cmr to file@ %a" Location.Doc.quoted_filename
      filename

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
