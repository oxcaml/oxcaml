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

module Shard = struct
  type t =
    { table_data : Flambda_cmx_format.table_data;
      fields : Fields_for_export.t;
      data : Rebuild_solution.data
    }

  let create data =
    { table_data =
        Flambda_cmx_format.create_table_data
          (Rebuild_solution.ids_for_export data);
      fields =
        Fields_for_export.export (Rebuild_solution.fields_for_export data);
      data
    }

  let deserialise { table_data; fields; data } =
    (* These arguments only drive pruning of exported types and code; neither
       occurs in a solution section. *)
    let renaming, (_code_ids : Code_id.importer) =
      Flambda_cmx_format.import_renaming ~table_data
        ~used_value_slots:Value_slot.Set.empty
        ~original_compilation_unit:(Symbol.external_symbols_compilation_unit ())
    in
    Rebuild_solution.apply_renaming data renaming
      ~rename_field:(Fields_for_export.import fields)
end

module Header = struct
  type t =
    { id_stamp_counters : Id_stamp_counters.t;
      participants : Compilation_unit.t list;
      index : File_sections.Idx.t Compilation_unit.Map.t;
      section_toc : int array
    }
end

type t =
  { filename : string;
    header : Header.t;
    sections : File_sections.t;
    loaded : Rebuild_solution.data Compilation_unit.Tbl.t
  }

let id_stamp_counters t = t.header.Header.id_stamp_counters

let participants t = t.header.Header.participants

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

let payload_version = 1

let save ~filename ~participants
    ~solution:({ uses; code_changes; queries } : Reaper.Staged.solution)
    ~(slot_offsets : Slot_offsets.result) =
  let data =
    Rebuild_solution.create_data ~queries ~unboxing:uses.unboxing ~code_changes
      ~slot_offsets:slot_offsets.exported_offsets
  in
  let by_unit = Rebuild_solution.partition_by_compilation_unit data in
  let builder =
    File_sections.Builder.create (Compilation_unit.Map.cardinal by_unit)
  in
  let index =
    Compilation_unit.Map.map
      (fun data ->
        File_sections.Builder.add builder (Obj.repr (Shard.create data)))
      by_unit
  in
  let serialized_sections, section_toc, _sections_length =
    File_sections.serialize (File_sections.Builder.build builder)
  in
  (* New identifiers created during rebuild must not collide with those created
     during solve. *)
  let header =
    { Header.id_stamp_counters = Id_stamp_counters.save ();
      participants;
      index;
      section_toc
    }
  in
  let oc = open_out_bin filename in
  Misc.try_finally
    (fun () ->
      output_string oc Config.ltosol_magic_number;
      output_binary_int oc payload_version;
      output_value oc (header : Header.t);
      Array.iter (output_string oc) serialized_sections)
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> raise (Error (Marshal_failed filename)))

let load filename =
  let ic = open_in_bin filename in
  (* On success File_sections owns the channel. *)
  try
    let magic = Config.ltosol_magic_number in
    let format_code = String.sub magic 0 9 in
    let buffer = really_input_string ic (String.length magic) in
    if String.equal buffer magic
    then
      let header =
        try
          if input_binary_int ic <> payload_version
          then raise (Error (Wrong_version filename));
          (input_value ic : Header.t)
        with End_of_file | Failure _ -> raise (Error (Corrupted filename))
      in
      let first_section_offset = pos_in ic in
      let sections =
        File_sections.create header.Header.section_toc filename ic
          ~first_section_offset
      in
      { filename; header; sections; loaded = Compilation_unit.Tbl.create 17 }
    else if String.starts_with ~prefix:format_code buffer
    then raise (Error (Wrong_version filename))
    else raise (Error (Wrong_format filename))
  with exn ->
    close_in_noerr ic;
    raise exn

let get_unit t cu =
  match Compilation_unit.Tbl.find_opt t.loaded cu with
  | Some data -> data
  | None ->
    let data =
      match Compilation_unit.Map.find_opt cu t.header.Header.index with
      | None -> Rebuild_solution.empty_data
      | Some idx ->
        let shard : Shard.t =
          try Obj.obj (File_sections.get_uncached t.sections idx)
          with End_of_file | Failure _ -> raise (Error (Corrupted t.filename))
        in
        let data =
          Profile.record_call ~accumulate:true "ltosol_deserialise" (fun () ->
              Shard.deserialise shard)
        in
        if Flambda_features.debug_reaper "sections"
        then
          Format.eprintf "ltosol: loaded section %s@."
            (Compilation_unit.full_path_as_string cu);
        data
    in
    (* Retain only the imported data, not a second, raw copy in File_sections.
       The cache shares identities and imports across the whole rebuild
       batch. *)
    Compilation_unit.Tbl.add t.loaded cu data;
    data

let solution_for_members t ~members =
  let participants = Compilation_unit.Set.of_list (participants t) in
  List.iter
    (fun member ->
      if not (Compilation_unit.Set.mem member participants)
      then
        Misc.fatal_errorf "Unit %a is not a participant in the LTO solution"
          (Format_doc.compat Compilation_unit.print)
          member)
    members;
  Rebuild_solution.create ~analysis_scope:(Lto_participants participants)
    ~get_unit:(get_unit t)

open Format_doc

let report_error ppf = function
  | Wrong_format filename ->
    fprintf ppf "Expected Ltosol format. Incompatible file %a"
      Location.Doc.quoted_filename filename
  | Wrong_version filename ->
    fprintf ppf "%a@ is not compatible with this version of OCaml"
      Location.Doc.quoted_filename filename
  | Corrupted filename ->
    fprintf ppf "Corrupted format@ %a" Location.Doc.quoted_filename filename
  | Marshal_failed filename ->
    fprintf ppf "Failed to marshal Ltosol to file@ %a"
      Location.Doc.quoted_filename filename

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
