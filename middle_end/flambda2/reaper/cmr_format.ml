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
    all_code : Exported_code.t;
    imported_offsets : Exported_offsets.t;
    deps : Global_flow_graph.graph;
    slot_offsets_inputs : Slot_offsets_analysis.Inputs.t;
    solve_inputs : Reaper.Staged.Solve_inputs.t;
    rebuild_data : Reaper.Staged.Traverse_rebuild.t
  }

module Deps_with_fields = struct
  (** Fields are hashconsed per-process, so the graph is stored with views of
      them in the style of [table_data]. *)
  type t =
    { deps : Global_flow_graph.graph;
      fields : Fields_for_export.t
    }

  let create deps =
    { deps;
      fields =
        Fields_for_export.export (Global_flow_graph.fields_for_export deps)
    }

  let deserialise { deps; fields } renaming =
    Global_flow_graph.apply_renaming deps renaming
      ~rename_field:(Fields_for_export.import fields)
end

module Serialisable : sig
  type cmr_format = t

  type t

  val create : cmr_format -> t

  val deserialise_for_rebuild :
    t ->
    Flambda_unit.Metadata.t * Exported_code.t * Reaper.Staged.Traverse_rebuild.t

  val deserialise_for_solve :
    t ->
    Global_flow_graph.graph
    * Slot_offsets_analysis.Inputs.t
    * Exported_offsets.t
    * Reaper.Staged.Solve_inputs.t

  val compilation_unit : t -> Compilation_unit.t
end = struct
  type cmr_format = t

  type t =
    { original_compilation_unit : Compilation_unit.t;
      table_data : Flambda_cmx_format.table_data;
      unit_metadata : Flambda_unit.Metadata.t;
      code_metadata : Code_metadata.t list;
      imported_offsets : Exported_offsets.t;
      deps : Deps_with_fields.t;
      slot_offsets_inputs : Slot_offsets_analysis.Inputs.t;
      solve_inputs : Reaper.Staged.Solve_inputs.t;
      rebuild_data : Reaper.Staged.Traverse_rebuild.t
    }

  let create
      ({ unit_metadata;
         all_code;
         imported_offsets;
         deps;
         slot_offsets_inputs;
         solve_inputs;
         rebuild_data
       } :
        cmr_format) : t =
    (* Bodies are already stored in [rebuild_data]. Keep only metadata, without
       export types, and do not force any unloaded code. These local copies
       leave the live compilation's code and solve inputs unchanged. *)
    let code_metadata =
      Exported_code.fold_code_metadata all_code ~init:[] ~f:(fun metadata acc ->
          Code_metadata.with_result_types Unknown metadata :: acc)
    in
    let code_metadata_ids =
      List.fold_left
        (fun ids code_metadata ->
          Ids_for_export.union ids (Code_metadata.ids_for_export code_metadata))
        Ids_for_export.empty code_metadata
    in
    let solve_inputs =
      { solve_inputs with
        code_deps =
          Code_id.Map.map
            (fun (code_dep : Traverse_acc.code_dep) ->
              { code_dep with
                code_metadata =
                  Code_metadata.with_result_types Unknown code_dep.code_metadata
              })
            solve_inputs.code_deps;
        (* Only normal Reaper's type rewriting needs this list. *)
        all_sets_of_closures = []
      }
    in
    let exported_ids =
      Ids_for_export.union_list
        [ Flambda_unit.Metadata.ids_for_export unit_metadata;
          code_metadata_ids;
          Global_flow_graph.ids_for_export deps;
          Slot_offsets_analysis.Inputs.ids_for_export slot_offsets_inputs;
          Reaper.Staged.Solve_inputs.ids_for_export solve_inputs;
          Reaper.Staged.Traverse_rebuild.ids_for_export rebuild_data ]
    in
    { original_compilation_unit = Current_unit.get_cu_exn ();
      table_data = Flambda_cmx_format.create_table_data exported_ids;
      unit_metadata;
      code_metadata;
      (* Slots not hashconsed so we can store them as is. *)
      imported_offsets;
      deps = Deps_with_fields.create deps;
      slot_offsets_inputs;
      solve_inputs;
      rebuild_data
    }

  let deserialise_for_rebuild
      { original_compilation_unit;
        table_data;
        unit_metadata;
        code_metadata;
        imported_offsets = _;
        deps = _;
        slot_offsets_inputs = _;
        solve_inputs = _;
        rebuild_data
      } =
    (* Restore hashconsed IDs. There are no export types or Flambda code bodies
       to prune: [Rev_expr] renaming preserves all closure value slots. *)
    let renaming, _code_ids =
      Flambda_cmx_format.import_renaming ~table_data
        ~used_value_slots:Value_slot.Set.empty ~original_compilation_unit
    in
    let unit_metadata =
      Flambda_unit.Metadata.apply_renaming unit_metadata renaming
    in
    let all_code =
      List.fold_left
        (fun all_code code_metadata ->
          Exported_code.add_code_metadata all_code
            (Code_metadata.apply_renaming code_metadata renaming))
        Exported_code.empty code_metadata
    in
    let rebuild_data =
      Reaper.Staged.Traverse_rebuild.apply_renaming rebuild_data renaming
    in
    unit_metadata, all_code, rebuild_data

  let deserialise_for_solve
      { original_compilation_unit;
        table_data;
        unit_metadata = _;
        code_metadata = _;
        imported_offsets;
        deps;
        slot_offsets_inputs;
        solve_inputs;
        rebuild_data = _
      } =
    (* Solve inputs contain no export types or code bodies to prune. *)
    let renaming, _code_ids =
      Flambda_cmx_format.import_renaming ~table_data
        ~used_value_slots:Value_slot.Set.empty ~original_compilation_unit
    in
    ( Deps_with_fields.deserialise deps renaming,
      Slot_offsets_analysis.Inputs.apply_renaming slot_offsets_inputs renaming,
      (* Slots are not hashconsed, so the offsets need no renaming. *)
      imported_offsets,
      Reaper.Staged.Solve_inputs.apply_renaming solve_inputs renaming )

  let compilation_unit t = t.original_compilation_unit
end

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

(* Version the staged payload independently of the compiler's other formats. *)
let payload_version = 2

let save ~filename t =
  let serialisable = Serialisable.create t in
  (* We need to store ID stamp counters so that stamp-based identifiers in the
     resumed process don't conflict with the ones created in this process. *)
  let id_stamp_counters = Id_stamp_counters.save () in
  let oc = open_out_bin filename in
  Misc.try_finally
    (fun () ->
      output_string oc Config.cmr_magic_number;
      output_binary_int oc payload_version;
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
        try
          if input_binary_int ic <> payload_version
          then raise (Error (Wrong_version filename));
          (input_value ic : Serialisable.t * Id_stamp_counters.t)
        with
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
