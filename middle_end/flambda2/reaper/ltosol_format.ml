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

module Serialisable_solution : sig
  type t

  val create : Unboxing_analysis.result -> t

  val deserialise : t -> Unboxing_analysis.result
end = struct
  (* CR mvellacott: add solution tables. *)

  (* Fields are hashconsed per-process, so the solution is stored with views of
     them in the style of [table_data]. *)
  type t =
    { table_data : Flambda_cmx_format.table_data;
      field_views : (Field.t * Field.view) list;
      unboxed_fields : Unboxing_analysis.unboxed Code_id_or_name.Map.t;
      changed_representation :
        (Unboxing_analysis.changed_representation * Code_id_or_name.t)
        Code_id_or_name.Map.t
    }

  let create
      ({ db = _; unboxed_fields; changed_representation } :
        Unboxing_analysis.result) =
    let ids = Ids_for_export.empty in
    let ids =
      Unboxing_analysis.unboxed_fields_ids_for_export unboxed_fields ids
    in
    let ids =
      Unboxing_analysis.changed_representation_ids_for_export
        changed_representation ids
    in
    let fields = Field.Set.empty in
    let fields =
      Unboxing_analysis.unboxed_fields_fields_for_export unboxed_fields fields
    in
    let fields =
      Unboxing_analysis.changed_representation_fields_for_export
        changed_representation fields
    in
    { table_data = Flambda_cmx_format.create_table_data ids;
      field_views = Field.export_views fields;
      unboxed_fields;
      changed_representation
    }

  let deserialise
      { table_data; field_views; unboxed_fields; changed_representation } :
      Unboxing_analysis.result =
    (* [used_value_slots] and [original_compilation_unit] only drive value-slot
       pruning, which is only consulted when rewriting Flambda types, and the
       solution contains no types. [code_ids] is only needed by
       [Exported_code.apply_renaming], and the solution contains no code. *)
    let renaming, (_code_ids : Code_id.t Code_id.Map.t) =
      Flambda_cmx_format.import_renaming ~table_data
        ~used_value_slots:Value_slot.Set.empty
        ~original_compilation_unit:(Symbol.external_symbols_compilation_unit ())
    in
    let rename_field = Field.import_views field_views in
    let unboxed_fields =
      Unboxing_analysis.unboxed_fields_apply_renaming unboxed_fields renaming
        ~rename_field
    in
    let changed_representation =
      Unboxing_analysis.changed_representation_apply_renaming
        changed_representation renaming ~rename_field
    in
    { db = Datalog.empty; unboxed_fields; changed_representation }
end

module File_contents = struct
  type t =
    { id_stamp_counters : Id_stamp_counters.t;
      participants : Compilation_unit.t list;
      solution : Serialisable_solution.t
    }
end

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

let save ~filename ~participants ~solution =
  let solution = Serialisable_solution.create solution in
  (* We need to store ID stamp counters so that stamp-based ids created during
     rebuild don't conflict with the ones created during solve. *)
  let id_stamp_counters = Id_stamp_counters.save () in
  let file_contents =
    { File_contents.id_stamp_counters; participants; solution }
  in
  let oc = open_out_bin filename in
  Misc.try_finally
    (fun () ->
      output_string oc Config.ltosol_magic_number;
      output_value oc file_contents)
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> raise (Error (Marshal_failed filename)))

let load filename =
  let ic = open_in_bin filename in
  Misc.try_finally
    (fun () ->
      let magic = Config.ltosol_magic_number in
      let format_code = String.sub magic 0 9 in
      let buffer = really_input_string ic (String.length magic) in
      if String.equal buffer magic
      then
        try (input_value ic : File_contents.t) with
        | End_of_file | Failure _ -> raise (Error (Corrupted filename))
        | Error e -> raise (Error e)
      else if String.starts_with ~prefix:format_code buffer
      then raise (Error (Wrong_version filename))
      else raise (Error (Wrong_format filename)))
    ~always:(fun () -> close_in ic)

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
