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

type t = { final_typing_env : Typing_env.t option }

module Id_stamp_counters = struct
  (** Identifiers are compared by compilation unit and stamp only, so a resuming
      process must not mint stamps that collide with the imported ones. *)
  type t =
    { variables : int;
      code_ids : int;
      continuations : int;
      function_slots : int;
      value_slots : int
    }

  let save () =
    { variables = Variable.get_name_stamp_counter ();
      code_ids = Code_id.get_name_stamp_counter ();
      continuations = Continuation.get_stamp_counter ();
      function_slots = Function_slot.get_stamp_counter ();
      value_slots = Value_slot.get_stamp_counter ()
    }

  let restore
      { variables; code_ids; continuations; function_slots; value_slots } =
    Variable.restore_name_stamp_counter variables;
    Code_id.restore_name_stamp_counter code_ids;
    Continuation.restore_stamp_counter continuations;
    Function_slot.restore_stamp_counter function_slots;
    Value_slot.restore_stamp_counter value_slots
end

module File_contents = struct
  type cmr_format = t

  type t =
    { id_stamp_counters : Id_stamp_counters.t;
      table_data : Flambda_cmx_format.table_data;
      used_value_slots : Value_slot.Set.t;
      final_typing_env : Typing_env.Serializable.t option
    }

  let create ~used_value_slots ({ final_typing_env } : cmr_format) : t =
    let final_typing_env =
      Option.map
        (fun typing_env ->
          (* CR mvellacott: the returned [canonicalise] must be applied to any
             code stored alongside the environment, once the CMR stores code. *)
          let env, _canonicalise =
            Typing_env.Pre_serializable.create typing_env ~used_value_slots
          in
          Typing_env.Serializable.create_without_pruning env)
        final_typing_env
    in
    let exported_ids =
      Option.fold ~none:Ids_for_export.empty
        ~some:Typing_env.Serializable.ids_for_export final_typing_env
    in
    { id_stamp_counters = Id_stamp_counters.save ();
      table_data = Flambda_cmx_format.create_table_data exported_ids;
      used_value_slots;
      final_typing_env
    }

  let deserialise ~machine_width ~resolver
      { id_stamp_counters; table_data; used_value_slots; final_typing_env } :
      cmr_format =
    (* Must happen before anything can create an identifier. *)
    Id_stamp_counters.restore id_stamp_counters;
    let renaming, _code_ids =
      Flambda_cmx_format.import_renaming ~table_data ~used_value_slots
        ~original_compilation_unit:(Compilation_unit.get_current_exn ())
    in
    let final_typing_env =
      Option.map
        (fun typing_env ->
          Typing_env.Serializable.apply_renaming typing_env renaming
          |> Typing_env.Serializable.to_typing_env ~machine_width ~resolver)
        final_typing_env
    in
    { final_typing_env }
end

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

let save ~filename ~used_value_slots t =
  let file_contents = File_contents.create ~used_value_slots t in
  let oc = open_out_bin filename in
  Misc.try_finally
    (fun () ->
      output_string oc Config.cmr_magic_number;
      output_value oc file_contents)
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> raise (Error (Marshal_failed filename)))

let restore ~filename ~machine_width ~resolver =
  let ic = open_in_bin filename in
  let file_contents =
    Misc.try_finally
      (fun () ->
        let magic = Config.cmr_magic_number in
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
  in
  File_contents.deserialise ~machine_width ~resolver file_contents

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
    fprintf ppf "Failed to marshal Ltosol to file@ %a"
      Location.Doc.quoted_filename filename

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
