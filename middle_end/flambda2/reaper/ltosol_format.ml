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
  type t = string

  (* CR mvellacott: the file currently contains only a placeholder string; it
     should hold the serialised whole-program Reaper solution instead. *)
  let create _solution = "imagine a whole datalog database"

  let deserialise t =
    Format.eprintf "Read placeholder .ltosol file: %s" t;
    Unboxing_analysis.
      { db = Datalog.empty;
        unboxed_fields = Code_id_or_name.Map.empty;
        changed_representation = Code_id_or_name.Map.empty
      }
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
