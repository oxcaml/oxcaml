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

module Header = struct
  type t =
    { id_stamp_counters : Flambda_cmx_format.Id_stamp_counters.t;
      participants : Compilation_unit.Set.t
    }
end

module Contents = struct
  type t =
    { table_data : Flambda_cmx_format.Table_data.t;
      solution : [`Without_types] Analysis.Solution.t
    }

  let create solution =
    { table_data =
        Flambda_cmx_format.Table_data.create
          (Analysis.Solution.ids_for_export solution);
      solution
    }

  let deserialise { table_data; solution } =
    let renaming =
      Flambda_cmx_format.Table_data.create_import_map table_data
        ~used_value_slots:Value_slot.Set.empty
        ~original_compilation_unit:(Symbol.external_symbols_compilation_unit ())
    in
    Analysis.Solution.apply_renaming solution renaming
end

type t =
  { header : Header.t;
    contents : Contents.t
  }

let id_stamp_counters t = t.header.Header.id_stamp_counters

let participants t = t.header.Header.participants

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

let save ~filename ~participants ~solution =
  let contents = Contents.create solution in
  (* New identifiers created during rebuild must not collide with those created
     during solve. *)
  let header =
    { Header.id_stamp_counters = Flambda_cmx_format.Id_stamp_counters.save ();
      participants
    }
  in
  let oc = open_out_bin filename in
  Misc.try_finally
    (fun () ->
      output_string oc Config.ltosol_magic_number;
      output_value oc (header : Header.t);
      output_value oc (contents : Contents.t))
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
        let header, contents =
          try
            let header = (input_value ic : Header.t) in
            header, (input_value ic : Contents.t)
          with End_of_file | Failure _ -> raise (Error (Corrupted filename))
        in
        { header; contents }
      else if String.starts_with ~prefix:format_code buffer
      then raise (Error (Wrong_version filename))
      else raise (Error (Wrong_format filename)))
    ~always:(fun () -> close_in ic)

let solution t = Contents.deserialise t.contents

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
