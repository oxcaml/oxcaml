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

type t = string

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

let save ~filename t =
  let oc = open_out_bin filename in
  Misc.try_finally
    (fun () ->
      output_string oc Config.cmr_magic_number;
      output_value oc t)
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> raise (Error (Marshal_failed filename)))

let restore ~filename =
  let ic = open_in_bin filename in
  Misc.try_finally
    (fun () ->
      let magic = Config.cmr_magic_number in
      let format_code = String.sub magic 0 9 in
      let buffer = really_input_string ic (String.length magic) in
      if String.equal buffer magic
      then
        try (input_value ic : t) with
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
    fprintf ppf "Failed to marshal Ltosol to file@ %a"
      Location.Doc.quoted_filename filename

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
