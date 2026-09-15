(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(* The solution of a participant, with the identifier tables needed to import
   it. *)
module Contents = struct
  type t =
    { table_data : Ids_for_export.Table_data.t;
      solution : [`Without_types] Analysis.Solution.t
    }

  let create solution =
    { table_data =
        Ids_for_export.Table_data.create
          (Analysis.Solution.ids_for_export solution);
      solution
    }

  let deserialise { table_data; solution } =
    let renaming =
      Ids_for_export.Table_data.create_import_map table_data
        ~used_value_slots:Value_slot.Set.empty
        ~original_compilation_unit:(Symbol.external_symbols_compilation_unit ())
    in
    Analysis.Solution.apply_renaming solution renaming
end

(* The solution of each participant is stored in a section of its own, so that a
   rebuild only reads the section of its unit. *)
module Header = struct
  type t =
    { stamp_state : Ids_for_export.Stamp_state.t;
      solutions : Contents.t File_sections.Idx.t Compilation_unit.Map.t;
      section_toc : int array
    }
end

type t =
  { filename : string;
    header : Header.t;
    sections : File_sections.t
  }

let stamp_state t = t.header.Header.stamp_state

let participants t =
  Compilation_unit.Set.of_list
    (List.map fst (Compilation_unit.Map.bindings t.header.Header.solutions))

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string
  | Not_a_participant of
      { filename : string;
        compilation_unit : Compilation_unit.t
      }

exception Error of error

let save ~filename ~solutions =
  let sections = File_sections.Builder.create (List.length solutions) in
  let solution_sections =
    List.fold_left
      (fun solution_sections (participant, solution) ->
        Compilation_unit.Map.add participant
          (File_sections.Builder.add sections (Contents.create solution))
          solution_sections)
      Compilation_unit.Map.empty solutions
  in
  let serialized_sections, section_toc, (_ : int) =
    File_sections.serialize (File_sections.Builder.build sections)
  in
  (* New identifiers created during rebuild must not collide with those created
     during solve. *)
  let header =
    { Header.stamp_state = Ids_for_export.Stamp_state.save ();
      solutions = solution_sections;
      section_toc
    }
  in
  let oc = open_out_bin filename in
  Misc.try_finally
    (fun () ->
      output_string oc Config.ltosol_magic_number;
      output_value oc (header : Header.t);
      Array.iter (output_string oc) serialized_sections)
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> raise (Error (Marshal_failed filename)))

let load filename =
  let ic = open_in_bin filename in
  try
    let magic = Config.ltosol_magic_number in
    let format_code = String.sub magic 0 9 in
    let buffer = really_input_string ic (String.length magic) in
    if String.equal buffer magic
    then
      let header =
        try (input_value ic : Header.t)
        with End_of_file | Failure _ -> raise (Error (Corrupted filename))
      in
      (* [File_sections] now owns the channel. *)
      let sections =
        File_sections.create header.section_toc filename ic
          ~first_section_offset:(pos_in ic)
      in
      { filename; header; sections }
    else if String.starts_with ~prefix:format_code buffer
    then raise (Error (Wrong_version filename))
    else raise (Error (Wrong_format filename))
  with exn ->
    close_in_noerr ic;
    raise exn

let solution_for_unit t compilation_unit =
  match
    Compilation_unit.Map.find_opt compilation_unit t.header.Header.solutions
  with
  | None ->
    raise
      (Error (Not_a_participant { filename = t.filename; compilation_unit }))
  | Some section ->
    let contents = File_sections.get t.sections section in
    Contents.deserialise contents

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
  | Not_a_participant { filename = _; compilation_unit } ->
    fprintf ppf
      "This file has no solution for unit %a,@ which did not take part in the \
       solve"
      Compilation_unit.print compilation_unit

let () =
  Location.register_error_of_exn (function
    | Error err -> (
      match err with
      | Not_a_participant { filename; compilation_unit = _ } ->
        Some
          (Location.error_of_printer
             ~loc:(Location.in_file filename)
             report_error err)
      | Wrong_format _ | Wrong_version _ | Corrupted _ | Marshal_failed _ ->
        Some (Location.error_of_printer_file report_error err))
    | _ -> None)
