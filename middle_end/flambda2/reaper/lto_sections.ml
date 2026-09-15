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

type t =
  { imported_offsets : Exported_offsets.t;
    used_value_slots : Value_slot.Set.t;
    problem : [`Without_types] Traverse.Problem.t;
    skeleton : Traverse.Skeleton.t
  }

let create ~imported_offsets ~used_value_slots ~problem ~skeleton =
  { imported_offsets; used_value_slots; problem; skeleton }

let ids_for_export
    { imported_offsets = _; used_value_slots = _; problem; skeleton } =
  (* Slots are not hashconsed, so the offsets need no renaming. *)
  Ids_for_export.union
    (Traverse.Problem.ids_for_export problem)
    (Traverse.Skeleton.ids_for_export skeleton)

module Solve = struct
  (** Fields are hashconsed per-process, so the solve inputs are stored with
      views of their fields in the style of the export information's table. *)
  type t =
    { problem : [`Without_types] Traverse.Problem.t;
      imported_offsets : Exported_offsets.t
    }

  let create ~imported_offsets problem = { problem; imported_offsets }

  let import { problem; imported_offsets } renaming =
    imported_offsets, Traverse.Problem.apply_renaming problem renaming
end

module Header = struct
  type t =
    { id_stamp_counters : Flambda_cmx_format.Id_stamp_counters.t;
      table_data : Flambda_cmx_format.Table_data.t;
      original_compilation_unit : Compilation_unit.t;
      used_value_slots : Value_slot.Set.t;
      problem : Solve.t File_sections.Idx.t;
      skeleton : Traverse.Skeleton.t File_sections.Idx.t
    }

  let id_stamp_counters t = t.id_stamp_counters

  let renaming { table_data; original_compilation_unit; used_value_slots; _ } =
    Flambda_cmx_format.Table_data.create_import_map table_data ~used_value_slots
      ~original_compilation_unit
end

let to_sections ~sections
    ({ imported_offsets; used_value_slots; problem; skeleton } as t) =
  let table_data = Flambda_cmx_format.Table_data.create (ids_for_export t) in
  let problem =
    File_sections.Builder.add sections (Solve.create ~imported_offsets problem)
  in
  let skeleton = File_sections.Builder.add sections skeleton in
  (* We need to store ID stamp counters so that stamp-based identifiers in the
     resumed processes don't conflict with the ones created in this process. *)
  let header : Header.t =
    { id_stamp_counters = Flambda_cmx_format.Id_stamp_counters.save ();
      table_data;
      original_compilation_unit = Current_unit.get_cu_exn ();
      used_value_slots;
      problem;
      skeleton
    }
  in
  File_sections.Builder.add sections header

type error =
  | No_lto_info of string
  | Corrupted of string

exception Error of error

let read_section ~filename ~sections idx =
  try File_sections.get sections idx
  with End_of_file | Failure _ -> raise (Error (Corrupted filename))

let read_header ~filename ~sections idx =
  match idx with
  | None -> raise (Error (No_lto_info filename))
  | Some idx -> read_section ~filename ~sections idx

let read_for_solve ~filename ~sections (header : Header.t) =
  let solve = read_section ~filename ~sections header.problem in
  Solve.import solve (Header.renaming header)

let read_for_rebuild ~filename ~sections (header : Header.t) =
  let skeleton = read_section ~filename ~sections header.skeleton in
  Traverse.Skeleton.apply_renaming skeleton (Header.renaming header)

open Format_doc

let report_error ppf = function
  | No_lto_info _ ->
    fprintf ppf
      "This file has no LTO information: it was not compiled with -support-lto"
  | Corrupted _ -> fprintf ppf "Corrupted LTO information"

let filename = function No_lto_info filename | Corrupted filename -> filename

let () =
  Location.register_error_of_exn (function
    | Error err ->
      Some
        (Location.error_of_printer
           ~loc:(Location.in_file (filename err))
           report_error err)
    | _ -> None)
