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
  { unit_metadata : Flambda_unit.Metadata.t;
    imported_offsets : Exported_offsets.t;
    deps : Global_flow_graph.graph;
    slot_offsets_inputs : Slot_offsets_analysis.Inputs.t;
    solve_inputs : Reaper.Staged.Solve_inputs.t;
    rebuild_data : Reaper.Staged.Traverse_rebuild.t
  }

let create ~unit_metadata ~imported_offsets ~deps ~slot_offsets_inputs
    ~solve_inputs ~rebuild_data =
  { unit_metadata;
    imported_offsets;
    deps;
    slot_offsets_inputs;
    solve_inputs = Reaper.Staged.Solve_inputs.prune_for_lto solve_inputs;
    rebuild_data
  }

let ids_for_export
    { unit_metadata;
      imported_offsets = _;
      deps;
      slot_offsets_inputs;
      solve_inputs;
      rebuild_data
    } =
  (* Slots are not hashconsed, so the offsets need no renaming. *)
  Ids_for_export.union_list
    [ Flambda_unit.Metadata.ids_for_export unit_metadata;
      Global_flow_graph.ids_for_export deps;
      Slot_offsets_analysis.Inputs.ids_for_export slot_offsets_inputs;
      Reaper.Staged.Solve_inputs.ids_for_export solve_inputs;
      Reaper.Staged.Traverse_rebuild.ids_for_export rebuild_data ]

module Deps_with_fields = struct
  (** Fields are hashconsed per-process, so the graph is stored with views of
      them in the style of the export information's table. *)
  type t =
    { deps : Global_flow_graph.graph;
      fields : Fields_for_export.t
    }

  let create deps =
    { deps;
      fields =
        Fields_for_export.export (Global_flow_graph.fields_for_export deps)
    }

  let import { deps; fields } renaming =
    Global_flow_graph.apply_renaming deps renaming
      ~rename_field:(Fields_for_export.import fields)
end

module Header = struct
  type t =
    { id_stamp_counters : Id_stamp_counters.t;
      solve : File_sections.Idx.t;
      rebuild : File_sections.Idx.t
    }

  let id_stamp_counters t = t.id_stamp_counters
end

module Solve = struct
  type t =
    { deps : Deps_with_fields.t;
      slot_offsets_inputs : Slot_offsets_analysis.Inputs.t;
      imported_offsets : Exported_offsets.t;
      solve_inputs : Reaper.Staged.Solve_inputs.t
    }
end

module Rebuild = struct
  type t =
    { unit_metadata : Flambda_unit.Metadata.t;
      rebuild_data : Reaper.Staged.Traverse_rebuild.t
    }
end

(* CR sspies: the sections stay live until the .cmx is written, after the
   backend has run. Marshalling them here would let the graph and rebuild data
   be collected earlier, if [File_sections] learnt to store pre-marshalled
   sections. *)
let to_sections ~sections
    { unit_metadata;
      imported_offsets;
      deps;
      slot_offsets_inputs;
      solve_inputs;
      rebuild_data
    } =
  let solve : Solve.t =
    { deps = Deps_with_fields.create deps;
      slot_offsets_inputs;
      imported_offsets;
      solve_inputs
    }
  in
  let rebuild : Rebuild.t = { unit_metadata; rebuild_data } in
  let solve = File_sections.Builder.add sections (Obj.repr solve) in
  let rebuild = File_sections.Builder.add sections (Obj.repr rebuild) in
  (* We need to store ID stamp counters so that stamp-based identifiers in the
     resumed processes don't conflict with the ones created in this process. *)
  let header : Header.t =
    { id_stamp_counters = Id_stamp_counters.save (); solve; rebuild }
  in
  File_sections.Builder.add sections (Obj.repr header)

type error =
  | No_lto_info of string
  | Corrupted of string

exception Error of error

let read_section (type a) ~filename ~sections idx : a =
  try Obj.obj (File_sections.get_uncached sections idx)
  with End_of_file | Failure _ -> raise (Error (Corrupted filename))

let read_header ~filename ~sections idx =
  match idx with
  | None -> raise (Error (No_lto_info filename))
  | Some idx -> (read_section ~filename ~sections idx : Header.t)

let read_for_solve ~filename ~sections ~renaming ({ solve; _ } : Header.t) =
  let ({ deps; slot_offsets_inputs; imported_offsets; solve_inputs } : Solve.t)
      =
    read_section ~filename ~sections solve
  in
  ( Deps_with_fields.import deps renaming,
    Slot_offsets_analysis.Inputs.apply_renaming slot_offsets_inputs renaming,
    imported_offsets,
    Reaper.Staged.Solve_inputs.apply_renaming solve_inputs renaming )

let read_for_rebuild ~filename ~sections ~renaming ({ rebuild; _ } : Header.t) =
  let ({ unit_metadata; rebuild_data } : Rebuild.t) =
    read_section ~filename ~sections rebuild
  in
  ( Flambda_unit.Metadata.apply_renaming unit_metadata renaming,
    Reaper.Staged.Traverse_rebuild.apply_renaming rebuild_data renaming )

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
