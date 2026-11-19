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
  { stamp_state : Ids_for_export.Stamp_state.t;
    table_data : Ids_for_export.Table_data.t;
    original_compilation_unit : Compilation_unit.t;
    used_value_slots : Value_slot.Set.t;
    problem :
      (Exported_offsets.t * [`Without_types] Traverse.Problem.t)
      File_sections.Idx.t;
    skeleton : Traverse.Skeleton.t File_sections.Idx.t
  }

let stamp_state t = t.stamp_state

let renaming { table_data; original_compilation_unit; used_value_slots; _ } =
  Ids_for_export.Table_data.create_import_map table_data ~used_value_slots
    ~original_compilation_unit

let create_for_current_unit ~sections ~imported_offsets ~used_value_slots
    ~problem ~skeleton =
  let table_data =
    Ids_for_export.Table_data.create
      (Ids_for_export.union
         (Traverse.Problem.ids_for_export problem)
         (Traverse.Skeleton.ids_for_export skeleton))
  in
  let problem =
    File_sections.Builder.add sections (imported_offsets, problem)
  in
  let skeleton = File_sections.Builder.add sections skeleton in
  (* We need to store the stamp state so that stamp-based identifiers in the
     resumed processes don't conflict with the ones created in this process. *)
  { stamp_state = Ids_for_export.Stamp_state.save ();
    table_data;
    original_compilation_unit = Current_unit.get_cu_exn ();
    used_value_slots;
    problem;
    skeleton
  }

let read_for_solve ~sections t =
  let imported_offsets, problem = File_sections.get sections t.problem in
  imported_offsets, Traverse.Problem.apply_renaming problem (renaming t)

let read_for_rebuild ~sections t =
  let skeleton = File_sections.get sections t.skeleton in
  Traverse.Skeleton.apply_renaming skeleton (renaming t)
