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

(* A cohort may have several members in this unit, related by [newer_version_of]
   as the simplifier produces new versions of the code. Only the newest gets the
   shared symbol; an older member whose body somehow survives keeps its own
   private symbol. *)
let code_names all_code =
  let current_unit = Current_unit.get_cu_exn () in
  let code_names, members, superseded =
    Exported_code.fold_code_metadata all_code
      ~init:(Code_id.Map.empty, Cohort_id.Map.empty, Code_id.Set.empty)
      ~f:(fun code_id metadata ((code_names, members, superseded) as acc) ->
        match Code_metadata.cohort metadata with
        | None -> acc
        | Some cohort
          when not (Code_id.in_compilation_unit code_id current_unit) ->
          let name =
            Linkage_name.to_string (Cohort_id.code_linkage_name cohort)
          in
          Code_id.Map.add code_id name code_names, members, superseded
        | Some cohort ->
          let members =
            Cohort_id.Map.update cohort
              (fun existing ->
                Some (code_id :: Option.value existing ~default:[]))
              members
          in
          let superseded =
            match Code_metadata.newer_version_of metadata with
            | None -> superseded
            | Some older -> Code_id.Set.add older superseded
          in
          code_names, members, superseded)
  in
  Cohort_id.Map.fold
    (fun cohort members code_names ->
      let newest =
        List.filter
          (fun code_id -> not (Code_id.Set.mem code_id superseded))
          members
      in
      match newest with
      | [code_id] ->
        Code_id.Map.add code_id
          (Linkage_name.to_string (Cohort_id.code_linkage_name cohort))
          code_names
      | [] | _ :: _ :: _ ->
        Misc.fatal_errorf
          "Cohort %a should have exactly one newest member in this compilation \
           unit, but has: %a"
          Cohort_id.print cohort Code_id.Set.print
          (Code_id.Set.of_list newest))
    members code_names
