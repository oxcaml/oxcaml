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

(* CR mvellacott: validate the closed-world assumption described in the mli, for
   instance from the import tables of the participating units' .cmx files, and
   consider an escape hatch that forces [any_usage] on the module symbols of
   units that are also consumed by code outside the set. *)

let participants per_unit_graphs =
  List.fold_left
    (fun set (unit, _graph) ->
      if Compilation_unit.Set.mem unit set
      then
        Misc.fatal_errorf
          "Multiple dependency graphs were given for compilation unit %a"
          (Format_doc.compat Compilation_unit.print)
          unit
      else Compilation_unit.Set.add unit set)
    Compilation_unit.Set.empty per_unit_graphs

(* Restore per-unit conservatism for identifiers defined outside the
   participating set: they could be anything. *)
let close_boundary graph ~participants =
  let ids = Global_flow_graph.ids_for_export graph in
  Symbol.Set.iter
    (fun symbol ->
      if
        not
          (Compilation_unit.Set.mem
             (Symbol.compilation_unit symbol)
             participants)
      then
        Global_flow_graph.add_any_source graph (Code_id_or_name.symbol symbol))
    ids.symbols;
  Code_id.Set.iter
    (fun code_id ->
      if
        not
          (Compilation_unit.Set.mem
             (Code_id.get_compilation_unit code_id)
             participants)
      then
        Global_flow_graph.add_any_source graph (Code_id_or_name.code_id code_id))
    ids.code_ids

let combine per_unit_graphs =
  let participants = participants per_unit_graphs in
  let graph =
    List.fold_left
      (fun combined (_unit, graph) -> Global_flow_graph.union combined graph)
      (Global_flow_graph.create ())
      per_unit_graphs
  in
  close_boundary graph ~participants;
  graph
