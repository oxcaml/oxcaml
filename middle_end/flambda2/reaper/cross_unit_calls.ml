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

module Graph = Global_flow_graph

(* Look up the referenced code among the code of the current unit. Since we use
   this module for cross unit calls, it currently always returns [None]. This
   will change once we add support for link-time optimization.  *)
let find_code_dep ~code_deps code_id =
  if not (Current_unit.is_current (Code_id.get_compilation_unit code_id))
  then None
  else
    match Code_id.Map.find_opt code_id code_deps with
    | Some code_dep -> Some code_dep
    | None ->
      Misc.fatal_errorf "Missing code dep for local code ID %a" Code_id.print
        code_id

let add_alias_for_caller graph ~caller ~from ~to_ =
  match caller with
  | None -> Graph.add_alias graph ~from ~to_
  | Some code_id ->
    Graph.add_propagate_dep graph
      ~if_used:(Code_id_or_name.code_id code_id)
      ~from ~to_

let link_closure graph ~code_deps ~closure ~code_id =
  match find_code_dep ~code_deps code_id with
  | Some code_dep ->
    Traverse_acc.connect_closure graph ~closure ~code_id code_dep
  | None ->
    (* A fresh node standing for the unknown code. *)
    let external_witness =
      Code_id_or_name.var
        (Variable.create
           (Format.asprintf "external_code_id_witness_%s" (Code_id.name code_id))
           Flambda_kind.value)
    in
    Graph.add_any_source graph external_witness;
    Graph.add_constructor_dep graph ~base:closure Field.known_arity_call_witness
      ~from:external_witness;
    Graph.add_constructor_dep graph ~base:closure
      Field.unknown_arity_call_witness ~from:external_witness;
    Graph.add_constructor_dep graph ~base:external_witness
      Field.code_id_of_call_witness ~from:closure

let link_direct_call graph ~code_deps ~le_monde_exterieur ~call ~code_id
    ~closure ~caller =
  match find_code_dep ~code_deps code_id with
  | Some (code_dep : Traverse_acc.code_dep) ->
    add_alias_for_caller graph ~caller ~to_:call
      ~from:code_dep.known_arity_call_witness;
    Option.iter
      (fun closure ->
        add_alias_for_caller graph ~caller ~from:closure
          ~to_:(Code_id_or_name.var code_dep.my_closure))
      closure
  | None ->
    (match caller with
    | None -> Graph.add_any_source graph call
    | Some caller ->
      Graph.add_propagate_dep graph
        ~if_used:(Code_id_or_name.code_id caller)
        ~to_:call
        ~from:(Code_id_or_name.symbol le_monde_exterieur));
    Option.iter
      (fun closure ->
        match caller with
        | None -> Graph.add_any_usage graph closure
        | Some caller ->
          Graph.add_use_dep graph
            ~to_:(Code_id_or_name.code_id caller)
            ~from:closure)
      closure

let link graph ~code_deps ~le_monde_exterieur references =
  List.iter
    (fun (reference : Traverse_acc.code_reference) ->
      match reference with
      | Closure { closure; code_id } ->
        link_closure graph ~code_deps ~closure ~code_id
      | Direct_call { call; code_id; closure; caller } ->
        link_direct_call graph ~code_deps ~le_monde_exterieur ~call ~code_id
          ~closure ~caller)
    references
