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

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

(* See [natural_loop.mli] for the interface. *)

open Ssa.Export

type loop =
  { header : finished Block.t;
    body : Block.Set.t;
    back_edges : finished Block.t list
  }

(* An edge u -> v is a back edge iff v dominates u. The natural loop of such an
   edge consists of the header v plus every block that can reach u in the graph
   without passing through v. *)
let natural_loop_body ~(header : finished Block.t)
    ~(back_preds : finished Block.t list) : Block.Set.t =
  let body = ref (Block.Set.singleton header) in
  let rec walk worklist =
    match worklist with
    | [] -> ()
    | bl :: rest ->
      let ps = Block.predecessors bl in
      let added = ref rest in
      List.iter
        (fun p ->
          if not (Block.Set.mem p !body)
          then (
            body := Block.Set.add p !body;
            added := p :: !added))
        ps;
      walk !added
  in
  let seeds =
    List.filter
      (fun bp ->
        if Block.Set.mem bp !body
        then false
        else (
          body := Block.Set.add bp !body;
          true))
      back_preds
  in
  walk seeds;
  !body

let find_loops (ssa : finished Ssa.graph) : loop list =
  let header_tbl : finished Block.t list Block.Tbl.t = Block.Tbl.create 8 in
  List.iter
    (fun (bl : finished Block.t) ->
      List.iter
        (fun (succ : finished Block.t) ->
          if Block.dominates succ bl
          then
            let existing =
              match Block.Tbl.find_opt header_tbl succ with
              | Some l -> l
              | None -> []
            in
            Block.Tbl.replace header_tbl succ (bl :: existing))
        (Block.all_successors bl))
    (Ssa.blocks ssa);
  Block.Tbl.fold
    (fun header back_preds acc ->
      let body = natural_loop_body ~header ~back_preds in
      { header; body; back_edges = back_preds } :: acc)
    header_tbl []

let edge_dominates ~(src : finished Block.t) ~(succ : finished Block.t)
    ~(target : finished Block.t) : bool =
  Block.dominates succ target
  &&
  match Block.predecessors succ with
  | [p] -> Block.equal p src
  | [] | _ :: _ :: _ -> false
