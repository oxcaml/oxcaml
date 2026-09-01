(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

(** The implementation is based on the Wu-Larus algorithm: "Static Branch
    Frequency and Program Profile Analysis", Wu and Larus, MICRO 1994. *)

(* Fraction of the probability budget assigned to back edges collectively
   (loop-back heuristic). The Wu-Larus paper uses 0.88; we use 7/8 = 0.875. CR:
   extend with additional Wu-Larus heuristics (e.g. opcode, pointer equality).
   Multiple heuristics should be combined using Dempster-Shafer theory as
   described in the paper. *)
let frac_loop_back = 0.875

(* Fraction of the non-back-edge probability budget for the exceptional edge.
   CR: could be tuned per call-site or informed by profiling data. *)
let frac_exn = 0.05

(* Fraction of the non-back non-exn probability budget assigned collectively to
   edges leading to cold blocks. CR: consider a richer block-temperature model
   if one becomes available. *)
let frac_cold = 0.1

type t = float Label.Tbl.t

(* Compute the reverse post-order of CFG blocks, treating back edges as absent.
   The result is a topological order of the DAG obtained by removing back edges,
   with the entry block first. *)
let reverse_postorder (cfg : Cfg.t) (back_edges : Cfg_edge.Set.t) : Label.t list
    =
  let n = Label.Tbl.length cfg.Cfg.blocks in
  let visited = Label.Tbl.create n in
  let order = ref [] in
  let rec dfs label =
    if not (Label.Tbl.mem visited label)
    then begin
      Label.Tbl.add visited label ();
      let block = Cfg.get_block_exn cfg label in
      let succs = Cfg.successor_labels ~normal:true ~exn:true block in
      Label.Set.iter
        (fun succ ->
          if
            not
              (Cfg_edge.Set.mem { Cfg_edge.src = label; dst = succ } back_edges)
          then dfs succ)
        succs;
      order := label :: !order
    end
  in
  dfs cfg.Cfg.entry_label;
  (* Handle blocks unreachable from the entry (should be rare in practice). *)
  Cfg.iter_blocks cfg ~f:(fun label _ ->
      if not (Label.Tbl.mem visited label) then dfs label);
  !order

(* Compute edge probabilities for all outgoing edges of [block]. Returns a table
   mapping each unique successor label to a probability; all values sum to 1.0
   for blocks with at least one successor.

   Successors are classified into four exclusive groups in priority order:
   back-edge > exceptional > cold > normal. Within each group, probability is
   distributed uniformly. *)
let edge_probabilities (cfg : Cfg.t) (loop_infos : Cfg_loop_infos.t)
    (block : Cfg.basic_block) : float Label.Tbl.t =
  let src = block.Cfg.start in
  let all_succs = Cfg.successor_labels ~normal:true ~exn:true block in
  let n = Label.Set.cardinal all_succs in
  let result = Label.Tbl.create n in
  (match n with
  | 0 -> ()
  | 1 -> Label.Set.iter (fun l -> Label.Tbl.replace result l 1.0) all_succs
  | _ ->
    let is_back dst =
      Cfg_edge.Set.mem { Cfg_edge.src; dst }
        loop_infos.Cfg_loop_infos.back_edges
    in
    let is_exn dst = Option.equal Label.equal (Some dst) block.Cfg.exn in
    let is_cold dst =
      match Cfg.get_block cfg dst with None -> false | Some b -> b.Cfg.cold
    in
    let back = Label.Set.filter is_back all_succs in
    let exn =
      Label.Set.filter (fun l -> (not (is_back l)) && is_exn l) all_succs
    in
    let cold =
      Label.Set.filter
        (fun l -> (not (is_back l)) && (not (is_exn l)) && is_cold l)
        all_succs
    in
    let normal =
      Label.Set.filter
        (fun l -> (not (is_back l)) && (not (is_exn l)) && not (is_cold l))
        all_succs
    in
    let n_back = Label.Set.cardinal back in
    let n_exn = Label.Set.cardinal exn in
    let n_cold = Label.Set.cardinal cold in
    let n_normal = Label.Set.cardinal normal in
    let n_non_back = n_exn + n_cold + n_normal in
    let n_non_back_non_exn = n_cold + n_normal in
    (* Assign the total probability budget for each group, cascading through the
       priority order. *)
    let total_back =
      if n_back > 0 && n_non_back > 0
      then frac_loop_back
      else Float.of_int n_back /. Float.of_int n
    in
    let total_non_back = 1.0 -. total_back in
    let total_exn =
      if n_exn > 0 && n_non_back_non_exn > 0
      then frac_exn *. total_non_back
      else
        Float.of_int n_exn /. Float.of_int (max 1 n_non_back) *. total_non_back
    in
    let total_non_back_non_exn = total_non_back -. total_exn in
    let total_cold =
      if n_cold > 0 && n_normal > 0
      then frac_cold *. total_non_back_non_exn
      else
        Float.of_int n_cold
        /. Float.of_int (max 1 n_non_back_non_exn)
        *. total_non_back_non_exn
    in
    let total_normal = total_non_back_non_exn -. total_cold in
    let assign group n_group total =
      if n_group > 0
      then
        let p = total /. Float.of_int n_group in
        Label.Set.iter (fun l -> Label.Tbl.replace result l p) group
    in
    assign back n_back total_back;
    assign exn n_exn total_exn;
    assign cold n_cold total_cold;
    assign normal n_normal total_normal);
  result

let build (cfg : Cfg.t) (loop_infos : Cfg_loop_infos.t) : t =
  let back_edges = loop_infos.Cfg_loop_infos.back_edges in
  let rpo = reverse_postorder cfg back_edges in
  (* Precompute edge probabilities for all blocks. *)
  let all_edge_probs =
    let tbl = Label.Tbl.create (Label.Tbl.length cfg.Cfg.blocks) in
    Cfg.iter_blocks cfg ~f:(fun label block ->
        Label.Tbl.replace tbl label (edge_probabilities cfg loop_infos block));
    tbl
  in
  (* Initialize all block frequencies to 0.0, then set entry to 1.0. *)
  let freq = Label.Tbl.create (Label.Tbl.length cfg.Cfg.blocks) in
  Cfg.iter_blocks cfg ~f:(fun label _ -> Label.Tbl.replace freq label 0.0);
  Label.Tbl.replace freq cfg.Cfg.entry_label 1.0;
  (* Propagate frequencies in reverse post-order. For each block, sum the
     contributions of its non-back-edge predecessors. For loop headers, divide
     by (1 - frac_loop_back) to account for the average number of iterations.
     Because we process in RPO, all non-back predecessors of a block are
     guaranteed to have been processed before it. *)
  List.iter
    (fun label ->
      if not (Label.equal label cfg.Cfg.entry_label)
      then begin
        let block = Cfg.get_block_exn cfg label in
        let in_freq = ref 0.0 in
        Label.Set.iter
          (fun pred_label ->
            if
              not
                (Cfg_edge.Set.mem
                   { Cfg_edge.src = pred_label; dst = label }
                   back_edges)
            then begin
              let pred_freq = Label.Tbl.find freq pred_label in
              let pred_probs = Label.Tbl.find all_edge_probs pred_label in
              match Label.Tbl.find_opt pred_probs label with
              | None -> ()
              | Some p -> in_freq := !in_freq +. (pred_freq *. p)
            end)
          block.Cfg.predecessors;
        (* A loop header has at least one back-edge predecessor. Scale its
           frequency by 1/(1 - frac_loop_back) to account for loop iterations.
           CR: a more precise cycle probability could be computed by tracking
           the actual back-edge probabilities at each latch, rather than using
           the constant [frac_loop_back]. *)
        let is_loop_header =
          Label.Set.exists
            (fun pred_label ->
              Cfg_edge.Set.mem
                { Cfg_edge.src = pred_label; dst = label }
                back_edges)
            block.Cfg.predecessors
        in
        let f =
          if is_loop_header
          then !in_freq /. (1.0 -. frac_loop_back)
          else !in_freq
        in
        Label.Tbl.replace freq label f
      end)
    rpo;
  freq
