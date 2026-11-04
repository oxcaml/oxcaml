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

(* Bounds on loop counters: a certificate that a header parameter which is a
   basic induction variable (InductionVar.v) with a positive literal step
   never falls below a literal [lower] while its loop runs, nor rises above
   a value [upper] defined before the loop when the certificate names one.
   The extraction of this file, InductionRange.ml, is the implementation; it
   needs no review. What the certificates mean, and the proofs, are in
   InductionRangeProofs.v. BoundsCheckElim.v turns the certificates into
   facts for the bounds-check entailment.

   The argument is an induction over the executions entering the loop's
   header. On entry from outside the loop the parameter receives a literal,
   and [lower] is the least of those literals ([least_literal_argument]). Over
   a back edge it receives the induction variable's update, [counter + step]
   (or [counter - c] with [step = -c]) with machine wraparound; [no_wrap_at]
   asks the facts holding at the back-edge source (AffineSsa.guards_at), with
   [lower <= counter] and [counter <= upper], to entail [counter + step <=
   max_int], so that the sum does not wrap and exceeds [lower] by [step]. The
   upper bound is a value some two-way branch compares the counter, or one of
   its updates, with ([upper_candidates]; a loop's exit test on the next
   value of the counter is tried first). It is defined in a block that
   strictly dominates the header ([defined_above]), so its register does not
   change while the loop runs, and every predecessor of the header passes the
   parameter a value the facts at that predecessor show to be at most it
   ([argument_at_most]). The header must not be the entry block, whose
   parameters are the function's arguments. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlInt64 OxCamlList
  IntArith.
From OxCamlBackend Require Import IntegerComparison Operation.
From OxCamlSsa Require Import SsaModel SsaGraph SsaDominance.
From OxCamlLoop Require Import FourierMotzkin AffineSsa NaturalLoop
  InductionVar.
Import ListNotations.

Local Open Scope sint63_scope.

(* A certificate: the counter [biv] is never below [lower] in its loop, nor
   above the value [upper] when there is one. *)
Record range_certificate : Type :=
  { biv : InductionVar.biv;
    lower : int;
    upper : option value }.

(* The literal [pred] passes, in its [Goto] to [header], for the parameter at
   [index], when it passes one. *)
Definition literal_argument (header : block) (index : int) (pred : block)
    : option int :=
  let* args := goto_args header pred in
  let* argument := arg_value args index in
  match argument with
  | Res _ (Const_int n) _ _ =>
    let n := Int64.of_nativeint n in
    if int64_is_int n then Some (Int64.to_int n) else None
  | _ => None
  end.

(* The predecessors of the header that are not back-edge sources: the blocks
   the loop is entered from. *)
Definition entry_predecessors (loop : natural_loop) : list block :=
  let sources := back_edge_set loop in
  filter (fun pred => negb (set_mem pred sources))
    (predecessors loop.(header)).

(* The least literal the loop is entered with for the parameter at [index];
   none if some entry passes something else, or there is no entry. *)
Definition least_literal_argument (loop : natural_loop) (index : int)
    : option int :=
  let* literals :=
    map_opt (literal_argument loop.(header) index) (entry_predecessors loop)
  in
  match literals with
  | [] => None
  | first :: rest =>
    Some (OxCamlList.fold_left (fun least k => if k <? least then k else least)
      first rest)
  end.

(* Whether the value [pred] passes, in its [Goto] to [header], for the
   parameter at [index] is at most [u] there: the facts holding at [pred]
   (AffineSsa.guards_at) entail [argument <= u]. *)
Definition argument_at_most (u : value) (header : block) (index : int)
    (pred : block) : bool :=
  match goto_args header pred with
  | Some args =>
    match arg_value args index with
    | Some argument =>
      let '((bound, side), c) := linearize new_ctx [] u in
      let '((passed, side), c) := linearize c side argument in
      let '((guards, side), c) := guards_at c side pred in
      match Affine.sub_checked bound passed with
      | Some goal => entails (guards ++ side) goal
      | None => false
      end
    | None => false
    end
  | None => false
  end.

(* The values two-way branches of the graph compare a value [is_counter]
   accepts with: the other operand of such a comparison that a [Switch]
   tests. *)
Definition compared_with (blocks : list block) (is_counter : value -> bool)
    : list value :=
  concat_map
    (fun b =>
      match terminator b with
      | Terminator.Switch (Res _ (Intop (Icomp _)) [x; y] _) _ =>
        if is_counter x then [y] else if is_counter y then [x] else []
      | _ => []
      end)
    blocks.

(* The candidate upper bounds of the counter, the header parameter at
   [index]: the values the graph's branches compare one of the [updates] it
   receives over the back edges with, as a loop's exit test bounds the
   counter's next value, then those they compare the parameter itself
   with. *)
Definition upper_candidates (blocks : list block) (header : block)
    (index : int) (updates : list value) : list value :=
  compared_with blocks (fun v => existsb (value_eqb v) updates)
  ++ compared_with blocks (fun v => value_eqb v (param header index)).

(* Whether [v] is defined in a strict dominator of [header]: a result, in the
   block [op_def] records for its instruction; a parameter, of its block. Its
   register is then not written while the loop runs. *)
Definition defined_above (op_def : op_def) (header : block) (v : value)
    : bool :=
  match v with
  | Res id _ _ _ =>
    match op_def_find op_def id with
    | Some d => strictly_dominates d header
    | None => false
    end
  | Block_param d _ => strictly_dominates d header
  end.

(* Whether [u] bounds the counter at [index] of [loop] from above: it is
   defined above the loop, and every predecessor of the header passes a value
   at most [u]. *)
Definition verify_upper (op_def : op_def) (loop : natural_loop) (index : int)
    (u : value) : bool :=
  defined_above op_def loop.(header) u
  && forallb (argument_at_most u loop.(header) index)
       (predecessors loop.(header)).

(* An upper bound of the counter of [biv] among the candidates, when one
   verifies. *)
Definition find_upper (op_def : op_def) (blocks : list block)
    (biv : InductionVar.biv) : option value :=
  let loop := biv.(InductionVar.loop) in
  find (verify_upper op_def loop biv.(param_index))
    (upper_candidates blocks loop.(header) biv.(param_index) biv.(update)).

(* The fact [lower <= counter] about the form [counter], when it fits. *)
Definition lower_facts (lower : int) (counter : Affine.t) : list Affine.t :=
  match Affine.sub_checked counter (Affine.constant lower) with
  | Some fact => [fact]
  | None => []
  end.

(* The facts about a counter with the form [counter]: [lower <= counter] and,
   when there is an upper bound [u], [counter <= u], with the side facts of
   linearizing [u] pushed onto [side]. A fact whose form does not fit is left
   out. *)
Definition bound_facts (c : ctx) (side : list Affine.t) (lower : int)
    (upper : option value) (counter : Affine.t)
    : (list Affine.t * list Affine.t) * ctx :=
  match upper with
  | Some u =>
    let '((bound, side), c) := linearize c side u in
    match Affine.sub_checked bound counter with
    | Some fact => ((fact :: lower_facts lower counter, side), c)
    | None => ((lower_facts lower counter, side), c)
    end
  | None => ((lower_facts lower counter, side), c)
  end.

(* Whether the update over the back edge from [source] cannot wrap: the facts
   holding at [source], with the bounds on the counter, entail [counter + step
   <= max_int]. *)
Definition no_wrap_at (step lower : int) (upper : option value)
    (header source : block) (index : int) : bool :=
  let '((counter, side), c) := linearize new_ctx [] (param header index) in
  let '((bounds, side), c) := bound_facts c side lower upper counter in
  let '((guards, side), c) := guards_at c side source in
  let goal :=
    let* lifted := Affine.add_const_checked counter step in
    Affine.sub_checked (Affine.constant max_int) lifted
  in
  match goal with
  | Some goal => entails (bounds ++ guards ++ side) goal
  | None => false
  end.

(* Whether [lower] (and [upper]) are certified for [biv]: its step is a
   positive literal, its header is not the entry block, and no back edge can
   wrap its update. *)
Definition verify_certificate (entry : block) (biv : InductionVar.biv)
    (lower : int) (upper : option value) : bool :=
  let loop := biv.(InductionVar.loop) in
  match signed_step biv with
  | Some step =>
    (zero <? step) && negb (block_eqb loop.(header) entry)
    && forallb
         (fun source =>
           no_wrap_at step lower upper loop.(header) source
             biv.(param_index))
         loop.(back_edge_sources)
  | None => false
  end.

(* The certificates of the graph's loops, given its entry block, block list
   and table of defining blocks. *)
Definition find_certificates (op_def : op_def) (entry : block)
    (blocks : list block) : list range_certificate :=
  concat_map
    (fun '(_, bivs) =>
      filter_map
        (fun biv =>
          match least_literal_argument biv.(InductionVar.loop)
                  biv.(param_index) with
          | Some lower =>
            let upper := find_upper op_def blocks biv in
            if verify_certificate entry biv lower upper
            then Some {| biv := biv; lower := lower; upper := upper |}
            else None
          | None => None
          end)
        bivs)
    (InductionVar.analyze op_def blocks).

(* The facts a certificate contributes about its counter, interned in [c]:
   the bounds and the side facts of the upper bound. *)
Definition certificate_facts (c : ctx) (cert : range_certificate)
    : list Affine.t * ctx :=
  let header := cert.(biv).(InductionVar.loop).(header) in
  let '(id, c) := intern c (param header cert.(biv).(param_index)) in
  let '((facts, side), c) :=
    bound_facts c [] cert.(lower) cert.(upper) (Affine.var id)
  in
  (facts ++ side, c).

(* The body of the walk of [range_facts]: add the facts of [cert] when its
   loop contains [target]. *)
Definition add_range_fact (target : block) (acc : list Affine.t * ctx)
    (cert : range_certificate) : list Affine.t * ctx :=
  let '(facts, c) := acc in
  if set_mem target cert.(biv).(InductionVar.loop).(body)
  then
    let '(new_facts, c) := certificate_facts c cert in
    (new_facts ++ facts, c)
  else (facts, c).

(* The facts holding at [target] from the certificates whose loop contains
   it. *)
Definition range_facts (certificates : list range_certificate)
    (target : block) (c : ctx) : list Affine.t * ctx :=
  OxCamlList.fold_left (add_range_fact target) ([], c) certificates.

Local Close Scope sint63_scope.
