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

(* The basic induction variables a loop computes for nothing but its own
   control: used only by the exit comparison and their own updates, with the
   comparison used only by the header's branch. The extraction of this file,
   DeadInductionVar.ml, is the implementation.

   See the reading guide at the top of LoopComparisons.v for the Rocq
   vocabulary, SsaModel.v for how the models see an SSA graph, SsaGraph.v
   (backend/ssa/rocq/) for the uses of a value in the graph, and NaturalLoop.v
   and InductionVar.v for the loops, their exit branches and the basic
   induction variables this file is given. As there, the file has four parts:
   (1) the definitions, (2) the intended meaning, (3) stepping-stone lemmas,
   (4) the theorems. Parts 1 and 2 are the part to check by eye.

   Why the uses of the exit comparison are checked ([exit_comparison_uses_ok]):
   deleting the loop (DeleteEmptyLoops.v) leaves the comparison computed from
   the loop's initial parameters, so a use of it other than the header's branch
   -- an instruction, or a [Return] of it after the loop -- would observe a
   value the iteration that ended the loop did not produce. The first version
   of the analysis did not check this; the error was found while writing the
   model.

   The result, in part 4, is [analyze_spec]: for each basic induction variable
   reported dead, [dead_spec] (part 2) holds, which spells out what was checked
   of every instruction and terminator of the graph: the parameter is used only
   by the exit comparison and its own updates, the exit comparison only by the
   header's branch, and each update only by the exit comparison and as that
   parameter's argument of its back edge's [Goto]; and that a loop reported
   useless has every header parameter among its dead induction variables. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlList.
From OxCamlSsa Require Import SsaModel SsaGraph.
From OxCamlLoop Require Import NaturalLoop InductionVar.
Import ListNotations.

(* ======================================================================== *)
(* Part 1: the definitions                                                  *)
(* ======================================================================== *)

(* A BIV with its verdict, and a loop with its BIVs' verdicts and whether every
   header parameter is a dead BIV. After these, the type [biv] of
   InductionVar.v is written [InductionVar.biv], its field [loop]
   [InductionVar.loop]. *)
Record biv_result : Type :=
  { biv : biv;
    dead : bool }.

Record loop_result : Type :=
  { loop : natural_loop;
    bivs : list biv_result;
    useless : bool }.

(* The parameter [parameter_index] of [header] is read only by the exit
   comparison [exit_comparison] and by the [updates], and by no terminator. *)
Definition param_uses_ok (blocks : list block) (header : block)
    (parameter_index : int) (exit_comparison : value)
    (updates : list value) : bool :=
  all_uses_ok blocks (is_param header parameter_index)
    (fun instr =>
      instr_defines_value instr exit_comparison
      || existsb (instr_defines_value instr) updates)
    (fun _ _ => false).

(* The exit comparison is read by no instruction, and by no terminator but the
   header's. *)
Definition exit_comparison_uses_ok (blocks : list block) (header : block)
    (exit_comparison : value) : bool :=
  all_uses_ok blocks (value_eqb exit_comparison)
    (fun _ => false)
    (fun block _ => block_eqb block header).

(* The update value [updated_value] is read by no instruction but the exit
   comparison, and by no terminator but the [Goto] to the header of a back-edge
   source (one of [back_edge_sources]), as the argument at position
   [parameter_index] and at no other position. *)
Definition update_uses_ok (blocks : list block) (header : block)
    (back_edge_sources : blockset) (parameter_index : int)
    (exit_comparison updated_value : value) : bool :=
  all_uses_ok blocks (value_eqb updated_value)
    (fun instr => instr_defines_value instr exit_comparison)
    (fun block _ =>
      set_mem block back_edge_sources && goto_passes_only_at header
        parameter_index updated_value block).

(* Whether a BIV is dead: its parameter, the exit comparison and its update
   values are used only as the loop's own control requires, as [dead_spec]
   (part 2) spells out. *)
Definition is_dead (blocks : list block) (biv : InductionVar.biv) : bool :=
  match find_exit_branch biv.(InductionVar.loop) with
  | None => false
  | Some exit_info =>
    let cond := exit_info.(condition) in
    let header := biv.(InductionVar.loop).(header) in
    let k := biv.(param_index) in
    let back_edge_sources := back_edge_set biv.(InductionVar.loop) in
    param_uses_ok blocks header k cond biv.(update)
    && exit_comparison_uses_ok blocks header cond
    && forallb (update_uses_ok blocks header back_edge_sources k cond)
         biv.(update)
  end.

(* Every header parameter is a dead BIV. *)
Definition loop_is_useless (loop : natural_loop) (bivs : list biv_result)
    : bool :=
  same_length (params loop.(header)) bivs && forallb dead bivs.

(* The verdicts for each loop given, with its BIVs. *)
Definition analyze (blocks : list block)
    (loops : list (natural_loop * list InductionVar.biv))
    : list loop_result :=
  map
    (fun '(loop, bivs) =>
      let bivs :=
        map (fun biv => {| biv := biv; dead := is_dead blocks biv |}) bivs
      in
      {| loop := loop; bivs := bivs; useless := loop_is_useless loop bivs |})
    loops.

(* ======================================================================== *)
(* Part 2: the intended meaning                                             *)
(* ======================================================================== *)

(* The parameter [parameter_index] of [header] is used only by the exit
   comparison [exit_comparison] and the [updates], and by no terminator
   ([uses_only], SsaGraph.v). *)
Definition param_used_only_by_exit_comparison_and_updates
    (blocks : list block) (header : block) (parameter_index : int)
    (exit_comparison : value) (updates : list value) : Prop :=
  uses_only blocks (is_param header parameter_index)
    (fun instr =>
      instr_defines_value instr exit_comparison = true
      ∨ ∃ updated_value,
           updated_value ∈ updates
           ∧ instr_defines_value instr updated_value = true)
    (fun _ _ => False).

(* The exit comparison is used by no instruction, and by no terminator but the
   header's. *)
Definition exit_comparison_used_only_by_header_branch (blocks : list block)
    (header : block) (exit_comparison : value) : Prop :=
  uses_only blocks (value_eqb exit_comparison) (fun _ => False)
    (fun block _ => block = header).

(* The update value [updated_value] is used by no instruction but the exit
   comparison, and by no terminator but the [Goto] to the header of a back-edge
   source, as the argument at position [parameter_index] and at no other
   ([passes_only_at]). *)
Definition update_used_only_by_exit_comparison_and_back_edge
    (blocks : list block) (loop : natural_loop) (parameter_index : int)
    (exit_comparison updated_value : value) : Prop :=
  uses_only blocks (value_eqb updated_value)
    (fun instr => instr_defines_value instr exit_comparison = true)
    (fun block _ =>
      block ∈ loop.(back_edge_sources)
      ∧ passes_only_at loop.(header) parameter_index updated_value block).

(* What [dead = true] establishes of a basic induction variable [biv]: its loop
   has an exit branch, and the three properties above hold of its parameter, of
   the exit comparison and of each of its update values. *)
Definition dead_spec (blocks : list block) (biv : InductionVar.biv) : Prop :=
  ∃ exit_info,
    find_exit_branch biv.(InductionVar.loop) = Some exit_info
    ∧ let exit_comparison := exit_info.(condition) in
       let loop := biv.(InductionVar.loop) in
       let parameter_index := biv.(param_index) in
       param_used_only_by_exit_comparison_and_updates blocks loop.(header)
         parameter_index exit_comparison biv.(update)
       ∧ exit_comparison_used_only_by_header_branch blocks loop.(header)
            exit_comparison
       ∧ ∀ (updated_value : value), updated_value ∈ biv.(update) ->
            update_used_only_by_exit_comparison_and_back_edge blocks loop
              parameter_index exit_comparison updated_value.

(* ======================================================================== *)
(* Part 3: lemmas used by the proofs                                        *)
(* ======================================================================== *)

(* The approval [update_uses_ok] asks of a terminator use, read back. *)
Lemma update_terminator_use_approved :
  ∀ (loop : natural_loop) (header : block) (parameter_index : int)
    (updated_value : value)
    (block : block),
  set_mem block (back_edge_set loop) && goto_passes_only_at header
    parameter_index updated_value block = true ->
  block ∈ loop.(back_edge_sources) ∧ passes_only_at header parameter_index
    updated_value block.
Proof.
  intros l header k v bl H. apply andb_true_iff in H. destruct H as [Hset H].
  split; [ apply back_edge_set_spec; exact Hset | ].
  apply goto_passes_only_at_spec. exact H.
Qed.

(* ======================================================================== *)
(* Part 4: theorems                                                         *)
(* ======================================================================== *)

(* A BIV [is_dead] accepts satisfies [dead_spec]. *)
Theorem is_dead_spec : ∀ (blocks : list block) (biv : InductionVar.biv),
  is_dead blocks biv = true -> dead_spec blocks biv.
Proof.
  intros blocks b H. unfold is_dead in H.
  destruct (find_exit_branch b.(InductionVar.loop)) as [exit_info | ]
    eqn:Hexit; [ | discriminate ].
  cbv zeta in H.
  apply andb_true_iff in H. destruct H as [H Hupd].
  apply andb_true_iff in H. destruct H as [Hself Hcond].
  rewrite forallb_forall in Hupd.
  exists exit_info. split; [ exact Hexit | ]. cbv zeta.
  split; [ | split ].
  - unfold param_uses_ok in Hself. apply all_uses_ok_spec in Hself.
    unfold param_used_only_by_exit_comparison_and_updates.
    refine (uses_only_weaken _ _ _ _ _ _ _ _ Hself).
    + intros i Hi. apply orb_true_iff in Hi. destruct Hi as [Hi | Hi].
      * left. exact Hi.
      * right. apply existsb_exists in Hi. exact Hi.
    + intros bl arr Hf. discriminate.
  - unfold exit_comparison_uses_ok in Hcond. apply all_uses_ok_spec in Hcond.
    unfold exit_comparison_used_only_by_header_branch.
    refine (uses_only_weaken _ _ _ _ _ _ _ _ Hcond).
    + intros i Hi. discriminate.
    + intros bl arr Hbl. apply block_eqb_spec. exact Hbl.
  - intros u Hu. specialize (Hupd u Hu).
    unfold update_uses_ok in Hupd. apply all_uses_ok_spec in Hupd.
    unfold update_used_only_by_exit_comparison_and_back_edge.
    refine (uses_only_weaken _ _ _ _ _ _ _ _ Hupd).
    + intros i Hi. exact Hi.
    + intros bl arr Hbl. apply update_terminator_use_approved. exact Hbl.
Qed.

(* A loop [loop_is_useless] accepts has as many BIVs as its header has
   parameters, all of them dead. *)
Theorem loop_is_useless_spec :
  ∀ (loop : natural_loop) (bivs : list biv_result),
  loop_is_useless loop bivs = true ->
  length (params loop.(header)) = length bivs
  ∧ ∀ (biv_result : biv_result),
       biv_result ∈ bivs -> biv_result.(dead) = true.
Proof.
  intros loop bivs H. unfold loop_is_useless in H.
  apply andb_true_iff in H. destruct H as [Hlen Hdead].
  split; [ apply same_length_spec; exact Hlen | ].
  rewrite forallb_forall in Hdead. exact Hdead.
Qed.

(* Each loop result is for one of the loops given, its [bivs] are those of the
   loop with [dead] computed by [is_dead], every one reported dead satisfies
   [dead_spec], and a loop reported useless has as many dead induction
   variables as its header has parameters. *)
Theorem analyze_spec :
  ∀ (blocks : list block)
    (loops : list (natural_loop * list InductionVar.biv))
    (result : loop_result),
  result ∈ (analyze blocks loops) ->
  (result.(loop), map biv result.(bivs)) ∈ loops
  ∧ (∀ (biv_result : biv_result), biv_result ∈ result.(bivs) ->
        biv_result.(dead) = is_dead blocks biv_result.(biv)
        ∧ (biv_result.(dead) = true -> dead_spec blocks biv_result.(biv)))
  ∧ (result.(useless) = true ->
        length (params result.(loop).(header)) = length result.(bivs)
        ∧ ∀ (biv_result : biv_result),
             biv_result ∈ result.(bivs) -> biv_result.(dead) = true).
Proof.
  intros blocks loops r H. unfold analyze in H.
  apply in_map_iff in H. destruct H as [[l bs] [<- Hin]]. cbv zeta.
  cbn [loop bivs useless].
  split; [ | split ].
  - rewrite map_map. cbn [biv]. rewrite map_id. exact Hin.
  - intros br Hbr. apply in_map_iff in Hbr. destruct Hbr as [b [<- Hb]].
    cbn [biv dead]. split; [ reflexivity | ].
    intros Hd. apply is_dead_spec. exact Hd.
  - intros Hu. apply loop_is_useless_spec. exact Hu.
Qed.
