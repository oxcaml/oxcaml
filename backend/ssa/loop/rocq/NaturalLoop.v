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

(* The natural loops of an SSA graph, from its back edges, and the exit branch
   of a loop's header. The extraction of this file, NaturalLoop.ml, is the
   implementation.

   See the reading guide at the top of LoopComparisons.v for the Rocq
   vocabulary, and SsaModel.v for how the models see an SSA graph: blocks are
   abstract, with [predecessors], [all_successors] and [dominates] as the
   compiler provides them. As in the other files, there are four parts: (1) the
   definitions, (2) the intended meaning, (3) stepping-stone lemmas, (4) the
   theorems. Parts 1 and 2 are the part to check by eye.

   [natural_loop_body] runs a worklist until the worklist is empty, which Rocq
   cannot see terminates on its own; so its [walk_predecessors] also stops when
   a list of "fuel" runs out, and [find_loops] passes the graph's block list as
   fuel. That is enough fuel: every pop of the worklist corresponds to a block
   added to the body, and a block is added at most once
   ([natural_loop_body_complete] proves that nothing is lost).

   The results, in part 4:

   - [find_loops_back_edges] and [find_loops_complete]: the back edges of the
     loops found are exactly the edges [source -> header] of the graph with
     [header] dominating [source], grouped by [header];
   - [find_loops_body]: a loop's body is its header plus every block from which
     a back-edge source can be reached without passing through the header,
     which is the definition of the natural loop (and in particular the body is
     the same whatever the order of the worklist);
   - [find_exit_branch_spec]: the exit branch found is the header's two-way
     [Switch], one way into the body and the other out of it.

   The trusted part is SsaModel.v: that [predecessors] and [all_successors] are
   the two ends of the same edges, that the block list given is the whole graph
   (which the theorems state as hypotheses), and that [dominates] is dominance
   in the graph (SsaDominance.v says what that means). *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlList.
From OxCamlSsa Require Import SsaModel SsaGraph SsaDominance.
Import ListNotations.

(* ======================================================================== *)
(* Part 1: the definitions                                                  *)
(* ======================================================================== *)

(* A natural loop: its header, its body (the set of its blocks, the header
   included) and the sources of its back edges (the blocks of the body with an
   edge to the header). *)
Record natural_loop : Type :=
  { header : block;
    body : blockset;
    back_edge_sources : list block }.

(* Add a block to the loop body, and to the worklist [added], when it is not in
   the body yet. *)
Definition add_new_body_block (acc : blockset * list block) (pred : block)
    : blockset * list block :=
  let '(body, added) := acc in
  if set_mem pred body then (body, added)
  else (set_add pred body, pred :: added).

(* The worklist walk of [natural_loop_body], with the fuel explained above. *)
Fixpoint walk_predecessors (fuel : list block) (body : blockset)
  (worklist : list block)
    : blockset :=
  match fuel, worklist with
  | [], _ | _, [] => body
  | _ :: fuel, block :: rest =>
    let '(body, added) :=
      OxCamlList.fold_left add_new_body_block (body, rest) (predecessors block)
    in
    walk_predecessors fuel body added
  end.

(* The body of the loop with header [header] and back-edge sources
   [back_edge_sources]: the header and every block from which a back-edge
   source can be reached without passing through the header. The order of the
   walk does not affect the body it computes. *)
Definition natural_loop_body (fuel : list block) (header : block)
    (back_edge_sources : list block) : blockset :=
  let '(body, seeds) :=
    OxCamlList.fold_left add_new_body_block (set_singleton header, [])
      back_edge_sources
  in
  walk_predecessors fuel body seeds.

(* Record the back edge [source -> header] in the association list from headers
   to their back-edge sources. *)
Fixpoint add_back_edge (header source : block)
    (table : list (block * list block)) : list (block * list block) :=
  match table with
  | [] => [(header, [source])]
  | (header', back_edge_sources) :: table =>
    if block_eqb header' header
    then (header', source :: back_edge_sources) :: table
    else (header', back_edge_sources) :: add_back_edge header source table
  end.

(* For each block [source] and each of its successors [succ], record the edge
   [source -> succ] if it is a back edge: one whose target dominates its
   source. *)
Definition record_back_edge (source : block)
    (table : list (block * list block)) (succ : block)
    : list (block * list block) :=
  if dominates succ source then add_back_edge succ source table else table.

(* Record every back edge leaving [source]. *)
Definition record_back_edges (table : list (block * list block))
    (source : block) : list (block * list block) :=
  OxCamlList.fold_left (record_back_edge source) table
    (all_successors source).

(* The natural loops of the graph, given its block list ([Ssa.blocks ssa]), in
   the order their headers are first met. *)
Definition find_loops (blocks : list block) : list natural_loop :=
  let header_table := OxCamlList.fold_left record_back_edges [] blocks in
  map
    (fun '(header, back_edge_sources) =>
      {| header := header;
         body := natural_loop_body blocks header back_edge_sources;
         back_edge_sources := back_edge_sources |})
    header_table.

(* The header's exit branch: the comparison it tests, which way continues the
   loop, and where the other way goes. *)
Record exit_branch : Type :=
  { condition : value;
    continue_when_true : bool;
    exit_target : block }.

(* The exit branch of a loop: the header's terminator must be a two-target
   [Switch], [ifnot] its false edge and [ifso] its true edge, one of them into
   the body and the other out of it. *)
Definition find_exit_branch (loop : natural_loop) : option exit_branch :=
  match terminator loop.(header) with
  | Terminator.Switch index [ifnot; ifso] =>
    match set_mem ifso loop.(body), set_mem ifnot loop.(body) with
    | true, false =>
      Some {| condition := index;
              continue_when_true := true;
              exit_target := ifnot |}
    | false, true =>
      Some {| condition := index;
              continue_when_true := false;
              exit_target := ifso |}
    | true, true | false, false => None
    end
  | _ => None
  end.

(* ======================================================================== *)
(* Part 2: the intended meaning                                             *)
(* ======================================================================== *)

(* [reaches_back_edge_without_header header back_edge_sources block]: from
   [block], one of [back_edge_sources] can be reached along the graph's edges
   without passing through [header], [block] itself not being [header]. The
   natural loop of the back edges [back_edge_sources -> header] is [header]
   plus these blocks. *)
Inductive reaches_back_edge_without_header (header : block)
  (back_edge_sources : list block)
    : block -> Prop :=
  | reaches_back_edge_source : ∀ (source : block),
      source ∈ back_edge_sources -> source <> header ->
      reaches_back_edge_without_header header back_edge_sources source
  | reaches_back_edge_predecessor : ∀ (block pred : SsaModel.block),
      reaches_back_edge_without_header header back_edge_sources block ->
      pred ∈ predecessors block ->
      pred <> header -> reaches_back_edge_without_header header
        back_edge_sources pred.

(* ======================================================================== *)
(* Part 3: lemmas used by the proofs                                        *)
(* ======================================================================== *)

(* --- The walk: soundness --- *)

Section Walk.
  Variable header : block.
  Variable back_edge_sources : list block.

  (* The loop body computed so far is sound: it holds the header, and every
     other block of it reaches a back-edge source without passing through the
     header. *)
  Definition body_sound (body : blockset) : Prop :=
    set_mem header body = true
    ∧ ∀ (block : block), set_mem block body = true ->
         block = header ∨ reaches_back_edge_without_header header
           back_edge_sources block.

  (* One round of [add_new_body_block] over [preds] keeps the loop body sound
     and the worklist made of blocks that reach a back-edge source, provided
     every block of [preds] other than the header reaches one. *)
  Lemma fold_add_new_body_block_sound :
    ∀ (preds : list block) (body : blockset) (added : list block),
    body_sound body -> Forall (reaches_back_edge_without_header header
      back_edge_sources) added ->
    (∀ (pred : block), pred ∈ preds -> pred <> header ->
       reaches_back_edge_without_header header back_edge_sources pred) ->
    body_sound (fst (List.fold_left add_new_body_block preds (body, added)))
    ∧ Forall (reaches_back_edge_without_header header back_edge_sources)
         (snd (List.fold_left add_new_body_block preds (body, added))).
  Proof.
    induction preds as [ | p preds IH ]; intros body added Hb Ha Hp.
    - split; assumption.
    - cbn [List.fold_left add_new_body_block]. destruct (set_mem p body)
      eqn:Hm.
      + apply IH; [ assumption | assumption | ].
        intros q Hq. apply Hp. right. exact Hq.
      + assert (Hph : p <> header).
        { intros ->. destruct Hb as [Hh _]. rewrite Hh in Hm. discriminate. }
        assert (Hrp : reaches_back_edge_without_header header back_edge_sources
          p)
          by (apply Hp; [ left; reflexivity | exact Hph ]).
        apply IH.
        * destruct Hb as [Hh Hb]. split.
          -- apply set_mem_add_iff. right. exact Hh.
          -- intros b Hin. apply set_mem_add_iff in Hin.
             destruct Hin as [ -> | Hin ];
               [ right; exact Hrp | apply Hb, Hin ].
        * constructor; assumption.
        * intros q Hq. apply Hp. right. exact Hq.
  Qed.

  (* The walk keeps the loop body sound when the worklist holds only blocks
     that reach a back-edge source. *)
  Lemma walk_predecessors_sound :
    ∀ (fuel : list block) (body : blockset) (worklist : list block),
    body_sound body -> Forall (reaches_back_edge_without_header header
      back_edge_sources) worklist ->
    body_sound (walk_predecessors fuel body worklist).
  Proof.
    induction fuel as [ | f fuel IH ]; intros body worklist Hb Hw;
      [ exact Hb | ].
    destruct worklist as [ | bl rest ]; [ exact Hb | ].
    cbn [walk_predecessors]. unfold OxCamlList.fold_left.
    inversion Hw as [ | ? ? Hbl Hrest ]; subst.
    pose proof (fold_add_new_body_block_sound (predecessors bl) body rest Hb
      Hrest
                  (fun p Hp Hne => reaches_back_edge_predecessor _ _ _ _ Hbl Hp
                    Hne)) as H.
    destruct (List.fold_left add_new_body_block (predecessors bl) (body, rest))
      as [body' worklist'].
    cbn [fst snd] in H. destruct H as [H1 H2]. apply IH; assumption.
  Qed.

  (* The loop body computed is sound, whatever the fuel. *)
  Lemma natural_loop_body_sound_aux : ∀ (fuel : list block),
    body_sound (natural_loop_body fuel header back_edge_sources).
  Proof.
    intros fuel. unfold natural_loop_body, OxCamlList.fold_left.
    assert (Hb0 : body_sound (set_singleton header)).
    { split; [ apply set_mem_singleton_iff; reflexivity | ].
      intros b Hb. left. apply set_mem_singleton_iff, Hb. }
    pose proof (fold_add_new_body_block_sound back_edge_sources (set_singleton
      header) []
                  Hb0 (Forall_nil _)
                  (fun s Hs Hne => reaches_back_edge_source _ _ _ Hs Hne)) as
                    Hf.
    destruct (List.fold_left add_new_body_block back_edge_sources
                (set_singleton header, []))
      as [body0 seeds].
    cbn [fst snd] in Hf. destruct Hf as [Hf1 Hf2].
    exact (walk_predecessors_sound fuel body0 seeds Hf1 Hf2).
  Qed.

  (* The header is in the loop body. *)
  Theorem natural_loop_body_header : ∀ (fuel : list block),
    set_mem header (natural_loop_body fuel header back_edge_sources) = true.
  Proof. intros fuel. exact (proj1 (natural_loop_body_sound_aux fuel)). Qed.

  (* Every block of the loop body is the header or reaches a back-edge source
     without passing through the header. *)
  Theorem natural_loop_body_sound : ∀ (fuel : list block) (block : block),
    set_mem block (natural_loop_body fuel header back_edge_sources) = true ->
    block = header ∨ reaches_back_edge_without_header header back_edge_sources
      block.
  Proof. intros fuel. exact (proj2 (natural_loop_body_sound_aux fuel)). Qed.

  (* --- The walk: completeness --- *)

  Variable blocks : list block.
  Hypothesis blocks_closed : ∀ (block pred : SsaModel.block),
    block ∈ blocks -> pred ∈ (predecessors block) -> pred ∈ blocks.

  (* The number of blocks of the graph not yet in the loop body: the measure
     that each pop of the worklist decreases. *)
  Definition blocks_outside_body_count (body : blockset) : nat :=
    length (filter (fun block => negb (set_mem block body)) blocks).

  (* [body_predecessors_closed_off_worklist body worklist]: every block of the
     loop body other than the header that is no longer on the worklist has all
     its predecessors in the body. *)
  Definition body_predecessors_closed_off_worklist (body : blockset)
    (worklist : list block) : Prop :=
    ∀ (succ : block),
      set_mem succ body = true -> succ <> header -> succ ∉ worklist ->
      ∀ (pred : block),
        pred ∈ (predecessors succ) -> set_mem pred body = true.

  (* Adding a block of the graph not yet in the loop body decreases
     [blocks_outside_body_count]. *)
  Lemma blocks_outside_body_count_add : ∀ (body : blockset)
    (block : block),
    block ∈ blocks -> set_mem block body = false ->
    blocks_outside_body_count (set_add block body) < blocks_outside_body_count
      body.
  Proof.
    intros body p Hin Hm. unfold blocks_outside_body_count.
    apply (filter_length_lt _ _ _ p).
    - intros y Hy. apply negb_true_iff in Hy. apply negb_true_iff.
      destruct (set_mem y body) eqn:E; [ | reflexivity ].
      rewrite <- Hy. symmetry. apply set_mem_add_iff. right. exact E.
    - exact Hin.
    - rewrite Hm. reflexivity.
    - apply negb_false_iff. apply set_mem_add_iff. left. reflexivity.
  Qed.

  (* [blocks_outside_body_count] is at most the number of blocks. *)
  Lemma blocks_outside_body_count_le_length : ∀ (body : blockset),
    blocks_outside_body_count body <= length blocks.
  Proof. intros body. apply length_filter_le. Qed.

  (* One round of [add_new_body_block] over [preds]: the loop body only grows
     and gains every block of [preds]; a block new to the body joins the
     worklist; the worklist only grows, and stays within the body if it was;
     the measure [length worklist + blocks_outside_body_count body] does not
     increase; and a block on the new worklist was on the old one or is one of
     [preds]. *)
  Lemma fold_add_new_body_block_spec :
    ∀ (preds : list block) (body : blockset) (added : list block)
      (body' : blockset) (added' : list block),
    List.fold_left add_new_body_block preds (body, added) = (body', added') ->
    (∀ (pred : block), pred ∈ preds -> pred ∈ blocks) ->
    (∀ (block : block), set_mem block body = true ->
       set_mem block body' = true)
    ∧ (∀ (pred : block), pred ∈ preds -> set_mem pred body' = true)
    ∧ (∀ (block : block), set_mem block body' = true ->
          set_mem block body = true ∨ block ∈ added')
    ∧ (∀ (block : block), block ∈ added -> block ∈ added')
    ∧ ((∀ (block : block),
           block ∈ added -> set_mem block body = true) ->
        ∀ (block : block), block ∈ added' -> set_mem block body' = true)
    ∧ length added' + blocks_outside_body_count body' <= length added +
      blocks_outside_body_count body
    ∧ (∀ (block : block), block ∈ added' ->
          block ∈ added ∨ block ∈ preds).
  Proof.
    induction preds as [ | p preds IH ]; intros body added body' added' H Hin.
    - cbn [List.fold_left] in H. injection H as <- <-.
      repeat split; try tauto; [ intros b [] | lia ].
    - cbn [List.fold_left add_new_body_block] in H. destruct (set_mem p body)
      eqn:Hm.
      + specialize (IH _ _ _ _ H (fun q Hq => Hin q (or_intror Hq))).
        destruct IH as [H1 [H2 [H3 [H4 [H5 [H6 H7]]]]]].
        repeat split; try assumption.
        * intros q [ -> | Hq ]; [ apply H1, Hm | apply H2, Hq ].
        * intros b Hb. destruct (H7 b Hb) as [Hb' | Hb'].
          -- left. exact Hb'.
          -- right. right. exact Hb'.
      + specialize (IH _ _ _ _ H (fun q Hq => Hin q (or_intror Hq))).
        destruct IH as [H1 [H2 [H3 [H4 [H5 [H6 H7]]]]]].
        assert (Hpb : p ∈ blocks) by (apply Hin; left; reflexivity).
        pose proof (blocks_outside_body_count_add body p Hpb Hm) as Habs.
        repeat split.
        * intros b Hb. apply H1. apply set_mem_add_iff. right. exact Hb.
        * intros q [ -> | Hq ].
          -- apply H1. apply set_mem_add_iff. left. reflexivity.
          -- apply H2, Hq.
        * intros b Hb. destruct (H3 b Hb) as [Hb' | Hb'].
          -- apply set_mem_add_iff in Hb'. destruct Hb' as [ -> | Hb' ].
             ++ right. apply H4. left. reflexivity.
             ++ left. exact Hb'.
          -- right. exact Hb'.
        * intros b Hb. apply H4. right. exact Hb.
        * intros Hadd b Hb. apply H5; [ | exact Hb ].
          intros q [ -> | Hq ].
          -- apply set_mem_add_iff. left. reflexivity.
          -- apply set_mem_add_iff. right. apply Hadd, Hq.
        * cbn [length] in H6. lia.
        * intros b Hb. destruct (H7 b Hb) as [ [ -> | Hb' ] | Hb' ].
          -- right. left. reflexivity.
          -- left. exact Hb'.
          -- right. right. exact Hb'.
  Qed.

  (* With fuel for [length worklist + blocks_outside_body_count body] pops, the
     walk keeps every block of the loop body and ends with a body closed under
     predecessors, except at the header. *)
  Lemma walk_predecessors_complete :
    ∀ (fuel : list block) (body : blockset) (worklist : list block),
    (∀ (block : block), block ∈ worklist -> set_mem block body = true) ->
    (∀ (block : block), set_mem block body = true -> block ∈ blocks) ->
    body_predecessors_closed_off_worklist body worklist ->
    length worklist + blocks_outside_body_count body <= length fuel ->
    (∀ (block : block), set_mem block body = true ->
       set_mem block (walk_predecessors fuel body worklist) = true)
    ∧ body_predecessors_closed_off_worklist (walk_predecessors fuel body
      worklist) [].
  Proof.
    induction fuel as [ | f fuel IH ]; intros body worklist Hwl Hbl Hcl Hn.
    - destruct worklist as [ | bl rest ].
      + cbn [walk_predecessors]. split; [ tauto | exact Hcl ].
      + cbn [length] in Hn. lia.
    - destruct worklist as [ | bl rest ].
      + cbn [walk_predecessors]. split; [ tauto | exact Hcl ].
      + cbn [walk_predecessors]. unfold OxCamlList.fold_left.
        destruct (List.fold_left add_new_body_block (predecessors bl)
          (body, rest))
          as [body1 added1] eqn:Hf.
        assert (Hpreds : forall p, p ∈ (predecessors bl) -> p ∈ blocks).
        { intros p Hp. apply (blocks_closed bl); [ | exact Hp ].
          apply Hbl, Hwl. left. reflexivity. }
        destruct (fold_add_new_body_block_spec _ _ _ _ _ Hf Hpreds)
          as [H1 [H2 [H3 [H4 [H5 [H6 H7]]]]]].
        assert (Hwl1 : forall b, b ∈ added1 -> set_mem b body1 = true).
        { apply H5. intros b Hb. apply Hwl. right. exact Hb. }
        assert (Hbl1 : forall b, set_mem b body1 = true -> b ∈ blocks).
        { intros b Hb. destruct (H3 b Hb) as [Hb' | Hb'].
          - apply Hbl, Hb'.
          - destruct (H7 b Hb') as [Hr | Hp].
            + apply Hbl, Hwl. right. exact Hr.
            + apply Hpreds, Hp. }
        assert (Hcl1 : body_predecessors_closed_off_worklist body1 added1).
        { intros b Hb Hbh Hnin p Hp.
          destruct (H3 b Hb) as [Hb' | Hb']; [ | contradiction ].
          destruct (block_eqb b bl) eqn:E.
          - apply block_eqb_spec in E. subst b. apply H2, Hp.
          - apply H1. apply (Hcl b Hb' Hbh); [ | exact Hp ].
            intros [ -> | Hr ].
            + rewrite block_eqb_refl in E. discriminate.
            + apply Hnin, H4, Hr. }
        assert (Hn1 : length added1 + blocks_outside_body_count body1 <= length
          fuel).
        { cbn [length] in Hn. lia. }
        destruct (IH body1 added1 Hwl1 Hbl1 Hcl1 Hn1) as [IH1 IH2].
        split; [ | exact IH2 ].
        intros b Hb. apply IH1, H1, Hb.
  Qed.

  (* With the graph's blocks as fuel, the loop body holds every back-edge
     source and is closed under predecessors, except at the header. *)
  Lemma natural_loop_body_complete_aux :
    header ∈ blocks ->
    (∀ (source : block),
       source ∈ back_edge_sources -> source ∈ blocks) ->
    (∀ (source : block), source ∈ back_edge_sources ->
       set_mem source (natural_loop_body blocks header back_edge_sources)
       = true)
    ∧ body_predecessors_closed_off_worklist (natural_loop_body blocks header
      back_edge_sources) [].
  Proof.
    intros Hh Hs. unfold natural_loop_body, OxCamlList.fold_left.
    destruct (List.fold_left add_new_body_block back_edge_sources
                (set_singleton header, []))
      as [body0 seeds] eqn:Hf.
    destruct (fold_add_new_body_block_spec _ _ _ _ _ Hf Hs)
      as [H1 [H2 [H3 [H4 [H5 [H6 H7]]]]]].
    assert (Hwl : forall b, b ∈ seeds -> set_mem b body0 = true).
    { apply H5. intros b []. }
    assert (Hbl : forall b, set_mem b body0 = true -> b ∈ blocks).
    { intros b Hb. destruct (H3 b Hb) as [Hb' | Hb'].
      - apply set_mem_singleton_iff in Hb'. subst b. exact Hh.
      - destruct (H7 b Hb') as [ [] | Hb'' ]. apply Hs, Hb''. }
    assert (Hcl : body_predecessors_closed_off_worklist body0 seeds).
    { intros b Hb Hbh Hnin p Hp. exfalso.
      destruct (H3 b Hb) as [Hb' | Hb'].
      - apply set_mem_singleton_iff in Hb'. exact (Hbh Hb').
      - exact (Hnin Hb'). }
    assert (Hn : length seeds + blocks_outside_body_count body0 <= length
      blocks).
    { cbn [length] in H6.
      pose proof (blocks_outside_body_count_le_length (set_singleton header)).
        lia. }
    destruct (walk_predecessors_complete blocks body0 seeds Hwl Hbl Hcl Hn) as
      [W1 W2].
    split; [ | exact W2 ].
    intros s Hs'. apply W1, H2, Hs'.
  Qed.

  (* A block that reaches a back-edge source without passing through the header
     is not the header. *)
  Lemma reaches_back_edge_not_header : ∀ (block : block),
    reaches_back_edge_without_header header back_edge_sources block ->
    block <> header.
  Proof. intros b H. destruct H; assumption. Qed.

  (* Every block that reaches a back-edge source without passing through the
     header is in the loop body. *)
  Theorem natural_loop_body_complete :
    header ∈ blocks ->
    (∀ (source : block),
       source ∈ back_edge_sources -> source ∈ blocks) ->
    ∀ (block : block),
    reaches_back_edge_without_header header back_edge_sources block ->
    set_mem block (natural_loop_body blocks header back_edge_sources) = true.
  Proof.
    intros Hh Hs b Hr.
    destruct (natural_loop_body_complete_aux Hh Hs) as [Hsrc Hcl].
    induction Hr as [ s Hin Hne | b p Hr IH Hp Hne ].
    - apply Hsrc, Hin.
    - apply (Hcl b IH (reaches_back_edge_not_header b Hr)); [ intros [] | exact
      Hp ].
  Qed.
End Walk.

(* --- [find_loops] --- *)

Section FindLoops.
  Variable blocks : list block.
  Hypothesis blocks_closed : ∀ (block pred : SsaModel.block),
    block ∈ blocks -> pred ∈ (predecessors block) -> pred ∈ blocks.
  Hypothesis succs_closed : ∀ (block succ : SsaModel.block),
    block ∈ blocks -> succ ∈ (all_successors block) -> succ ∈ blocks.

  (* The invariant of the back-edge table: every entry [(header,
     back_edge_sources)] is non-empty and records edges [source -> header] of
     the graph with [header] dominating [source]. *)
  Definition back_edge_table_sound (table : list (block * list block))
      : Prop :=
    ∀ (header : block) (back_edge_sources : list block),
      (header, back_edge_sources) ∈ table ->
      back_edge_sources <> []
      ∧ ∀ (source : block), source ∈ back_edge_sources ->
           source ∈ blocks ∧ header ∈ (all_successors source)
           ∧ header ▷ source.

  (* The table records [source] as a back-edge source of [header]. *)
  Definition back_edge_table_contains (table : list (block * list block))
      (header source : block) : Prop :=
    ∃ back_edge_sources,
      (header, back_edge_sources) ∈ table ∧ source ∈ back_edge_sources.

  (* The entries after [add_back_edge header source]: a new entry [(header,
     [source])], the entry of [header] with [source] added, or an entry left
     alone. *)
  Lemma add_back_edge_entries :
    ∀ (header source : block) (table : list (block * list block))
      (header' : block) (back_edge_sources' : list block),
    (header', back_edge_sources') ∈ (add_back_edge header source table) ->
    (header' = header ∧ back_edge_sources' = [source])
    ∨ (header' = header
        ∧ ∃ back_edge_sources,
             (header, back_edge_sources) ∈ table
             ∧ back_edge_sources' = source :: back_edge_sources)
    ∨ (header', back_edge_sources') ∈ table.
  Proof.
    intros h s tbl. induction tbl as [ | [h0 srcs0] tbl IH ];
      intros h' srcs' Hin.
    - destruct Hin as [E | [] ]. injection E as <- <-. left. auto.
    - cbn in Hin. destruct (block_eqb h0 h) eqn:E.
      + apply block_eqb_spec in E. subst h0. destruct Hin as [E' | Hin].
        * injection E' as <- <-. right. left. split; [ reflexivity | ].
          exists srcs0. split; [ left; reflexivity | reflexivity ].
        * right. right. right. exact Hin.
      + destruct Hin as [E' | Hin].
        * injection E' as <- <-. right. right. left. reflexivity.
        * destruct (IH h' srcs' Hin) as [H | [ [Hh [srcs [H1 H2]]] | H ] ].
          -- left. exact H.
          -- right. left. split; [ exact Hh | ].
             exists srcs. split; [ right; exact H1 | exact H2 ].
          -- right. right. right. exact H.
  Qed.

  (* After [add_back_edge header source] the table has [source] as a back-edge
     source of [header] ... *)
  Lemma add_back_edge_has_new :
    ∀ (header source : block) (table : list (block * list block)),
    back_edge_table_contains (add_back_edge header source table) header source.
  Proof.
    intros h s tbl. induction tbl as [ | [h0 srcs0] tbl IH ].
    - exists [s]. split; left; reflexivity.
    - cbn. destruct (block_eqb h0 h) eqn:E.
      + apply block_eqb_spec in E. subst h0.
        exists (s :: srcs0). split; left; reflexivity.
      + destruct IH as [srcs [H1 H2]].
        exists srcs. split; [ right; exact H1 | exact H2 ].
  Qed.

  (* ... and keeps every back-edge source it had. *)
  Lemma add_back_edge_has_old :
    ∀ (header source : block) (table : list (block * list block))
      (header' source' : block),
    back_edge_table_contains table header' source' ->
    back_edge_table_contains (add_back_edge header source table) header'
      source'.
  Proof.
    intros h s tbl. induction tbl as [ | [h0 srcs0] tbl IH ];
      intros h' s' [srcs [Hin Hs]].
    - destruct Hin.
    - cbn. destruct (block_eqb h0 h) eqn:E.
      + destruct Hin as [E' | Hin].
        * injection E' as -> ->. exists (s :: srcs).
          split; [ left; reflexivity | right; exact Hs ].
        * exists srcs. split; [ right; exact Hin | exact Hs ].
      + destruct Hin as [E' | Hin].
        * injection E' as -> ->. exists srcs.
          split; [ left; reflexivity | exact Hs ].
        * destruct (IH h' s' (ex_intro _ srcs (conj Hin Hs)))
            as [srcs' [H1 H2]].
          exists srcs'. split; [ right; exact H1 | exact H2 ].
  Qed.

  (* Adding a back edge introduces no header other than the one being recorded.
     This is used to preserve uniqueness of table keys. *)
  Lemma add_back_edge_keys : ∀ (loop_header source : block)
      (table : list (block * list block)) (key : block),
    key ∈ (map fst (add_back_edge loop_header source table)) ->
    key = loop_header ∨ key ∈ (map fst table).
  Proof.
    intros h s table h' Hin. apply in_map_iff in Hin.
    destruct Hin as [[key sources] [E Hin]]. cbn in E. subst key.
    destruct (add_back_edge_entries _ _ _ _ _ Hin)
      as [[H _] | [[H _] | H]]; try (left; exact H).
    right. apply in_map_iff. exists (h', sources). auto.
  Qed.

  (* Each header still has one table entry after recording a back edge. *)
  Lemma add_back_edge_unique : ∀ (loop_header source : block)
      (table : list (block * list block)),
    NoDup (map fst table) ->
    NoDup (map fst (add_back_edge loop_header source table)).
  Proof.
    intros h s table. induction table as [ | [key sources] table IH ];
      intros Hunique; cbn [add_back_edge map fst].
    - constructor; [cbn; tauto | constructor].
    - inversion Hunique as [ | ? ? Hnot Htail ]; subst.
      destruct (block_eqb key h) eqn:E; cbn.
      + constructor; assumption.
      + constructor; [ | apply IH; exact Htail].
        intros Hin. apply add_back_edge_keys in Hin.
        destruct Hin as [Heq | Hin]; [ | contradiction].
        subst key. rewrite block_eqb_refl in E. discriminate.
  Qed.

  (* Recording successors and then blocks preserves unique header keys. *)
  Lemma record_back_edges_unique : ∀ (source : block)
      (table : list (block * list block)),
    NoDup (map fst table) ->
    NoDup (map fst (record_back_edges table source)).
  Proof.
    intros source table Hunique.
    unfold record_back_edges, OxCamlList.fold_left.
    generalize dependent table. induction (all_successors source)
      as [ | target targets IH ]; intros table Hunique; cbn.
    - exact Hunique.
    - apply IH. unfold record_back_edge.
      destruct (dominates target source); [apply add_back_edge_unique |];
        exact Hunique.
  Qed.

  (* The complete discovery table contains each header at most once. *)
  Lemma header_table_unique :
    NoDup (map fst (List.fold_left record_back_edges blocks [])).
  Proof.
    assert (forall bs table, NoDup (map fst table) ->
      NoDup (map fst (List.fold_left record_back_edges bs table))) as Hfold.
    { intros bs. induction bs as [ | b bs IH ]; intros table Hunique; cbn.
      - exact Hunique.
      - apply IH, record_back_edges_unique. exact Hunique. }
    apply Hfold. constructor.
  Qed.

  (* A table with distinct keys has one value for each key. *)
  Lemma unique_header_sources :
    ∀ (table : list (block * list block)) (loop_header : block)
      (sources sources' : list block),
    NoDup (map fst table) ->
    (loop_header, sources) ∈ table ->
    (loop_header, sources') ∈ table -> sources = sources'.
  Proof.
    intros table. induction table as [ | [key srcs] table IH ];
      intros h sources sources' Hunique Hin Hin'; [contradiction |].
    inversion Hunique as [ | ? ? Hnot Htail ]; subst.
    destruct Hin as [E | Hin], Hin' as [E' | Hin'].
    - congruence.
    - injection E as -> ->. exfalso. apply Hnot.
      apply in_map_iff. exists (h, sources'). auto.
    - injection E' as -> ->. exfalso. apply Hnot.
      apply in_map_iff. exists (h, sources). auto.
    - eapply IH; eassumption.
  Qed.

  (* [record_back_edge], unfolded. *)
  Lemma record_back_edge_eq :
    ∀ (source : block) (table : list (block * list block)) (succ : block),
    record_back_edge source table succ
    = if dominates succ source then add_back_edge succ source table else table.
  Proof. reflexivity. Qed.

  (* Recording the back edges among the successors [succs] of [source]: the
     table stays sound, keeps what it had, and gains each successor that
     dominates [source]. *)
  Lemma record_successor_back_edges_spec :
    ∀ (source : block) (succs : list block)
      (table : list (block * list block)),
    source ∈ blocks ->
    (∀ (succ : block),
       succ ∈ succs -> succ ∈ (all_successors source)) ->
    back_edge_table_sound table ->
    let table' := List.fold_left (record_back_edge source) succs table in
    back_edge_table_sound table'
    ∧ (∀ (header source' : block),
          back_edge_table_contains table header source' ->
            back_edge_table_contains table' header source')
    ∧ (∀ (succ : block), succ ∈ succs -> succ ▷ source ->
          back_edge_table_contains table' succ source).
  Proof.
    intros bl succs. induction succs as [ | h succs IH ];
      intros tbl Hbl Hsucc Hs; cbv zeta.
    - cbn [List.fold_left]. split; [ exact Hs | split; [ tauto | ] ].
      intros h [].
    - cbn [List.fold_left]. rewrite record_back_edge_eq.
      assert (Hsucc' : forall s, s ∈ succs -> s ∈ (all_successors bl))
        by (intros s Hs'; apply Hsucc; right; exact Hs').
      destruct (dominates h bl) eqn:Hd.
      + assert (Hs1 : back_edge_table_sound (add_back_edge h bl tbl)).
        { intros h' srcs' Hin.
          destruct (add_back_edge_entries _ _ _ _ _ Hin)
            as [ [-> ->] | [ [-> [srcs [Hin' ->]]] | Hin' ] ].
          - split; [ discriminate | ].
            intros s [ -> | [] ].
            split; [ exact Hbl | split; [ | exact Hd ] ].
            apply Hsucc. left. reflexivity.
          - destruct (Hs h srcs Hin') as [_ Hall].
            split; [ discriminate | ].
            intros s [ -> | Hs' ].
            + split; [ exact Hbl | split; [ | exact Hd ] ].
              apply Hsucc. left. reflexivity.
            + apply Hall, Hs'.
          - apply Hs, Hin'. }
        destruct (IH _ Hbl Hsucc' Hs1) as [S1 [S2 S3]].
        split; [ exact S1 | split ].
        * intros h' s H. apply S2, add_back_edge_has_old, H.
        * intros h' [ -> | Hh ] Hd'.
          -- apply S2, add_back_edge_has_new.
          -- apply S3; assumption.
      + destruct (IH _ Hbl Hsucc' Hs) as [S1 [S2 S3]].
        split; [ exact S1 | split; [ exact S2 | ] ].
        intros h' [ -> | Hh ] Hd'.
        * rewrite Hd in Hd'. discriminate.
        * apply S3; assumption.
  Qed.

  (* Recording the back edges leaving each block of [blocks']: the table stays
     sound, keeps what it had, and gains every back edge leaving one of them.
     *)
  Lemma record_block_back_edges_spec :
    ∀ (blocks' : list block) (table : list (block * list block)),
    (∀ (block : block), block ∈ blocks' -> block ∈ blocks) ->
    back_edge_table_sound table ->
    let table' := List.fold_left record_back_edges blocks' table in
    back_edge_table_sound table'
    ∧ (∀ (header source : block),
          back_edge_table_contains table header source ->
            back_edge_table_contains table' header source)
    ∧ (∀ (header source : block),
          source ∈ blocks' -> header ∈ (all_successors source) ->
          header ▷ source -> back_edge_table_contains table' header
            source).
  Proof.
    induction blocks' as [ | bl bls IH ]; intros tbl Hin Hs; cbv zeta.
    - cbn [List.fold_left]. split; [ exact Hs | split; [ tauto | ] ].
      intros h s [].
    - cbn [List.fold_left].
      change (record_back_edges tbl bl)
        with (List.fold_left (record_back_edge bl) (all_successors bl) tbl).
      destruct (record_successor_back_edges_spec bl (all_successors bl) tbl
                  (Hin bl (or_introl eq_refl)) (fun s Hs' => Hs') Hs)
        as [S1 [S2 S3]].
      destruct (IH _ (fun b Hb => Hin b (or_intror Hb)) S1) as [T1 [T2 T3]].
      split; [ exact T1 | split ].
      + intros h s H. apply T2, S2, H.
      + intros h s [ -> | Hbs ] Hh Hd.
        * apply T2. apply S3; assumption.
        * apply T3; assumption.
  Qed.

  (* The table [find_loops] builds is sound and holds every back edge of the
     graph. *)
  Lemma header_table_spec :
    let table := List.fold_left record_back_edges blocks [] in
    back_edge_table_sound table
    ∧ (∀ (header source : block),
          source ∈ blocks -> header ∈ (all_successors source) ->
          header ▷ source -> back_edge_table_contains table header
            source).
  Proof.
    cbv zeta.
    destruct (record_block_back_edges_spec blocks [] (fun b Hb => Hb)
                ltac:(intros h srcs [])) as [S1 [_ S3]].
    split; assumption.
  Qed.

  (* The loops found are exactly the entries of the table, each with its body
     computed. *)
  Lemma find_loops_In : ∀ (loop : natural_loop),
    loop ∈ (find_loops blocks) <->
    ∃ header back_edge_sources,
      (header, back_edge_sources) ∈ (List.fold_left record_back_edges blocks [])
      ∧ loop = {| header := header;
                   body := natural_loop_body blocks header back_edge_sources;
                   back_edge_sources := back_edge_sources |}.
  Proof.
    intros l. unfold find_loops, OxCamlList.fold_left. cbv zeta.
    rewrite in_map_iff. split.
    - intros [ [h srcs] [<- Hin] ]. exists h, srcs. split; [ exact Hin | ].
      reflexivity.
    - intros [h [srcs [Hin ->]]]. exists (h, srcs). split; [ reflexivity | ].
      exact Hin.
  Qed.

  (* Every back edge of a loop found is an edge of the graph whose target, the
     header, dominates its source. *)
  Theorem find_loops_back_edges :
    ∀ (loop : natural_loop) (source : block),
    loop ∈ (find_loops blocks) -> source ∈ loop.(back_edge_sources) ->
    source ∈ blocks ∧ loop.(header) ∈ (all_successors source)
    ∧ loop.(header) ▷ source.
  Proof.
    intros l s Hl Hs. apply find_loops_In in Hl.
    destruct Hl as [h [srcs [Hin ->]]]. cbn [header back_edge_sources] in *.
    destruct header_table_spec as [S1 _].
    exact (proj2 (S1 h srcs Hin) s Hs).
  Qed.

  (* Every edge whose target dominates its source is a back edge of a loop
     found, with that target as header. *)
  Theorem find_loops_complete : ∀ (source header : block),
    source ∈ blocks -> header ∈ (all_successors source) ->
    header ▷ source ->
    ∃ loop,
      loop ∈ (find_loops blocks) ∧ loop.(NaturalLoop.header) = header
      ∧ source ∈ loop.(back_edge_sources).
  Proof.
    intros s h Hs Hh Hd.
    destruct header_table_spec as [_ S3].
    destruct (S3 h s Hs Hh Hd) as [srcs [Hin Hsrc]].
    exists {| header := h;
              body := natural_loop_body blocks h srcs;
              back_edge_sources := srcs |}.
    split; [ | split; [ reflexivity | exact Hsrc ] ].
    apply find_loops_In. exists h, srcs. split; [ exact Hin | reflexivity ].
  Qed.

  (* There is one natural-loop record for a header, not merely some loop with
     that header. Back-edge completeness can therefore be applied to the
     particular record returned to the deletion analysis. *)
  Theorem find_loops_unique_header : ∀ (loop other_loop : natural_loop),
    loop ∈ (find_loops blocks) -> other_loop ∈ (find_loops blocks) ->
    loop.(header) = other_loop.(header) -> loop = other_loop.
  Proof.
    intros l l' Hl Hl' Heq.
    apply find_loops_In in Hl, Hl'.
    destruct Hl as [h [sources [Hin ->]]].
    destruct Hl' as [h' [sources' [Hin' ->]]]. cbn in Heq. subst h'.
    assert (sources = sources') by
      (eapply unique_header_sources; eauto using header_table_unique).
    subst sources'. reflexivity.
  Qed.

  (* Every back edge to a found header belongs to that loop's source list. *)
  Theorem find_loops_back_edges_complete :
    ∀ (loop : natural_loop) (source : block),
    loop ∈ (find_loops blocks) -> source ∈ blocks ->
    loop.(header) ∈ (all_successors source) ->
    loop.(header) ▷ source ->
    source ∈ loop.(back_edge_sources).
  Proof.
    intros l source Hl Hsource Hedge Hdom.
    destruct (find_loops_complete source l.(header) Hsource Hedge Hdom)
      as [l' [Hl' [Hheader Hback]]].
    assert (l' = l) by (eapply find_loops_unique_header; eassumption).
    subst l'. exact Hback.
  Qed.

  (* A found loop's header and body blocks belong to the supplied graph. The
     closure assumptions are graph observations, not execution laws. *)
  Theorem find_loops_blocks : ∀ (loop : natural_loop),
    loop ∈ (find_loops blocks) ->
    loop.(header) ∈ blocks ∧
    ∀ (body_block : block),
      set_mem body_block loop.(body) = true -> body_block ∈ blocks.
  Proof.
    intros l Hl. pose proof Hl as Hrecord. apply find_loops_In in Hrecord.
    destruct Hrecord as [h [sources [Hin Heq]]]. subst l. cbn.
    destruct header_table_spec as [Hsound _].
    destruct (Hsound h sources Hin) as [Hnonempty Hsources].
    assert (Hheader : h ∈ blocks).
    { destruct sources as [ | source sources]; [contradiction |].
      destruct (Hsources source (or_introl eq_refl)) as [Hs [Hedge _]].
      eapply succs_closed; eassumption. }
    split; [exact Hheader |]. intros b Hb.
    destruct (natural_loop_body_sound h sources blocks b Hb)
      as [-> | Hreach]; [exact Hheader |].
    clear Hb. induction Hreach.
    - apply Hsources. exact H.
    - eapply blocks_closed; eassumption.
  Qed.

  (* The body of a loop found is its header plus the blocks that reach a
     back-edge source without passing through the header. *)
  Theorem find_loops_body : ∀ (loop : natural_loop) (block : block),
    loop ∈ (find_loops blocks) ->
    (set_mem block loop.(body) = true
     <-> block = loop.(header)
         ∨ reaches_back_edge_without_header loop.(header)
           loop.(back_edge_sources) block).
  Proof.
    intros l b Hl. apply find_loops_In in Hl.
    destruct Hl as [h [srcs [Hin ->]]]. cbn [header body back_edge_sources].
    destruct header_table_spec as [S1 _].
    destruct (S1 h srcs Hin) as [Hne Hall].
    split.
    - apply natural_loop_body_sound.
    - intros [ -> | Hr ].
      + apply natural_loop_body_header.
      + apply (natural_loop_body_complete h srcs blocks blocks_closed);
          [ | | exact Hr ].
        * destruct srcs as [ | s srcs' ]; [ contradiction | ].
          destruct (Hall s (or_introl eq_refl)) as [Hsb [Hhs _]].
          exact (succs_closed s h Hsb Hhs).
        * intros s Hs. exact (proj1 (Hall s Hs)).
  Qed.
End FindLoops.

(* The semantic domination of a back-edge source extends backwards to every
   block reaching it without passing through the header. *)
Lemma reaches_back_edge_dominated : ∀ (entry loop_header : block)
    (sources : list block),
  dominance_semantics entry ->
  (∀ (source : block),
    source ∈ sources -> loop_header ▷ source) ->
  ∀ (body_block : block),
  reaches_back_edge_without_header loop_header sources body_block ->
  ∀ (visited_blocks : list block),
  block_path_from entry visited_blocks body_block ->
  loop_header ∈ visited_blocks.
Proof.
  intros entry h sources Hdom Hsources b Hreach.
  induction Hreach as [source Hin Hne | b pred Hr IH Hedge Hne];
    intros p Hp.
  - eapply dominates_spec; [exact Hdom | apply Hsources; exact Hin | exact Hp].
  - pose proof (IH _ (block_path_snoc _ _ _ _ Hp Hedge)) as Hin.
    apply in_app_or in Hin. destruct Hin as [Hin | [Heq | []]];
      [exact Hin |].
    exfalso. apply (reaches_back_edge_not_header h sources b Hr). exact Heq.
Qed.

(* Every path into a found loop body passes through its header. This connects
   the reverse CFG walk to semantic dominance, without assuming that property
   of the analysis's result. *)
Theorem find_loops_body_dominated : ∀ (blocks : list block)
    (entry : block) (loop : natural_loop),
  dominance_semantics entry -> loop ∈ (find_loops blocks) ->
  ∀ (body_block : block), set_mem body_block loop.(body) = true ->
  ∀ (visited_blocks : list block),
  block_path_from entry visited_blocks body_block ->
  loop.(header) ∈ visited_blocks.
Proof.
  intros blocks entry l Hdom Hl b Hb p Hp.
  pose proof Hl as Hrecord. apply find_loops_In in Hrecord.
  destruct Hrecord as [h [sources [Hin Heq]]]. subst l. cbn in *.
  destruct (natural_loop_body_sound h sources blocks b Hb)
    as [-> | Hreach]; [eapply block_path_last; exact Hp |].
  eapply reaches_back_edge_dominated; [exact Hdom | | exact Hreach | exact Hp].
  intros source Hsource.
  exact (proj2 (proj2 (find_loops_back_edges blocks _ source Hl Hsource))).
Qed.

(* --- [find_exit_branch] --- *)

(* The reported exit target is an actual Switch target outside the body. This
   identifies the target, not merely the orientation of the test. *)
Theorem find_exit_branch_target : ∀ (loop : natural_loop)
    (exit_info : exit_branch),
  find_exit_branch loop = Some exit_info ->
  set_mem exit_info.(exit_target) loop.(body) = false ∧
  ∃ (targets : list block),
    terminator loop.(header) = Terminator.Switch exit_info.(condition) targets
    ∧ exit_info.(exit_target) ∈ targets.
Proof.
  intros loop exit_info Hfound. unfold find_exit_branch in Hfound.
  destruct (terminator loop.(header)) as [ | index targets | |] eqn:Hterm;
    try discriminate.
  destruct targets as [ | ifnot [ | ifso [ | extra rest]]]; try discriminate.
  destruct (set_mem ifso loop.(body)) eqn:Hso;
    destruct (set_mem ifnot loop.(body)) eqn:Hnot; try discriminate;
    injection Hfound as <-; cbn; split; try assumption.
  all: exists [ifnot; ifso]; split; [reflexivity | cbn; auto].
Qed.

(* The exit branch found is the header's two-way [Switch] on [condition]; the
   loop continues along the edge [continue_when_true] names, which is into the
   body, and the other edge leaves it. *)
Theorem find_exit_branch_spec :
  ∀ (loop : natural_loop) (exit_info : exit_branch),
  find_exit_branch loop = Some exit_info ->
  ∃ ifnot ifso,
    terminator loop.(header)
    = Terminator.Switch exit_info.(condition) [ifnot; ifso]
    ∧ set_mem ifso loop.(body) = exit_info.(continue_when_true)
    ∧ set_mem ifnot loop.(body) = negb exit_info.(continue_when_true).
Proof.
  intros l eb H. unfold find_exit_branch in H.
  destruct (terminator l.(header)) as [ | index targets | | ] eqn:Ht;
    try discriminate.
  destruct targets as [ | ifnot [ | ifso [ | ] ] ]; try discriminate.
  exists ifnot, ifso.
  destruct (set_mem ifso l.(body)) eqn:Hso;
    destruct (set_mem ifnot l.(body)) eqn:Hnot; try discriminate;
    injection H as <-; cbn; repeat split.
Qed.
