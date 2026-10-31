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

(* Finite traversals of a certified loop in SsaSemantics.step.

   [region_iteration] uses CFG accessibility and instruction-list induction to
   reach the next header entry or an exit. [region_steps] includes only the
   first exiting edge, so a segment leaving the region is a [steps_until] its
   exit block ([region_steps_until]). Graph observer contracts are
   [control_edges] and [parameter_layout]; no arithmetic or loop invariant is
   assumed. Nothing here is part of the final statement. *)

From Stdlib Require Import ZArith Lia List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlList.
From OxCamlSsa Require Import SsaModel SsaGraph SsaDominance SsaSemantics
  SsaExecution SsaProgress SsaContext.
From OxCamlLoop Require Import NaturalLoop InductionVar DeleteEmptyLoops.
Import ListNotations.
Local Open Scope nat_scope.

(* A finite region segment may leave the region only on its final step.
   Requiring only its source states to be inside includes the exiting edge. *)
Inductive region_steps (region : blockset) : state -> state -> Prop :=
  | Region_refl : ∀ (s : state), region_steps region s s
  | Region_cons : ∀ (before middle after : state),
      set_mem (current_block before) region = true ->
      SsaSemantics.step before middle -> region_steps region middle after ->
      region_steps region before after.

(* Region segments are executions in the same original graph. *)
Lemma region_steps_execution : ∀ (region : blockset)
    (before after : state),
  region_steps region before after -> steps before after.
Proof.
  intros region before after Hregion. induction Hregion.
  - constructor.
  - eapply Steps_cons; eassumption.
Qed.

(* A segment whose final state is outside the region first reaches that state's
   block there: every earlier source state is inside the region. *)
Lemma region_steps_until : ∀ (region : blockset) (before after : state),
  region_steps region before after ->
  set_mem (current_block after) region = false ->
  steps_until (current_block after) before after.
Proof.
  intros region before after Hregion Houtside.
  induction Hregion as [s | before middle after Hinside Hstep Hrest IH].
  - apply Until_here. reflexivity.
  - eapply Until_step; [ | exact Hstep | apply IH; exact Houtside].
    intros Heq. rewrite Heq, Houtside in Hinside. discriminate.
Qed.

(* Region segments compose when the second segment can start: its own
   source-state conditions prevent continuing after an earlier exit. *)
Lemma region_steps_trans : ∀ (region : blockset)
    (before middle after : state),
  region_steps region before middle -> region_steps region middle after ->
  region_steps region before after.
Proof.
  intros region before middle after Hprefix.
  induction Hprefix as [s | before middle last Hinside Hstep Hrest IH];
    intros Hsuffix; [exact Hsuffix |].
  eapply Region_cons; [exact Hinside | exact Hstep | apply IH; exact Hsuffix].
Qed.

(* Executing a block body inside the region remains in that region and ends
   before taking its terminator. *)
Lemma executes_insns_region : ∀ (region : blockset) (b : block)
    (insns : list instr) (before after : registers),
  set_mem b region = true -> executes_insns before insns after ->
  region_steps region
    {| current_block := b; remaining := insns; current_registers := before |}
    {| current_block := b; remaining := []; current_registers := after |}.
Proof.
  intros region b insns before after Hinside Hbody. induction Hbody.
  - constructor.
  - eapply Region_cons; [exact Hinside | constructor; exact H | exact IHHbody].
Qed.

(* A finite exit must first execute the current block's remaining body.
   Extracting this prefix needs no separate progress assumption. *)
Lemma region_exit_body_prefix : ∀ (region : blockset) (b : block)
    (insns : list instr) (before : registers) (after : state),
  set_mem b region = true -> set_mem (current_block after) region = false ->
  region_steps region
    {| current_block := b; remaining := insns; current_registers := before |}
    after ->
  ∃ (body_regs : registers), executes_insns before insns body_regs ∧
    region_steps region
      {| current_block := b; remaining := []; current_registers := body_regs |}
      after.
Proof.
  intros region b insns. induction insns as [ | insn rest IH];
    intros before after Hinside Houtside Hsegment.
  - exists before. split; [constructor | exact Hsegment].
  - inversion Hsegment as [s | s middle last Hsource Hstep Hrest]; subst.
    + cbn in Houtside. congruence.
    + inversion Hstep as [source current suffix regs regs' Hexec |
        source target args regs values regs' Hterm Hread Hbind |
        source index targets regs word target Hterm Hread Hnonneg Hnth
          Hparams];
        subst.
      destruct (IH regs' after Hinside Houtside Hrest)
        as [body_regs [Hbody Hsuffix]].
      exists body_regs. split; [econstructor; eassumption | exact Hsuffix].
Qed.

(* Outside registers remain unchanged while inside the region and across an
   exit into a parameterless block, which writes no register. *)
Theorem region_steps_frame : parameter_layout -> ∀ (region : blockset)
    (before after : state) (reg : register),
  region_steps region before after -> valid_position before ->
  (set_mem (current_block after) region = true
   ∨ params (current_block after) = []) -> ~ register_defined_in_region region
     reg ->
  current_registers after reg = current_registers before reg.
Proof.
  intros Hlayout region before after reg Hsegment.
  induction Hsegment as [s | before middle after Hinside Hstep Hrest IH];
    intros Hposition Hparams Houtside; [reflexivity |].
  destruct (set_mem (current_block middle) region) eqn:Hmiddle.
  - rewrite (IH ltac:(eapply step_valid_position; eassumption)
      Hparams Houtside).
    eapply step_region_frame; eassumption.
  - inversion Hrest; subst; [ | congruence].
    assert (Hparams' : params (current_block after) = []).
    { destruct Hparams; [congruence | assumption]. }
    assert (Hfinished : remaining before = []).
    { destruct Hstep; cbn in *; try reflexivity. congruence. }
    rewrite (step_to_parameterless_frame _ _ Hfinished Hparams' Hstep).
    reflexivity.
Qed.

(* Leaving a region is a block transfer, not an instruction. The new state is
   exactly the target's entry, before any exit instruction runs. *)
Lemma step_exits_region : control_edges -> ∀ (region : blockset)
    (before after : state),
  set_mem (current_block before) region = true ->
  set_mem (current_block after) region = false ->
  SsaSemantics.step before after ->
  (current_block after) ∈ (all_successors (current_block before))
    ∧ after = enter (current_block after) (current_registers after).
Proof.
  intros Hedges region before after Hinside Houtside Hstep.
  destruct Hstep; cbn in *; [congruence | |]; split; try reflexivity.
  - eapply goto_successor; eassumption.
  - eapply switch_successor;
      [exact Hedges | eassumption | eapply nth_error_In; eassumption].
Qed.

(* When a region has one designated exit, its finite first exit reaches that
   block's entry. No instructions in the exit block are included. *)
Theorem region_exit_target : control_edges -> ∀ (loop : natural_loop)
    (exit : block) (before after : state),
  exits_only_to loop exit ->
  set_mem (current_block before) loop.(body) = true ->
  region_steps loop.(body) before after ->
  set_mem (current_block after) loop.(body) = false ->
  ∃ (regs : registers), after = enter exit regs.
Proof.
  intros Hedges loop exit before after Honly Hinside Hrun.
  revert Hinside.
  induction Hrun as [s | before middle after Hsource Hstep Hrest IH];
    intros Hinside Houtside; [congruence |].
  destruct (set_mem (current_block middle) loop.(body)) eqn:Hmiddle.
  - apply IH; [reflexivity | exact Houtside].
  - assert (Hsame : middle = after).
    { inversion Hrest; subst; [reflexivity | congruence]. }
    subst after.
    destruct (step_exits_region Hedges _ _ _ Hsource Hmiddle Hstep)
      as [Hsucc Hentry].
    destruct (Honly _ Hsource _ Hsucc) as [Hin | Hexit]; [congruence |].
    exists (current_registers middle). rewrite <- Hexit. exact Hentry.
Qed.

(* A terminator step follows a graph edge and starts the target's body. *)
Lemma terminator_step_edge : control_edges -> ∀ (before after : state),
  remaining before = [] -> SsaSemantics.step before after ->
  (current_block after) ∈ (all_successors (current_block before))
  ∧ remaining after = block_body (current_block after).
Proof.
  intros Hedges s s' Hremaining Hstep.
  destruct Hstep; cbn in *; try discriminate; split; try reflexivity.
  - eapply goto_successor; eassumption.
  - eapply switch_successor; [exact Hedges | eassumption |].
    eapply nth_error_In. eassumption.
Qed.

(* Progress suffices to execute a finite instruction suffix. The caller proves
   progress from the whitelist and ordinary operand definedness. *)
Lemma region_finish_body : ∀ (region : blockset) (start : state),
  (∀ (reached : state), region_steps region start reached ->
    set_mem (current_block reached) region = true ->
    ∃ (next : state), SsaSemantics.step reached next) ->
  ∀ (b : block) (insns : list instr) (regs : registers),
  set_mem b region = true ->
  region_steps region start
    {| current_block := b; remaining := insns; current_registers := regs |} ->
  ∃ (final : registers), executes_insns regs insns final.
Proof.
  intros region start Hprogress b insns. induction insns as [|insn rest IH];
    intros regs Hinside Hprefix.
  - exists regs. constructor.
  - destruct (Hprogress _ Hprefix Hinside) as [next Hstep].
    inversion Hstep as [source current suffix before after Hexec |
      source target args before values after Hterm Hread Hbind |
      source index targets before word target Hterm Hread Hnonneg Hnth
        Hparams]; subst.
    destruct (IH after Hinside) as [final Hbody].
    { eapply region_steps_trans; [exact Hprefix |].
      eapply Region_cons; [exact Hinside | exact Hstep | constructor]. }
    exists final. econstructor; eassumption.
Qed.

(* A finite iteration ends at the first header return or region exit.
   Accessibility handles block transfers, and instruction-list induction
   handles each block body. The last source still has the original header
   parameters, allowing the counter proof to read its back-edge arguments. *)
Theorem region_iteration : control_edges -> parameter_layout ->
  ∀ (region : blockset) (header : block) (start : state),
  region_acyclic_except region header -> remaining start = [] ->
  set_mem (current_block start) region = true ->
  (∀ (reached : state), region_steps region start reached ->
    set_mem (current_block reached) region = true ->
    ∃ (next : state), SsaSemantics.step reached next) ->
  ∃ (before after : state),
    region_steps region start before ∧ SsaSemantics.step before after
    ∧ set_mem (current_block before) region = true
    ∧ remaining before = []
    ∧ (set_mem (current_block after) region = false
        ∨ current_block after = header)
    ∧ after = enter (current_block after) (current_registers after)
    ∧ ∀ (index : int),
         current_registers before (Param header index)
         = current_registers start (Param header index).
Proof.
  intros Hedges Hlayout region header start Hacyclic Hfinished Hinside
    Hprogress.
  assert (Hwalk : forall (b : block), Acc (region_edge_avoiding region header)
    b ->
    forall (regs : registers), set_mem b region = true -> b <> header ->
    region_steps region start (enter b regs) ->
    (forall (index : int), regs (Param header index)
      = current_registers start (Param header index)) ->
    exists (before after : state),
      region_steps region start before ∧ SsaSemantics.step before after
      ∧ set_mem (current_block before) region = true
      ∧ remaining before = []
      ∧ (set_mem (current_block after) region = false
          ∨ current_block after = header)
      ∧ after = enter (current_block after) (current_registers after)
      ∧ forall (index : int), current_registers before (Param header index)
           = current_registers start (Param header index)).
  { intros b Hacc. induction Hacc as [b Hacc IH].
    intros regs Hb Hnot Hprefix Hparams.
    destruct (region_finish_body region start Hprogress b (block_body b)
      regs Hb Hprefix) as [final Hbody].
    set (terminal := {| current_block := b; remaining := [];
                       current_registers := final |}).
    assert (Hterminal : region_steps region start terminal).
    { eapply region_steps_trans; [exact Hprefix |].
      apply executes_insns_region; assumption. }
    assert (Hsame : forall (index : int),
      current_registers terminal (Param header index)
        = current_registers start (Param header index)).
    { intros index. cbn. rewrite (executes_insns_parameter_frame _ _ _ Hbody).
      apply Hparams. }
    destruct (Hprogress terminal Hterminal Hb) as [next Hstep].
    destruct (terminator_step_edge Hedges terminal next eq_refl Hstep)
      as [Hedge Hentry].
    assert (Hnext :
      next = enter (current_block next) (current_registers next)).
    { destruct next; unfold enter; cbn in *. congruence. }
    destruct (set_mem (current_block next) region) eqn:Hnext_in;
      [destruct (block_eqb (current_block next) header) eqn:Hheader |].
    - exists terminal, next. repeat split; try assumption; try reflexivity.
      right. apply block_eqb_spec. exact Hheader.
    - assert (Hnext_not : current_block next <> header).
      { intros Heq. rewrite Heq, block_eqb_refl in Hheader. discriminate. }
      eapply (IH (current_block next)) with (regs := current_registers next);
        try assumption.
      + unfold region_edge_avoiding. cbn in Hedge. auto.
      + rewrite <- Hnext. eapply region_steps_trans; [exact Hterminal |].
        eapply Region_cons; [exact Hb | exact Hstep | constructor].
      + intros index. rewrite (step_parameter_frame Hlayout _ _ _ _
          Hstep Hnext_not). apply Hsame.
    - exists terminal, next. repeat split; try assumption; try reflexivity.
      left. exact Hnext_in. }
  destruct (Hprogress start (Region_refl _ _) Hinside) as [next Hstep].
  destruct (terminator_step_edge Hedges _ _ Hfinished Hstep)
    as [Hedge Hentry].
  assert (Hnext : next = enter (current_block next) (current_registers next)).
  { destruct next; unfold enter; cbn in *. congruence. }
  destruct (set_mem (current_block next) region) eqn:Hnext_in;
    [destruct (block_eqb (current_block next) header) eqn:Hheader |].
  - exists start, next. repeat split; try assumption; try reflexivity;
      try solve [constructor].
    right. apply block_eqb_spec. exact Hheader.
  - assert (Hnext_not : current_block next <> header).
    { intros Heq. rewrite Heq, block_eqb_refl in Hheader. discriminate. }
    eapply (Hwalk (current_block next)) with (regs := current_registers next);
      try assumption.
    + apply Hacyclic; assumption.
    + rewrite <- Hnext. eapply Region_cons;
        [exact Hinside | exact Hstep | constructor].
    + intros index. eapply step_parameter_frame; eassumption.
  - exists start, next. repeat split; try assumption; try reflexivity;
      try solve [constructor].
    left. exact Hnext_in.
Qed.

(* Structural facts of the particular loop record returned by discovery. In
   particular, completeness refers to this record, not to an unknown second
   loop with the same header. *)
Record loop_graph_properties (blocks : list block) (entry : block)
    (loop : natural_loop) : Prop :=
  { loop_header_in_body : set_mem loop.(header) loop.(body) = true;
    loop_header_in_graph : loop.(header) ∈ blocks;
    loop_body_in_graph : ∀ (b : block),
      set_mem b loop.(body) = true -> b ∈ blocks;
    loop_header_dominates_body : ∀ (b : block),
      set_mem b loop.(body) = true ->
      ∀ (visited_blocks : list block), block_path_from entry
        visited_blocks b -> loop.(header) ∈ visited_blocks;
    loop_body_predecessors : ∀ (source target : block),
      set_mem target loop.(body) = true -> target <> loop.(header) ->
      source ∈ (predecessors target) -> set_mem source loop.(body) = true;
    loop_back_edge_in_body : ∀ (source : block),
      source ∈ loop.(back_edge_sources) -> set_mem source loop.(body) = true;
    loop_back_edges_complete : ∀ (source : block),
      set_mem source loop.(body) = true ->
      loop.(header) ∈ (all_successors source) ->
      source ∈ loop.(back_edge_sources) }.

Section Graph.
  Variable blocks : list block.
  Variable entry : block.
  Hypothesis predecessors_closed : ∀ (b pred : block),
    b ∈ blocks -> pred ∈ (predecessors b) -> pred ∈ blocks.
  Hypothesis successors_closed : ∀ (b succ : block),
    b ∈ blocks -> succ ∈ (all_successors b) -> succ ∈ blocks.
  Hypothesis dominance : dominance_semantics entry.
  Hypothesis dominance_complete : ∀ (dominator target : block),
    dominator ∈ blocks -> target ∈ blocks ->
    (∀ (visited_blocks : list block), block_path_from entry visited_blocks
      target -> dominator ∈ visited_blocks) ->
    dominator ▷ target.

  (* Graph closure and correct dominance observations suffice to prove the
     natural-loop structural properties used by execution proofs. *)
  Theorem found_loop_graph_properties : ∀ (loop : natural_loop),
    loop ∈ (find_loops blocks) -> loop_graph_properties blocks entry loop.
  Proof.
    intros l Hl.
    destruct (find_loops_blocks blocks predecessors_closed successors_closed
      l Hl) as [Hheader Hbody].
    pose proof (find_loops_body_dominated blocks entry l dominance Hl)
      as Hdom.
    constructor; try assumption.
    - pose proof Hl as Hrecord. apply find_loops_In in Hrecord.
      destruct Hrecord as [h [sources [Hin ->]]].
      apply natural_loop_body_header.
    - intros source target Htarget Hnot_header Hedge.
      apply (proj2 (find_loops_body blocks predecessors_closed
        successors_closed
        l source Hl)).
      destruct (block_eqb source l.(header)) eqn:E.
      + left. apply block_eqb_spec. exact E.
      + right. eapply reaches_back_edge_predecessor; [ | exact Hedge |].
        * apply (proj1 (find_loops_body blocks predecessors_closed
            successors_closed l target Hl)) in Htarget.
          destruct Htarget as [Heq | Hreaches];
            [contradiction | exact Hreaches].
        * intros Heq. subst source. rewrite block_eqb_refl in E. discriminate.
    - intros source Hsource.
      apply (proj2 (find_loops_body blocks predecessors_closed
        successors_closed
        l source Hl)).
      destruct (block_eqb source l.(header)) eqn:E.
      + left. apply block_eqb_spec. exact E.
      + right. constructor; [exact Hsource |].
        intros Heq. subst source. rewrite block_eqb_refl in E. discriminate.
    - intros source Hsource Hedge.
      eapply find_loops_back_edges_complete;
        [exact Hl | apply Hbody; exact Hsource | exact Hedge |].
      apply dominance_complete; [exact Hheader | apply Hbody; exact Hsource |].
      apply Hdom. exact Hsource.
  Qed.

  (* Compose graph discovery with the actual deletion decision. These
     properties no longer need to be assembled manually by a reviewer. *)
  Theorem find_deletions_graph_properties :
    ∀ (defs : op_def) (loop_header exit : block),
    (loop_header, exit) ∈ (find_deletions defs blocks) ->
    ∃ (loop : natural_loop) (bivs : list biv),
      (loop, bivs) ∈ (InductionVar.analyze defs blocks)
      ∧ loop_header = loop.(header)
      ∧ deletion_certificate defs blocks loop bivs exit
      ∧ loop_graph_properties blocks entry loop.
  Proof.
    intros defs h exit Hin.
    destruct (find_deletions_spec _ _ _ _ Hin)
      as [l [bivs [Hl [Hheader Hcert]]]].
    exists l, bivs. split; [exact Hl |].
    split; [exact Hheader |]. split; [exact Hcert |].
    apply found_loop_graph_properties.
    exact (proj1 (InductionVar.analyze_spec _ _ _ _ Hl)).
  Qed.
End Graph.

(* A return to the header from the loop body is one of the analyzed back-edge
   Gotos, and it passes the actual argument-register values to the header's
   parameters, all from a single operational transition. *)
Theorem loop_back_edge_step : ∀ (blocks : list block) (entry : block)
    (defs : op_def) (loop : natural_loop) (bivs : list biv) (exit : block),
  control_edges -> parameter_layout ->
  loop_graph_properties blocks entry loop ->
  deletion_certificate defs blocks loop bivs exit ->
  ∀ (before after : state),
    remaining before = [] ->
    set_mem (current_block before) loop.(body) = true ->
    SsaSemantics.step before after -> current_block after = loop.(header) ->
    ∃ (args : list Terminator.arg),
      (current_block before) ∈ loop.(back_edge_sources)
      ∧ (current_block before) ∈ (predecessors loop.(header))
      ∧ terminator (current_block before)
           = Terminator.Continue (Goto loop.(header)) args
      ∧ ∀ (index : int) (arg : value),
           nth_int args index = Some (Terminator.Arg arg) ->
           current_registers after
             (register_of_value (param loop.(header) index))
           = current_registers before (register_of_value arg).
Proof.
  intros blocks entry defs l bivs exit Hedges Hparams Hgraph Hcert
    s s' Hremaining Hin Hstep Hheader.
  destruct (terminator_step_edge Hedges _ _ Hremaining Hstep)
    as [Hedge _]. rewrite Hheader in Hedge.
  assert (Hback : (current_block s) ∈ l.(back_edge_sources)).
  { eapply loop_back_edges_complete; eassumption. }
  assert (Hpred : (current_block s) ∈ (predecessors l.(header))).
  { apply (proj2 (predecessor_successor Hedges _ _)). exact Hedge. }
  destruct (certificate_counter _ _ _ _ _ Hcert)
    as [biv [Hcounter_in [Hspec _]]].
  destruct Hspec as [_ [_ [_ [Hargs _]]]].
  destruct (Hargs _ Hback Hpred) as [args [v [Hgoto _]]].
  apply goto_args_terminator in Hgoto.
  exists args. repeat split; try assumption.
  exact (proj2 (proj2
    (goto_step_parameter Hparams _ _ _ _ Hremaining Hgoto Hstep))).
Qed.
