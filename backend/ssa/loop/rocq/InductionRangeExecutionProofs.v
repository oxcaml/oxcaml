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

(* Operational meaning of a loop-counter range certificate.

   InductionRange.v certifies, from the literals the loop is entered with and
   the no-wrap entailment at its back-edge sources, that a header parameter
   never falls below [lower] while the loop runs, and, when the certificate
   names an upper bound, never exceeds the value of that bound. This module
   proves that of actual executions in the observation semantics of
   SsaContextSpec.v: in every state of the loop body that a context execution
   from the entry reaches, the counter register holds a word at least
   [lower] and at most the word in the register of the upper bound
   ([counter_bounded]), so the facts InductionRange.range_facts adds hold of
   the snapshot of its registers ([certificates_counters_in_range], the
   premise of BoundsCheckElimProofs.check_passes_sound).

   The argument is an induction over the context steps. Only a transfer into
   the header writes the counter: over a back edge it receives the update,
   which LoopCounterExecutionProofs.biv_update_observed reads as the counter
   plus the step with wraparound and InductionRangeProofs.no_wrap_at_sound,
   on the snapshot of the source's registers, shows does not wrap; from any
   other predecessor it receives a literal at least [lower]. Either way the
   value passed is at most the upper bound by
   InductionRangeProofs.argument_at_most_sound on the snapshot at the
   predecessor. The upper bound is defined in a strict dominator of the
   header, which lies outside the loop body, so no step inside the body
   writes its register. Opaque instructions write only their results, and an
   opaque transfer cannot enter the header, whose predecessors all end in a
   [Goto]. Nothing here is extracted, and nothing needs review. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlIntProofs OxCamlInt64
  OxCamlInt64Proofs OxCamlList OxCamlListProofs.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaGraphSpec SsaGraph SsaGraphProofs
  SsaDominanceSpec SsaDominance SsaDominanceProofs SsaSemanticsSpec
  SsaSemantics SsaSemanticsProofs SsaExecutionSpec SsaExecutionProofs
  SsaProgressSpec SsaProgressProofs SsaContextSpec SsaContextProofs.
From OxCamlLoop Require Import AffineSsa AffineSsaProofs NaturalLoop
  NaturalLoopProofs InductionVar InductionVarProofs InductionRange
  InductionRangeProofs LoopExecutionProofs LoopValueSnapshotProofs
  LoopGuardExecutionProofs LoopRangeExecutionProofs
  LoopCounterExecutionProofs.
Import ListNotations.
Local Open Scope Z_scope.

(* The counter, parameter [index] of the header of [loop], is bound to a word
   at least [lower], and at most the word in the register of [upper] when
   there is one, whenever the state is in the body of [loop]. *)
Definition counter_in_range (loop : natural_loop) (index lower : int)
    (upper : option value) (current : state) : Prop :=
  set_mem (current_block current) loop.(body) = true ->
  ∃ (word : Z),
    current_registers current (Param loop.(header) index) = Some word
    ∧ to_Z lower <= word
    ∧ ∀ (u : value), upper = Some u ->
        word <= snapshot_atom (current_registers current) u.

Section CounterExecution.
  Variable blocks : list block.
  Variable entry : block.
  Variable defs : op_def.
  Variable loop : natural_loop.
  Variable biv : InductionVar.biv.
  Variable step lower : int.
  Variable upper : option value.
  Variable initial_regs : registers.

  Hypothesis finished : finished_graph blocks entry defs.
  Hypothesis size : graph_size_supported blocks.
  Hypothesis loop_found : loop ∈ (find_loops blocks).
  Hypothesis counter : biv_spec defs loop biv.
  Hypothesis step_signed : signed_step biv = Some step.
  Hypothesis step_positive : 0 < to_Z step.
  Hypothesis header_not_entry : loop.(header) <> entry.
  Hypothesis entry_literals : ∀ (pred : block),
    pred ∈ (predecessors loop.(header)) ->
    pred ∉ loop.(back_edge_sources) ->
    passes_literal_at_least loop.(header) biv.(param_index) lower pred.
  Hypothesis upper_defined : ∀ (u : value), upper = Some u ->
    defined_above_loop defs loop.(header) u.
  Hypothesis upper_used : ∀ (u : value), upper = Some u ->
    switch_operand blocks u.
  Hypothesis upper_arguments : ∀ (u : value), upper = Some u ->
    ∀ (pred : block), pred ∈ (predecessors loop.(header)) ->
    argument_at_most u loop.(header) biv.(param_index) pred = true.
  Hypothesis back_edges_no_wrap : ∀ (source : block),
    source ∈ loop.(back_edge_sources) ->
    no_wrap_at step lower upper loop.(header) source biv.(param_index)
      = true.
  Hypothesis bounded : bounded_registers initial_regs.

  (* The structural facts of the certificate's loop. *)
  Lemma counter_loop_graph : loop_graph_properties blocks entry loop.
  Proof.
    apply found_loop_graph_properties;
      [exact (graph_predecessors_closed _ _ _ finished)
      | exact (graph_successors_closed _ _ (graph_well_formed _ _ _ finished))
      | exact (graph_dominance _ _ _ finished)
      | exact (graph_dominance_complete _ _ _ finished)
      | exact loop_found].
  Qed.

  (* Every predecessor of the header reaches it by a [Goto] with one argument
     per parameter: a back-edge source passes an update, any other
     predecessor a literal. *)
  Lemma header_predecessor_goto : ∀ (pred : block),
    pred ∈ (predecessors loop.(header)) ->
    ∃ (args : list Terminator.arg), goto_args loop.(header) pred = Some args.
  Proof.
    intros pred Hpred.
    destruct (set_mem pred (back_edge_set loop)) eqn:Hback.
    - apply back_edge_set_spec in Hback.
      pose proof counter as [_ [_ [_ [Hpasses _]]]].
      destruct (Hpasses pred Hback Hpred) as [args [_ [Hgoto _]]].
      exists args. exact Hgoto.
    - assert (Hnot : pred ∉ loop.(back_edge_sources)).
      { intros Hin. apply back_edge_set_spec in Hin. congruence. }
      destruct (entry_literals pred Hpred Hnot)
        as [args [_ [_ [_ [_ [Hgoto _]]]]]].
      exists args. exact Hgoto.
  Qed.

  (* The graph-size contract leaves room for the atoms of the no-wrap check
     and of the argument check at any block. *)
  Lemma source_atoms_fit : ∀ (source : block), source ∈ blocks ->
    2 * linearize_atom_bound + guards_atom_bound source <= ocaml_max_int.
  Proof.
    intros source Hsource.
    destruct (graph_strict_dominators _ _ _ finished source Hsource)
      as [Hnodup Hmembers].
    pose proof (NoDup_incl_length Hnodup Hmembers) as Hlength.
    pose proof (guard_list_atom_bound_by_length source
      (strict_dominators source)) as Hguards.
    pose proof size as Hsize. unfold graph_size_supported in Hsize.
    unfold guards_atom_bound. rewrite linearize_atom_bound_eq.
    rewrite comparison_atom_bound_eq in Hguards.
    unfold ocaml_max_int, two62. cbn in Hsize. lia.
  Qed.

  (* The header is reachable at its entry point, so that antisymmetry of
     dominance applies to it. *)
  Lemma header_entry_reachable :
    program_point_reachable entry (block_entry_point loop.(header)).
  Proof.
    apply block_entry_reachable;
      [exact (graph_edges _ _ _ finished) | exact header_not_entry |].
    apply (graph_blocks_reachable _ _ (graph_well_formed _ _ _ finished)).
    exact (loop_header_in_graph _ _ _ counter_loop_graph).
  Qed.

  (* The upper bound has a definition site in the graph, in a block outside
     the loop body, that dominates the header's entry point. *)
  Lemma upper_site : ∀ (u : value), upper = Some u ->
    ∃ (site : program_point),
      value_definition u site ∧ (fst site) ∈ blocks
      ∧ set_mem (fst site) loop.(body) = false
      ∧ program_point_dominates entry site (block_entry_point loop.(header)).
  Proof.
    intros u Hu.
    pose proof (graph_well_formed _ _ _ finished) as Hwf.
    pose proof (graph_edges _ _ _ finished) as Hedges.
    destruct (upper_used u Hu)
      as [g [id [cmp [x [y [result_index [targets [Hg [Hterm Hxy]]]]]]]]].
    assert (Hargs : [Res id (Intop (Icomp cmp)) [x; y] result_index]
      ∈ (terminator_args (terminator g))).
    { rewrite Hterm. left. reflexivity. }
    destruct (terminator_definitions _ _ Hwf g _ Hg Hargs _
      (or_introl eq_refl)) as [cond_site [Hcond [Hcond_block _]]].
    destruct Hcond as [count [Hcond _]].
    assert (Hoperand : u ∈ [x; y]).
    { destruct Hxy as [-> | ->]; [left | right; left]; reflexivity. }
    destruct (operand_definitions _ _ Hwf _ _ _ _ _ Hcond_block Hcond u
      Hoperand) as [site [Hdef [Hsite _]]].
    assert (Hblock : (fst site) ▷ loop.(header) ∧ fst site <> loop.(header)
      ∧ (snd site <= length (block_body (fst site)))%nat).
    { pose proof (upper_defined u Hu) as Habove.
      unfold defined_above_loop in Habove.
      destruct u as [uid uop uargs uindex | d dindex].
      - destruct Habove as [d [Hfind [Hdom Hne]]].
        destruct Hdef as [ucount [[prefix [suffix [Hbody Hlen]]] _]].
        assert (Hin : (Op uid uop uargs ucount) ∈ (block_body (fst site))).
        { rewrite Hbody. apply in_or_app. right. left. reflexivity. }
        pose proof (graph_definitions _ _ _ finished (fst site) uid uop uargs
          ucount Hsite Hin) as Hactual.
        rewrite Hfind in Hactual. injection Hactual as Heq. subst d.
        split; [exact Hdom | split; [exact Hne |]].
        rewrite Hbody, Hlen, length_app. cbn. lia.
      - destruct Hdef as [-> _]. cbn. split; [exact (proj1 Habove) |].
        split; [exact (proj2 Habove) | lia]. }
    destruct Hblock as [Hdom [Hne Hlen]].
    assert (Hentry_dom : program_point_dominates entry
      (block_entry_point (fst site)) (block_entry_point loop.(header))).
    { apply program_point_dominance_of_block; [exact Hedges |].
      intros path Hpath.
      exact (dominates_spec (graph_dominance _ _ _ finished) _ _ Hdom _
        Hpath). }
    exists site. split; [exact Hdef | split; [exact Hsite |]].
    split.
    - destruct (set_mem (fst site) loop.(body)) eqn:Hinside; [ | reflexivity].
      exfalso.
      assert (Hto_body : program_point_dominates entry
        (block_entry_point loop.(header)) (block_entry_point (fst site))).
      { apply program_point_dominance_of_block; [exact Hedges |].
        exact (loop_header_dominates_body _ _ _ counter_loop_graph _
          Hinside). }
      pose proof (program_point_dominance_antisymmetric entry _ _
        header_entry_reachable Hentry_dom Hto_body) as Heq.
      apply (f_equal fst) in Heq. cbn in Heq. exact (Hne Heq).
    - destruct site as [d remaining]. cbn in *.
      apply dominating_block_point_dominates;
        [cbn; intros Heq; exact (Hne (eq_sym Heq)) | exact Hlen
        | exact Hentry_dom].
  Qed.

  (* The upper bound is available at every point of the loop body. *)
  Lemma upper_available_in_body : ∀ (u : value), upper = Some u ->
    ∀ (position : program_point),
    set_mem (fst position) loop.(body) = true ->
    available_value blocks entry position u.
  Proof.
    intros u Hu position Hinside.
    destruct (upper_site u Hu) as [site [Hdef [Hsite [_ Hdom]]]].
    exists site. split; [exact Hdef | split; [exact Hsite |]].
    eapply program_point_dominance_transitive; [exact Hdom |].
    apply program_point_dominance_of_block;
      [exact (graph_edges _ _ _ finished) |].
    exact (loop_header_dominates_body _ _ _ counter_loop_graph _ Hinside).
  Qed.

  (* The upper bound is available at the terminator of every predecessor of
     the header. *)
  Lemma upper_available_at_predecessor : ∀ (u : value), upper = Some u ->
    ∀ (pred : block), pred ∈ (predecessors loop.(header)) ->
    available_value blocks entry (pred, 0%nat) u.
  Proof.
    intros u Hu pred Hpred.
    destruct (upper_site u Hu) as [site [Hdef [Hsite [Houtside Hdom]]]].
    exists site. split; [exact Hdef | split; [exact Hsite |]].
    eapply program_point_dominates_predecessor; [exact Hdom | |].
    - intros Heq. apply (f_equal fst) in Heq. cbn in Heq.
      rewrite Heq, (loop_header_in_body _ _ _ counter_loop_graph) in Houtside.
      discriminate.
    - constructor.
      apply (proj1 (predecessor_successor (graph_edges _ _ _ finished) pred
        loop.(header))).
      exact Hpred.
  Qed.

  (* A context step into the loop body leaves the register of the upper bound
     as it was. *)
  Lemma upper_frame : ∀ (before after : state),
    context_steps (enter entry initial_regs) before ->
    context_step before after ->
    set_mem (current_block after) loop.(body) = true ->
    ∀ (u : value), upper = Some u ->
    current_registers after (register_of_value u)
      = current_registers before (register_of_value u).
  Proof.
    intros before after Hprefix Hstep Hafter u Hu.
    pose proof (graph_well_formed _ _ _ finished) as Hwf.
    destruct (upper_site u Hu) as [site [Hdef [Hsite [Houtside _]]]].
    eapply context_step_definition_frame;
      [exact Hwf | | | exact Hdef | exact Hsite | | exact Hstep].
    - eapply context_steps_valid_position;
        [exact Hprefix | apply enter_valid_position].
    - eapply context_steps_in_graph;
        [exact (graph_edges _ _ _ finished) | exact Hwf | exact Hprefix
        | exact (entry_in_graph _ _ Hwf)].
    - intros Heq. apply (f_equal fst) in Heq. cbn in Heq.
      rewrite Heq in Houtside. congruence.
  Qed.

  (* An argument of the [Goto] a state is about to take is available there,
     with the exact equation of its operation when it has one. *)
  Lemma goto_argument_coherent : ∀ (before : state) (pred : block)
      (args : list Terminator.arg) (argument : value),
    context_steps (enter entry initial_regs) before ->
    current_block before = pred -> remaining before = [] ->
    goto_args loop.(header) pred = Some args ->
    arg_value args biv.(param_index) = Some argument ->
    value_coherent (current_registers before) argument.
  Proof.
    intros before pred args argument Hprefix Hblock Hfinished Hargs Harg.
    pose proof (graph_well_formed _ _ _ finished) as Hwf.
    pose proof (graph_edges _ _ _ finished) as Hedges.
    pose proof (goto_args_terminator _ _ _ Hargs) as Hterm.
    destruct (continue_argument_used (Goto loop.(header)) args
      biv.(param_index) argument (arg_value_nth _ _ _ Harg))
      as [uses [Huses Hused]].
    assert (Havailable : available_value blocks entry
      (state_program_point before) argument).
    { eapply terminator_operand_available;
        [exact Hwf | | exact Hfinished | | exact Hused].
      - eapply context_steps_in_graph;
          [exact Hedges | exact Hwf | exact Hprefix | cbn].
        exact (entry_in_graph _ _ Hwf).
      - rewrite Hblock, Hterm. exact Huses. }
    exact (context_execution_values_coherent Hedges blocks entry initial_regs
      before Hwf Hprefix argument Havailable).
  Qed.

  (* The value a predecessor of the header passes for the counter is at most
     the upper bound: the argument entailment holds of the snapshot of the
     registers at the predecessor's terminator, where both are available. *)
  Lemma argument_at_most_passed : ∀ (before : state) (pred : block)
      (args : list Terminator.arg) (argument : value) (word : Z) (u : value),
    context_steps (enter entry initial_regs) before ->
    current_block before = pred -> remaining before = [] ->
    pred ∈ (predecessors loop.(header)) ->
    goto_args loop.(header) pred = Some args ->
    arg_value args biv.(param_index) = Some argument ->
    current_registers before (register_of_value argument) = Some word ->
    upper = Some u ->
    word <= snapshot_atom (current_registers before) u.
  Proof.
    intros before pred args argument word u Hprefix Hblock Hfinished Hpred
      Hargs Harg Hword Hu.
    pose proof (graph_well_formed _ _ _ finished) as Hwf.
    pose proof (graph_edges _ _ _ finished) as Hedges.
    assert (Hin : pred ∈ blocks).
    { rewrite <- Hblock. eapply context_steps_in_graph;
        [exact Hedges | exact Hwf | exact Hprefix
        | exact (entry_in_graph _ _ Hwf)]. }
    pose proof (proj2 (context_execution_invariants _ _ _ bounded Hprefix))
      as Hbounded.
    pose proof (context_execution_values_coherent Hedges blocks entry
      initial_regs before Hwf Hprefix) as Hcoherent.
    assert (Heqb : ∀ (v w : value), value_eqb v w = true ->
      snapshot_value (current_registers before) v
      = snapshot_value (current_registers before) w).
    { intros v w Heq. apply value_eqb_spec in Heq. subst w. reflexivity. }
    assert (Htaken : guards_taken (snapshot_value (current_registers before))
      pred).
    { eapply executed_guards_taken;
        [exact Hedges | exact Hwf | exact (graph_dominance _ _ _ finished)
        | exact (graph_dominance_complete _ _ _ finished)
        | exact (graph_switch_edges _ _ _ finished)
        | exact (proj2 (graph_strict_dominators _ _ _ finished pred Hin))
        | exact bounded | exact Hprefix | exact Hblock]. }
    pose proof (argument_at_most_sound (snapshot_value (current_registers
      before)) (snapshot_value_bounded _) Heqb
      (snapshot_operation_semantics _) u loop.(header) biv.(param_index) pred
      args argument (source_atoms_fit pred Hin)
      (upper_arguments u Hu pred Hpred) Hargs Harg Htaken) as Hle.
    pose proof (goto_args_terminator _ _ _ Hargs) as Hterm.
    destruct (continue_argument_used (Goto loop.(header)) args
      biv.(param_index) argument (arg_value_nth _ _ _ Harg))
      as [uses [Huses Hused]].
    assert (Havailable : available_value blocks entry
      (state_program_point before) argument).
    { eapply terminator_operand_available;
        [exact Hwf | rewrite Hblock; exact Hin | exact Hfinished
        | rewrite Hblock, Hterm; exact Huses | exact Hused]. }
    rewrite (snapshot_read_available blocks entry _ _ argument word Hwf
      Hcoherent Hbounded Havailable Hword) in Hle.
    assert (Hpoint : state_program_point before = (pred, 0%nat)).
    { unfold state_program_point. rewrite Hblock, Hfinished. reflexivity. }
    rewrite Hpoint in Hcoherent.
    rewrite (snapshot_agrees_available blocks entry _ _ Hwf Hcoherent
      Hbounded u (upper_available_at_predecessor u Hu pred Hpred)) in Hle.
    exact Hle.
  Qed.

  (* Over a back edge from [source], the counter receives its update, which
     is the counter plus the step: the no-wrap entailment holds of the
     snapshot of the registers at [source], so the sum does not wrap. *)
  Lemma back_edge_update_at_least : ∀ (before : state) (source : block)
      (args : list Terminator.arg) (updated : value) (word : Z),
    context_steps (enter entry initial_regs) before ->
    current_block before = source -> remaining before = [] ->
    source ∈ loop.(back_edge_sources) ->
    goto_args loop.(header) source = Some args ->
    arg_value args biv.(param_index) = Some updated ->
    updated ∈ biv.(update) ->
    current_registers before (Param loop.(header) biv.(param_index))
      = Some word ->
    to_Z lower <= word ->
    (∀ (u : value), upper = Some u ->
       word <= snapshot_atom (current_registers before) u) ->
    current_registers before (register_of_value updated)
      = Some (word + to_Z step)
    ∧ to_Z lower <= word + to_Z step.
  Proof.
    intros before source args updated word Hprefix Hblock Hfinished Hback
      Hargs Harg Hupdate Hword Hlow Hhigh.
    pose proof (graph_well_formed _ _ _ finished) as Hwf.
    pose proof (graph_edges _ _ _ finished) as Hedges.
    pose proof (graph_parameters _ _ Hwf) as Hlayout.
    pose proof (biv_parameter_identity Hlayout defs loop biv counter)
      as Hparam.
    assert (Hinside : set_mem source loop.(body) = true).
    { apply (loop_back_edge_in_body _ _ _ counter_loop_graph). exact Hback. }
    assert (Hsource : source ∈ blocks).
    { apply (loop_body_in_graph _ _ _ counter_loop_graph). exact Hinside. }
    destruct (biv_update_observed defs loop biv step updated
      (current_registers before) counter step_signed Hupdate
      (goto_argument_coherent before source args updated Hprefix Hblock
        Hfinished Hargs Harg)) as [current [Hcurrent Hupdated]].
    rewrite Hparam in Hcurrent. cbn [register_of_value] in Hcurrent.
    rewrite Hword in Hcurrent. injection Hcurrent as <-.
    pose proof (proj2 (context_execution_invariants _ _ _ bounded Hprefix))
      as Hbounded.
    pose proof (context_execution_values_coherent Hedges blocks entry
      initial_regs before Hwf Hprefix) as Hcoherent.
    assert (Heqb : ∀ (v w : value), value_eqb v w = true ->
      snapshot_value (current_registers before) v
      = snapshot_value (current_registers before) w).
    { intros v w Heq. apply value_eqb_spec in Heq. subst w. reflexivity. }
    assert (Htaken : guards_taken (snapshot_value (current_registers before))
      source).
    { eapply executed_guards_taken;
        [exact Hedges | exact Hwf | exact (graph_dominance _ _ _ finished)
        | exact (graph_dominance_complete _ _ _ finished)
        | exact (graph_switch_edges _ _ _ finished)
        | exact (proj2 (graph_strict_dominators _ _ _ finished source
            Hsource))
        | exact bounded | exact Hprefix | exact Hblock]. }
    assert (Hsnapshot : snapshot_value (current_registers before)
      (param loop.(header) biv.(param_index)) = word).
    { rewrite Hparam. cbn [snapshot_value].
      eapply snapshot_atom_read; [exact Hbounded | exact Hword]. }
    assert (Hpoint : state_program_point before = (source, 0%nat)).
    { unfold state_program_point. rewrite Hblock, Hfinished. reflexivity. }
    rewrite Hpoint in Hcoherent.
    assert (Hupper : ∀ (u : value), upper = Some u ->
      snapshot_value (current_registers before)
        (param loop.(header) biv.(param_index))
      <= snapshot_value (current_registers before) u).
    { intros u Hu. rewrite Hsnapshot.
      rewrite (snapshot_agrees_available blocks entry _ _ Hwf Hcoherent
        Hbounded u (upper_available_in_body u Hu (source, 0%nat) Hinside)).
      exact (Hhigh u Hu). }
    pose proof (no_wrap_at_sound (snapshot_value (current_registers before))
      (snapshot_value_bounded _) Heqb (snapshot_operation_semantics _)
      step lower upper loop.(header) source biv.(param_index)
      (source_atoms_fit source Hsource) (back_edges_no_wrap source Hback)
      Htaken ltac:(rewrite Hsnapshot; exact Hlow) Hupper) as Hnowrap.
    rewrite Hsnapshot, to_Z_max_int in Hnowrap.
    pose proof (int_bounds lower) as Hlower.
    assert (Hfits : in_int64 (word + to_Z step)).
    { unfold in_int64, two63, two62 in *. lia. }
    split; [ | lia].
    rewrite Hupdated, (wrap64_id _ Hfits). reflexivity.
  Qed.

  (* From a predecessor that is not a back-edge source, the counter receives
     the literal that predecessor passes. *)
  Lemma entry_literal_passed : ∀ (before : state) (pred : block)
      (args : list Terminator.arg) (id : instr_id) (n : nativeint)
      (operands : list value) (result_index : int),
    context_steps (enter entry initial_regs) before ->
    current_block before = pred -> remaining before = [] ->
    goto_args loop.(header) pred = Some args ->
    arg_value args biv.(param_index)
      = Some (Res id (Const_int n) operands result_index) ->
    current_registers before (Result id result_index)
      = Some (Int64.to_Z (Int64.of_nativeint n)).
  Proof.
    intros before pred args id n operands result_index Hprefix Hblock
      Hfinished Hargs Harg.
    pose proof (goto_argument_coherent before pred args _ Hprefix Hblock
      Hfinished Hargs Harg) as Hcoherent.
    cbn [value_coherent modeled_operation] in Hcoherent.
    destruct Hcoherent as [values [result [_ [Heval Hvalue]]]].
    cbn [eval_operation] in Heval. injection Heval as <-. exact Hvalue.
  Qed.

  (* One context step preserves the bounds. *)
  Lemma counter_step : ∀ (before middle : state),
    context_steps (enter entry initial_regs) before ->
    context_step before middle ->
    counter_in_range loop biv.(param_index) lower upper before ->
    counter_in_range loop biv.(param_index) lower upper middle.
  Proof.
    intros before middle Hprefix Hstep Hbefore.
    pose proof (graph_well_formed _ _ _ finished) as Hwf.
    pose proof (graph_edges _ _ _ finished) as Hedges.
    pose proof (graph_parameters _ _ Hwf) as Hlayout.
    pose proof counter_loop_graph as Hloop.
    pose proof (biv_parameter_identity Hlayout defs loop biv counter)
      as Hparam.
    assert (Hupper : set_mem (current_block middle) loop.(body) = true ->
      ∀ (u : value), upper = Some u ->
      snapshot_atom (current_registers middle) u
      = snapshot_atom (current_registers before) u).
    { intros Hin u Hu. unfold snapshot_atom.
      rewrite (upper_frame before middle Hprefix Hstep Hin u Hu).
      reflexivity. }
    destruct Hstep as [s s' Hexec
      | b id op args count rest regs regs' words Hopaque Hread Houtputs
      | source target regs regs' Hopaque Hsucc Houtputs
      | b marker rest regs Hmarker].
    - pose proof Hexec as Hstep.
      destruct Hexec as [source insn rest regs regs' Hexec
        | source target args regs words regs' Hgoto Hread Hbind
        | source cond targets regs word target Hswitch Hread Hnonneg Hnth
            Hparams];
        unfold counter_in_range in *; cbn in *; intros Hin.
      + destruct (Hbefore Hin) as [word [Hword [Hlow Hhigh]]].
        exists word. split; [ | split; [exact Hlow |]].
        * destruct insn as [id op args count | |]; try discriminate.
          rewrite <- Hword.
          eapply execute_instruction_frame; [exact Hexec | discriminate].
        * intros u Hu. rewrite (Hupper Hin u Hu). exact (Hhigh u Hu).
      + pose proof (goto_successor Hedges _ _ _ Hgoto) as Hpred.
        apply (proj2 (predecessor_successor Hedges _ _)) in Hpred.
        destruct (block_eqb target loop.(header)) eqn:Hheader.
        * apply block_eqb_spec in Hheader. subst target.
          destruct (goto_step_parameter Hlayout
            {| current_block := source; remaining := [];
               current_registers := regs |}
            (enter loop.(header) regs') loop.(header) args eq_refl Hgoto
            Hstep) as [_ [_ Hbinding]].
          destruct (set_mem source (back_edge_set loop)) eqn:Hback.
          -- apply back_edge_set_spec in Hback.
             pose proof counter as [_ [_ [_ [Hpasses _]]]].
             destruct (Hpasses source Hback Hpred)
               as [args' [updated [Hargs [Harg Hupdate]]]].
             pose proof (goto_args_terminator _ _ _ Hargs) as Hterm.
             rewrite Hgoto in Hterm. injection Hterm as <-.
             assert (Hsource : set_mem source loop.(body) = true)
               by (apply (loop_back_edge_in_body _ _ _ Hloop); exact Hback).
             destruct (Hbefore Hsource) as [word [Hword [Hlow Hhigh]]].
             destruct (back_edge_update_at_least
               {| current_block := source; remaining := [];
                  current_registers := regs |}
               source args updated word Hprefix eq_refl eq_refl Hback Hargs
               Harg Hupdate Hword Hlow Hhigh) as [Hupdated Hlow'].
             exists (word + to_Z step).
             specialize (Hbinding _ _ (arg_value_nth _ _ _ Harg)).
             rewrite Hparam in Hbinding.
             cbn [register_of_value current_registers enter] in Hbinding.
             split; [rewrite Hbinding; exact Hupdated |].
             split; [exact Hlow' |].
             intros u Hu. rewrite (Hupper Hin u Hu).
             exact (argument_at_most_passed
               {| current_block := source; remaining := [];
                  current_registers := regs |}
               source args updated (word + to_Z step) u Hprefix eq_refl
               eq_refl Hpred Hargs Harg Hupdated Hu).
          -- assert (Hnot : source ∉ loop.(back_edge_sources)).
             { intros Hin'. apply back_edge_set_spec in Hin'. congruence. }
             destruct (entry_literals source Hpred Hnot)
               as [args' [id [n [operands [result_index
                 [Hargs [Harg Hlow]]]]]]].
             pose proof (goto_args_terminator _ _ _ Hargs) as Hterm.
             rewrite Hgoto in Hterm. injection Hterm as <-.
             pose proof (entry_literal_passed
               {| current_block := source; remaining := [];
                  current_registers := regs |}
               source args id n operands result_index Hprefix eq_refl eq_refl
               Hargs Harg) as Hliteral.
             exists (Int64.to_Z (Int64.of_nativeint n)).
             specialize (Hbinding _ _ (arg_value_nth _ _ _ Harg)).
             rewrite Hparam in Hbinding.
             cbn [register_of_value current_registers enter] in Hbinding.
             split; [rewrite Hbinding; exact Hliteral |].
             split; [exact Hlow |].
             intros u Hu. rewrite (Hupper Hin u Hu).
             exact (argument_at_most_passed
               {| current_block := source; remaining := [];
                  current_registers := regs |}
               source args (Res id (Const_int n) operands result_index)
               (Int64.to_Z (Int64.of_nativeint n)) u Hprefix eq_refl eq_refl
               Hpred Hargs Harg Hliteral Hu).
        * assert (Hneq : target <> loop.(header)).
          { intros ->. rewrite block_eqb_refl in Hheader. discriminate. }
          assert (Hsource : set_mem source loop.(body) = true).
          { eapply loop_body_predecessors;
              [exact Hloop | exact Hin | exact Hneq | exact Hpred]. }
          destruct (Hbefore Hsource) as [word [Hword [Hlow Hhigh]]].
          exists word. split; [ | split; [exact Hlow |]].
          -- rewrite <- Hword.
             exact (step_parameter_frame Hlayout loop.(header)
               biv.(param_index) _ _ Hstep Hneq).
          -- intros u Hu. rewrite (Hupper Hin u Hu). exact (Hhigh u Hu).
      + pose proof (switch_successor Hedges _ _ _ _ Hswitch
          (nth_error_In _ _ Hnth)) as Hpred.
        apply (proj2 (predecessor_successor Hedges _ _)) in Hpred.
        assert (Hneq : target <> loop.(header)).
        { intros Heq. subst target.
          destruct (header_predecessor_goto source Hpred) as [args Hargs].
          rewrite (goto_args_terminator _ _ _ Hargs) in Hswitch.
          discriminate. }
        assert (Hsource : set_mem source loop.(body) = true).
        { eapply loop_body_predecessors;
            [exact Hloop | exact Hin | exact Hneq | exact Hpred]. }
        exact (Hbefore Hsource).
    - unfold counter_in_range in *. cbn in *. intros Hin.
      destruct (Hbefore Hin) as [word [Hword [Hlow Hhigh]]].
      exists word. split; [ | split; [exact Hlow |]].
      + rewrite (proj1 Houtputs); [exact Hword |].
        intros Hwritten. destruct Hwritten as [index [Hreg _]]. discriminate.
      + intros u Hu. rewrite (Hupper Hin u Hu). exact (Hhigh u Hu).
    - unfold counter_in_range in *. cbn in *. intros Hin.
      apply (proj2 (predecessor_successor Hedges _ _)) in Hsucc.
      assert (Hneq : target <> loop.(header)).
      { intros Heq. subst target.
        destruct (header_predecessor_goto source Hsucc) as [args Hargs].
        rewrite (goto_args_terminator _ _ _ Hargs) in Hopaque.
        cbn in Hopaque. discriminate. }
      assert (Hsource : set_mem source loop.(body) = true).
      { eapply loop_body_predecessors;
          [exact Hloop | exact Hin | exact Hneq | exact Hsucc]. }
      destruct (Hbefore Hsource) as [word [Hword [Hlow Hhigh]]].
      exists word. split; [ | split; [exact Hlow |]].
      + rewrite (proj1 Houtputs); [exact Hword |].
        intros Hwritten. apply in_map_iff in Hwritten.
        destruct Hwritten as [parameter [Hreg Hparameter]].
        destruct (parameters_local Hlayout _ _ Hparameter) as [index ->].
        cbn in Hreg. injection Hreg as Heq _. exact (Hneq Heq).
      + intros u Hu. rewrite (Hupper Hin u Hu). exact (Hhigh u Hu).
    - unfold counter_in_range in *. cbn in *. exact Hbefore.
  Qed.

  (* The counter is within its bounds in every state of the loop body that a
     context execution from the entry reaches. The entry is outside the body,
     since the header is not the entry and dominates the body. *)
  Theorem counter_bounded : ∀ (reached : state),
    context_steps (enter entry initial_regs) reached ->
    counter_in_range loop biv.(param_index) lower upper reached.
  Proof.
    intros reached Hrun.
    assert (Hpreserved : ∀ (before after : state),
      context_steps before after ->
      context_steps (enter entry initial_regs) before ->
      counter_in_range loop biv.(param_index) lower upper before ->
      counter_in_range loop biv.(param_index) lower upper after).
    { intros before after Hsteps.
      induction Hsteps as [s | s t u Hstep Hsteps IH];
        intros Hprefix Hbefore; [exact Hbefore |].
      apply IH.
      - eapply context_steps_trans; [exact Hprefix |].
        eapply Context_cons; [exact Hstep | constructor].
      - eapply counter_step; eassumption. }
    apply (Hpreserved _ _ Hrun (Context_refl _)).
    unfold counter_in_range. cbn. intros Hin. exfalso.
    destruct (loop_header_dominates_body _ _ _ counter_loop_graph entry Hin
      [entry] (Block_path_one entry)) as [Heq | []].
    exact (header_not_entry (eq_sym Heq)).
  Qed.

  (* The same, read through the snapshot of the registers: the counter's
     snapshot is its word, and the upper bound's is its register, it being
     available throughout the body. *)
  Theorem counter_bounded_snapshot : ∀ (reached : state),
    context_steps (enter entry initial_regs) reached ->
    set_mem (current_block reached) loop.(body) = true ->
    to_Z lower
      <= snapshot_value (current_registers reached)
           (param loop.(header) biv.(param_index))
    ∧ ∀ (u : value), upper = Some u ->
        snapshot_value (current_registers reached)
          (param loop.(header) biv.(param_index))
        <= snapshot_value (current_registers reached) u.
  Proof.
    intros reached Hrun Hin.
    pose proof (graph_well_formed _ _ _ finished) as Hwf.
    pose proof (graph_edges _ _ _ finished) as Hedges.
    pose proof (proj2 (context_execution_invariants _ _ _ bounded Hrun))
      as Hregs.
    destruct (counter_bounded reached Hrun Hin) as [word [Hword [Hlow Hhigh]]].
    rewrite (biv_parameter_identity (graph_parameters _ _ Hwf) defs _ _
      counter).
    cbn [snapshot_value].
    rewrite (snapshot_atom_read _ (Block_param _ _) _ Hregs Hword).
    split; [exact Hlow |].
    intros u Hu.
    rewrite (snapshot_agrees_available blocks entry _ _ Hwf
      (context_execution_values_coherent Hedges blocks entry initial_regs
        reached Hwf Hrun) Hregs u
      (upper_available_in_body u Hu (state_program_point reached) Hin)).
    exact (Hhigh u Hu).
  Qed.
End CounterExecution.

(* The counters of the certificates whose loop contains the block of a state
   a context execution reaches are within their bounds, read through the
   snapshot of the registers there: the premise of
   BoundsCheckElimProofs.check_passes_sound. *)
Theorem certificates_counters_in_range : ∀ (blocks : list block)
    (entry : block) (defs : op_def) (certificates : list range_certificate)
    (initial_regs : registers) (reached : state),
  finished_graph blocks entry defs -> graph_size_supported blocks ->
  (∀ (cert : range_certificate), cert ∈ certificates ->
     certificate_spec defs entry blocks cert) ->
  bounded_registers initial_regs ->
  context_steps (enter entry initial_regs) reached ->
  counters_in_range (snapshot_value (current_registers reached)) certificates
    (current_block reached).
Proof.
  intros blocks entry defs certificates initial_regs reached Hfinished Hsize
    Hcertificates Hbounded Hrun cert Hcert Hin.
  pose proof (Hcertificates cert Hcert) as Hspec. cbv zeta in Hspec.
  destruct Hspec as [Hloop [Hbiv [step [Hstep [Hpositive [Hentry
    [Hliterals [Hupper Hnowrap]]]]]]]].
  exact (counter_bounded_snapshot blocks entry defs _ _ step _ cert.(upper)
    initial_regs Hfinished Hsize Hloop Hbiv Hstep Hpositive Hentry Hliterals
    (fun u Hu => proj1 (Hupper u Hu))
    (fun u Hu => proj1 (proj2 (Hupper u Hu)))
    (fun u Hu => proj2 (proj2 (Hupper u Hu)))
    Hnowrap Hbounded reached Hrun Hin).
Qed.
