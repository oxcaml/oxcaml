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

(* Live-out values of an accepted empty loop.

   The proof concerns only the original graph. It uses ordinary SSA
   definition/use dominance and the analysis's checked dead-use facts to
   separate changing loop control from values needed at the exit. No live-out
   invariance is assumed of the compiler. The public theorem
   [finite_exit_live_agreement] derives agreement between the first header body
   execution and a finite exit, using the liveness definition in SsaLiveness.v.
   It does not establish existence of that finite exit. Nothing is extracted.
   *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlList.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaGraph SsaDominance SsaSemantics
  SsaExecution SsaLiveness SsaContext.
From OxCamlLoop Require Import NaturalLoop InductionVar DeadInductionVar
  LoopTermination DeleteEmptyLoops LoopExecution LoopCounterExecution
  AffineSsa.
Import ListNotations.

Section Geometry.
  Variable blocks : list block.
  Variable entry : block.
  Variable loop : natural_loop.
  Hypothesis edges : control_edges.
  Hypothesis loop_graph : loop_graph_properties blocks entry loop.

  (* An incoming edge from outside the natural-loop body can only arrive at the
     header entry, not in the middle of another body block. *)
  Lemma body_program_point_predecessor : ∀ (before after : program_point),
    program_point_edge before after ->
    set_mem (fst after) loop.(body) = true ->
    after <> block_entry_point loop.(header) ->
    set_mem (fst before) loop.(body) = true.
  Proof.
    intros before after Hedge Hinside Hnot. destruct Hedge; cbn in *.
    - exact Hinside.
    - eapply loop_body_predecessors; [exact loop_graph | exact Hinside | |].
      + intros Heq. subst target. contradiction.
      + apply (proj2 (predecessor_successor edges source target)). exact H.
  Qed.

  (* A reachable body point has a suffix path from the header wholly inside the
     body. The last-header suffix excludes earlier iterations. *)
  Lemma header_body_program_point_path : ∀ (position : program_point),
    program_point_reachable entry position ->
    set_mem (fst position) loop.(body) = true ->
    ∃ (points : list program_point),
      program_point_path_from (block_entry_point loop.(header)) points position
        ∧
      Forall (fun p => set_mem (fst p) loop.(body) = true) points.
  Proof.
    intros position [points Hpath] Hinside.
    assert (Hdom : program_point_dominates entry (block_entry_point
      loop.(header)) position).
    { apply program_point_dominance_of_block; [exact edges |].
      exact (loop_header_dominates_body _ _ _ loop_graph _ Hinside). }
    destruct (program_point_path_last_visit _ _ _ Hpath _ (Hdom _ Hpath))
      as [tail [Hsuffix Hlast]].
    exists (block_entry_point loop.(header) :: tail).
    split; [exact Hsuffix |].
    eapply program_point_path_region_suffix;
      [apply body_program_point_predecessor | exact Hsuffix | exact Hinside |
       exact Hlast].
  Qed.

  (* A definition outside the body that dominates a reachable body point must
     already dominate the header entry. *)
  Lemma outside_dominator_of_body : ∀ (site position : program_point),
    program_point_reachable entry position ->
    set_mem (fst position) loop.(body) = true ->
    set_mem (fst site) loop.(body) = false ->
    program_point_dominates entry site position ->
    program_point_dominates entry site (block_entry_point loop.(header)).
  Proof.
    intros site position Hreachable Hinside Houtside Hdom prefix Hprefix.
    destruct (header_body_program_point_path position Hreachable Hinside)
      as [suffix [Hsuffix Hmembers]].
    destruct (program_point_path_join _ _ _ _ _ Hprefix Hsuffix)
      as [joined [Hjoined Hjoined_members]].
    destruct (Hjoined_members site (Hdom _ Hjoined)) as [Hin | Hin];
      [exact Hin |].
    apply Forall_forall with (x := site) in Hmembers; [ | exact Hin].
    congruence.
  Qed.
End Geometry.

(* Definitions that contribute data from a header parameter. This is a
   syntactic dependency, not an assumption that any execution changes it. *)
Inductive depends_on_header_parameter (header : block) : value -> Prop :=
  | Depends_parameter : ∀ (index : int),
      depends_on_header_parameter header (Block_param header index)
  | Depends_result : ∀ (id : instr_id) (op : operation)
      (args : list value) (index : int) (arg : value),
      arg ∈ args -> depends_on_header_parameter header arg ->
      depends_on_header_parameter header (Res id op args index).

(* A result depending on a header parameter cannot be defined before the header
   entry. This is a consequence of SSA operand dominance. *)
Lemma header_dependency_dominates_definition : ∀ (blocks : list block)
    (entry header : block) (v : value) (site : program_point),
  well_formed_ssa blocks entry -> depends_on_header_parameter header v ->
  value_definition v site -> (fst site) ∈ blocks ->
  program_point_dominates entry (block_entry_point header) site.
Proof.
  intros blocks entry header v site Hwf Hdepends. revert site.
  induction Hdepends; intros site Hdefinition Hsite.
  - destruct Hdefinition as [-> Hparam]. apply program_point_dominates_refl.
  - destruct Hdefinition as [count [Hdefinition Hindex]].
    destruct (operand_definitions _ _ Hwf _ _ _ _ _ Hsite Hdefinition _ H)
      as [arg_site [Harg [Harg_site [Hdom Hstrict]]]].
    eapply program_point_dominance_transitive; [eapply IHHdepends; eassumption
      |].
    exact Hdom.
Qed.

(* A header-dependent definition used in the body must itself be in the body.
   An outside definition would both dominate and be dominated by the header
   entry, contradicting its different block. *)
Theorem header_dependent_definition_in_body : control_edges ->
  ∀ (blocks : list block) (entry : block) (loop : natural_loop)
    (position : program_point) (v : value) (site : program_point),
  well_formed_ssa blocks entry -> loop_graph_properties blocks entry loop ->
  program_point_reachable entry position ->
  set_mem (fst position) loop.(body) = true ->
  depends_on_header_parameter loop.(header) v ->
  value_definition v site -> (fst site) ∈ blocks ->
  program_point_dominates entry site position ->
  set_mem (fst site) loop.(body) = true.
Proof.
  intros Hedges blocks entry loop position v site Hwf Hgraph Hreachable
    Hinside Hdepends Hdefinition Hsite Hdom.
  destruct (set_mem (fst site) loop.(body)) eqn:Houtside;
    [reflexivity | exfalso].
  pose proof (outside_dominator_of_body blocks entry loop Hedges Hgraph
    site position Hreachable Hinside Houtside Hdom) as Hto_header.
  pose proof (header_dependency_dominates_definition blocks entry loop.(header)
    v site Hwf Hdepends Hdefinition Hsite) as Hfrom_header.
  destruct Hreachable as [points Hpath].
  destruct (program_point_path_member _ _ _ Hpath site (Hdom _ Hpath))
    as [prefix [Hprefix Hmembers]].
  assert (Heq : block_entry_point loop.(header) = site).
  { eapply program_point_dominance_antisymmetric;
      [exists prefix; exact Hprefix | exact Hfrom_header | exact Hto_header]. }
  rewrite <- Heq in Houtside. cbn in Houtside.
  rewrite (loop_header_in_body _ _ _ Hgraph) in Houtside. discriminate.
Qed.

(* These are the only values allowed to depend on the changing header
   parameters. The dead-use proof will exclude them from exit liveness. *)
Definition loop_control_value (loop : natural_loop)
  (bivs : list InductionVar.biv)
    (exit_comparison value_ref : value) : Prop :=
  (∃ (index : int), value_ref = Block_param loop.(header) index)
  ∨ value_ref = exit_comparison
  ∨ ∃ (counter : InductionVar.biv),
       counter ∈ bivs ∧ value_ref ∈ counter.(update).

Section ControlValues.
  Variable blocks : list block.
  Variable entry : block.
  Variable defs : op_def.
  Variable loop : natural_loop.
  Variable bivs : list InductionVar.biv.
  Variable exit : block.
  Variable exit_info : exit_branch.
  Hypothesis edges : control_edges.
  Hypothesis well_formed : well_formed_ssa blocks entry.
  Hypothesis loop_graph : loop_graph_properties blocks entry loop.
  Hypothesis certificate : deletion_certificate defs blocks loop bivs exit.
  Hypothesis selected_exit : find_exit_branch loop = Some exit_info.

  (* The certificate supplies the BIV and dead-use facts together. *)
  Lemma certified_biv : ∀ (counter : InductionVar.biv), counter ∈ bivs ->
    biv_spec defs loop counter ∧ dead_spec blocks counter.
  Proof.
    intros counter Hin. destruct certificate as [_ [_ [_ [[_ Hspecs] _]]]].
    apply Hspecs. exact Hin.
  Qed.

  (* The dead-use predicates refer to this same loop and exit condition, rather
     than to unrelated witnesses of the recognition functions. *)
  Lemma certified_dead_uses : ∀ (counter : InductionVar.biv),
    counter ∈ bivs ->
    param_used_only_by_exit_comparison_and_updates blocks loop.(header)
      counter.(param_index) exit_info.(condition) counter.(update)
    ∧ exit_comparison_used_only_by_header_branch blocks loop.(header)
         exit_info.(condition)
    ∧ ∀ (updated : value), updated ∈ counter.(update) ->
         update_used_only_by_exit_comparison_and_back_edge blocks loop
           counter.(param_index) exit_info.(condition) updated.
  Proof.
    intros counter Hin.
    destruct (certified_biv counter Hin) as [Hspec [found [Hfound Hdead]]].
    rewrite (proj1 Hspec) in Hfound, Hdead.
    rewrite selected_exit in Hfound. injection Hfound as <-. exact Hdead.
  Qed.

  (* The header comparison is a canonical terminator operand. *)
  Lemma exit_comparison_available :
    available_value blocks entry (loop.(header), 0%nat) exit_info.(condition).
  Proof.
    destruct (find_exit_branch_spec loop exit_info selected_exit)
      as [ifnot [ifso [Hterm Htargets]]].
    destruct (terminator_definitions _ _ well_formed loop.(header)
      [exit_info.(condition)] (loop_header_in_graph _ _ _ loop_graph)
      ltac:(rewrite Hterm; cbn; auto) exit_info.(condition)
      ltac:(cbn; auto)) as [site [Hdefinition [Hsite Hdom]]].
    exists site. auto.
  Qed.

  (* Each listed update is a canonical argument at a back-edge source, whose
     block and program point belong to the original graph. *)
  Lemma control_update_available : ∀ (counter : InductionVar.biv)
      (updated : value),
    counter ∈ bivs -> updated ∈ counter.(update) ->
    ∃ (source : block), set_mem source loop.(body) = true ∧
      source ∈ blocks ∧
      available_value blocks entry (source, 0%nat) updated.
  Proof.
    intros counter updated Hin Hupdated.
    destruct (certified_biv counter Hin) as [Hspec Hdead].
    destruct Hspec as [_ [_ [_ [_ Harrives]]]].
    destruct (Harrives updated Hupdated)
      as [source [args [Hsource [Hgoto Harg]]]].
    pose proof (loop_back_edge_in_body _ _ _ loop_graph source Hsource)
      as Hinside.
    pose proof (loop_body_in_graph _ _ _ loop_graph source Hinside) as Hblock.
    pose proof (goto_args_terminator _ _ _ Hgoto) as Hterm.
    destruct (continue_argument_used (Goto loop.(header)) args
      counter.(param_index) updated (arg_value_nth _ _ _ Harg))
      as [uses [Huses Hused]].
    destruct (terminator_definitions _ _ well_formed source uses Hblock
      ltac:(rewrite Hterm; exact Huses) updated Hused)
      as [site [Hdefinition [Hsite Hdom]]].
    exists source. split; [exact Hinside |]. split; [exact Hblock |].
    exists site. auto.
  Qed.

  (* Every update directly depends on its actual header parameter. *)
  Lemma control_update_depends : ∀ (counter : InductionVar.biv)
      (updated : value),
    counter ∈ bivs -> updated ∈ counter.(update) ->
    depends_on_header_parameter loop.(header) updated.
  Proof.
    intros counter updated Hin Hupdated.
    pose proof (proj1 (certified_biv counter Hin)) as Hspec.
    pose proof (biv_parameter_identity (graph_parameters _ _ well_formed)
      defs loop counter Hspec) as Hparameter.
    destruct Hspec as [_ [_ [Hupdates _]]].
    destruct (Hupdates updated Hupdated)
      as [step [sign [Hclass Hsame]]].
    pose proof (classify_update_reads_self _ _ _ _ _ _ Hclass) as Hreads.
    destruct updated as [id op args index | b index]; [ | contradiction].
    destruct Hreads as [self [Harg Hself]].
    unfold is_param in Hself. apply value_eqb_spec in Hself.
    rewrite Hparameter in Hself. subst self.
    eapply Depends_result; [exact Harg | constructor].
  Qed.

  (* The accepted termination witness ensures the actual header test depends on
     a header parameter, directly or through one update. *)
  Lemma exit_comparison_depends_on_header_parameter :
    depends_on_header_parameter loop.(header) exit_info.(condition).
  Proof.
    destruct (certificate_counter _ _ _ _ _ certificate)
      as [counter [Hin [Hspec Haccepted]]].
    destruct (biv_implies_termination_spec defs loop counter
      (proj1 Hspec) Haccepted)
      as [found [step [cmp [iv_left [bnd
          [Hfound [Hstep [Hextract Hverdict]]]]]]]].
    rewrite selected_exit in Hfound. injection Hfound as <-.
    destruct (extract_comparison_shape _ _ _ _ _ _ Hextract)
      as [counter_ref [Hcounter [Hbound [id [index Hshape]]]]].
    assert (Hdepends : depends_on_header_parameter loop.(header) counter_ref).
    { apply orb_true_iff in Hcounter. destruct Hcounter as [Hself | Hupdate].
      - unfold is_param in Hself. apply value_eqb_spec in Hself.
        rewrite (biv_parameter_identity (graph_parameters _ _ well_formed)
          defs loop counter Hspec) in Hself. subst counter_ref. constructor.
      - apply existsb_exists in Hupdate.
        destruct Hupdate as [updated [Hupdated Heq]].
        apply value_eqb_spec in Heq. subst counter_ref.
        eapply control_update_depends; eassumption. }
    destruct bnd as [bound_ref | constant].
    - rewrite Hshape. eapply Depends_result; [ | exact Hdepends].
      destruct iv_left; cbn; auto.
    - destruct Hshape as [Hleft Hshape]. rewrite Hshape.
      eapply Depends_result; [left; reflexivity | exact Hdepends].
  Qed.

  (* Control results are defined inside the body, not in surrounding code. This
     fact follows from dependency and dominance; it is not inferred merely from
     an instruction ID appearing in the update list. *)
  Theorem control_result_definition_inside : ∀ (value_ref : value)
    (site : program_point),
    (value_ref = exit_info.(condition) ∨
      ∃ (counter : InductionVar.biv),
        counter ∈ bivs ∧ value_ref ∈ counter.(update)) ->
    value_definition value_ref site -> (fst site) ∈ blocks ->
    set_mem (fst site) loop.(body) = true.
  Proof.
    intros v site Hcontrol Hdefinition Hsite.
    assert (Havailable : exists (source : block),
      set_mem source loop.(body) = true ∧ source ∈ blocks ∧
      available_value blocks entry (source, 0%nat) v).
    { destruct Hcontrol as [-> | [counter [Hin Hupdated]]].
      - exists loop.(header). split;
          [exact (loop_header_in_body _ _ _ loop_graph) |].
        split; [exact (loop_header_in_graph _ _ _ loop_graph) |].
        exact exit_comparison_available.
      - eapply control_update_available; eassumption. }
    destruct Havailable as [source [Hinside [Hsource
      [actual_site [Hactual [Hactual_in Hdom]]]]]].
    assert (Hdepends : depends_on_header_parameter loop.(header) v).
    { destruct Hcontrol as [-> | [counter [Hin Hupdated]]].
      - exact exit_comparison_depends_on_header_parameter.
      - eapply control_update_depends; eassumption. }
    assert (Heq : site = actual_site).
    { destruct v as [id op args index | b index].
      - destruct Hdefinition as [count [Hdefinition Hindex]].
        destruct Hactual as [actual_count [Hactual Hactual_index]].
        eapply result_sites_unique; eassumption.
      - destruct Hdefinition as [-> Hparam]. destruct Hactual as [-> Hparam'].
        reflexivity. }
    subst actual_site.
    eapply header_dependent_definition_in_body; try eassumption.
    - apply (graph_blocks_reachable _ _ well_formed). exact Hsource.
    - exact Hinside.
  Qed.

  (* Every condition/update reference has an available canonical definition
     somewhere in the graph, irrespective of where a later use occurs. *)
  Lemma control_result_available : ∀ (value_ref : value),
    (value_ref = exit_info.(condition) ∨
      ∃ (counter : InductionVar.biv),
        counter ∈ bivs ∧ value_ref ∈ counter.(update)) ->
    ∃ (position : program_point),
      available_value blocks entry position value_ref.
  Proof.
    intros v [-> | [counter [Hin Hupdated]]].
    - exists (loop.(header), 0%nat). exact exit_comparison_available.
    - destruct (control_update_available counter v Hin Hupdated)
        as [source [Hinside [Hsource Havailable]]].
      exists (source, 0%nat). exact Havailable.
  Qed.

  (* An instruction identified by a control result is in the loop body.
     Canonical definition sites bridge the checker's ID test to this fact. *)
  Lemma control_insn_inside : ∀ (id : instr_id) (op : operation)
      (args : list value) (count : int) (site : program_point) (v : value),
    (fst site) ∈ blocks -> result_definition id op args count site ->
    (v = exit_info.(condition) ∨
      ∃ (counter : InductionVar.biv),
        counter ∈ bivs ∧ v ∈ counter.(update)) ->
    instr_defines_value (Op id op args count) v = true ->
    set_mem (fst site) loop.(body) = true.
  Proof.
    intros id op args count site v Hsite Hdefinition Hcontrol Hdefines.
    destruct (control_result_available v Hcontrol)
      as [position [actual_site [Hactual [Hactual_in Hdom]]]].
    destruct v as [actual_id actual_op actual_args index | b index];
      [ | discriminate].
    cbn [instr_defines_value] in Hdefines.
    apply instr_id_eqb_spec in Hdefines. subst actual_id.
    pose proof Hactual as [actual_count [Hactual_definition Hindex]].
    assert (Heq : site = actual_site).
    { eapply result_sites_unique; eassumption. }
    subst site. eapply control_result_definition_inside; eassumption.
  Qed.

  (* Dead-use restrictions confine every read of changing loop control to the
     body. The locations of the condition/update instructions are proved above,
     rather than assumed from their membership in a list. *)
  Theorem control_operand_inside : ∀ (position : program_point)
    (value_ref : value),
    (fst position) ∈ blocks -> operand_at position value_ref ->
    loop_control_value loop bivs exit_info.(condition) value_ref ->
    set_mem (fst position) loop.(body) = true.
  Proof.
    intros position v Hblock Huse Hcontrol.
    destruct Hcontrol as [[index ->] | [-> | [counter [Hin Hupdated]]]].
    - pose proof (operand_at_available _ _ _ _ well_formed Hblock Huse)
        as [site [[Hsite Hparameter] Havailable]].
      pose proof certificate as [_ [_ [_ [[Hcover Hspecs] Hrest]]]].
      destruct (Hcover index _ Hparameter) as [counter [Hin Hindex]].
      pose proof (proj1 (certified_dead_uses counter Hin)) as Huses.
      assert (Hself : is_param loop.(header) counter.(param_index)
        (Block_param loop.(header) index) = true).
      { unfold is_param. rewrite Hindex,
          (parameter_lookup (graph_parameters _ _ well_formed)
             _ _ _ Hparameter).
        apply value_eqb_spec. reflexivity. }
      destruct Huse as [b remaining id op args count arg Hdefinition Harg |
        b args arg Hargs Harg]; cbn in *.
      + destruct (Huses b Hblock) as [Hinsns Hterms].
        assert (Hmember : (Op id op args count) ∈ (block_body b)).
        { destruct Hdefinition as [prefix [suffix [Hbody Hlength]]].
          cbn in Hbody. rewrite Hbody.
          apply in_or_app. right. left. reflexivity. }
        specialize (Hinsns _ Hmember).
        assert (Hread : existsb (is_param loop.(header) counter.(param_index))
          (args_of_instr (Op id op args count)) = true).
        { apply existsb_exists. eexists. split; [exact Harg | exact Hself]. }
        destruct (Hinsns Hread) as [Hcond | [updated [Hupdated Hdefines]]].
        * eapply control_insn_inside with (site := (b, remaining));
            [exact Hblock | exact Hdefinition | | exact Hcond].
          left. reflexivity.
        * eapply control_insn_inside with (site := (b, remaining));
            [exact Hblock | exact Hdefinition | | exact Hdefines].
          right. exists counter. auto.
      + destruct (Huses b Hblock) as [Hinsns Hterms].
        exfalso. eapply Hterms; [exact Hargs |].
        apply existsb_exists. eexists. split; [exact Harg | exact Hself].
    - destruct (certificate_counter _ _ _ _ _ certificate)
        as [counter [Hin Haccepted]].
      pose proof (proj1 (proj2 (certified_dead_uses counter Hin))) as Huses.
      assert (Hself :
        value_eqb exit_info.(condition) exit_info.(condition) = true)
        by (apply value_eqb_spec; reflexivity).
      destruct Huse as [b remaining id op args count arg Hdefinition Harg |
        b args arg Hargs Harg]; cbn in *.
      + destruct (Huses b Hblock) as [Hinsns Hterms].
        exfalso. eapply Hinsns.
        * destruct Hdefinition as [prefix [suffix [Hbody Hlength]]].
          cbn in Hbody. rewrite Hbody.
          apply in_or_app. right. left. reflexivity.
        * apply existsb_exists. eexists. split; [exact Harg | exact Hself].
      + destruct (Huses b Hblock) as [Hinsns Hterms].
        assert (Heq : b = loop.(header)).
        { eapply Hterms; [exact Hargs |].
          apply existsb_exists. eexists. split; [exact Harg | exact Hself]. }
        subst b. exact (loop_header_in_body _ _ _ loop_graph).
    - pose proof (proj2 (proj2 (certified_dead_uses counter Hin)) v Hupdated)
        as Huses.
      assert (Hself : value_eqb v v = true)
        by (apply value_eqb_spec; reflexivity).
      destruct Huse as [b remaining id op args count arg Hdefinition Harg |
        b args arg Hargs Harg]; cbn in *.
      + destruct (Huses b Hblock) as [Hinsns Hterms].
        assert (Hdefines : instr_defines_value (Op id op args count)
          exit_info.(condition) = true).
        { apply Hinsns.
          - destruct Hdefinition as [prefix [suffix [Hbody Hlength]]].
            cbn in Hbody. rewrite Hbody.
            apply in_or_app. right. left. reflexivity.
          - apply existsb_exists. eexists. split; [exact Harg | exact Hself]. }
        eapply control_insn_inside with (site := (b, remaining));
          [exact Hblock | exact Hdefinition | | exact Hdefines].
        left. reflexivity.
      + destruct (Huses b Hblock) as [Hinsns Hterms].
        assert (Hback : b ∈ loop.(back_edge_sources)).
        { apply (proj1 (Hterms args Hargs ltac:(apply existsb_exists;
            eexists; split; [exact Harg | exact Hself]))). }
        exact (loop_back_edge_in_body _ _ _ loop_graph b Hback).
  Qed.

  (* A condition or update selected by these analyses is an instruction result,
     never an arbitrary block parameter masquerading as one. *)
  Lemma control_value_is_instruction_result : ∀ (value_ref : value),
    (value_ref = exit_info.(condition) ∨
      ∃ (counter : InductionVar.biv),
        counter ∈ bivs ∧ value_ref ∈ counter.(update)) ->
    ∃ (id : instr_id) (op : operation) (args : list value) (index : int),
      value_ref = Res id op args index.
  Proof.
    intros v [-> | [counter [Hin Hupdated]]].
    - destruct (certificate_counter _ _ _ _ _ certificate)
        as [counter [Hin [Hspec Haccepted]]].
      destruct (biv_implies_termination_spec defs loop counter
        (proj1 Hspec) Haccepted)
        as [found [step [cmp [iv_left [bnd
          [Hfound [Hstep [Hextract Hverdict]]]]]]]].
      rewrite selected_exit in Hfound. injection Hfound as <-.
      destruct exit_info.(condition);
        [repeat eexists; reflexivity | discriminate].
    - pose proof (proj1 (certified_biv counter Hin))
        as [_ [_ [Hupdates Hrest]]].
      destruct (Hupdates v Hupdated) as [step [sign [Hclass Hsame]]].
      destruct v; [repeat eexists; reflexivity | discriminate].
  Qed.

  (* Structural reachability of the finished header includes its entry point.
     It does not require any assumption that the loop terminates. *)
  Lemma live_out_header_reachable :
    program_point_reachable entry (block_entry_point loop.(header)).
  Proof.
    destruct (graph_blocks_reachable _ _ well_formed loop.(header)
      (loop_header_in_graph _ _ _ loop_graph)) as [points Hpath].
    assert (Hdom : program_point_dominates entry (block_entry_point
      loop.(header))
      (loop.(header), 0%nat)).
    { apply program_point_dominance_of_block; [exact edges |].
      intros block_path_from Hblock_path. eapply block_path_last. exact
        Hblock_path. }
    destruct (program_point_path_member _ _ _ Hpath _ (Hdom _ Hpath))
      as [prefix [Hprefix Hmembers]].
    exists prefix. exact Hprefix.
  Qed.

  (* A changing control result is not available at the header entry: its
     dependency on a header parameter forces its definition after entry. *)
  Lemma control_result_unavailable_at_entry : ∀ (v : value),
    (v = exit_info.(condition) ∨
      ∃ (counter : InductionVar.biv),
        counter ∈ bivs ∧ v ∈ counter.(update)) ->
    ~ available_value blocks entry (block_entry_point loop.(header)) v.
  Proof.
    intros v Hcontrol [site [Hdefinition [Hsite Hdom]]].
    assert (Hdepends : depends_on_header_parameter loop.(header) v).
    { destruct Hcontrol as [-> | [counter [Hin Hupdated]]].
      - exact exit_comparison_depends_on_header_parameter.
      - eapply control_update_depends; eassumption. }
    pose proof (header_dependency_dominates_definition _ _ _ _ _ well_formed
      Hdepends Hdefinition Hsite) as Hreverse.
    assert (Heq : site = block_entry_point loop.(header)).
    { eapply program_point_dominance_antisymmetric;
        [exact live_out_header_reachable | exact Hdom | exact Hreverse]. }
    subst site.
    destruct (control_value_is_instruction_result v Hcontrol)
      as [id [op [args [index ->]]]].
    destruct Hdefinition as [count [Hdefinition Hindex]].
    eapply result_definition_not_entry. exact Hdefinition.
  Qed.

  (* Register identity cannot hide a control use behind a different canonical
     payload. Parameter references need only their identity. *)
  Lemma control_register_identity : ∀ (control_ref available_ref : value)
    (position : program_point),
    loop_control_value loop bivs exit_info.(condition) control_ref ->
    available_value blocks entry position available_ref ->
    register_of_value control_ref = register_of_value available_ref ->
      control_ref = available_ref.
  Proof.
    intros v w position Hcontrol Havailable Hreg.
    destruct Hcontrol as [[index ->] | Hresult].
    - destruct w; cbn in Hreg; [discriminate |].
      inversion Hreg. reflexivity.
    - destruct (control_result_available v Hresult) as [site Hcanonical].
      eapply available_value_identity; eassumption.
  Qed.

  (* A live control register cannot propagate out of the body. On an incoming
     header edge, parameters are rebound and control results are not yet
     available; neither carries an old value into a new visit. *)
  Theorem live_control_inside : ∀ (position : program_point)
    (reg : register),
    register_live_at position reg -> (fst position) ∈ blocks ->
    ∀ (v : value), loop_control_value loop bivs exit_info.(condition) v ->
    reg = register_of_value v -> set_mem (fst position) loop.(body) = true.
  Proof.
    intros position reg Hlive.
    induction Hlive as [position operand Huse |
      before after reg Hedge Hnot Hlive IH]; intros Hblock v Hcontrol Hreg.
    - assert (Heq : v = operand).
      { eapply control_register_identity;
          [exact Hcontrol | | symmetry; exact Hreg].
        eapply operand_at_available; eassumption. }
      subst v. eapply control_operand_inside; eassumption.
    - pose proof (program_point_edge_in_graph _ _ _ _ well_formed Hblock Hedge)
        as Hafter.
      destruct (program_point_eqb after (block_entry_point loop.(header)))
        eqn:Hentry.
      + apply program_point_eqb_spec in Hentry. subst after.
        destruct (live_at_available _ _ _ _ well_formed Hafter Hlive)
          as [actual [Hactual Havailable]].
        assert (Heq : v = actual).
        { eapply control_register_identity;
            [exact Hcontrol | exact Havailable | congruence]. }
        subst actual. exfalso.
        destruct Hcontrol as [[index ->] | Hresult].
        * apply Hnot. exists (Block_param loop.(header) index).
          split; [ | symmetry; exact Hreg].
          destruct Havailable as [site [[Hsite Hparam] Hdom]].
          split; [reflexivity | exact Hparam].
        * eapply control_result_unavailable_at_entry; eassumption.
      + eapply body_program_point_predecessor; [exact edges | exact loop_graph
        |
          exact Hedge | |].
        * eapply IH; eassumption.
        * intros Heq. rewrite Heq in Hentry.
          assert (program_point_eqb (block_entry_point loop.(header))
            (block_entry_point loop.(header)) = true)
            by (apply program_point_eqb_spec; reflexivity).
          congruence.
  Qed.

  (* No header parameter, update result or exit-comparison result is live at
     the exit. This is about reads before redefinition, including paths that
     later re-enter the loop, not merely immediate outside uses. *)
  Theorem exit_live_not_control : ∀ (value_ref : value),
    register_live_at (block_entry_point exit) (register_of_value value_ref) ->
    ~ loop_control_value loop bivs exit_info.(condition) value_ref.
  Proof.
    intros v Hlive Hcontrol.
    pose proof certificate as [[found [Hfound [Htarget Hparams]]] Hrest].
    rewrite selected_exit in Hfound. injection Hfound as <-.
    destruct (find_exit_branch_target loop exit_info selected_exit)
      as [Houtside [targets [Hterm Htarget_in]]].
    assert (Hexit : exit ∈ blocks).
    { eapply graph_successors_closed;
        [exact well_formed | exact (loop_header_in_graph _ _ _ loop_graph) |].
      rewrite <- Htarget. eapply switch_successor; eassumption. }
    pose proof (live_control_inside _ _ Hlive Hexit v Hcontrol eq_refl) as Hin.
    cbn in Hin. rewrite <- Htarget in Hin. congruence.
  Qed.

  (* For a single-result instruction, defining an accepted control value means
     its sole result is exactly that value. *)
  Lemma defines_control_result : ∀ (b : block) (id : instr_id)
      (op : operation) (args : list value) (control : value),
    b ∈ blocks -> (Op id op args one) ∈ (block_body b) ->
    (control = exit_info.(condition) ∨
      ∃ (counter : InductionVar.biv),
        counter ∈ bivs ∧ control ∈ counter.(update)) ->
    instr_defines_value (Op id op args one) control = true ->
    loop_control_value loop bivs exit_info.(condition) (Res id op args zero).
  Proof.
    intros b id op args control Hblock Hmember Hcontrol Hdefines.
    destruct (control_result_available control Hcontrol)
      as [position Havailable].
    pose proof (single_result_reference _ _ _ _ _ _ _ _ well_formed Hblock
      Hmember Havailable Hdefines) as Heq.
    rewrite <- Heq. right. exact Hcontrol.
  Qed.

  (* An ordinary instruction result cannot depend directly on loop control:
     dead-use approvals permit those operands only in control instructions. *)
  Lemma noncontrol_operands : ∀ (b : block) (id : instr_id)
      (op : operation) (args : list value) (arg : value),
    b ∈ blocks -> (Op id op args one) ∈ (block_body b) -> arg ∈ args ->
    ~ loop_control_value loop bivs exit_info.(condition)
      (Res id op args zero) ->
    ~ loop_control_value loop bivs exit_info.(condition) arg.
  Proof.
    intros b id op args arg Hblock Hmember Harg Hresult Hcontrol.
    destruct Hcontrol as [[index ->] | [-> | [counter [Hin Hupdated]]]].
    - destruct (result_definition_of_membership _ _ _ _ _ Hmember)
        as [site [Hsite Hdefinition]].
      destruct (operand_definitions _ _ well_formed _ _ _ _ _
        ltac:(rewrite Hsite; exact Hblock) Hdefinition _ Harg)
        as [parameter_site [[Hparameter_site Hparameter] Hdom]].
      pose proof certificate as [_ [_ [_ [[Hcover Hspecs] Hrest]]]].
      destruct (Hcover index _ Hparameter) as [counter [Hin Hindex]].
      pose proof (proj1 (certified_dead_uses counter Hin)) as Huses.
      assert (Hself : is_param loop.(header) counter.(param_index)
        (Block_param loop.(header) index) = true).
      { unfold is_param. rewrite Hindex,
          (parameter_lookup (graph_parameters _ _ well_formed)
             _ _ _ Hparameter).
        apply value_eqb_spec. reflexivity. }
      destruct (uses_only_instruction _ _ _ _ _ _ _ Huses Hblock
        Hmember Harg Hself) as [Hcond | [updated [Hupdated Hdefines]]];
        apply Hresult.
      + eapply defines_control_result; [exact Hblock | exact Hmember |
          left; reflexivity | exact Hcond].
      + eapply defines_control_result;
          [exact Hblock | exact Hmember | | exact Hdefines].
        right. exists counter. auto.
    - destruct (certificate_counter _ _ _ _ _ certificate)
        as [counter [Hin Haccepted]].
      pose proof (proj1 (proj2 (certified_dead_uses counter Hin))) as Huses.
      eapply uses_only_instruction; [exact Huses | exact Hblock |
        exact Hmember | exact Harg |].
      apply value_eqb_spec. reflexivity.
    - pose proof (proj2 (proj2 (certified_dead_uses counter Hin)) arg Hupdated)
        as Huses.
      pose proof (uses_only_instruction _ _ _ _ _ _ _ Huses Hblock
        Hmember Harg ltac:(apply value_eqb_spec; reflexivity)) as Hdefines.
      apply Hresult. eapply defines_control_result;
        [exact Hblock | exact Hmember | left; reflexivity | exact Hdefines].
  Qed.

  (* The certificate's parameterless exit is an actual successor of the header.
     This direct edge is crucial to the live-out dominance proof. *)
  Lemma certified_exit_edge : params exit = [] ∧
    exit ∈ (all_successors loop.(header)).
  Proof.
    pose proof certificate as [[found [Hfound [Htarget Hparams]]] Hrest].
    rewrite selected_exit in Hfound. injection Hfound as <-.
    split; [exact Hparams |].
    destruct (find_exit_branch_target loop exit_info selected_exit)
      as [Houtside [targets [Hterm Htarget_in]]].
    rewrite <- Htarget. eapply switch_successor; eassumption.
  Qed.

  (* A value available at the parameterless exit was already available after
     the first header body. The direct header-to-exit edge cannot introduce a
     definition needed there. *)
  Lemma exit_value_available_at_header_terminator : ∀ (value_ref : value),
    available_value blocks entry (block_entry_point exit) value_ref ->
    available_value blocks entry (loop.(header), 0%nat) value_ref.
  Proof.
    intros v [site [Hdefinition [Hsite Hdom]]].
    destruct certified_exit_edge as [Hparams Hedge].
    exists site. split; [exact Hdefinition |]. split; [exact Hsite |].
    eapply program_point_dominates_predecessor;
      [exact Hdom | | constructor; exact Hedge].
    intros Heq. subst site. destruct v as [id op args index | b index].
    - destruct Hdefinition as [count [Hdefinition Hindex]].
      eapply result_definition_not_entry. exact Hdefinition.
    - destruct Hdefinition as [Heq Hparameter].
      apply (f_equal fst) in Heq. cbn in Heq. subst b.
      rewrite Hparams in Hparameter. discriminate.
  Qed.

  (* Of definitions inside the loop, only header definitions can be available
     at the header terminator, hence at its direct exit. *)
  Lemma header_available_body_definition : ∀ (site : program_point),
    program_point_dominates entry site (loop.(header), 0%nat) ->
    set_mem (fst site) loop.(body) = true -> fst site = loop.(header).
  Proof.
    intros site Hdom Hinside.
    destruct (block_eqb (fst site) loop.(header)) eqn:Heq;
      [apply block_eqb_spec; exact Heq |].
    assert (Hother : fst site <> loop.(header)).
    { intros Hequal. rewrite Hequal, block_eqb_refl in Heq. discriminate. }
    pose proof (other_block_dominates_entry entry loop.(header) site
      Hother Hdom) as Hto_header.
    assert (Hfrom_header : program_point_dominates entry
      (block_entry_point loop.(header)) site).
    { apply program_point_dominance_of_block; [exact edges |].
      exact (loop_header_dominates_body _ _ _ loop_graph _ Hinside). }
    pose proof (program_point_dominance_antisymmetric entry _ _
      live_out_header_reachable
      Hto_header Hfrom_header) as Hequal.
    apply (f_equal fst) in Hequal. exact Hequal.
  Qed.

  (* Given actual value equations at the first header and at the exit, ordinary
     available values agree. Induction follows their finite operand DAG;
     dead-use checks exclude changing control at every edge. *)
  Lemma noncontrol_values_agree : ∀ (first_header_regs exit_regs :
    registers),
    coherent_at blocks entry first_header_regs (loop.(header), 0%nat) ->
    coherent_at blocks entry exit_regs (block_entry_point exit) ->
    (∀ (reg : register), ~ register_defined_in_region loop.(body) reg ->
       exit_regs reg = first_header_regs reg) ->
    ∀ (v : value),
    available_value blocks entry (block_entry_point exit) v ->
    ~ loop_control_value loop bivs exit_info.(condition) v ->
    exit_regs (register_of_value v) = first_header_regs (register_of_value v).
  Proof.
    intros first final Hfirst Hfinal Houtside v.
    induction v as [id op args index IHargs | b index] using value_ind';
      intros Havailable Hnot.
    - pose proof Havailable as [site [Hdefinition [Hsite Hdom]]].
      destruct (set_mem (fst site) loop.(body)) eqn:Hinside.
      + destruct Hdefinition as [count [Hdefinition Hindex]].
        assert (Hmember : (Op id op args count) ∈ (block_body (fst site))).
        { destruct Hdefinition as [prefix [suffix [Hbody Hlength]]].
          rewrite Hbody. apply in_or_app. right. left. reflexivity. }
        pose proof certificate as [_ [_ [_ [_ [Hsupported Hrest]]]]].
        pose proof (proj1 (Hsupported _ Hinside) _ Hmember) as Hsupported_insn.
        apply andb_true_iff in Hsupported_insn.
        destruct Hsupported_insn as [Hcount Hop].
        apply eqb_int in Hcount. subst count.
        assert (index = zero) as ->.
        { apply Sint63.to_Z_inj. rewrite to_Z_zero, to_Z_one in *. lia. }
        assert (Hargs : read_values final args = read_values first args).
        { apply read_values_ext. intros arg Harg.
          apply Forall_forall with (x := arg) in IHargs; [ | exact Harg].
          apply IHargs.
          - eapply operand_available; eassumption.
          - eapply noncontrol_operands; eassumption. }
        specialize (Hfirst _ (exit_value_available_at_header_terminator _
          Havailable)).
        specialize (Hfinal _ Havailable).
        cbn [value_coherent] in Hfirst, Hfinal.
        rewrite (supported_operation_modeled _ _ _ Hop) in Hfirst, Hfinal.
        destruct Hfirst as [first_args [first_result
          [Hfirst_read [Hfirst_eval Hfirst_value]]]].
        destruct Hfinal as [final_args [final_result
          [Hfinal_read [Hfinal_eval Hfinal_value]]]].
        rewrite Hargs, Hfirst_read in Hfinal_read. injection Hfinal_read as <-.
        rewrite Hfirst_eval in Hfinal_eval. injection Hfinal_eval as <-.
        cbn [register_of_value]. rewrite Hfirst_value, Hfinal_value.
        reflexivity.
      + apply Houtside. eapply definition_outside_region;
          [exact well_formed | | exact Hdefinition | exact Hsite |
           exact Hinside].
        exact (loop_body_in_graph _ _ _ loop_graph).
    - pose proof (exit_value_available_at_header_terminator _ Havailable)
        as [site [[Hsite Hparameter] [Hblock Hdom]]].
      subst site. destruct (set_mem b loop.(body)) eqn:Hinside.
      + assert (Heq : b = loop.(header)).
        { exact (header_available_body_definition (block_entry_point b) Hdom
          Hinside). }
        subst b. exfalso. apply Hnot. left. exists index. reflexivity.
      + apply Houtside. cbn [register_of_value register_defined_in_region].
        congruence.
  Qed.

  (* Every exit-live register is an available non-control value. Together with
     the frame property, this gives the precise boundary agreement. *)
  Theorem live_out_values_agree : ∀ (first_header_regs exit_regs :
    registers),
    coherent_at blocks entry first_header_regs (loop.(header), 0%nat) ->
    coherent_at blocks entry exit_regs (block_entry_point exit) ->
    (∀ (reg : register), ~ register_defined_in_region loop.(body) reg ->
       exit_regs reg = first_header_regs reg) ->
    agree_on_live (block_entry_point exit) first_header_regs exit_regs.
  Proof.
    intros first final Hfirst Hfinal Houtside reg Hlive.
    assert (Hexit : exit ∈ blocks).
    { eapply graph_successors_closed;
        [exact well_formed | exact (loop_header_in_graph _ _ _ loop_graph) |].
      exact (proj2 certified_exit_edge). }
    destruct (live_at_available blocks entry (block_entry_point exit) reg
      well_formed Hexit Hlive)
      as [v [Hreg Havailable]].
    rewrite <- Hreg in *. symmetry.
    apply noncontrol_values_agree;
      [exact Hfirst | exact Hfinal | exact Houtside | exact Havailable |].
    apply exit_live_not_control. exact Hlive.
  Qed.

  (* The live-out theorem for actual executions. Value equations and
     outside-register stability are discharged here, not left as caller
     assumptions. Only termination of this segment is still a premise. *)
  Theorem finite_exit_live_agreement : ∀
      (initial_regs header_regs first_header_regs exit_regs : registers),
    context_steps (enter entry initial_regs)
      (enter loop.(header) header_regs) ->
    executes_insns header_regs (block_body loop.(header)) first_header_regs ->
    region_steps loop.(body)
      {| current_block := loop.(header); remaining := [];
         current_registers := first_header_regs |} (enter exit exit_regs) ->
    agree_on_live (block_entry_point exit) first_header_regs exit_regs.
  Proof.
    intros initial_regs header_regs first final Hprefix Hbody Hsegment.
    assert (Hfirst : context_steps (enter entry initial_regs)
      {| current_block := loop.(header); remaining := [];
         current_registers := first |}).
    { eapply context_steps_trans; [exact Hprefix |].
      apply context_of_steps, executes_insns_steps. exact Hbody. }
    apply live_out_values_agree.
    - exact (context_execution_values_coherent edges blocks entry initial_regs
      _
        well_formed Hfirst).
    - apply (context_execution_values_coherent edges blocks entry initial_regs
        (enter exit final) well_formed).
      eapply context_steps_trans; [exact Hfirst |].
      apply context_of_steps. exact (region_steps_execution _ _ _ Hsegment).
    - intros reg Houtside.
      apply (region_steps_frame (graph_parameters _ _ well_formed)
        loop.(body) _ _ reg Hsegment); [ | | exact Houtside].
      + exists (block_body loop.(header)). cbn. rewrite app_nil_r. reflexivity.
      + right. exact (proj1 certified_exit_edge).
  Qed.
End ControlValues.
