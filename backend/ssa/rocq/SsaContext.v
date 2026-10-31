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

(* Word observations of surrounding SSA code.

   Integer operations and Goto/Switch use SsaSemantics.step. An opaque
   instruction may produce arbitrary signed-word observations in exactly its
   declared result registers. An opaque terminator may take any CFG successor
   and supply arbitrary word observations for its parameters. Memory and other
   effects are deliberately unconstrained, not treated as no-ops: this relation
   is used only for the unchanged prefix before a loop. The loop's silent
   execution still uses SsaSemantics.step.

   This is an overapproximation, including nondeterministic opaque outputs, not
   a concrete semantics of calls, memory or vector values. Exact equations are
   derived only for [modeled_operation] results. [Push_trap] and [Pop_trap]
   change the trap stack, which no register observes, so they step without
   changing any register; exceptional transfers happen at Call and Raise
   terminators, whose handler is among [all_successors], and are therefore
   opaque transfers like any other. Mid-block operations never transfer control
   in this SSA. DeleteEmptyLoops.graph_supported rejects graphs where an
   exactly modeled operation family has other than one result, so that every
   prefix instruction has a rule here.

   Registers hold a word observation, [option Z] within [in_int64]. A register
   of vector type has no faithful such observation; read it as an arbitrary
   in-range word. No modeled operation reads such a register and no accepted
   loop writes one, so nothing concluded depends on it. Nothing here is
   extracted. *)

From Stdlib Require Import ZArith Lia List Classical_Prop.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlInt64 OxCamlList.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaGraph SsaDominance SsaSemantics
  SsaExecution SsaLiveness SsaProgress.
Import ListNotations.
Local Open Scope Z_scope.

Definition result_register (id : instr_id) (result_count : int)
  (reg : register)
    : Prop :=
  ∃ (index : int),
    reg = Result id index ∧ 0 <= to_Z index < to_Z result_count.

(* An opaque effect writes only its declared outputs. Each output is an
   arbitrary word observation; all other register observations persist. *)
Definition opaque_outputs (written : register -> Prop)
    (before after : registers) : Prop :=
  (∀ (reg : register), ~ written reg -> after reg = before reg)
  ∧ (∀ (reg : register), written reg ->
        ∃ (word : Z), in_int64 word ∧ after reg = Some word).

Inductive context_step : state -> state -> Prop :=
  | Context_integer : ∀ (before after : state),
      step before after -> context_step before after
  | Context_opaque_instruction : ∀ (b : block) (id : instr_id)
      (op : operation) (args : list value) (count : int) (rest : list instr)
      (before after : registers) (words : list Z),
      modeled_operation op args = false ->
      read_values before args = Some words ->
      opaque_outputs (result_register id count) before after ->
      context_step
        {| current_block := b; remaining := Op id op args count :: rest;
           current_registers := before |}
        {| current_block := b; remaining := rest; current_registers := after |}
  | Context_opaque_transfer : ∀ (source target : block)
      (before after : registers),
      terminator_pure_control (terminator source) = false ->
      target ∈ (all_successors source) ->
      opaque_outputs (fun reg => reg ∈ (map register_of_value (params
        target)))
        before after ->
      context_step
        {| current_block := source; remaining := [];
           current_registers := before |}
        (enter target after)
  | Context_trap_marker : ∀ (b : block) (marker : instr)
      (rest : list instr) (regs : registers),
      is_trap_marker marker = true ->
      context_step
        {| current_block := b; remaining := marker :: rest;
           current_registers := regs |}
        {| current_block := b; remaining := rest; current_registers := regs |}.

Inductive context_steps : state -> state -> Prop :=
  | Context_refl : ∀ (s : state), context_steps s s
  | Context_cons : ∀ (before middle after : state),
      context_step before middle -> context_steps middle after ->
      context_steps before after.

(* Pure finite execution is included without changing any step. *)
Lemma context_of_steps : ∀ (before after : state),
  steps before after -> context_steps before after.
Proof.
  intros before after Hrun. induction Hrun; [constructor |].
  eapply Context_cons; [constructor; exact H | exact IHHrun].
Qed.

Lemma context_steps_trans : ∀ (before middle after : state),
  context_steps before middle -> context_steps middle after ->
  context_steps before after.
Proof.
  intros before middle after Hfirst Hlast. induction Hfirst;
    [exact Hlast |]. eapply Context_cons; [exact H |].
  apply IHHfirst. exact Hlast.
Qed.

(* Opaque steps still follow instruction positions and actual CFG edges. *)
Lemma context_step_valid_position : ∀ (before after : state),
  context_step before after -> valid_position before -> valid_position after.
Proof.
  intros before after Hstep Hposition. destruct Hstep.
  - eapply step_valid_position; eassumption.
  - destruct Hposition as [prefix Hbody].
    exists (prefix ++ [Op id op args count]).
    cbn in *. rewrite Hbody, <- app_assoc. reflexivity.
  - apply enter_valid_position.
  - destruct Hposition as [prefix Hbody].
    exists (prefix ++ [marker]).
    cbn in *. rewrite Hbody, <- app_assoc. reflexivity.
Qed.

(* A trap marker occupies an instruction position but defines nothing: no
   result site coincides with the position after it. *)
Lemma trap_marker_no_result : ∀ (b : block) (marker : instr)
    (rest : list instr) (regs : registers) (id : instr_id) (op : operation)
    (args : list value) (count : int),
  is_trap_marker marker = true ->
  valid_position
    {| current_block := b; remaining := marker :: rest;
       current_registers := regs |} ->
  ~ result_definition id op args count (b, length rest).
Proof.
  intros b marker rest regs id op args count Hmarker [prefix Hbody]
    [prefix' [suffix [Hbody' Hlength]]].
  cbn in *. rewrite Hbody in Hbody'.
  pose proof (same_position_element _ _ _ _ _ _ _ Hbody' Hlength) as Heq.
  subst marker. discriminate.
Qed.

(* Nor can the position after a trap marker be a block entry. *)
Lemma trap_marker_not_entry : ∀ (b : block) (marker : instr)
    (rest : list instr) (regs : registers) (target : block),
  valid_position
    {| current_block := b; remaining := marker :: rest;
       current_registers := regs |} ->
  (b, length rest) <> block_entry_point target.
Proof.
  intros b marker rest regs target [prefix Hbody] Hentry.
  unfold block_entry_point in Hentry. injection Hentry as <- Hlength.
  cbn in Hbody. rewrite Hbody, length_app in Hlength. cbn in Hlength. lia.
Qed.

Lemma context_step_program_point_edge : control_edges -> ∀ (before after :
  state),
  valid_position before -> context_step before after ->
  program_point_edge (state_program_point before) (state_program_point after).
Proof.
  intros Hedges before after Hposition Hstep. destruct Hstep.
  - eapply step_program_point_edge; eassumption.
  - unfold state_program_point. cbn. constructor.
    destruct Hposition as [prefix Hbody]. cbn in Hbody.
    rewrite Hbody, length_app. cbn. lia.
  - constructor. exact H0.
  - unfold state_program_point. cbn. constructor.
    destruct Hposition as [prefix Hbody]. cbn in Hbody.
    rewrite Hbody, length_app. cbn. lia.
Qed.

Lemma context_step_in_graph : control_edges -> ∀ (blocks : list block)
    (entry : block) (before after : state),
  well_formed_ssa blocks entry -> (current_block before) ∈ blocks ->
  context_step before after -> (current_block after) ∈ blocks.
Proof.
  intros Hedges blocks entry before after Hwf Hin Hstep. destruct Hstep.
  - eapply step_in_graph; eassumption.
  - exact Hin.
  - eapply graph_successors_closed; eassumption.
  - exact Hin.
Qed.

(* Arbitrary opaque outputs are still machine-word observations. *)
Lemma opaque_outputs_bounded : ∀ (written : register -> Prop)
    (before after : registers),
  bounded_registers before -> opaque_outputs written before after ->
  bounded_registers after.
Proof.
  intros written before after Hbounded [Hframe Houtputs] reg word Hread.
  destruct (classic (written reg)) as [Hwritten | Hunwritten].
  - destruct (Houtputs _ Hwritten) as [result [Hresult Hvalue]].
    rewrite Hread in Hvalue. injection Hvalue as <-. exact Hresult.
  - rewrite Hframe in Hread by exact Hunwritten. eapply Hbounded. exact Hread.
Qed.

Lemma context_step_bounded : ∀ (before after : state),
  bounded_registers (current_registers before) -> context_step before after ->
  bounded_registers (current_registers after).
Proof.
  intros before after Hbounded Hstep. destruct Hstep.
  - eapply step_bounded; eassumption.
  - eapply opaque_outputs_bounded; eassumption.
  - eapply opaque_outputs_bounded; eassumption.
  - exact Hbounded.
Qed.

(* The definition-site frame theorem also holds across opaque effects. SSA
   registers are distinct from mutable memory and are written only by their
   defining instruction or incoming parameter binding. *)
Lemma context_step_definition_frame : ∀ (blocks : list block)
    (entry : block) (before after : state) (v : value) (site : program_point),
  well_formed_ssa blocks entry -> valid_position before ->
  (current_block before) ∈ blocks ->
  value_definition v site -> (fst site) ∈ blocks ->
  site <> state_program_point after -> context_step before after ->
  current_registers after (register_of_value v)
    = current_registers before (register_of_value v).
Proof.
  intros blocks entry before after v site Hwf Hposition Hin Hdefinition Hsite
    Hneq Hstep. destruct Hstep.
  - eapply step_definition_frame; eassumption.
  - apply (proj1 H1). intros Hwritten.
    destruct Hwritten as [index [Hreg Hindex]].
    destruct v as [actual_id actual_op actual_args actual_index | b' index'];
      cbn in Hreg; [ | discriminate].
    injection Hreg as -> ->.
    destruct Hdefinition as [actual_count [Hdefinition Hactual_index]].
    apply Hneq. eapply result_sites_unique;
      [exact Hwf | exact Hsite | exact Hin | exact Hdefinition |].
    eapply executing_result_definition. exact Hposition.
  - apply (proj1 H1). intros Hwritten. apply in_map_iff in Hwritten.
    destruct Hwritten as [parameter [Hreg Hparameter]].
    destruct (parameters_local (graph_parameters _ _ Hwf) _ _ Hparameter)
      as [index ->].
    destruct v as [id op args result_index | b index']; cbn in Hreg;
      [discriminate |]. injection Hreg as -> ->.
    destruct Hdefinition as [Hentry Hparam]. apply Hneq. exact Hentry.
  - reflexivity.
Qed.

(* A modeled result newly available after a context step must have been
   produced by an ordinary integer step, never by an opaque effect. *)
Lemma context_step_new_result : ∀ (before after : state)
    (id : instr_id) (op : operation) (args : list value) (index : int),
  valid_position before -> context_step before after ->
  modeled_operation op args = true ->
  value_definition (Res id op args index) (state_program_point after) ->
  index = zero ∧ execute_instruction (current_registers before)
    (Op id op args one) = Some (current_registers after).
Proof.
  intros before after id op args index Hposition Hstep Hmodeled Hdefinition.
  destruct Hstep.
  - eapply step_new_result; eassumption.
  - destruct Hdefinition as [actual_count [Hdefinition Hindex]].
    pose proof (executing_result_definition _ _ _ _ _ _ _ Hposition)
      as Hactual.
    pose proof (result_definition_at_point _ _ _ _ _ _ _ _ _ Hdefinition
      Hactual) as Heq.
    injection Heq as -> -> -> ->. congruence.
  - destruct Hdefinition as [count [Hdefinition Hindex]].
    exfalso. eapply result_definition_not_entry. exact Hdefinition.
  - destruct Hdefinition as [count [Hdefinition Hindex]].
    exfalso. eapply trap_marker_no_result; eassumption.
Qed.

(* Available operands of a modeled result are unchanged by an incoming step,
   including an opaque step elsewhere in the graph. *)
Lemma context_available_operands_frame : ∀ (blocks : list block)
    (entry : block) (before after : state) (id : instr_id) (op : operation)
    (args : list value) (count : int) (site : program_point),
  well_formed_ssa blocks entry -> valid_position before ->
  (current_block before) ∈ blocks ->
  program_point_reachable entry (state_program_point after) ->
  result_definition id op args count site -> (fst site) ∈ blocks ->
  program_point_dominates entry site (state_program_point after) ->
    context_step before after ->
  read_values (current_registers after) args
    = read_values (current_registers before) args.
Proof.
  intros blocks entry before after id op args count site Hwf Hposition Hin
    Hreachable Hdefinition Hsite Hdom Hstep.
  apply read_values_ext. intros arg Harg.
  destruct (operand_definitions _ _ Hwf _ _ _ _ _ Hsite Hdefinition _ Harg)
    as [arg_site [Harg_definition [Harg_site [Harg_dom Hstrict]]]].
  eapply context_step_definition_frame; try eassumption.
  intros Heq. subst arg_site.
  assert (site = state_program_point after) by
    (eapply program_point_dominance_antisymmetric; eassumption).
  congruence.
Qed.

(* Exact available-value equations follow from context execution too. Opaque
   results carry no arithmetic equation; their observed registers remain
   available as atoms to the affine analysis. *)
Theorem context_step_values_coherent : control_edges ->
  ∀ (blocks : list block) (entry : block) (before after : state),
  well_formed_ssa blocks entry -> valid_position before ->
  (current_block before) ∈ blocks ->
  program_point_reachable entry (state_program_point before) ->
  coherent_at blocks entry (current_registers before)
    (state_program_point before) ->
  context_step before after ->
  coherent_at blocks entry (current_registers after)
    (state_program_point after).
Proof.
  intros Hedges blocks entry before after Hwf Hposition Hin Hreachable
    Hcoherent Hstep v [site [Hdefinition [Hsite Hdom]]].
  destruct v as [id op args index | b index]; [ | exact I].
  cbn [value_coherent].
  destruct (modeled_operation op args) eqn:Hmodeled; [ | exact I].
  pose proof Hdefinition as [count [Hresult Hindex]].
  pose proof (context_step_program_point_edge Hedges _ _ Hposition Hstep) as
    Hedge.
  assert (Hreachable_after : program_point_reachable entry (state_program_point
    after)).
  { destruct Hreachable as [points Hpath].
    exists (points ++ [state_program_point after]).
    eapply program_point_path_snoc; eassumption. }
  assert (Hargs : read_values (current_registers after) args
    = read_values (current_registers before) args).
  { eapply context_available_operands_frame; eassumption. }
  destruct (program_point_eqb site (state_program_point after)) eqn:Heq.
  - apply program_point_eqb_spec in Heq. subst site.
    destruct (context_step_new_result _ _ _ _ _ _ Hposition Hstep Hmodeled
      Hdefinition) as [-> Hexec].
    destruct (execute_instruction_result _ _ _ _ _ _ Hexec)
      as [_ [words [result [Hread [Heval Hregs]]]]].
    exists words, result. split; [rewrite Hargs; exact Hread |].
    split; [exact Heval |]. rewrite Hregs. apply write_register_same.
  - assert (Hneq : site <> state_program_point after).
    { intros ->. rewrite (proj2 (program_point_eqb_spec _ _) eq_refl) in Heq.
      discriminate. }
    assert (Havailable : available_value blocks entry (state_program_point
      before)
      (Res id op args index)).
    { exists site. split; [exact Hdefinition |]. split; [exact Hsite |].
      eapply program_point_dominates_predecessor; eassumption. }
    specialize (Hcoherent _ Havailable). cbn [value_coherent] in Hcoherent.
    rewrite Hmodeled in Hcoherent.
    destruct Hcoherent as [words [result [Hread [Heval Hvalue]]]].
    exists words, result. split; [rewrite Hargs; exact Hread |].
    split; [exact Heval |]. rewrite <- Hvalue.
    eapply context_step_definition_frame with (v := Res id op args index)
      (site := site); eassumption.
Qed.

(* Opaque outputs, like ordinary results and Goto bindings, define every live
   register introduced by their step. *)
Lemma context_step_new_live_register_defined : ∀ (blocks : list block)
    (entry : block) (before after : state) (reg : register),
  well_formed_ssa blocks entry -> control_layout blocks ->
  valid_position before -> (current_block before) ∈ blocks ->
  register_defined_at (state_program_point after) reg -> register_used blocks
    reg ->
  context_step before after ->
  ∃ (word : Z), current_registers after reg = Some word.
Proof.
  intros blocks entry before after reg Hwf Hlayout Hposition Hin Hdefinition
    Hused Hstep. destruct Hstep.
  - eapply step_new_live_register_defined; eassumption.
  - destruct Hdefinition as [v [Hdefinition <-]].
    destruct v as [actual_id actual_op actual_args index |
      parameter_block index].
    + destruct Hdefinition as [actual_count [Hdefinition Hindex]].
      pose proof (executing_result_definition _ _ _ _ _ _ _ Hposition)
        as Hactual.
      pose proof (result_definition_at_point _ _ _ _ _ _ _ _ _ Hdefinition
        Hactual) as Heq. injection Heq as -> -> -> ->.
      destruct (proj2 H1 (Result id index) ltac:(exists index; auto))
        as [word [Hbounded Hword]]. exists word. exact Hword.
    + destruct Hdefinition as [Hentry Hparam].
      cbn [state_program_point] in Hentry. unfold block_entry_point in Hentry.
      injection Hentry as Hblock Hlength. subst parameter_block.
      destruct Hposition as [prefix Hbody]. cbn in Hbody.
      rewrite Hbody, length_app in Hlength. cbn in Hlength. lia.
  - destruct Hdefinition as [v [Hdefinition <-]].
    destruct v as [id op args index | parameter_block index].
    + destruct Hdefinition as [count [Hdefinition Hindex]].
      exfalso. eapply result_definition_not_entry. exact Hdefinition.
    + destruct Hdefinition as [Hentry Hparam].
      apply (f_equal fst) in Hentry. cbn in Hentry. subst parameter_block.
      assert (Hin_param : register_of_value (Block_param target index)
                ∈ map register_of_value (params target)).
      { apply in_map. eapply OxCamlList.nth_int_In. exact Hparam. }
      destruct (proj2 H1 _ Hin_param) as [word [Hbounded Hword]].
      exists word. exact Hword.
  - destruct Hdefinition as [v [Hdefinition <-]].
    destruct v as [id op args index | parameter_block index].
    + destruct Hdefinition as [count [Hdefinition Hindex]].
      exfalso. eapply trap_marker_no_result; eassumption.
    + destruct Hdefinition as [Hentry Hparam].
      exfalso. eapply trap_marker_not_entry; eassumption.
Qed.

(* Live-register definedness holds across the context overapproximation; it is
   not an assumption about an arbitrary reached snapshot. *)
Lemma context_step_live_registers_defined : control_edges ->
  ∀ (blocks : list block) (entry : block) (before after : state),
  well_formed_ssa blocks entry -> control_layout blocks ->
  valid_position before -> (current_block before) ∈ blocks ->
  live_registers_defined (state_program_point before)
    (current_registers before) ->
  context_step before after ->
  live_registers_defined (state_program_point after) (current_registers after).
Proof.
  intros Hedges blocks entry before after Hwf Hlayout Hposition Hin Hbound
    Hstep reg Hlive.
  pose proof (context_step_in_graph Hedges _ _ _ _ Hwf Hin Hstep) as Hafter.
  destruct (classic (register_defined_at (state_program_point after) reg)) as
    [Hnew | Hold].
  - eapply context_step_new_live_register_defined; try eassumption.
    exact (live_register_used blocks entry (state_program_point after) reg
      Hwf Hafter Hlive).
  - assert (Hlive_before : register_live_at (state_program_point before) reg).
    { eapply Live_predecessor; [ | exact Hold | exact Hlive].
      eapply context_step_program_point_edge; eassumption. }
    destruct (Hbound _ Hlive_before) as [word Hword]. exists word.
    destruct (live_at_available blocks entry (state_program_point after) reg
      Hwf Hafter Hlive) as [v [Hreg [site [Hdefinition [Hsite Hdom]]]]].
    assert (Hneq : site <> state_program_point after).
    { intros Heq. subst site. apply Hold. exists v. auto. }
    rewrite <- Hreg.
    rewrite (context_step_definition_frame blocks entry before after v site
      Hwf Hposition Hin Hdefinition Hsite Hneq Hstep).
    rewrite Hreg. exact Hword.
Qed.

(* Finite context prefixes preserve valid program positions. *)
Lemma context_steps_valid_position : ∀ (before after : state),
  context_steps before after -> valid_position before -> valid_position after.
Proof.
  intros before after Hrun. induction Hrun; intros Hposition;
    [exact Hposition |]. apply IHHrun.
  eapply context_step_valid_position; eassumption.
Qed.

(* The observations of a finite context prefix fit signed words. *)
Theorem context_execution_invariants : ∀ (entry : block)
    (initial_regs : registers) (reached : state),
  bounded_registers initial_regs ->
  context_steps (enter entry initial_regs) reached ->
  valid_position reached ∧ bounded_registers (current_registers reached).
Proof.
  intros entry initial_regs reached Hbounded Hrun. split.
  - eapply context_steps_valid_position;
      [exact Hrun | apply enter_valid_position].
  - assert (Hpreserved : forall (before after : state),
      context_steps before after ->
      bounded_registers (current_registers before) ->
      bounded_registers (current_registers after)).
    { intros before after Hsteps. induction Hsteps; intros Hbefore;
        [exact Hbefore |]. apply IHHsteps.
      eapply context_step_bounded; eassumption. }
    eapply Hpreserved; eassumption.
Qed.

(* Context paths project to the same graph used by dominance. *)
Lemma context_steps_program_point_reachable : control_edges ->
  ∀ (entry : block) (before after : state),
  context_steps before after -> valid_position before ->
  program_point_reachable entry (state_program_point before) ->
  program_point_reachable entry (state_program_point after).
Proof.
  intros Hedges entry before after Hrun. induction Hrun;
    intros Hposition Hreachable; [exact Hreachable |].
  apply IHHrun.
  - eapply context_step_valid_position; eassumption.
  - destruct Hreachable as [points Hpath].
    exists (points ++ [state_program_point middle]).
    eapply program_point_path_snoc; [exact Hpath |].
    eapply context_step_program_point_edge; eassumption.
Qed.

(* Context execution derives exact equations only for modeled results; opaque
   computations before the loop remain unconstrained atoms. *)
Theorem context_execution_values_coherent : control_edges ->
  ∀ (blocks : list block) (entry : block) (initial_regs : registers)
    (reached : state),
  well_formed_ssa blocks entry ->
  context_steps (enter entry initial_regs) reached ->
  coherent_at blocks entry (current_registers reached)
    (state_program_point reached).
Proof.
  intros Hedges blocks entry initial_regs reached Hwf Hrun.
  assert (Hpreserved : forall (before after : state),
    context_steps before after ->
    valid_position before -> (current_block before) ∈ blocks ->
    program_point_reachable entry (state_program_point before) ->
    coherent_at blocks entry (current_registers before)
      (state_program_point before) ->
    coherent_at blocks entry (current_registers after)
      (state_program_point after)).
  { intros before after Hsteps. induction Hsteps;
      intros Hposition Hin Hreachable Hcoherent; [exact Hcoherent |].
    apply IHHsteps.
    - eapply context_step_valid_position; eassumption.
    - eapply context_step_in_graph; eassumption.
    - eapply context_steps_program_point_reachable; [exact Hedges | | exact
      Hposition |
        exact Hreachable].
      eapply Context_cons; [exact H | constructor].
    - eapply context_step_values_coherent; eassumption. }
  eapply Hpreserved; [exact Hrun | apply enter_valid_position |
    exact (entry_in_graph _ _ Hwf) | | apply entry_values_coherent].
  exists [block_entry_point entry]. constructor.
Qed.

(* Bound function inputs suffice for all live observations in a context prefix,
   including after opaque instructions and calls. *)
Theorem context_execution_live_registers_defined : control_edges ->
  ∀ (blocks : list block) (entry : block) (initial_regs : registers)
    (reached : state),
  well_formed_ssa blocks entry -> control_layout blocks ->
  live_registers_defined (block_entry_point entry) initial_regs ->
  context_steps (enter entry initial_regs) reached ->
  live_registers_defined (state_program_point reached)
    (current_registers reached).
Proof.
  intros Hedges blocks entry initial_regs reached Hwf Hlayout Hinputs Hrun.
  assert (Hpreserved : forall (before after : state),
    context_steps before after ->
    valid_position before -> (current_block before) ∈ blocks ->
    live_registers_defined (state_program_point before)
      (current_registers before) ->
    live_registers_defined (state_program_point after)
      (current_registers after)).
  { intros before after Hsteps. induction Hsteps;
      intros Hposition Hin Hbound; [exact Hbound |].
    apply IHHsteps.
    - eapply context_step_valid_position; eassumption.
    - eapply context_step_in_graph; eassumption.
    - eapply context_step_live_registers_defined; eassumption. }
  eapply Hpreserved; [exact Hrun | apply enter_valid_position |
    exact (entry_in_graph _ _ Hwf) | exact Hinputs].
Qed.

(* The inputs the eligibility theorem speaks of: the function is entered with
   machine words in its registers and its live parameters bound, the
   observation semantics takes it to [header] with registers [header_regs], and
   from there the exact execution takes only defined Switch indices
   (out-of-range indices are undefined behaviour in SSA). *)
Record reached_header (entry header : block)
    (initial_regs header_regs : registers) : Prop :=
  { inputs_bounded : bounded_registers initial_regs;
    live_inputs_defined : live_registers_defined (block_entry_point entry)
      initial_regs;
    header_reached :
      context_steps (enter entry initial_regs) (enter header header_regs);
    switches_defined : switches_in_range (enter header header_regs) }.
