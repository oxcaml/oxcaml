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

(* The arithmetic of a loop counter in actual SSA execution.

   [counter_iteration] derives the update from an actual finite traversal and
   its back-edge argument. [accepted_loop_finite_exit] then iterates up to the
   arithmetic exit witness from LoopComparisons.v, unless an earlier traversal
   exits. No infinite trace or sampled counter valuation is used.

   LoopInvariantExecution.v proves bound constancy on finite segments;
   LoopRangeExecution.v supplies affine range facts from the actual prefix.
   SsaContext.v permits opaque surrounding computations in that prefix and
   proves the available-value equations used here. LoopEligibility.v discharges
   progress and combines finite exit with live-out agreement.

   Review the observation definitions and [accepted_loop_finite_exit], together
   with the graph contracts in their premises. The mechanical graph rewrite is
   deliberately outside this proof. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlInt64 OxCamlList.
From OxCamlBackend Require Import Operation IntegerComparison.
From OxCamlSsa Require Import SsaModel SsaGraph SsaSemantics SsaExecution
  SsaContext.
From OxCamlLoop Require Import NaturalLoop InductionVar LoopTermination
  LoopComparisons DeleteEmptyLoops LoopExecution LoopInvariantExecution
  LoopRangeExecution.
Import ListNotations.
Local Open Scope Z_scope.

(* ======================================================================== *)
(* Static coverage and observations (nothing in this file is extracted)     *)
(* ======================================================================== *)

(* The comparison may test the parameter or its just-computed update. This
   choice depends on the reference, not on the execution state. *)
Definition tested_counter (loop : natural_loop) (biv : biv)
    (counter_ref : value) (step_size : int) (counter : Z) : Z :=
  if is_param loop.(header) biv.(param_index) counter_ref then counter
  else wrap64 (counter + to_Z step_size).

(* The concrete instruction shape of an extracted comparison. *)
Definition comparison_shape (condition : value) (cmp : integer_comparison)
    (iv_is_left : bool) (counter_ref : value)
      (comparison_bound : bound) : Prop :=
  ∃ (id : instr_id) (index : int),
    match comparison_bound with
    | Const_bound constant =>
      iv_is_left = true
      ∧ condition =
           Res id (Intop_imm (Icomp cmp) constant) [counter_ref] index
    | Value_bound bound_ref =>
      condition = Res id (Intop (Icomp cmp))
        (if iv_is_left then [counter_ref; bound_ref]
         else [bound_ref; counter_ref]) index
    end.

Definition read_comparison_bound (regs : registers)
  (comparison_bound : bound) : option Z :=
  match comparison_bound with
  | Const_bound constant => Some (to_Z constant)
  | Value_bound bound_ref => regs (register_of_value bound_ref)
  end.

(* ======================================================================== *)
(* Operational lemmas                                                      *)
(* ======================================================================== *)

(* A reported counter index is in range: an update is passed at that position
   by a Goto whose argument list matches the header parameters. *)
Lemma biv_parameter_identity : parameter_layout ->
  ∀ (defs : op_def) (loop : natural_loop) (biv : biv),
  biv_spec defs loop biv ->
  param loop.(header) biv.(param_index)
    = Block_param loop.(header) biv.(param_index).
Proof.
  intros Hlayout defs loop biv [_ [Hnonempty [_ [_ Harrives]]]].
  destruct biv.(update) as [ | updated updates] eqn:Hupdates;
    [contradiction |].
  assert (Hin : updated ∈ biv.(update)).
  { rewrite Hupdates. left. reflexivity. }
  destruct (Harrives updated Hin) as [source [args [_ [Hgoto Harg]]]].
  destruct (same_length_nth_int args (params loop.(header)) biv.(param_index)
    (Terminator.Arg updated) (goto_args_arity _ _ _ Hgoto)
    (arg_value_nth _ _ _ Harg)) as [parameter Hparameter].
  rewrite (parameter_lookup Hlayout _ _ _ Hparameter).
  apply (parameter_identity Hlayout _ _ _ Hparameter).
Qed.

(* An actual update result is the current parameter plus the signed step, with
   machine wraparound. Both the parameter and the result are bound. *)
Lemma biv_update_observed : ∀ (defs : op_def) (loop : natural_loop)
    (biv : biv) (step_size : int) (updated : value) (regs : registers),
  biv_spec defs loop biv -> signed_step biv = Some step_size ->
  updated ∈ biv.(update) -> value_coherent regs updated ->
  ∃ (counter : Z),
    regs (register_of_value (param loop.(header) biv.(param_index)))
      = Some counter
    ∧ regs (register_of_value updated)
         = Some (wrap64 (counter + to_Z step_size)).
Proof.
  intros defs loop biv step_size updated regs Hspec Hstep Hin Hcoherent.
  destruct Hspec as [_ [_ [Hupdates _]]].
  destruct (Hupdates updated Hin)
    as [step' [sign' [Hclass [Hsame Hsign]]]].
  unfold signed_step in Hstep.
  destruct biv.(step) as [constant | variable] eqn:Hbiv_step;
    [ | discriminate].
  destruct step' as [constant' | variable']; cbn [step_same] in Hsame;
    [ | discriminate].
  apply eqb_int in Hsame. subst constant'.
  destruct (classify_update_const defs loop _ _ _ _ Hclass)
    as [id [index [self [Hself Hshape]]]].
  unfold is_param in Hself. apply value_eqb_spec in Hself. subst self.
  destruct biv.(sign) eqn:Hbiv_sign.
  - injection Hstep as <-.
    destruct sign'; cbn [sign_same] in Hsign; [ | discriminate].
    destruct Hshape as [[_ Hshape] | [Hbad _]]; [ | discriminate].
    subst updated.
    destruct Hcoherent as [values [result [Hread [Heval Hvalue]]]].
    cbn [read_values] in Hread.
    destruct (regs (register_of_value (param loop.(header) biv.(param_index))))
      as [counter |] eqn:Hcounter; [ | discriminate].
    injection Hread as <-. cbn [eval_operation] in Heval.
    injection Heval as <-. exists counter. split; [reflexivity |].
    exact Hvalue.
  - destruct (constant =? min_int)%sint63 eqn:Hmin; [discriminate |].
    injection Hstep as <-.
    destruct sign'; cbn [sign_same] in Hsign; [discriminate |].
    destruct Hshape as [[Hbad _] | [_ Hshape]]; [discriminate |].
    subst updated.
    destruct Hcoherent as [values [result [Hread [Heval Hvalue]]]].
    cbn [read_values] in Hread.
    destruct (regs (register_of_value (param loop.(header) biv.(param_index))))
      as [counter |] eqn:Hcounter; [ | discriminate].
    injection Hread as <-. cbn [eval_operation] in Heval.
    injection Heval as <-. exists counter. split; [reflexivity |].
    rewrite Sint63.to_Z_opp by (apply eqb_int_false; exact Hmin).
    replace (counter + - to_Z constant) with (counter - to_Z constant) by ring.
    exact Hvalue.
Qed.

(* Observing an accepted counter operand determines the header parameter and
   whether the comparison sees it before or after the update. *)
Lemma biv_operand_observed : ∀ (defs : op_def) (loop : natural_loop)
    (biv : biv) (step_size : int) (counter_ref : value) (regs : registers)
    (observed : Z),
  biv_spec defs loop biv -> signed_step biv = Some step_size ->
  is_param loop.(header) biv.(param_index) counter_ref
    || existsb (value_eqb counter_ref) biv.(update) = true ->
  value_coherent regs counter_ref ->
  regs (register_of_value counter_ref) = Some observed ->
  ∃ (counter : Z),
    regs (register_of_value (param loop.(header) biv.(param_index)))
      = Some counter
    ∧ observed = tested_counter loop biv counter_ref step_size counter.
Proof.
  intros defs loop biv step_size counter_ref regs observed Hspec Hstep
    Haccepted Hcoherent Hread.
  unfold tested_counter.
  destruct (is_param loop.(header) biv.(param_index) counter_ref) eqn:Hself.
  - unfold is_param in Hself. apply value_eqb_spec in Hself. subst counter_ref.
    exists observed. auto.
  - cbn in Haccepted. apply existsb_exists in Haccepted.
    destruct Haccepted as [updated [Hin Heq]].
    apply value_eqb_spec in Heq. subst counter_ref.
    destruct (biv_update_observed _ _ _ _ _ _ Hspec Hstep Hin Hcoherent)
      as [counter [Hcounter Hupdated]].
    rewrite Hread in Hupdated. injection Hupdated as ->.
    exists counter. auto.
Qed.

(* The comparison recognizer supplies an actual counter operand and the
   instruction shape, not a hypothesis about the values it computes. *)
Lemma extract_comparison_shape : ∀ (is_counter is_bound : value -> bool)
    (condition : value) (cmp : integer_comparison) (iv_is_left : bool)
    (comparison_bound : bound),
  extract_comparison is_counter is_bound condition
    = Some (cmp, iv_is_left, comparison_bound) ->
  ∃ (counter_ref : value), is_counter counter_ref = true
    ∧ (match comparison_bound with Const_bound _ => True
        | Value_bound bound_ref => is_bound bound_ref = true end)
    ∧ comparison_shape condition cmp iv_is_left counter_ref comparison_bound.
Proof.
  intros is_counter is_bound condition cmp iv_is_left bnd Hextract.
  destruct (extract_comparison_spec _ _ _ _ _ _ Hextract)
    as [id [index Hcases]].
  destruct Hcases as [[counter [constant [-> [Hcounter [-> ->]]]]] |
    [[counter [bound_ref [-> [Hcounter [Hbound [-> ->]]]]]] |
     [bound_ref [counter [-> [Hcounter [Hbound [-> ->]]]]]]]].
  - exists counter. split; [exact Hcounter |]. split; [exact I |].
    exists id, index. auto.
  - exists counter. split; [exact Hcounter |]. split; [exact Hbound |].
    exists id, index. reflexivity.
  - exists counter. split; [exact Hcounter |]. split; [exact Hbound |].
    exists id, index. reflexivity.
Qed.

(* Operands of the actual comparison are available at every point where its
   result is available. This includes an update tested by the header. *)
Lemma comparison_operands_available : ∀ (blocks : list block)
    (entry : block) (position : SsaDominance.program_point) (condition : value)
    (cmp : integer_comparison) (iv_is_left : bool) (counter_ref : value)
    (comparison_bound : bound),
  well_formed_ssa blocks entry ->
  available_value blocks entry position condition ->
  comparison_shape condition cmp iv_is_left counter_ref comparison_bound ->
  available_value blocks entry position counter_ref
    ∧ match comparison_bound with Const_bound _ => True
       | Value_bound bound_ref =>
         available_value blocks entry position bound_ref end.
Proof.
  intros blocks entry position condition cmp iv_is_left counter_ref bnd
    Hwf Havailable [id [index Hshape]].
  destruct bnd as [bound_ref | constant].
  - subst condition. destruct iv_is_left; split;
      eapply operand_available; try eassumption; cbn; auto.
  - destruct Hshape as [-> ->]. split; [ | exact I].
    eapply operand_available; try eassumption; cbn; auto.
Qed.

(* Evaluating the comparison reads these two operand values and produces
   exactly its 0/1 result, for either orientation and either kind of bound. *)
Lemma comparison_values_observed : ∀ (condition : value)
    (cmp : integer_comparison) (iv_is_left : bool) (counter_ref : value)
    (comparison_bound : bound) (regs : registers),
  comparison_shape condition cmp iv_is_left counter_ref comparison_bound ->
  value_coherent regs condition ->
  ∃ (counter_word bound_word : Z),
    regs (register_of_value counter_ref) = Some counter_word
    ∧ read_comparison_bound regs comparison_bound = Some bound_word
    ∧ regs (register_of_value condition) = Some (bool_to_Z
         (if iv_is_left then IntegerComparison.eval cmp counter_word bound_word
          else IntegerComparison.eval cmp bound_word counter_word)).
Proof.
  intros condition cmp iv_is_left counter_ref bnd regs [id [index Hshape]]
    Hcoherent.
  destruct bnd as [bound_ref | constant].
  - subst condition. destruct iv_is_left;
      destruct Hcoherent as [values [result [Hread [Heval Hvalue]]]];
      cbn [read_values] in Hread.
    + destruct (regs (register_of_value counter_ref)) as [counter_word |]
        eqn:Hcounter; [ | discriminate].
      destruct (regs (register_of_value bound_ref)) as [bound_word |]
        eqn:Hbound; [ | discriminate].
      injection Hread as <-. cbn [eval_operation] in Heval.
      injection Heval as <-. exists counter_word, bound_word.
      split; [reflexivity |]. split.
      * unfold read_comparison_bound. exact Hbound.
      * exact Hvalue.
    + destruct (regs (register_of_value bound_ref)) as [bound_word |]
        eqn:Hbound; [ | discriminate].
      destruct (regs (register_of_value counter_ref)) as [counter_word |]
        eqn:Hcounter; [ | discriminate].
      injection Hread as <-. cbn [eval_operation] in Heval.
      injection Heval as <-. exists counter_word, bound_word.
      split; [reflexivity |]. split.
      * unfold read_comparison_bound. exact Hbound.
      * exact Hvalue.
  - destruct Hshape as [-> ->].
    destruct Hcoherent as [values [result [Hread [Heval Hvalue]]]].
    cbn [read_values] in Hread.
    destruct (regs (register_of_value counter_ref)) as [counter_word |]
      eqn:Hcounter; [ | discriminate].
    injection Hread as <-. cbn [eval_operation] in Heval.
    injection Heval as <-. exists counter_word, (to_Z constant).
    split; [reflexivity |]. split; [reflexivity | exact Hvalue].
Qed.

(* A bound that was actually read fits a machine register. *)
Lemma read_comparison_bound_bounded : ∀ (regs : registers)
  (comparison_bound : bound) (word : Z),
  bounded_registers regs -> read_comparison_bound regs comparison_bound = Some
    word -> in_int64 word.
Proof.
  intros regs [bound_ref | constant] word Hbounded Hread.
  - eapply Hbounded. exact Hread.
  - injection Hread as <-. pose proof (int_bounds constant).
    unfold in_int64, two63, two62 in *. lia.
Qed.

(* Testing the updated value shifts the same IV sequence by one step. *)
Lemma tested_counter_sequence : ∀ (loop : natural_loop) (biv : biv)
    (counter_ref : value) (step_size : int) (counter : Z) (iteration : nat),
  tested_counter loop biv counter_ref step_size
    (counter_after_iterations counter (to_Z step_size) iteration)
  = counter_after_iterations (tested_counter loop biv counter_ref step_size
    counter)
      (to_Z step_size) iteration.
Proof.
  intros loop biv counter_ref step_size counter iteration.
  unfold tested_counter. destruct (is_param _ _ _); [reflexivity |].
  rewrite <- counter_after_iterations_shift. reflexivity.
Qed.

Section CounterExecution.
  Variable blocks : list block.
  Variable entry : block.
  Variable defs : op_def.
  Variable loop : natural_loop.
  Variable bivs : list biv.
  Variable exit : block.
  Variable initial_regs header_regs : registers.

  Hypothesis edges : control_edges.
  Hypothesis well_formed : well_formed_ssa blocks entry.
  Hypothesis loop_graph : loop_graph_properties blocks entry loop.
  Hypothesis certificate : deletion_certificate defs blocks loop bivs exit.
  Let start := enter loop.(header) header_regs.
  Hypothesis prefix : context_steps (enter entry initial_regs) start.
  Hypothesis progress : ∀ (reached : state),
    region_steps loop.(body) start reached ->
    set_mem (current_block reached) loop.(body) = true ->
    ∃ (next : state), SsaSemantics.step reached next.

  (* The header test is an actual terminator operand and is therefore available
     after the header instructions have executed. *)
  Lemma header_comparison_available : ∀ (exit_info : exit_branch)
      (reached : state),
    find_exit_branch loop = Some exit_info ->
    current_block reached = loop.(header) ->
    remaining reached = [] ->
    available_value blocks entry (state_program_point reached)
      exit_info.(condition).
  Proof.
    intros exit_info reached Hexit Hblock Hfinished.
    destruct (find_exit_branch_spec loop exit_info Hexit)
      as [ifnot [ifso [Hterm _]]].
    eapply terminator_operand_available with (args := [exit_info.(condition)]);
      [exact well_formed | | exact Hfinished | | left; reflexivity].
    - rewrite Hblock. exact (loop_header_in_graph _ _ _ loop_graph).
    - rewrite Hblock, Hterm. cbn [terminator_args]. left. reflexivity.
  Qed.

  (* The header's comparison observes the actual counter (or its update) and
     bound, and its result is the corresponding machine boolean. *)
  Lemma header_comparison_observed : ∀ (biv : biv) (step_size : int)
      (exit_info : exit_branch) (cmp : integer_comparison) (iv_is_left : bool)
      (counter_ref : value) (comparison_bound : bound) (reached : state),
    biv_spec defs loop biv ->
    region_steps loop.(body) start reached ->
    signed_step biv = Some step_size ->
    find_exit_branch loop = Some exit_info ->
    comparison_shape exit_info.(condition) cmp iv_is_left counter_ref
      comparison_bound ->
    is_param loop.(header) biv.(param_index) counter_ref
      || existsb (value_eqb counter_ref) biv.(update) = true ->
    current_block reached = loop.(header) ->
    remaining reached = [] ->
    ∃ (counter bound_word : Z),
      current_registers reached (Param loop.(header) biv.(param_index))
        = Some counter
      ∧ read_comparison_bound (current_registers reached) comparison_bound =
        Some bound_word
      ∧ current_registers reached
           (register_of_value exit_info.(condition))
           = Some (bool_to_Z
               (if iv_is_left
                then IntegerComparison.eval cmp
                  (tested_counter loop biv counter_ref step_size counter)
                  bound_word
                else IntegerComparison.eval cmp bound_word
                  (tested_counter loop biv counter_ref step_size counter))).
  Proof.
    intros biv step_size exit_info cmp iv_is_left counter_ref bnd reached
      Hspec Hrun Hstep Hexit Hshape Haccepted Hblock Hfinished.
    pose proof (header_comparison_available exit_info reached
      Hexit Hblock Hfinished) as Havailable.
    pose proof (context_execution_values_coherent edges blocks entry
      initial_regs reached well_formed
      (context_steps_trans _ _ _ prefix
        (context_of_steps _ _ (region_steps_execution _ _ _ Hrun))))
      as Hvalues.
    destruct (comparison_operands_available _ _ _ _ _ _ _ _ well_formed
      Havailable Hshape) as [Hcounter_available _].
    destruct (comparison_values_observed _ _ _ _ _ _ Hshape
      (Hvalues _ Havailable))
      as [observed [bound_word [Hcounter_read [Hbound Hcondition]]]].
    destruct (biv_operand_observed _ _ _ _ _ _ _ Hspec Hstep Haccepted
      (Hvalues _ Hcounter_available) Hcounter_read)
      as [counter [Hcounter Htested]].
    pose proof (biv_parameter_identity (graph_parameters _ _ well_formed)
      defs loop biv Hspec) as Hparam.
    rewrite Hparam in Hcounter. cbn [register_of_value] in Hcounter.
    exists counter, bound_word. split; [exact Hcounter |].
    split; [exact Hbound |]. rewrite Htested in Hcondition. exact Hcondition.
  Qed.

  (* One traversal either exits or reaches the next header test with the
     counter advanced once. Every equation comes from an actual SSA read. *)
  Lemma counter_iteration : ∀ (biv : biv) (step_size : int)
      (header_test_state : state) (counter : Z),
    biv_spec defs loop biv -> signed_step biv = Some step_size ->
    region_steps loop.(body) start header_test_state ->
    current_block header_test_state = loop.(header) ->
    remaining header_test_state = [] ->
    current_registers header_test_state (Param loop.(header) biv.(param_index))
      = Some counter ->
    (∃ (exit_state : state),
       region_steps loop.(body) start exit_state
       ∧ set_mem (current_block exit_state) loop.(body) = false)
    ∨ ∃ (next_header_test_state : state),
       region_steps loop.(body) start next_header_test_state
       ∧ current_block next_header_test_state = loop.(header)
       ∧ remaining next_header_test_state = []
       ∧ current_registers next_header_test_state
            (Param loop.(header) biv.(param_index))
          = Some (wrap64 (counter + to_Z step_size)).
  Proof.
    intros biv step_size test counter Hspec Hstep Hrun Hheader Hfinished
      Hcounter.
    pose proof (graph_parameters _ _ well_formed) as Hlayout.
    pose proof (biv_parameter_identity Hlayout defs loop biv Hspec) as Hparam.
    pose proof certificate as [_ [_ [Hacyclic _]]].
    assert (Hinside : set_mem (current_block test) loop.(body) = true).
    { rewrite Hheader. exact (loop_header_in_body _ _ _ loop_graph). }
    destruct (region_iteration edges Hlayout loop.(body) loop.(header) test
      Hacyclic Hfinished Hinside) as [before [after
        [Hbetween [Htransfer [Hbefore [Hdone [Htarget [Hentry Hsame]]]]]]]].
    { intros reached Hsegment Hin. apply progress; [ | exact Hin].
      eapply region_steps_trans; eassumption. }
    assert (Hbefore_run : region_steps loop.(body) start before).
    { eapply region_steps_trans; eassumption. }
    assert (Hafter_run : region_steps loop.(body) start after).
    { eapply region_steps_trans; [exact Hbefore_run |].
      eapply Region_cons; [exact Hbefore | exact Htransfer | constructor]. }
    destruct Htarget as [Houtside | Hback_header].
    - left. exists after. auto.
    - right.
      destruct (loop_back_edge_step blocks entry defs loop bivs exit edges
        Hlayout loop_graph certificate before after Hdone Hbefore Htransfer
        Hback_header) as [args [Hback [Hpred [Hgoto Hbinding]]]].
      pose proof Hspec as [_ [_ [_ [Hupdates _]]]].
      destruct (Hupdates _ Hback Hpred)
        as [arguments [updated [Harguments [Harg Hin_updated]]]].
      pose proof (goto_args_terminator _ _ _ Harguments) as Hterm.
      rewrite Hgoto in Hterm. injection Hterm as <-.
      pose proof (arg_value_nth _ _ _ Harg) as Harg_index.
      destruct (continue_argument_used (Goto loop.(header)) args
        biv.(param_index) updated Harg_index) as [uses [Huses Hused]].
      assert (Havailable : available_value blocks entry
        (state_program_point before) updated).
      { eapply terminator_operand_available;
          [exact well_formed | | exact Hdone | | exact Hused].
        - apply (loop_body_in_graph _ _ _ loop_graph). exact Hbefore.
        - rewrite Hgoto. exact Huses. }
      pose proof (context_execution_values_coherent edges blocks entry
        initial_regs before well_formed
        (context_steps_trans _ _ _ prefix
          (context_of_steps _ _
            (region_steps_execution _ _ _ Hbefore_run))) _ Havailable)
        as Hcoherent.
      destruct (biv_update_observed defs loop biv step_size updated
        (current_registers before) Hspec Hstep Hin_updated Hcoherent)
        as [current [Hcurrent Hupdated]].
      rewrite Hparam in Hcurrent. cbn [register_of_value] in Hcurrent.
      rewrite Hsame, Hcounter in Hcurrent. injection Hcurrent as <-.
      specialize (Hbinding biv.(param_index) updated Harg_index).
      rewrite Hparam in Hbinding. cbn [register_of_value] in Hbinding.
      rewrite Hback_header in Hentry.
      destruct (region_finish_body loop.(body) start progress loop.(header)
        (block_body loop.(header)) (current_registers after)
        (loop_header_in_body _ _ _ loop_graph)) as [final Hbody].
      { change (region_steps loop.(body) start
          (enter loop.(header) (current_registers after))).
        rewrite <- Hentry. exact Hafter_run. }
      exists {| current_block := loop.(header); remaining := [];
                current_registers := final |}.
      split.
      + eapply region_steps_trans; [exact Hafter_run |].
        rewrite Hentry. apply executes_insns_region;
          [exact (loop_header_in_body _ _ _ loop_graph) | exact Hbody].
      + split; [reflexivity |]. split; [reflexivity |]. cbn.
        rewrite (executes_insns_parameter_frame _ _ _ Hbody), Hbinding.
        exact Hupdated.
  Qed.

  (* The arithmetic exit witness bounds the number of finite iterations. A
     traversal may exit earlier; otherwise its counter and fixed bound make the
     witnessed header test take the exiting edge. *)
  Theorem accepted_loop_finite_exit :
    SsaDominance.dominance_semantics entry -> op_def_correct defs blocks ->
    (∀ (dominator target : block),
      dominator ∈ blocks -> target ∈ blocks ->
      (∀ (visited_blocks : list block), SsaDominance.block_path_from entry
        visited_blocks target ->
         dominator ∈ visited_blocks) -> dominator ▷ target) ->
    switch_edge_multiplicity -> graph_size_supported blocks ->
    NoDup (strict_dominators loop.(header)) ->
    (∀ (dominator : block),
      dominator ∈ (strict_dominators loop.(header)) -> dominator ∈ blocks) ->
    bounded_registers initial_regs ->
    ∃ (exit_state : state), region_steps loop.(body) start exit_state
      ∧ set_mem (current_block exit_state) loop.(body) = false.
  Proof.
    intros Hdominance Hdefs Hcomplete Hmultiplicity Hsize Hnodup Hmembers
      Hbounded.
    pose proof (loop_header_in_body _ _ _ loop_graph) as Hinside.
    destruct (region_finish_body loop.(body) start progress loop.(header)
      (block_body loop.(header)) header_regs Hinside (Region_refl _ _))
      as [first_regs Hbody].
    set (first := {| current_block := loop.(header); remaining := [];
                    current_registers := first_regs |}).
    assert (Hfirst : region_steps loop.(body) start first).
    { apply executes_insns_region; assumption. }
    assert (Hfirst_prefix : context_steps (enter entry initial_regs) first).
    { eapply context_steps_trans; [exact prefix |].
      apply context_of_steps, (region_steps_execution _ _ _ Hfirst). }
    pose proof (proj2 (context_execution_invariants entry initial_regs first
      Hbounded Hfirst_prefix)) as Hfirst_bounded.
    destruct (certificate_counter _ _ _ _ _ certificate)
      as [biv [Hin [Hspec Haccepted]]].
    destruct (biv_implies_termination_spec defs loop biv (proj1 Hspec)
      Haccepted) as [exit_info [step_size [cmp [iv_is_left [bnd
        [Hexit [Hstep [Hextract Hverdict]]]]]]]].
    destruct (extract_comparison_shape _ _ _ _ _ _ Hextract)
      as [counter_ref [Hcounter_ref [Hinvariant Hshape]]].
    destruct (header_comparison_observed biv step_size exit_info cmp iv_is_left
      counter_ref bnd first Hspec Hfirst Hstep Hexit Hshape Hcounter_ref
      eq_refl eq_refl)
      as [counter [bound_word [Hcounter [Hbound Hcondition]]]].
    pose proof (header_comparison_available exit_info first Hexit
      eq_refl eq_refl) as Hcondition_available.
    destruct (comparison_operands_available _ _ _ _ _ _ _ _ well_formed
      Hcondition_available Hshape) as [_ Hbound_available].
    assert (Htested_bounded :
      in_int64 (tested_counter loop biv counter_ref step_size counter)).
    { unfold tested_counter. destruct (is_param _ _ _);
        [eapply Hfirst_bounded; exact Hcounter | apply wrap64_in_range]. }
    assert (Hbound_bounded : in_int64 bound_word).
    { eapply read_comparison_bound_bounded; eassumption. }
    assert (Harithmetic :
      continue_terminates step_size
        (oriented_continue_comparison iv_is_left
          exit_info.(continue_when_true) cmp) = LoopComparisons.Terminates
      ∨ (continue_terminates step_size
           (oriented_continue_comparison iv_is_left
             exit_info.(continue_when_true) cmp) = Terminates_if_bound_in_range
          ∧ LoopComparisons.bound_in_range (to_Z step_size) bound_word
               = true)).
    { destruct Hverdict as [Hverdict | [Hverdict Hrange]];
        [left; exact Hverdict | right; split; [exact Hverdict |]].
      destruct bnd as [bound_ref | constant].
      - eapply executed_bound_in_range with (reached := first)
          (initial_regs := initial_regs); try eassumption; reflexivity.
      - cbn [read_comparison_bound] in Hbound. injection Hbound as <-.
        eapply constant_bound_in_range. exact Hrange. }
    destruct (loop_exits iv_is_left exit_info.(continue_when_true) cmp
      step_size (tested_counter loop biv counter_ref step_size counter)
      bound_word Htested_bounded Hbound_bounded Harithmetic)
      as [iteration Hfails].
    assert (Hiterate : forall (n : nat),
      (exists (outside : state), region_steps loop.(body) start outside
        ∧ set_mem (current_block outside) loop.(body) = false)
      ∨ exists (test : state), region_steps loop.(body) start test
        ∧ current_block test = loop.(header) ∧ remaining test = []
        ∧ current_registers test (Param loop.(header) biv.(param_index))
             = Some (counter_after_iterations counter (to_Z step_size) n)).
    { intros n. induction n as [|n IH].
      - right. exists first. auto.
      - destruct IH as [Houtside | [test [Hrun [Hheader [Hdone Hvalue]]]]];
          [left; exact Houtside |].
        exact (counter_iteration biv step_size test _ Hspec Hstep Hrun
          Hheader Hdone Hvalue). }
    destruct (Hiterate iteration) as [Houtside |
      [test [Hrun [Hheader [Hdone Hvalue]]]]]; [exact Houtside |].
    destruct (header_comparison_observed biv step_size exit_info cmp iv_is_left
      counter_ref bnd test Hspec Hrun Hstep Hexit Hshape Hcounter_ref
      Hheader Hdone)
      as [actual_counter [actual_bound [Hcounter_now [Hbound_now Htest]]]].
    rewrite Hvalue in Hcounter_now. injection Hcounter_now as <-.
    assert (Hfixed : actual_bound = bound_word).
    { destruct bnd as [bound_ref | constant]; cbn [read_comparison_bound] in *;
        [ | congruence].
      pose proof (header_comparison_available exit_info test Hexit
        Hheader Hdone) as Havailable.
      destruct (comparison_operands_available _ _ _ _ _ _ _ _ well_formed
        Havailable Hshape) as [_ Hbound_now_available].
      assert (Hsame : current_registers test (register_of_value bound_ref)
        = current_registers first (register_of_value bound_ref)).
      { eapply loop_invariant_value_same with (start := start);
          try eassumption.
        - split; reflexivity.
        - exact (proj1 (proj2 (proj2 (proj2 (proj2 certificate))))).
        - rewrite Hheader. exact Hinside.
        - apply is_loop_invariant_spec. exact Hinvariant. }
      congruence. }
    subst actual_bound. rewrite tested_counter_sequence in Htest.
    destruct (progress test Hrun ltac:(rewrite Hheader; exact Hinside))
      as [outside Htransfer].
    destruct (find_exit_branch_spec loop exit_info Hexit)
      as [ifnot [ifso [Hterm [Hso Hnot]]]].
    pose proof (boolean_switch_step test outside exit_info.(condition)
      ifnot ifso _ Hdone ltac:(rewrite Hheader; exact Hterm) Htest Htransfer)
      as Htarget.
    exists outside. split.
    - eapply region_steps_trans; [exact Hrun |].
      eapply Region_cons;
        [rewrite Hheader; exact Hinside | exact Htransfer | constructor].
    - unfold loop_continues in Hfails. cbv zeta in Hfails.
      rewrite Htarget.
      destruct (if iv_is_left then IntegerComparison.eval cmp
        (counter_after_iterations (tested_counter loop biv counter_ref
          step_size counter)
          (to_Z step_size) iteration) bound_word
        else IntegerComparison.eval cmp bound_word
          (counter_after_iterations (tested_counter loop biv counter_ref
            step_size counter)
            (to_Z step_size) iteration));
        destruct exit_info.(continue_when_true); cbn in *; congruence.
  Qed.
End CounterExecution.
