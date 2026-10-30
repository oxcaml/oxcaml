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

(* Defined operands and progress for the supported SSA fragment.

   [control_layout] formalizes ordinary builder guarantees: Goto arguments
   match parameters, omitted arguments feed globally unused parameters, and
   Switch targets have no parameters. [switches_in_range] is the standard
   defined-input condition for SSA Switch (out-of-range indices are undefined
   behavior); it asserts neither progress nor termination. All instruction
   definedness below follows from liveness and execution. This module is
   proof-only. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlList.
From OxCamlSsa Require Import SsaModel SsaGraph SsaDominance SsaSemantics
  SsaExecution SsaLiveness.
Import ListNotations.
Local Open Scope Z_scope.

Definition register_used (blocks : list block) (reg : register) : Prop :=
  ∃ (position : program_point) (v : value), (fst position) ∈ blocks
    ∧ operand_at position v ∧ register_of_value v = reg.

Record control_layout (blocks : list block) : Prop :=
  { goto_parameter_layout : ∀ (source target : block)
      (args : list Terminator.arg),
      source ∈ blocks ->
      terminator source = Terminator.Continue (Goto target) args ->
      Forall2 (fun (arg : Terminator.arg) (parameter : value) =>
        match arg with
        | Terminator.Arg _ => True
        | Terminator.Omitted_since_unused =>
          ~ register_used blocks (register_of_value parameter)
        end) args (params target);
    switch_parameterless : ∀ (source : block) (index : value)
      (targets : list block) (target : block),
      source ∈ blocks ->
      terminator source = Terminator.Switch index targets ->
      target ∈ targets -> params target = [] }.

Definition live_registers_defined (position : program_point)
  (regs : registers) : Prop :=
  ∀ (reg : register), register_live_at position reg ->
    ∃ (word : Z), regs reg = Some word.

Definition switches_in_range (initial : state) : Prop :=
  ∀ (reached : state) (index : value) (targets : list block) (word : Z),
  steps initial reached -> remaining reached = [] ->
  terminator (current_block reached) = Terminator.Switch index targets ->
  current_registers reached (register_of_value index) = Some word ->
  0 <= word < Z.of_nat (length targets).

(* What [Ssa.finish_graph] guarantees of the graph the pass receives, as the
   eligibility theorem needs it: [blocks] is the graph's block list, [entry]
   its entry block and [defs] the table of defining blocks the pass builds from
   [blocks]. One field per builder guarantee; section 5.2 of
   ROCQ-REVIEW-GUIDE.md says where to look for each in the builder. *)
Record finished_graph (blocks : list block) (entry : block) (defs : op_def)
    : Prop :=
  { (* Definitions, uses, parameters, reachability, successor closure. *)
    graph_well_formed : well_formed_ssa blocks entry;
    (* The predecessors of a block of the graph are blocks of the graph. *)
    graph_predecessors_closed : ∀ (b pred : block),
      b ∈ blocks -> pred ∈ (predecessors b) -> pred ∈ blocks;
    (* Terminators, successors and predecessors describe the same edges. *)
    graph_edges : control_edges;
    graph_switch_edges : switch_edge_multiplicity;
    (* [dominates] is sound and complete for dominance along paths. *)
    graph_dominance : dominance_semantics entry;
    graph_dominance_complete : ∀ (dominator target : block),
      dominator ∈ blocks -> target ∈ blocks ->
      (∀ (visited_blocks : list block), block_path_from entry
        visited_blocks target -> dominator ∈ visited_blocks) ->
      dominator ▷ target;
    (* [strict_dominators] lists distinct blocks of the graph. *)
    graph_strict_dominators : ∀ (b : block), b ∈ blocks ->
      NoDup (strict_dominators b)
      ∧ (∀ (d : block), d ∈ (strict_dominators b) -> d ∈ blocks);
    (* Goto arguments match parameters; Switch targets have none. *)
    graph_layout : control_layout blocks;
    (* The table maps every instruction to its block. *)
    graph_definitions : op_def_correct defs blocks }.

(* Liveness witnesses a real operand somewhere in the successor-closed graph,
   even when the read is several blocks away. *)
Lemma live_register_used : ∀ (blocks : list block) (entry : block)
    (position : program_point) (reg : register),
  well_formed_ssa blocks entry -> (fst position) ∈ blocks ->
  register_live_at position reg -> register_used blocks reg.
Proof.
  intros blocks entry position reg Hwf Hin Hlive. revert Hin.
  induction Hlive; intros Hin.
  - exists position, v. auto.
  - apply IHHlive. eapply program_point_edge_in_graph; eassumption.
Qed.

(* Reading a list succeeds when every referenced register is bound. *)
Lemma read_values_of_defined_operands : ∀ (regs : registers)
  (args : list value),
  (∀ (arg : value), arg ∈ args ->
    ∃ (word : Z), regs (register_of_value arg) = Some word) ->
  ∃ (words : list Z), read_values regs args = Some words.
Proof.
  intros regs args. induction args as [ | arg args IH]; intros Hbound.
  - exists []. reflexivity.
  - destruct (Hbound arg (or_introl eq_refl)) as [word Hword].
    destruct (IH ltac:(intros v Hin; apply Hbound; right; exact Hin))
      as [words Hwords].
    exists (word :: words). cbn [read_values]. rewrite Hword, Hwords.
    reflexivity.
Qed.

(* A used parameter cannot receive an omitted argument. The actual read and
   simultaneous binding therefore give it a concrete word. *)
Lemma used_parameter_defined : ∀ (blocks : list block)
    (args : list Terminator.arg) (parameters : list value)
    (before after : registers) (words : list (option Z)),
  Forall2 (fun (arg : Terminator.arg) (parameter : value) =>
    match arg with
    | Terminator.Arg _ => True
    | Terminator.Omitted_since_unused =>
      ~ register_used blocks (register_of_value parameter)
    end) args parameters ->
  read_arguments before args = Some words ->
  Forall2 (fun (parameter : value) (word : option Z) =>
    after (register_of_value parameter) = word) parameters words ->
  ∀ (parameter : value), parameter ∈ parameters ->
  register_used blocks (register_of_value parameter) ->
  ∃ (word : Z), after (register_of_value parameter) = Some word.
Proof.
  intros blocks args parameters before after words Hlayout.
  revert words. induction Hlayout as [ | arg parameter args parameters
    Hallowed Hlayout IH]; intros words Hread Hbound selected Hin Hused.
  - contradiction.
  - destruct arg as [v |]; cbn [read_arguments] in Hread.
    + destruct (before (register_of_value v)) as [word |] eqn:Hvalue;
        [ | discriminate].
      destruct (read_arguments before args) as [rest |] eqn:Hrest;
        [ | discriminate]. injection Hread as <-.
      inversion Hbound; subst. destruct Hin as [<- | Hin].
      * exists word. assumption.
      * eapply IH; try eassumption; reflexivity.
    + destruct (read_arguments before args) as [rest |] eqn:Hrest;
        [ | discriminate]. injection Hread as <-.
      inversion Hbound; subst. destruct Hin as [<- | Hin].
      * contradiction.
      * eapply IH; try eassumption; reflexivity.
Qed.

(* A newly defined live register is bound by the actual step. Results come from
   executing an instruction; used parameters receive arguments. *)
Lemma step_new_live_register_defined : ∀ (blocks : list block)
  (entry : block)
    (before after : state) (reg : register),
  well_formed_ssa blocks entry -> control_layout blocks ->
  valid_position before -> (current_block before) ∈ blocks ->
  register_defined_at (state_program_point after) reg -> register_used blocks
    reg ->
  step before after ->
  ∃ (word : Z), current_registers after reg = Some word.
Proof.
  intros blocks entry before after reg Hwf Hlayout Hposition Hblock
    [v [Hdefinition <-]] Hused Hstep.
  destruct v as [id op args index | b index].
  - destruct (step_new_result _ _ _ _ _ _ Hposition Hstep Hdefinition)
      as [-> Hexec].
    destruct (execute_instruction_result _ _ _ _ _ _ Hexec)
      as [_ [words [result [Hread [Heval Hregs]]]]].
    exists result. rewrite Hregs. apply write_register_same.
  - destruct Hdefinition as [Hentry Hparam].
    destruct Hstep as [source insn rest before_regs after_regs Hexec |
      source target arguments before_regs argument_words after_regs
        Hterm Hread Hbind |
      source index_value targets regs index_word target
        Hterm Hread Hnonnegative Hnth Hparams];
      cbn [state_program_point enter] in Hentry.
    + unfold block_entry_point in Hentry. injection Hentry as Hsame Hlength.
      subst source. destruct Hposition as [prefix Hbody]. cbn in Hbody.
      rewrite Hbody, length_app in Hlength. cbn in Hlength. lia.
    + unfold block_entry_point in Hentry. injection Hentry as Hsame Hlength.
      subst target.
      eapply used_parameter_defined with (parameters := params b)
        (args := arguments) (before := before_regs) (words := argument_words).
      * eapply goto_parameter_layout; eassumption.
      * exact Hread.
      * eapply bind_parameters_values; [apply (parameters_distinct
          (graph_parameters _ _ Hwf)) | exact Hbind].
      * eapply nth_int_In. exact Hparam.
      * exact Hused.
    + unfold block_entry_point in Hentry. injection Hentry as Hsame Hlength.
      subst target. rewrite Hparams in Hparam. discriminate.
Qed.

(* Reading Goto arguments succeeds when each non-omitted input is bound;
   omitted arguments still occupy a parameter position. *)
Lemma read_arguments_of_defined_operands : ∀ (regs : registers)
    (args : list Terminator.arg),
  (∀ (arg : value), (Terminator.Arg arg) ∈ args ->
     ∃ (word : Z), regs (register_of_value arg) = Some word) ->
  ∃ (words : list (option Z)), read_arguments regs args = Some words
    ∧ length words = length args.
Proof.
  intros regs args. induction args as [ | [arg |] args IH]; intros Hbound.
  - exists []. auto.
  - destruct (Hbound arg (or_introl eq_refl)) as [word Hword].
    destruct (IH ltac:(intros v Hin; apply Hbound; right; exact Hin))
      as [words [Hwords Hlength]].
    exists (Some word :: words). split; [ | cbn; congruence].
    cbn [read_arguments]. rewrite Hword, Hwords. reflexivity.
  - destruct (IH ltac:(intros v Hin; apply Hbound; right; exact Hin))
      as [words [Hwords Hlength]].
    exists (None :: words). split; [ | cbn; congruence].
    cbn [read_arguments]. rewrite Hwords. reflexivity.
Qed.

(* Equal-length parameter and argument lists can always be bound. *)
Lemma bind_parameters_total : ∀ (parameters : list value)
    (words : list (option Z)) (regs : registers),
  length parameters = length words ->
  ∃ (after : registers),
    bind_parameters parameters words regs = Some after.
Proof.
  intros parameters. induction parameters as [ | parameter parameters IH];
    intros [ | word words] regs Hlength; try discriminate.
  - exists regs. reflexivity.
  - cbn [bind_parameters]. apply IH. cbn in Hlength. lia.
Qed.

(* A supported state with bound live registers makes progress, provided its
   Switch (if any) has a defined index. No loop property is used. *)
Theorem supported_state_progress : ∀ (blocks : list block)
    (entry : block) (current : state),
  well_formed_ssa blocks entry -> control_layout blocks ->
  valid_position current -> (current_block current) ∈ blocks ->
  live_registers_defined (state_program_point current)
    (current_registers current) ->
  supported_block (current_block current) = true ->
  (∀ (index : value) (targets : list block) (word : Z),
    remaining current = [] ->
    terminator (current_block current) = Terminator.Switch index targets ->
    current_registers current (register_of_value index) = Some word ->
    0 <= word < Z.of_nat (length targets)) ->
  ∃ (next : state), step current next.
Proof.
  intros blocks entry [b insns regs] Hwf Hlayout Hposition Hblock Hbound
    Hsupported Hswitch.
  unfold supported_block in Hsupported. apply andb_true_iff in Hsupported.
  destruct Hsupported as [Hinsns Hterm]. cbn in *.
  destruct insns as [ | insn rest].
  - destruct (terminator b) as [continuation args | index targets | |]
      eqn:Hterminator; try discriminate.
    + destruct continuation as [target | | kind |]; try discriminate.
      destruct (read_arguments_of_defined_operands regs args) as [words [Hread
        Hlength]].
      { intros arg Hin.
        destruct (continue_argument_operand (Goto target) args arg Hin)
          as [uses [Huses Huse]].
        apply Hbound. apply Live_read.
        eapply Terminator_operand with (b := b) (args := uses).
        - rewrite Hterminator. exact Huses.
        - exact Huse. }
      pose proof (goto_parameter_layout _ Hlayout b target args Hblock
        Hterminator) as Hparameters.
      assert (Harity : length (params target) = length words).
      { rewrite Hlength. symmetry. eapply Forall2_length. exact Hparameters. }
      destruct (bind_parameters_total _ _ regs Harity) as [after Hbind].
      exists (enter target after). econstructor; eassumption.
    + destruct (Hbound (register_of_value index)) as [word Hword].
      { apply Live_read.
        eapply Terminator_operand with (b := b) (args := [index]);
          [rewrite Hterminator; cbn; auto | cbn; auto]. }
      pose proof (Hswitch index targets word eq_refl eq_refl Hword) as Hrange.
      assert (Hindex : (Z.to_nat word < length targets)%nat) by lia.
      destruct (nth_error targets (Z.to_nat word)) as [target |] eqn:Htarget.
      * exists (enter target regs). eapply Step_switch; try eassumption.
        -- lia.
        -- eapply switch_parameterless; [exact Hlayout | exact Hblock |
             exact Hterminator | eapply nth_error_In; exact Htarget].
      * apply nth_error_None in Htarget. lia.
  - assert (Hin : insn ∈ (block_body b)).
    { destruct Hposition as [prefix Hbody]. cbn in Hbody. rewrite Hbody.
      apply in_or_app. right. left. reflexivity. }
    apply forallb_forall with (x := insn) in Hinsns; [ | exact Hin].
    assert (Hreads : exists (words : list Z),
      read_values regs (args_of_instr insn) = Some words).
    { apply read_values_of_defined_operands. intros arg Harg. apply Hbound.
      destruct insn as [id op args count | |]; try contradiction.
      apply Live_read. eapply Instruction_operand; [ | exact Harg].
      eapply executing_result_definition. exact Hposition. }
    destruct Hreads as [words Hread].
    destruct (supported_instruction_executes _ _ _ Hinsns Hread)
      as [id [op [args [result [Hinsn [Heval Hexec]]]]]].
    eexists. apply Step_instruction. exact Hexec.
Qed.
