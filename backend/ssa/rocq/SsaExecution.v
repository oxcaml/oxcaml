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

(* SSA well-formedness and the values available during execution.

   The well-formedness conditions below concern definitions, instruction
   positions and graph paths only. They contain no valuations, execution
   invariants or loop-analysis verdicts. The purpose of this layer is to derive
   operation equations at their actual uses, rather than assume equations for
   every freely constructed [value] term. Only operations selected by
   [modeled_operation] have exact equations. SsaContext.v uses the frame and
   newly-defined-result lemmas here to prove preservation once, for both
   integer steps and surrounding opaque computations. *)

From Stdlib Require Import ZArith Lia List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlInt64 OxCamlList.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaGraph SsaDominance SsaSemantics.
Import ListNotations.

Definition state_program_point (current : state) : program_point :=
  (current_block current, length (remaining current)).

(* The point immediately after an instruction is its definition site. *)
Definition result_definition (id : instr_id) (op : operation)
    (args : list value) (result_count : int) (site : program_point) : Prop :=
  ∃ (prefix suffix : list instr),
    block_body (fst site) = prefix ++ Op id op args result_count :: suffix
    ∧ snd site = length suffix.

(* A reference carries the actual definition's payload, not just its ID.
   Parameters are available at entry; operation results after execution. *)
Definition value_definition (v : value) (site : program_point) : Prop :=
  match v with
  | Res id op args index =>
    ∃ (result_count : int),
      result_definition id op args result_count site
      ∧ (0 <= to_Z index < to_Z result_count)%Z
  | Block_param b index =>
    site = block_entry_point b ∧ nth_int (params b) index = Some v
  end.

Record well_formed_ssa (blocks : list block) (entry : block) : Prop :=
  { entry_in_graph : entry ∈ blocks;
    (* Finished graphs contain only reachable blocks. This is structural
       reachability through program points, not a claim that operations execute
       successfully or that branches are dynamically feasible. *)
    graph_blocks_reachable : ∀ (b : block), b ∈ blocks ->
      program_point_reachable entry (b, 0%nat);
    graph_successors_closed : ∀ (source target : block),
      source ∈ blocks -> target ∈ (all_successors source) ->
      target ∈ blocks;
    graph_parameters : parameter_layout;
    result_sites_unique : ∀ (id : instr_id) (op op' : operation)
      (args args' : list value) (count count' : int)
        (site site' : program_point),
      (fst site) ∈ blocks -> (fst site') ∈ blocks ->
      result_definition id op args count site ->
      result_definition id op' args' count' site' -> site = site';
    operand_definitions : ∀ (id : instr_id) (op : operation)
      (args : list value) (count : int) (site : program_point),
      (fst site) ∈ blocks -> result_definition id op args count site ->
      ∀ (arg : value), arg ∈ args ->
      ∃ (arg_site : program_point),
        value_definition arg arg_site ∧ (fst arg_site) ∈ blocks
        ∧ program_point_dominates entry arg_site site ∧ arg_site <> site;
    terminator_definitions : ∀ (b : block) (args : list value),
      b ∈ blocks -> args ∈ (terminator_args (terminator b)) ->
      ∀ (arg : value), arg ∈ args ->
      ∃ (arg_site : program_point),
        value_definition arg arg_site ∧ (fst arg_site) ∈ blocks
        ∧ program_point_dominates entry arg_site (b, 0%nat) }.

(* Exact equations for modeled instruction results, and no equations for opaque
   results or parameters. Register definedness is proved separately in
   SsaProgress.v; this predicate does not silently default missing reads. *)
Definition value_coherent (regs : registers) (v : value) : Prop :=
  match v with
  | Res id op args index =>
    if modeled_operation op args then
      ∃ (arg_values : list Z) (result : Z),
        read_values regs args = Some arg_values
        ∧ eval_operation op arg_values = Some result
        ∧ regs (Result id index) = Some result
    else True
  | Block_param _ _ => True
  end.

(* Availability is structural: a canonical definition in this graph dominates
   the program point. Merely sharing an instruction ID is not sufficient, and a
   definition later in the block is not available. *)
Definition available_value (blocks : list block) (entry : block)
    (position : program_point) (v : value) : Prop :=
  ∃ (site : program_point), value_definition v site ∧ (fst site) ∈ blocks
    ∧ program_point_dominates entry site position.

(* Only available values are asserted coherent. Values from another branch,
   later instructions and stale results from a previous loop iteration need not
   satisfy any operation equation. *)
Definition coherent_at (blocks : list block) (entry : block)
    (regs : registers) (position : program_point) : Prop :=
  ∀ (v : value), available_value blocks entry position v ->
    value_coherent regs v.

(* Operands of an available result are available as well. *)
Lemma operand_available : ∀ (blocks : list block) (entry : block)
    (position : program_point) (id : instr_id) (op : operation)
      (args : list value)
    (index : int) (arg : value),
  well_formed_ssa blocks entry ->
  available_value blocks entry position (Res id op args index) ->
  arg ∈ args -> available_value blocks entry position arg.
Proof.
  intros blocks entry position id op args index arg Hwf
    [site [[count [Hdefinition Hindex]] [Hsite Hdominates]]] Hin.
  destruct (operand_definitions _ _ Hwf id op args count site
    Hsite Hdefinition arg Hin) as [arg_site [Harg [Hargsite [Hdom _]]]].
  exists arg_site. split; [exact Harg |]. split; [exact Hargsite |].
  eapply program_point_dominance_transitive; eassumption.
Qed.

(* A terminator's operands are available when that terminator executes. *)
Lemma terminator_operand_available : ∀ (blocks : list block)
    (entry : block) (current : state) (args : list value) (arg : value),
  well_formed_ssa blocks entry -> (current_block current) ∈ blocks ->
  remaining current = [] ->
  args ∈ (terminator_args (terminator (current_block current))) ->
  arg ∈ args ->
  available_value blocks entry (state_program_point current) arg.
Proof.
  intros blocks entry s args arg Hwf Hblock Hremaining Hargs Hin.
  destruct (terminator_definitions _ _ Hwf _ _ Hblock Hargs arg Hin)
    as [site [Hdefinition [Hsite Hdominates]]].
  exists site. split; [exact Hdefinition |]. split; [exact Hsite |].
  unfold state_program_point. rewrite Hremaining. exact Hdominates.
Qed.

(* A result site is strictly inside its block, never at block entry. *)
Lemma result_definition_not_entry : ∀ (id : instr_id) (op : operation)
    (args : list value) (result_count : int) (b : block),
  ~ result_definition id op args result_count (block_entry_point b).
Proof.
  intros id op args result_count b [prefix [suffix [Hbody Hlength]]].
  unfold block_entry_point in *. cbn in *.
  rewrite Hbody, length_app in Hlength. cbn in Hlength. lia.
Qed.

(* Two instructions at one definition point are the same instruction. *)
Lemma result_definition_at_point : ∀ (id id' : instr_id)
    (op op' : operation) (args args' : list value)
    (count count' : int) (site : program_point),
  result_definition id op args count site ->
  result_definition id' op' args' count' site ->
  Op id op args count = Op id' op' args' count'.
Proof.
  intros id id' op op' args args' count count' site
    [prefix [suffix [Hbody Hlength]]]
    [prefix' [suffix' [Hbody' Hlength']]].
  eapply same_position_element; [rewrite <- Hbody, <- Hbody'; reflexivity |].
  congruence.
Qed.

(* Canonical available references sharing a register share the complete value
   payload. The two references may be available at different points. *)
Lemma available_value_identity : ∀ (blocks : list block) (entry : block)
    (first second : program_point) (first_ref second_ref : value),
  well_formed_ssa blocks entry ->
  available_value blocks entry first first_ref ->
  available_value blocks entry second second_ref ->
  register_of_value first_ref = register_of_value second_ref ->
  first_ref = second_ref.
Proof.
  intros blocks entry first second
    [id op args index | b index] [id' op' args' index' | b' index'] Hwf
    [site [Hdefinition [Hsite Hdom]]]
    [site' [Hdefinition' [Hsite' Hdom']]] Hreg;
    cbn [register_of_value] in Hreg; try discriminate;
    inversion Hreg; subst.
  - destruct Hdefinition as [count [Hdefinition Hindex]].
    destruct Hdefinition' as [count' [Hdefinition' Hindex']].
    assert (site = site') by (eapply result_sites_unique; eassumption).
    subst site'.
    pose proof (result_definition_at_point _ _ _ _ _ _ _ _ _
      Hdefinition Hdefinition') as Hsame.
    injection Hsame as -> -> ->. reflexivity.
  - reflexivity.
Qed.

(* An instruction in a block has a definition site at the suffix after it. *)
Lemma result_definition_of_membership : ∀ (b : block) (id : instr_id)
    (op : operation) (args : list value) (count : int),
  (Op id op args count) ∈ (block_body b) ->
  ∃ (site : program_point), fst site = b ∧
    result_definition id op args count site.
Proof.
  intros b id op args count Hin. apply in_split in Hin.
  destruct Hin as [prefix [suffix Hbody]].
  exists (b, length suffix). split; [reflexivity |].
  exists prefix, suffix. auto.
Qed.

(* An available reference to a single-result instruction is its unique result,
   with the actual payload. Sharing an ID alone is not enough. *)
Lemma single_result_reference : ∀ (blocks : list block) (entry : block)
    (position : program_point) (b : block) (id : instr_id) (op : operation)
    (args : list value) (v : value),
  well_formed_ssa blocks entry -> b ∈ blocks ->
  (Op id op args one) ∈ (block_body b) ->
  available_value blocks entry position v ->
  instr_defines_value (Op id op args one) v = true ->
  v = Res id op args zero.
Proof.
  intros blocks entry position b id op args [actual_id actual_op actual_args
    index | parameter_block index] Hwf Hblock Hin
    [site [Hdefinition [Hsite Hdom]]] Hdefines; [ | discriminate].
  cbn [instr_defines_value] in Hdefines.
  apply instr_id_eqb_spec in Hdefines. subst actual_id.
  destruct Hdefinition as [count [Hdefinition Hindex]].
  destruct (result_definition_of_membership _ _ _ _ _ Hin)
    as [actual_site [Hactual_block Hactual]].
  assert (site = actual_site) as Heq.
  { eapply result_sites_unique;
      [exact Hwf | exact Hsite | | exact Hdefinition | exact Hactual].
    rewrite Hactual_block. exact Hblock. }
  subst site.
  pose proof (result_definition_at_point _ _ _ _ _ _ _ _ _
    Hdefinition Hactual) as Hsame.
  injection Hsame as -> -> ->.
  assert (index = zero) as ->.
  { apply Sint63.to_Z_inj. rewrite to_Z_zero, to_Z_one in *. lia. }
  reflexivity.
Qed.

(* A canonical definition outside a region cannot share its register with a
   definition inside. Unique sites rule out borrowed IDs. *)
Lemma definition_outside_region : ∀ (blocks : list block) (entry : block)
    (region : blockset) (v : value) (site : program_point),
  well_formed_ssa blocks entry ->
  (∀ (b : block), set_mem b region = true -> b ∈ blocks) ->
  value_definition v site -> (fst site) ∈ blocks ->
  set_mem (fst site) region = false ->
  ~ register_defined_in_region region (register_of_value v).
Proof.
  intros blocks entry region [id op args index | b index] site Hwf Hmembers
    Hdefinition Hsite Houtside Hinside.
  - destruct Hdefinition as [count [Hdefinition Hindex]].
    destruct Hinside as [b [actual_op [actual_args [actual_count
      [Hblock Hmember]]]]].
    destruct (result_definition_of_membership _ _ _ _ _ Hmember)
      as [actual_site [Hactual_block Hactual]].
    assert (site = actual_site) as Heq.
    { eapply result_sites_unique;
        [exact Hwf | exact Hsite | | exact Hdefinition | exact Hactual].
      rewrite Hactual_block. apply Hmembers. exact Hblock. }
    rewrite Heq, Hactual_block in Houtside. congruence.
  - destruct Hdefinition as [-> Hparameter]. cbn in *. congruence.
Qed.

(* An executable instruction position corresponds to its actual body suffix.
   The successor point is precisely that instruction's site. *)
Lemma executing_result_definition : ∀ (b : block) (id : instr_id)
    (op : operation) (args : list value) (count : int) (rest : list instr)
    (regs : registers),
  valid_position
    {| current_block := b; remaining := Op id op args count :: rest;
       current_registers := regs |} ->
  result_definition id op args count (b, length rest).
Proof.
  intros b id op args count rest regs [prefix Hbody].
  exists prefix, rest. auto.
Qed.

(* Actual transitions project to structural program-point edges. *)
Theorem step_program_point_edge : control_edges -> ∀ (before after :
  state),
  valid_position before -> step before after ->
  program_point_edge (state_program_point before) (state_program_point after).
Proof.
  intros Hedges before after Hposition Hstep.
  destruct Hstep; unfold state_program_point; cbn.
  - apply Program_point_instruction. destruct Hposition as [prefix Hbody].
    cbn in Hbody. rewrite Hbody, length_app. cbn. lia.
  - apply Program_point_transfer. eapply goto_successor; eassumption.
  - apply Program_point_transfer. eapply switch_successor;
      [exact Hedges | eassumption | eapply nth_error_In; eassumption].
Qed.

(* A step preserves membership in a successor-closed graph. *)
Theorem step_in_graph : control_edges -> ∀ (blocks : list block)
    (entry : block) (before after : state),
  well_formed_ssa blocks entry -> (current_block before) ∈ blocks ->
  step before after -> (current_block after) ∈ blocks.
Proof.
  intros Hedges blocks entry before after Hwf Hin Hstep.
  destruct Hstep; cbn in *; [exact Hin | |].
  - eapply graph_successors_closed; [exact Hwf | exact Hin |].
    eapply goto_successor; eassumption.
  - eapply graph_successors_closed; [exact Hwf | exact Hin |].
    eapply switch_successor;
      [exact Hedges | eassumption | eapply nth_error_In; eassumption].
Qed.

(* Registers of definitions at other sites cannot be changed by this step. For
   results this uses unique instruction identities; for parameters it uses the
   actual parameter layout of the target. *)
Theorem step_definition_frame :
  ∀ (blocks : list block) (entry : block) (before after : state)
    (v : value) (site : program_point),
  well_formed_ssa blocks entry -> valid_position before ->
  (current_block before) ∈ blocks ->
  value_definition v site -> (fst site) ∈ blocks ->
  site <> state_program_point after -> step before after ->
  current_registers after (register_of_value v)
    = current_registers before (register_of_value v).
Proof.
  intros blocks entry before after v site Hwf Hposition Hbefore
    Hdefinition Hsite Hneq Hstep.
  destruct v as [id op args index | b index].
  - destruct Hdefinition as [count [Hdefinition Hindex]].
    destruct Hstep as [source insn rest regs regs' Hexec |
      source target arguments regs values regs' Hterm Hread Hbind |
      source condition targets regs value target Hterm Hvalue Hpos Hnth
        Hparams];
      cbn in *.
    + destruct insn as [defined_id defined_op defined_args results | |];
        try discriminate.
      eapply execute_instruction_frame; [exact Hexec |].
      intros Heq. inversion Heq; subst.
      apply Hneq. eapply result_sites_unique;
        [exact Hwf | exact Hsite | exact Hbefore | exact Hdefinition |].
      eapply executing_result_definition. exact Hposition.
    + eapply bind_parameters_frame; [exact Hbind |].
      intros Hin. apply in_map_iff in Hin.
      destruct Hin as [parameter [Hreg Hin]].
      destruct (parameters_local (graph_parameters _ _ Hwf)
        target parameter Hin) as [param_index ->]. discriminate.
    + reflexivity.
  - destruct Hdefinition as [Hentry Hparam]. subst site.
    destruct Hstep as [source insn rest regs regs' Hexec |
      source target arguments regs values regs' Hterm Hread Hbind |
      source condition targets regs value target Hterm Hvalue Hpos Hnth
        Hparams];
      cbn in *.
    + destruct insn as [id op args results | |]; try discriminate.
      eapply execute_instruction_frame; [exact Hexec | discriminate].
    + eapply bind_parameters_frame; [exact Hbind |].
      intros Hin. apply in_map_iff in Hin.
      destruct Hin as [parameter [Hreg Hin]].
      destruct (parameters_local (graph_parameters _ _ Hwf)
        target parameter Hin) as [param_index ->].
      cbn in Hreg. inversion Hreg; subst.
      apply Hneq. reflexivity.
    + reflexivity.
Qed.

(* If a result's definition site is the new program point, this very step
   executed its instruction. A transfer cannot manufacture such a result
   because instruction result sites are never block-entry points. *)
Lemma step_new_result : ∀ (before after : state) (id : instr_id)
    (op : operation) (args : list value) (index : int),
  valid_position before -> step before after ->
  value_definition (Res id op args index) (state_program_point after) ->
  index = zero ∧
  execute_instruction (current_registers before) (Op id op args one)
    = Some (current_registers after).
Proof.
  intros before after id op args index Hposition Hstep
    [count [Hdefinition Hindex]].
  destruct Hstep as [source insn rest regs regs' Hexec |
    source target arguments regs values regs' Hterm Hread Hbind |
    source condition targets regs value target Hterm Hvalue Hpos Hnth Hparams];
    cbn in *.
  - destruct insn as [actual_id actual_op actual_args actual_count | |];
      try discriminate.
    pose proof (executing_result_definition _ _ _ _ _ _ _ Hposition)
      as Hactual.
    pose proof (result_definition_at_point _ _ _ _ _ _ _ _ _
      Hdefinition Hactual) as Heq.
    injection Heq as -> -> -> ->.
    destruct (execute_instruction_result _ _ _ _ _ _ Hexec) as [Hcount _].
    subst actual_count. split; [ | exact Hexec].
    apply Sint63.to_Z_inj. rewrite to_Z_zero.
    rewrite to_Z_one in Hindex. lia.
  - exfalso. eapply result_definition_not_entry. exact Hdefinition.
  - exfalso. eapply result_definition_not_entry. exact Hdefinition.
Qed.

(* No instruction result is available at function entry. Consequently initial
   coherence holds for arbitrary input-register values; it is not an assumption
   about a valuation or a real execution. *)
Lemma entry_values_coherent : ∀ (blocks : list block) (entry : block)
    (initial_regs : registers),
  coherent_at blocks entry initial_regs (block_entry_point entry).
Proof.
  intros blocks entry initial_regs v
    [site [Hdefinition [Hsite Hdominates]]].
  destruct v as [id op args index | b index]; [ | exact I].
  pose proof (Hdominates [block_entry_point entry] (Program_point_path_one _))
    as Hin.
  destruct Hin as [Heq | []]. subst site.
  destruct Hdefinition as [count [Hresult _]].
  exfalso. eapply result_definition_not_entry. exact Hresult.
Qed.
