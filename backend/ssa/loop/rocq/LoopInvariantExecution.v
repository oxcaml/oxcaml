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

(* Operational meaning of loop-invariant values.

   Strict dominators are outside a reachable natural-loop body, so their
   registers are not rewritten while execution stays in that body. Constant
   results have their literal value whenever available. Together these facts
   justify [loop_invariant_value] for actual available SSA values, without
   assigning invariance to fabricated expressions. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlList.
From OxCamlStdlib Require Import OxCamlInt OxCamlInt64.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaGraph SsaDominance SsaSemantics
  SsaExecution SsaContext.
From OxCamlLoop Require Import NaturalLoop InductionVar LoopExecution
  DeleteEmptyLoops.
Import ListNotations.

Section Invariants.
  Variable blocks : list block.
  Variable entry : block.
  Variable defs : op_def.
  Variable loop : natural_loop.
  Variable initial_regs : registers.
  Variable start : state.

  Hypothesis edges : control_edges.
  Hypothesis well_formed : well_formed_ssa blocks entry.
  Hypothesis loop_graph : loop_graph_properties blocks entry loop.
  Hypothesis dominance : dominance_semantics entry.
  Hypothesis definitions : op_def_correct defs blocks.
  Hypothesis prefix : context_steps (enter entry initial_regs) start.
  Hypothesis starts_at_header : at_entry loop.(header) start.
  Hypothesis body_supported : body_in_supported_fragment loop.

  (* Header reachability follows from the actual prefix, so antisymmetry of
     dominance cannot be used vacuously on unreachable blocks. *)
  Lemma invariant_header_reachable :
    program_point_reachable entry (block_entry_point loop.(header)).
  Proof.
    assert (Hreachable : program_point_reachable entry (state_program_point
      start)).
    { eapply context_steps_program_point_reachable;
        [exact edges | exact prefix | apply enter_valid_position |].
      exists [block_entry_point entry]. constructor. }
    destruct starts_at_header as [Hblock Hremaining].
    unfold state_program_point in Hreachable. rewrite Hblock, Hremaining in
      Hreachable.
    exact Hreachable.
  Qed.

  (* A strict dominator of the header cannot also lie in its body. *)
  Lemma strict_dominator_outside_loop : ∀ (dominator : block),
    dominator ▷ loop.(header) -> dominator <> loop.(header) ->
    set_mem dominator loop.(body) = false.
  Proof.
    intros dominator Hdom Hstrict.
    destruct (set_mem dominator loop.(body)) eqn:Hinside; [ | reflexivity].
    assert (Hto_header : program_point_dominates entry (block_entry_point
      dominator)
      (block_entry_point loop.(header))).
    { apply program_point_dominance_of_block; [exact edges |].
      intros block_path_from Hpath. exact (dominates_spec dominance _ _ Hdom _
        Hpath). }
    assert (Hto_body : program_point_dominates entry (block_entry_point
      loop.(header))
      (block_entry_point dominator)).
    { apply program_point_dominance_of_block; [exact edges |].
      exact (loop_header_dominates_body _ _ _ loop_graph dominator Hinside). }
    pose proof (program_point_dominance_antisymmetric entry _ _
      invariant_header_reachable Hto_header Hto_body) as Heq.
    apply (f_equal fst) in Heq. cbn in Heq. contradiction.
  Qed.

  (* The defining-block table identifies every actual result register. If it
     says a strict dominator defines the register, the body cannot contain
     another instruction that rewrites that same register. *)
  Lemma invariant_result_defined_outside_loop : ∀ (id : instr_id)
    (index : int)
      (defining_block : block),
    op_def_find defs id = Some defining_block ->
    defining_block ▷ loop.(header) ->
    defining_block <> loop.(header) ->
    ~ register_defined_in_region loop.(body) (Result id index).
  Proof.
    intros id index defining_block Hlookup Hdom Hstrict
      [body_block [op [args [results [Hinside Hdefinition]]]]].
    pose proof (definitions body_block id op args results
      (loop_body_in_graph _ _ _ loop_graph body_block Hinside) Hdefinition)
      as Hactual.
    rewrite Hlookup in Hactual. injection Hactual as <-.
    rewrite (strict_dominator_outside_loop defining_block Hdom Hstrict) in
      Hinside.
    discriminate.
  Qed.

  (* Opaque operations, including non-integer constants, cannot occur in the
     whitelisted loop. An available such definition is outside it. *)
  Lemma unmodeled_result_defined_outside_loop : ∀ (id : instr_id)
      (op : operation) (args : list value) (index : int)
        (position : program_point),
    modeled_operation op args = false ->
    available_value blocks entry position (Res id op args index) ->
    ~ register_defined_in_region loop.(body) (Result id index).
  Proof.
    intros id op args index position Hopaque
      [site [Hdefinition [Hsite Hdom]]].
    eapply definition_outside_region with (v := Res id op args index)
      (site := site); [exact well_formed |
      exact (loop_body_in_graph _ _ _ loop_graph) | exact Hdefinition |
      exact Hsite |].
    destruct (set_mem (fst site) loop.(body)) eqn:Hinside; [ | reflexivity].
    destruct Hdefinition as [count [[before [suffix [Hbody Hlength]]] Hindex]].
    assert (Hin : (Op id op args count) ∈ (block_body (fst site))).
    { rewrite Hbody. apply in_or_app. right. left. reflexivity. }
    pose proof (proj1 (body_supported _ Hinside) _ Hin) as Hsupported.
    apply andb_true_iff in Hsupported.
    pose proof (supported_operation_modeled _ _ _ (proj2 Hsupported)).
    congruence.
  Qed.

  (* The syntactic invariant classifier is sound for available actual values.
     Availability is essential: constants need not have executed at arbitrary
     states, and fabricated results are not definitions. *)
  Theorem loop_invariant_value_same :
    ∀ (invariant_ref : value) (first second : state),
    region_steps loop.(body) start first ->
    region_steps loop.(body) start second ->
    set_mem (current_block first) loop.(body) = true ->
    set_mem (current_block second) loop.(body) = true ->
    loop_invariant_value defs loop.(header) invariant_ref ->
    available_value blocks entry (state_program_point first) invariant_ref ->
    available_value blocks entry (state_program_point second) invariant_ref ->
    current_registers second (register_of_value invariant_ref)
      = current_registers first (register_of_value invariant_ref).
  Proof.
    intros v first second
      Hrun_first Hrun_second Hinside_first Hinside_second
      Hinvariant Hfirst Hsecond.
    assert (Hposition : valid_position start).
    { eapply context_steps_valid_position;
        [exact prefix | apply enter_valid_position]. }
    assert (Hframe : forall (reg : register),
      ~ register_defined_in_region loop.(body) reg ->
      current_registers second reg = current_registers first reg).
    { intros reg Houtside.
      rewrite (region_steps_frame (graph_parameters _ _ well_formed)
        _ _ _ reg Hrun_first Hposition (or_introl Hinside_first) Houtside),
        (region_steps_frame (graph_parameters _ _ well_formed)
        _ _ _ reg Hrun_second Hposition (or_introl Hinside_second) Houtside).
      reflexivity. }
    assert (Hvalues : forall (reached : state),
      region_steps loop.(body) start reached ->
      coherent_at blocks entry (current_registers reached)
        (state_program_point reached)).
    { intros reached Hrun.
      eapply context_execution_values_coherent;
        [exact edges | exact well_formed |].
      eapply context_steps_trans; [exact prefix |].
      apply context_of_steps, (region_steps_execution _ _ _ Hrun). }
    destruct v as [id op args index | b index].
    - destruct op; cbn [loop_invariant_value] in Hinvariant;
        try solve
          [lazymatch type of Hinvariant with ex _ => idtac end;
           destruct Hinvariant as [defining_block [Hlookup [Hdom Hstrict]]];
           apply Hframe;
           eapply invariant_result_defined_outside_loop; eassumption].
      all: try solve [apply Hframe;
        eapply unmodeled_result_defined_outside_loop; [ | exact Hfirst];
          reflexivity].
      all: pose proof (Hvalues first Hrun_first _ Hfirst)
          as Hcoherent_first;
        destruct Hcoherent_first as [values_first [result_first
          [Hread_first [Heval_first Hvalue_first]]]];
        cbn [eval_operation] in Heval_first; try discriminate.
      injection Heval_first as <-.
      pose proof (Hvalues second Hrun_second _ Hsecond)
        as Hcoherent_second.
      destruct Hcoherent_second
        as [values_second [result_second
          [Hread_second [Heval_second Hvalue_second]]]].
      cbn [eval_operation] in Heval_second.
      injection Heval_second as <-. cbn [register_of_value].
      rewrite Hvalue_first, Hvalue_second. reflexivity.
    - destruct Hinvariant as [Hdom Hstrict]. apply Hframe.
      change (~ (set_mem b loop.(body) = true)).
      rewrite (strict_dominator_outside_loop b Hdom Hstrict).
      discriminate.
  Qed.
End Invariants.
