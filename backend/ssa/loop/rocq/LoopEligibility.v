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

(* Semantic eligibility for bypassing a loop, stated on the original graph.

   This file is the statement to review: [eligible_entry] and
   [find_deletions_eligible]. Everything else in it is proof.

   [eligible_entry header exit header_regs] is the target property, not a
   record of analysis answers. Entering [header] with registers [header_regs],
   the original graph executes the header's instructions once, then its
   execution first arrives at [exit] after finitely many steps of
   SsaSemantics.step, at the exit's entry with no parameter to bind, holding
   the value computed by that first header execution in every register the
   exit's continuation reads before redefining it. The retained header is
   executed, not skipped. [steps_until] is a property of the one execution from
   the header, since [step] is deterministic; so the loop neither diverges nor
   gets stuck before the exit, and its steps are whitelisted integer
   operations, Gotos and Switches, because [step] has no other rule.

   [find_deletions_eligible] proves this for every header/exit pair the
   extracted gate and analyses report, given [finished_graph] (what
   Ssa.finish_graph guarantees of the graph, SsaProgress.v) and
   [reached_header] (bound inputs and an observed prefix to the header,
   SsaContext.v). It assumes no recurrence, invariant bound, guard outcome or
   finite exit; those are conclusions. The loop and its body never appear in
   the statement: which blocks the execution visits before the exit is
   irrelevant to bypassing it.

   The loop executes in SsaSemantics.v. Its unchanged prefix uses
   SsaContext.v's word-observation overapproximation, so loads, calls and other
   opaque computations need not be given full memory semantics. Review that
   abstraction and the graph contracts in SsaExecution.v, SsaGraph.v and
   SsaProgress.v alongside the extraction veneer. Finite exit follows by
   iteration; definedness uses classical case splits. Neither introduces
   semantic axioms.

   No post-deletion graph, renaming or pruning is modelled here, and
   eligibility is not assumed as a compiler contract. The step from
   [eligible_entry], per loop and per header entry, to the rewritten function
   behaving like the original is the usual liveness simulation; it is written
   out in ROCQ-REVIEW-GUIDE.md, section 4. Its one non-obvious point: a
   register live at the exit's entry is live at the header's terminator,
   because the exit is a successor of the header and has no parameters, so
   nothing is defined on entering it. Nothing is extracted. *)

From Stdlib Require Import List Bool.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlList.
From OxCamlSsa Require Import SsaModel SsaGraph SsaDominance SsaSemantics
  SsaExecution SsaLiveness SsaProgress SsaContext.
From OxCamlLoop Require Import NaturalLoop InductionVar DeleteEmptyLoops
  LoopExecution LoopLiveOut LoopCounterExecution LoopRangeExecution.
Import ListNotations.

(* ======================================================================== *)
(* Part 1: the statement                                                    *)
(* ======================================================================== *)

(* A valid bypass retains the first header body, then replaces only its
   terminator by a jump to the parameterless exit. All execution relations here
   refer to the unchanged original graph. [steps_until exit] stops at the first
   state in [exit], so no instruction of the exit is included. *)
Definition eligible_entry (header exit : block) (header_regs : registers)
    : Prop :=
  params exit = []
  ∧ ∃ (first_header_regs exit_regs : registers),
       executes_insns header_regs (block_body header) first_header_regs
       ∧ steps_until exit
            {| current_block := header; remaining := [];
               current_registers := first_header_regs |}
            (enter exit exit_regs)
       ∧ agree_on_live (block_entry_point exit) first_header_regs exit_regs.

(* ======================================================================== *)
(* Part 2: proof                                                            *)
(* ======================================================================== *)

(* The same property phrased on the discovered loop: the execution to the exit
   stays in the loop body. This is what the operational proofs establish; it
   implies [eligible_entry]. *)
Definition eligible_loop_entry (loop : natural_loop) (exit : block)
    (header_regs : registers) : Prop :=
  set_mem loop.(header) loop.(body) = true
  ∧ set_mem exit loop.(body) = false
  ∧ params exit = []
  ∧ ∃ (first_header_regs exit_regs : registers),
       executes_insns header_regs (block_body loop.(header)) first_header_regs
       ∧ region_steps loop.(body)
            {| current_block := loop.(header); remaining := [];
               current_registers := first_header_regs |}
            (enter exit exit_regs)
       ∧ agree_on_live (block_entry_point exit) first_header_regs exit_regs.

Lemma eligible_loop_entry_eligible : ∀ (loop : natural_loop)
    (exit : block) (header_regs : registers),
  eligible_loop_entry loop exit header_regs ->
  eligible_entry loop.(header) exit header_regs.
Proof.
  intros loop exit header_regs
    [_ [Houtside [Hparams [first [last [Hbody [Hrest Hagree]]]]]]].
  split; [exact Hparams |]. exists first, last.
  split; [exact Hbody |]. split; [| exact Hagree].
  change exit with (current_block (enter exit last)) at 1.
  apply (region_steps_until loop.(body)); [exact Hrest | exact Houtside].
Qed.

(* For an accepted loop, a finite original execution to its exit suffices: the
   first header computation and live-out agreement are both derived. No
   equality of dead counter values is required. *)
Theorem certificate_finite_exit_eligible : control_edges ->
  ∀ (blocks : list block) (entry : block) (defs : op_def)
    (loop : natural_loop) (bivs : list biv) (exit : block)
    (initial_regs header_regs exit_regs : registers),
  well_formed_ssa blocks entry -> loop_graph_properties blocks entry loop ->
  deletion_certificate defs blocks loop bivs exit ->
  context_steps (enter entry initial_regs) (enter loop.(header) header_regs) ->
  region_steps loop.(body) (enter loop.(header) header_regs)
    (enter exit exit_regs) ->
  eligible_loop_entry loop exit header_regs.
Proof.
  intros Hedges blocks entry defs loop bivs exit initial_regs header_regs
    exit_regs Hwf Hgraph Hcertificate Hprefix Hrun.
  pose proof Hcertificate as [[exit_info [Hexit [Htarget Hparams]]] Hrest].
  destruct (find_exit_branch_target loop exit_info Hexit) as [Houtside
    Htargets].
  rewrite Htarget in Houtside.
  pose proof (loop_header_in_body _ _ _ Hgraph) as Hinside.
  destruct (region_exit_body_prefix loop.(body) loop.(header)
    (block_body loop.(header)) header_regs (enter exit exit_regs)
    Hinside Houtside Hrun) as [first [Hbody Hsegment]].
  split; [exact Hinside |]. split; [exact Houtside |].
  split; [exact Hparams |]. exists first, exit_regs.
  split; [exact Hbody |]. split; [exact Hsegment |].
  eapply finite_exit_live_agreement; eassumption.
Qed.

(* Progress inside an accepted loop follows from the instruction whitelist and
   ordinary SSA input/argument definedness. The only dynamic side condition is
   the language's defined-index rule for Switch. *)
Theorem certificate_region_progress : control_edges ->
  ∀ (blocks : list block) (entry : block) (defs : op_def)
    (loop : natural_loop) (bivs : list biv) (exit : block)
    (initial_regs header_regs : registers),
  well_formed_ssa blocks entry -> control_layout blocks ->
  loop_graph_properties blocks entry loop ->
  deletion_certificate defs blocks loop bivs exit ->
  live_registers_defined (block_entry_point entry) initial_regs ->
  switches_in_range (enter loop.(header) header_regs) ->
  context_steps (enter entry initial_regs) (enter loop.(header) header_regs) ->
  ∀ (reached : state),
  region_steps loop.(body) (enter loop.(header) header_regs) reached ->
  set_mem (current_block reached) loop.(body) = true ->
  ∃ (next : state), SsaSemantics.step reached next.
Proof.
  intros Hedges blocks entry defs loop bivs exit initial_regs header_regs
    Hwf Hlayout Hgraph Hcertificate Hinputs Hswitches Hprefix reached
    Hsegment Hinside.
  assert (Hrun : context_steps (enter entry initial_regs) reached).
  { eapply context_steps_trans; [exact Hprefix |].
    apply context_of_steps. exact (region_steps_execution _ _ _ Hsegment). }
  eapply supported_state_progress; [exact Hwf | exact Hlayout | | | | |].
  - eapply context_steps_valid_position;
      [exact Hrun | apply enter_valid_position].
  - apply (loop_body_in_graph _ _ _ Hgraph). exact Hinside.
  - eapply context_execution_live_registers_defined; eassumption.
  - destruct Hcertificate as [_ [_ [_ [_ [Hsupported _]]]]].
    destruct (Hsupported _ Hinside) as [Hinsns Hterm].
    unfold supported_block. apply andb_true_iff. split; [ | exact Hterm].
    apply forallb_forall. exact Hinsns.
  - intros index targets word Hremaining Hterm Hword.
    eapply Hswitches; [exact (region_steps_execution _ _ _ Hsegment) |
      exact Hremaining | exact Hterm | exact Hword].
Qed.

(* Full semantic eligibility in the integer/control-flow semantics. Finite exit
   is a conclusion, as are the actual counter recurrence, bound constancy,
   entry guards, range facts and live-out agreement. No post-deletion graph is
   needed. *)
Theorem certificate_entry_eligible : control_edges ->
  ∀ (blocks : list block) (entry : block) (defs : op_def)
    (loop : natural_loop) (bivs : list biv) (exit : block)
    (initial_regs header_regs : registers),
  well_formed_ssa blocks entry -> control_layout blocks ->
  loop_graph_properties blocks entry loop -> dominance_semantics entry ->
  op_def_correct defs blocks ->
  (∀ (dominator target : block),
    dominator ∈ blocks -> target ∈ blocks ->
    (∀ (visited_blocks : list block), SsaDominance.block_path_from entry
      visited_blocks target ->
       dominator ∈ visited_blocks) -> dominator ▷ target) ->
  switch_edge_multiplicity -> graph_size_supported blocks ->
  NoDup (strict_dominators loop.(header)) ->
  (∀ (dominator : block),
     dominator ∈ (strict_dominators loop.(header)) -> dominator ∈ blocks) ->
  deletion_certificate defs blocks loop bivs exit ->
  bounded_registers initial_regs ->
  live_registers_defined (block_entry_point entry) initial_regs ->
  switches_in_range (enter loop.(header) header_regs) ->
  context_steps (enter entry initial_regs) (enter loop.(header) header_regs) ->
  eligible_loop_entry loop exit header_regs.
Proof.
  intros Hedges blocks entry defs loop bivs exit initial_regs header_regs
    Hwf Hlayout Hgraph Hdominance Hdefs Hcomplete Hmultiplicity Hsize Hnodup
    Hmembers Hcertificate Hbounded Hinputs Hswitches Hprefix.
  assert (Hprogress : forall (reached : state),
    region_steps loop.(body) (enter loop.(header) header_regs) reached ->
    set_mem (current_block reached) loop.(body) = true ->
    exists (next : state), SsaSemantics.step reached next).
  { eapply certificate_region_progress; eassumption. }
  destruct (accepted_loop_finite_exit blocks entry defs loop bivs exit
    initial_regs header_regs Hedges Hwf Hgraph Hcertificate Hprefix
    Hprogress Hdominance Hdefs Hcomplete Hmultiplicity Hsize Hnodup Hmembers
    Hbounded) as [reached [Hrun Houtside]].
  pose proof Hcertificate as [_ [_ [_ [_ [_ Honly]]]]].
  destruct (region_exit_target Hedges loop exit
    (enter loop.(header) header_regs) reached Honly
    (loop_header_in_body _ _ _ Hgraph) Hrun Houtside) as [exit_regs Heq].
  subst reached. eapply certificate_finite_exit_eligible; eassumption.
Qed.

(* The public theorem starts from the actual deletion decision. Its graph
   premise is builder metadata only; its input premise is ordinary bound
   arguments, an observed prefix and defined Switch indices. No loop recurrence
   or termination premise remains. *)
Theorem find_deletions_eligible :
  ∀ (blocks : list block) (entry : block) (defs : op_def)
    (header exit : block),
  finished_graph blocks entry defs ->
  (header, exit) ∈ (find_deletions defs blocks) ->
  ∀ (initial_regs header_regs : registers),
    reached_header entry header initial_regs header_regs ->
    eligible_entry header exit header_regs.
Proof.
  intros blocks entry defs header exit Hgraph Hin initial_regs header_regs
    Hreached.
  destruct Hgraph as [Hwf Hpreds Hedges Hmultiplicity Hdominance Hcomplete
    Hdominators Hlayout Hdefs].
  destruct Hreached as [Hbounded Hinputs Hprefix Hswitches].
  pose proof (proj1 (find_deletions_supported _ _ _ _ Hin)) as Hsize.
  destruct (find_deletions_graph_properties blocks entry Hpreds
    (graph_successors_closed _ _ Hwf) Hdominance Hcomplete defs header exit
    Hin) as [loop [bivs [Hloop [Hheader [Hcertificate Hloop_graph]]]]].
  subst header.
  destruct (Hdominators _ (loop_header_in_graph _ _ _ Hloop_graph))
    as [Hnodup Hmembers].
  apply eligible_loop_entry_eligible.
  eapply certificate_entry_eligible; eassumption.
Qed.
