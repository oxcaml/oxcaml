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

(* Which loops to delete: [find_deletions], which combines the analyses of
   NaturalLoop.v, InductionVar.v, LoopTermination.v and DeadInductionVar.v into
   the decision. The extraction of this file, DeleteEmptyLoops.ml, is the
   implementation; delete_empty_loops.ml in the parent directory wraps it,
   builds the table of defining blocks ([op_def]) it is given, and rewrites the
   graph at the (header, exit target) pairs it returns ([run], over
   [Ssa_reducer]), which is not modelled.

   See the reading guide at the top of LoopComparisons.v for the Rocq
   vocabulary and SsaModel.v for how the models see an SSA graph. As there, the
   file has four parts: (1) the definitions, (2) the intended meaning, (3)
   stepping-stone lemmas, (4) the theorems. Review parts 1 and 2 and the
   theorem statements; Rocq checks the proofs.

   The global applicability gate checks graph size and excludes incompatible
   result counts of modeled operations in surrounding code.
   [find_deletions_supported] reads these checks back; graph size is not
   assumed by the public eligibility theorem.

   The result, in part 4, is [find_deletions_spec]: a loop is deleted only when
   [deletion_certificate] (part 2) holds of it. Its header's exit branch is the
   simple one LoopTermination.v recognises, with an exit target that takes no
   arguments; the termination analysis has returned [Terminates], and the body
   without the header has a well-founded successor relation
   ([SsaGraph.region_acyclic_except]); each header parameter is a basic
   induction variable (InductionVar.v) that is dead in the sense of
   DeadInductionVar.v; every block of its body has only whitelisted integer
   instructions and a [Goto] or [Switch] terminator; and all edges leaving the
   body go to that exit target. These are analysis certificates, not a
   rewrite-correctness theorem. The operational root is
   LoopEligibility.find_deletions_eligible: from the decision, graph contracts
   and a defined input, it proves finite normal first exit and live-out
   agreement with the first header execution. Counter recurrence, bound
   constancy and affine entry-guard facts are derived from execution, including
   opaque surrounding computations in the unchanged prefix. The mechanical
   graph rewrite is outside this proof's scope.

   The whitelist also covers the header's own instructions, which still run
   once after the rewrite, as they did on the first iteration before it. Being
   free of side effects, they can only be observed through a value used after
   the loop. By SSA scoping no block of the body but the header dominates the
   exit, so such a value is a header parameter, which DeadInductionVar.v
   forbids any use of outside the loop's own control, or is defined by a header
   instruction, whose inputs are values defined before the loop, earlier
   integer instructions, or header parameters; and a parameter used by an
   instruction other than the exit comparison and its own update is not dead,
   which keeps the loop. So every such live-out value agrees between the first
   header execution and the finite exit. LoopLiveOut.finite_exit_live_agreement
   proves this from execution and SSA well-formedness. Its liveness definition
   includes later reads before redefinition, including paths that re-enter the
   loop. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlList.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaGraph SsaSemantics.
From OxCamlLoop Require Import NaturalLoop InductionVar LoopTermination
  DeadInductionVar.
Import ListNotations.

(* ======================================================================== *)
(* Part 1: the definitions                                                  *)
(* ======================================================================== *)

(* This leaves room in the affine atom counter even if every graph block
   supplies a two-operand guard. Check before running any loop analysis. *)
Definition graph_block_limit : int := 4294967296%sint63.

Definition graph_supported (blocks : list block) : bool :=
  length_at_most graph_block_limit blocks
  && forallb (fun b => forallb context_instruction_supported (block_body b))
       blocks.

(* Only the integer/control-flow fragment of SsaSemantics.v is eligible. In
   particular, a load is rejected even though the compiler's
   [Ssa.Instruction.has_side_effect] says it has none. The header belongs to
   the body, so its instructions are checked too. *)
Definition body_supported (loop : natural_loop) : bool :=
  set_for_all supported_block loop.(body).

(* Every edge out of a body block stays in the body or goes to [exit].
   [body_supported] alone does not exclude a [Switch] or [Goto] to another
   outside target. Early exits are allowed, but only to this same target. *)
Definition single_exit (loop : natural_loop) (exit : block) : bool :=
  set_for_all
    (fun block =>
      forallb (fun succ => set_mem succ loop.(body) || block_eqb succ exit)
        (all_successors block))
    loop.(body).

(* [LoopTermination.analyze] as a boolean. It is the costly check, so
   [find_deletions] runs it last, once the others have passed. *)
Definition loop_terminates (op_def : op_def) (bivs : list InductionVar.biv)
    : bool :=
  match LoopTermination.analyze op_def bivs with
  | Terminates => true
  | Unknown => false
  end.

(* The loops to delete, as (header, exit target) pairs, given the graph's block
   list and the table of defining blocks. *)
Definition find_deletions (op_def : op_def) (blocks : list block)
    : list (block * block) :=
  if graph_supported blocks then
  let loops := InductionVar.analyze op_def blocks in
  let dead := DeadInductionVar.analyze blocks loops in
  filter_map
    (fun result =>
      let loop := result.(loop) in
      let bivs := map (fun biv_result => biv_result.(biv)) result.(bivs) in
      if result.(useless) && body_supported loop
      then
        match find_exit_branch loop with
        | Some exit_info =>
          match params exit_info.(exit_target) with
          | [] =>
            if single_exit loop exit_info.(exit_target)
               && acyclic_except blocks loop.(body) loop.(header)
               && loop_terminates op_def bivs
            then Some (loop.(header), exit_info.(exit_target))
            else None
          | _ :: _ => None
          end
        | None => None
        end
      else None)
    dead
  else [].

(* ======================================================================== *)
(* Part 2: the intended meaning                                             *)
(* ======================================================================== *)

(* Every instruction can execute in the integer fragment, and every terminator
   is internal control flow. [supported_instruction_executes] gives instruction
   progress when the operands are bound; the frame and boundedness theorems
   there describe exactly what execution changes. *)
Definition body_in_supported_fragment (loop : natural_loop) : Prop :=
  ∀ (block : block), set_mem block loop.(body) = true ->
    (∀ (insn : instr), insn ∈ (block_body block) ->
       supported_instruction insn = true)
    ∧ terminator_pure_control (terminator block) = true.

(* Every edge out of a block of the body stays in the body or goes to [exit].
   *)
Definition exits_only_to (loop : natural_loop) (exit : block) : Prop :=
  ∀ (pred : block), set_mem pred loop.(body) = true ->
    ∀ (succ : block), succ ∈ (all_successors pred) ->
      set_mem succ loop.(body) = true ∨ succ = exit.

(* The header's exit branch, in the shape LoopTermination.v recognises, goes to
   [exit], which takes no arguments. *)
Definition exit_branch_to (loop : natural_loop) (exit : block) : Prop :=
  ∃ (exit_info : exit_branch),
    find_exit_branch loop = Some exit_info
    ∧ exit_info.(exit_target) = exit ∧ params exit = [].

(* Every parameter of the header is one of [bivs], and each of those is a basic
   induction variable of [loop] that is dead. *)
Definition params_are_dead_bivs (op_def : op_def) (blocks : list block)
    (loop : natural_loop) (bivs : list InductionVar.biv) : Prop :=
  (∀ (parameter_index : int) (parameter : value), nth_int (params
    loop.(header)) parameter_index = Some parameter ->
     ∃ (biv : InductionVar.biv),
       biv ∈ bivs ∧ biv.(param_index) = parameter_index)
  ∧ (∀ (biv : InductionVar.biv), biv ∈ bivs ->
        biv_spec op_def loop biv ∧ dead_spec blocks biv).

(* What holds of a loop [loop], with the basic induction variables [bivs]
   InductionVar.v reports for it, when [find_deletions] deletes it. *)
Definition deletion_certificate (op_def : op_def) (blocks : list block)
    (loop : natural_loop) (bivs : list InductionVar.biv) (exit : block)
    : Prop :=
  exit_branch_to loop exit
  ∧ LoopTermination.analyze op_def bivs = Terminates
  ∧ region_acyclic_except loop.(body) loop.(header)
  ∧ params_are_dead_bivs op_def blocks loop bivs
  ∧ body_in_supported_fragment loop
  ∧ exits_only_to loop exit.

(* ======================================================================== *)
(* Part 3: lemmas used by the proofs                                        *)
(* ======================================================================== *)

(* The loop's instruction whitelist and control-flow restriction. *)
Lemma body_supported_spec : ∀ (loop : natural_loop),
  body_supported loop = true -> body_in_supported_fragment loop.
Proof.
  intros l H bl Hbl. unfold body_supported in H.
  rewrite set_for_all_spec in H. apply supported_block_spec, H. exact Hbl.
Qed.

(* What [single_exit] checks, read back. *)
Lemma single_exit_spec : ∀ (loop : natural_loop) (exit : block),
  single_exit loop exit = true -> exits_only_to loop exit.
Proof.
  intros l exit H bl Hbl s Hs. unfold single_exit in H.
  rewrite set_for_all_spec in H. specialize (H bl Hbl).
  rewrite forallb_forall in H. specialize (H s Hs).
  apply orb_true_iff in H. destruct H as [H | H]; [ left; exact H | ].
  right. apply block_eqb_spec. exact H.
Qed.

(* A successful termination check supplies a particular analyzed counter whose
   comparison passed. This witness is used by operational proofs. *)
Lemma certificate_counter : ∀ (defs : op_def) (blocks : list block)
    (loop : natural_loop) (bivs : list InductionVar.biv) (exit : block),
  deletion_certificate defs blocks loop bivs exit ->
  ∃ (counter : InductionVar.biv), counter ∈ bivs
    ∧ biv_spec defs loop counter
    ∧ biv_implies_termination defs counter = true.
Proof.
  intros defs blocks loop bivs exit [_ [Htest [_ [[_ Hspecs] _]]]].
  unfold LoopTermination.analyze in Htest.
  destruct (existsb (biv_implies_termination defs) bivs) eqn:Hcounter;
    [ | discriminate].
  apply existsb_exists in Hcounter.
  destruct Hcounter as [counter [Hin Haccepted]].
  exists counter. split; [exact Hin |]. split.
  - exact (proj1 (Hspecs counter Hin)).
  - exact Haccepted.
Qed.

(* ======================================================================== *)
(* Part 4: theorems                                                         *)
(* ======================================================================== *)

(* Every reported deletion passed both applicability checks. This removes graph
   size as a premise of the public eligibility theorem and enforces the
   syntactic coverage of the unchanged-prefix observation semantics: every
   exactly modeled operation in the graph has one result, so SsaContext.v has a
   rule for every instruction of every prefix. *)
Theorem find_deletions_supported :
  ∀ (defs : op_def) (blocks : list block) (header exit : block),
  (header, exit) ∈ (find_deletions defs blocks) ->
  graph_size_supported blocks ∧
  ∀ (b : block) (id : instr_id) (op : operation) (args : list value)
    (count : int),
    b ∈ blocks -> (Op id op args count) ∈ (block_body b) ->
    modeled_operation op args = true -> count = one.
Proof.
  intros defs blocks header exit Hin. unfold find_deletions in Hin.
  destruct (graph_supported blocks) eqn:Hcheck; [ | contradiction].
  unfold graph_supported in Hcheck. apply andb_true_iff in Hcheck.
  destruct Hcheck as [Hsize Hprefix]. split.
  - apply length_at_most_spec in Hsize. exact Hsize.
  - intros b id op args count Hb Hinsn. rewrite forallb_forall in Hprefix.
    specialize (Hprefix b Hb). rewrite forallb_forall in Hprefix.
    specialize (Hprefix _ Hinsn).
    intros Hmodeled. destruct op;
      cbn [context_instruction_supported modeled_operation] in *;
      try discriminate; try (apply eqb_int; exact Hprefix).
    all: destruct op; cbn [context_instruction_supported] in Hprefix;
      try (apply eqb_int; exact Hprefix);
      destruct args as [|x [|y [|extra rest]]]; discriminate.
Qed.

(* Every pair [(header, exit)] that [find_deletions] returns names the header
   of a loop [InductionVar.analyze] reports, with its basic induction
   variables, and that loop satisfies [deletion_certificate] with exit [exit].
   *)
Theorem find_deletions_spec :
  ∀ (op_def : op_def) (blocks : list block) (header exit : block),
  (header, exit) ∈ (find_deletions op_def blocks) ->
  ∃ (loop : natural_loop) (bivs : list InductionVar.biv),
    (loop, bivs) ∈ (InductionVar.analyze op_def blocks)
    ∧ header = loop.(NaturalLoop.header)
    ∧ deletion_certificate op_def blocks loop bivs exit.
Proof.
  intros op_def blocks h exit H. unfold find_deletions in H.
  destruct (graph_supported blocks); [ | contradiction]. cbv zeta in H.
  apply filter_map_In in H. destruct H as [dr [Hdr H]].
  destruct (DeadInductionVar.analyze_spec _ _ _ Hdr) as [Hin [Hbivs Huseless]].
  cbv zeta in H.
  destruct (dr.(useless) && body_supported dr.(loop)) eqn:Hcond;
    [ | discriminate ].
  apply andb_true_iff in Hcond. destruct Hcond as [Hu Hfree].
  destruct (find_exit_branch dr.(loop)) as [exit_info | ] eqn:Hexit;
    [ | discriminate ].
  destruct (params exit_info.(exit_target)) eqn:Hparams; [ | discriminate ].
  destruct (single_exit dr.(loop) exit_info.(exit_target) && _ && _)
    eqn:Hcond; [ | discriminate ].
  apply andb_true_iff in Hcond. destruct Hcond as [Hcond Hterm].
  apply andb_true_iff in Hcond. destruct Hcond as [Hsingle Hacyclic].
  unfold loop_terminates in Hterm.
  destruct (LoopTermination.analyze op_def _) eqn:Hanalyze;
    [ | discriminate ].
  injection H as <- <-.
  exists dr.(loop), (map biv dr.(bivs)).
  split; [ exact Hin | split; [ reflexivity | ] ].
  destruct (InductionVar.analyze_spec _ _ _ _ Hin) as [_ [Hdef Hspec]].
  destruct (Huseless Hu) as [Hlen Hdead].
  unfold deletion_certificate.
  split; [ | split; [ | split; [ | split; [ | split ] ] ] ].
  - unfold exit_branch_to. exists exit_info.
    split; [ exact Hexit | split; [ reflexivity | exact Hparams ] ].
  - exact Hanalyze.
  - apply (acyclic_except_sound blocks). exact Hacyclic.
  - unfold params_are_dead_bivs. split.
    + intros k v Hk. rewrite Hdef.
      apply (analyze_loop_cover _ _ _ v); [ | exact Hk ].
      rewrite <- Hdef, length_map. symmetry. exact Hlen.
    + intros b Hb. split; [ apply Hspec; exact Hb | ].
      apply in_map_iff in Hb. destruct Hb as [br [<- Hbr]].
      destruct (Hbivs br Hbr) as [_ Hd]. apply Hd, Hdead. exact Hbr.
  - apply body_supported_spec. exact Hfree.
  - apply single_exit_spec. exact Hsingle.
Qed.
