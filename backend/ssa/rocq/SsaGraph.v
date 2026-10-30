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

(* Properties of the blocks, terminators, instructions and values of an SSA
   graph that any pass may ask about, computed on the graph as SsaModel.v sees
   it: what a terminator does, what an instruction or a terminator reads, which
   values a block's [Goto] passes, whether every use of a value in the graph is
   approved, and whether a region is acyclic after removing one block. The
   extraction of this file, SsaGraph.ml, is the implementation.

   See the reading guide in backend/ssa/loop/rocq/LoopComparisons.v
   for the Rocq vocabulary and SsaModel.v for how the models see an SSA graph.
   As there, the file has four parts: (1) the definitions, (2) the intended
   meaning, (3) stepping-stone lemmas, (4) the theorems. Parts 1 and 2 are the
   part to check by eye.

   The arguments of instructions and terminators, and a block's parameters, are
   lists here, indexed by [nth_int] (OxCamlList.v).

   The theorems of part 4 read each boolean check of part 1 back as its meaning
   in part 2. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlList.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel.
Import ListNotations.

(* ======================================================================== *)
(* Part 1: the definitions                                                  *)
(* ======================================================================== *)

Local Open Scope sint63_scope.

(* A terminator that only transfers control within the function. *)
Definition terminator_pure_control (t : Terminator.t) : bool :=
  match t with
  | Terminator.Continue (Goto _) _ | Terminator.Switch _ _ => true
  | Terminator.Continue (Return | Raise _ | Unreachable) _
  | Terminator.Call _ | Terminator.Invalid _ => false
  end.

(* [v] is parameter [index] of [block]. *)
Definition is_param (block : block) (index : int) (v : value) : bool :=
  value_eqb v (param block index).

(* [instr] is the instruction defining [v]. *)
Definition instr_defines_value (instr : instr) (v : value) : bool :=
  match instr, v with
  | Op id _ _ _, Res id' _ _ _ => instr_id_eqb id id'
  | _, _ => false
  end.

(* The values an instruction reads. *)
Definition args_of_instr (instr : instr) : list value :=
  match instr with
  | Op _ _ args _ => args
  | Push_trap | Pop_trap => []
  end.

(* The values a terminator reads, one list per kind of use: the arguments a
   [Continue] passes, the index of a [Switch], the arguments of a [Call] or an
   [Invalid]. *)
Definition terminator_args (term : Terminator.t) : list (list value) :=
  match term with
  | Terminator.Continue _ args =>
    [filter_map
       (fun arg =>
         match arg with
         | Terminator.Arg v => Some v
         | Terminator.Omitted_since_unused => None
         end)
       args]
  | Terminator.Switch index _ => [[index]]
  | Terminator.Call args => [args]
  | Terminator.Invalid args => [args]
  end.

(* [arg] passes the value [v]. *)
Definition arg_is (v : value) (arg : Terminator.arg) : bool :=
  match arg with
  | Terminator.Arg w => value_eqb w v
  | Terminator.Omitted_since_unused => false
  end.

(* The value an argument at position [index] passes, if any. *)
Definition arg_value (args : list Terminator.arg) (index : int)
    : option value :=
  match nth_int args index with
  | Some (Terminator.Arg v) => Some v
  | Some Terminator.Omitted_since_unused | None => None
  end.

(* The arguments [pred] passes to [target], when its terminator is a [Goto]
   there with one argument per parameter. *)
Definition goto_args (target pred : block) : option (list Terminator.arg) :=
  match terminator pred with
  | Terminator.Continue (Goto goto) args =>
    if block_eqb goto target && same_length args (params target)
    then Some args
    else None
  | _ => None
  end.

(* The terminator of [block] is a [Goto] to [target] passing [argument_value]
   as the argument at position [argument_index], and at no other position. *)
Definition goto_passes_only_at (target : block) (argument_index : int)
  (argument_value : value) (block : block)
    : bool :=
  match terminator block with
  | Terminator.Continue (Goto goto) args =>
    block_eqb goto target
    && (match arg_value args argument_index with
        | Some w => value_eqb w argument_value
        | None => false
        end)
    && (count_occurrences (arg_is argument_value) args =? one)
  | Terminator.Continue (Return | Raise _ | Unreachable) _
  | Terminator.Switch _ _ | Terminator.Call _ | Terminator.Invalid _ => false
  end.

(* Every use in the graph of a value [same] accepts is approved: by
   [approve_instr] for an instruction reading it, by [approve_term] for the
   block whose terminator reads it, given the list it is read in. *)
Definition all_uses_ok (blocks : list block) (same : value -> bool)
    (approve_instr : instr -> bool)
    (approve_term : block -> list value -> bool) : bool :=
  forallb
    (fun block =>
      forallb
        (fun instr =>
          negb (existsb same (args_of_instr instr)) || approve_instr instr)
        (block_body block)
      && forallb
           (fun operands =>
             negb (existsb same operands) || approve_term block operands)
           (terminator_args (terminator block)))
    blocks.

(* Visit roots from left to right, threading the completed set. *)
Fixpoint visit_all (visit : blockset -> block -> option blockset)
    (seen : blockset) (roots : list block) : option blockset :=
  match roots with
  | [] => Some seen
  | block :: roots =>
    let* seen := visit seen block in visit_all visit seen roots
  end.

(* A depth-first acyclicity check on [region] without [cut]. [active] holds the
   current path, so a cycle is rejected immediately; [seen] memoizes completed
   blocks, so shared successors are traversed only once. Fuel bounds recursive
   depth for Rocq; the graph's block list is enough for any acyclic region. *)
Fixpoint visit_acyclic (fuel : list block) (region : blockset) (cut : block)
    (active seen : blockset) (block : block) : option blockset :=
  if block_eqb block cut || negb (set_mem block region) || set_mem block seen
  then Some seen
  else if set_mem block active then None
  else
    match fuel with
    | [] => None
    | _ :: fuel =>
      let* seen := visit_all
        (visit_acyclic fuel region cut (set_add block active)) seen
        (all_successors block) in
      Some (set_add block seen)
    end.

(* Check every block of the region, not just the part reached from [cut]. The
   final coverage check prevents missing roots from hiding a cycle. *)
Definition acyclic_except (blocks : list block) (region : blockset)
    (cut : block) : bool :=
  match visit_all (visit_acyclic blocks region cut set_empty)
    set_empty blocks with
  | None => false
  | Some seen =>
    set_for_all (fun block => block_eqb block cut || set_mem block seen) region
  end.

(* ======================================================================== *)
(* Part 2: the intended meaning                                             *)
(* ======================================================================== *)

(* The compiler's edge observations agree with the terminators and with one
   another. Extra successors (for example exception edges) are allowed; every
   ordinary control transfer must still appear in the graph. *)
Record control_edges : Prop :=
  { goto_successor : ∀ (source target : block)
      (args : list Terminator.arg),
      terminator source = Terminator.Continue (Goto target) args ->
      target ∈ (all_successors source);
    switch_successor : ∀ (source : block) (index : value)
      (targets : list block) (target : block),
      terminator source = Terminator.Switch index targets ->
      target ∈ targets -> target ∈ (all_successors source);
    predecessor_successor : ∀ (source target : block),
      source ∈ (predecessors target) <-> target ∈ (all_successors source) }.

(* Predecessor metadata retains one occurrence for each Switch edge, including
   repeated targets. Other incoming edges may add occurrences. This is a
   property of graph construction, not of branch execution. *)
Definition switch_edge_multiplicity : Prop :=
  ∀ (source : block) (index : value) (targets : list block)
    (target : block),
  terminator source = Terminator.Switch index targets ->
  (length (filter (block_eqb target) targets)
    <= length (filter (block_eqb source) (predecessors target)))%nat.

(* Every instruction in the supplied graph resolves to its containing block in
   the table built by the OCaml veneer. No claim is needed about IDs that do
   not occur in the graph. *)
Definition op_def_correct (defs : op_def) (blocks : list block) : Prop :=
  ∀ (b : block) (id : instr_id) (op : operation) (args : list value)
    (results : int),
  b ∈ blocks -> (Op id op args results) ∈ (block_body b) ->
  op_def_find defs id = Some b.

(* [uses_only blocks same approve_instr approve_term]: in every block of the
   graph, every instruction with an argument [same] accepts satisfies
   [approve_instr], and every argument list of a terminator with a value [same]
   accepts satisfies [approve_term] with its block. *)
Definition uses_only (blocks : list block) (same : value -> bool)
    (approve_instr : instr -> Prop)
    (approve_term : block -> list value -> Prop) : Prop :=
  ∀ (block : block), block ∈ blocks ->
    (∀ (instr : instr), instr ∈ (block_body block) ->
       existsb same (args_of_instr instr) = true -> approve_instr instr)
    ∧ (∀ (operands : list value),
          operands ∈ (terminator_args (terminator block)) ->
          existsb same operands = true -> approve_term block operands).

(* The terminator of [block] is a [Goto] to [target] whose argument at position
   [argument_index] is [argument_value], and no other argument is
   ([count_occurrences ... = one]: one position passes it). *)
Definition passes_only_at (target : block) (argument_index : int)
  (argument_value : value) (block : block)
    : Prop :=
  ∃ args,
    terminator block = Terminator.Continue (Goto target) args
    ∧ arg_value args argument_index = Some argument_value
    ∧ count_occurrences (arg_is argument_value) args = one.

(* The checked graph-size limit used to bound affine atom allocation. *)
Definition graph_size_supported (blocks : list block) : Prop :=
  (Z.of_nat (length blocks) <= 2 ^ 32)%Z.

(* The successor relation restricted to [region] without [cut], oriented for
   accessibility: following an edge decreases in the relation. *)
Definition region_edge_avoiding (region : blockset)
  (cut succ block : block) : Prop :=
  set_mem block region = true ∧ block <> cut
  ∧ set_mem succ region = true ∧ succ <> cut
  ∧ succ ∈ (all_successors block).

(* Removing the header leaves a well-founded successor relation. *)
Definition region_acyclic_except (region : blockset) (cut : block) : Prop :=
  ∀ (b : block), set_mem b region = true -> b <> cut ->
    Acc (region_edge_avoiding region cut) b.

(* ======================================================================== *)
(* Part 3: lemmas used by the proofs                                        *)
(* ======================================================================== *)

(* --- Block equality and sets --- *)

(* [block_eqb] is equality: it accepts a block and itself, and rejects two
   different blocks. *)
Lemma block_eqb_refl : ∀ (block : block), block_eqb block block = true.
Proof. intros b. apply block_eqb_spec. reflexivity. Qed.

(* Membership in a set after [set_add], and in a singleton. *)
Lemma set_mem_add_iff : ∀ (block block' : SsaModel.block) (set : blockset),
  set_mem block (set_add block' set) = true
  <-> block = block' ∨ set_mem block set = true.
Proof.
  intros b x s. rewrite set_mem_add, orb_true_iff, block_eqb_spec. tauto.
Qed.

(* A singleton holds its element and nothing else. *)
Lemma set_mem_singleton_iff : ∀ (block block' : SsaModel.block),
  set_mem block (set_singleton block') = true <-> block = block'.
Proof. intros b x. rewrite set_mem_singleton. apply block_eqb_spec. Qed.

(* --- Acyclic regions --- *)

Section Acyclic.
  Variable region : blockset.
  Variable cut : block.

  (* Every block of [seen] is accessible for [region_edge_avoiding]: from it,
     no sequence of edges within the region avoiding [cut] goes on for ever. *)
  Definition completed_blocks_accessible (seen : blockset) : Prop :=
    ∀ (block : block),
      set_mem block seen = true -> Acc (region_edge_avoiding region cut) block.

  (* [block] needs no visit: it is [cut], outside the region, or in [seen]. *)
  Definition block_visit_covered (seen : blockset) (block : block) : Prop :=
    block = cut ∨ set_mem block region = false ∨ set_mem block seen = true.

  (* [old] is a subset of [new]. *)
  Definition blockset_subset (old new : blockset) : Prop :=
    ∀ (block : block),
      set_mem block old = true -> set_mem block new = true.

  (* Growing the completed set preserves coverage of a block. *)
  Lemma block_visit_covered_subset : ∀ (old new : blockset)
    (block : block),
    blockset_subset old new -> block_visit_covered old block ->
      block_visit_covered new block.
  Proof.
    intros old new b Hinc [H | [H | H]];
      [ left | right; left | right; right ]; auto.
  Qed.

  (* A skipped block is outside the region, the cut, or already completed. *)
  Lemma skipped_block_visit_covered : ∀ (seen : blockset) (block : block),
    block_eqb block cut || negb (set_mem block region) || set_mem block seen
    = true ->
    block_visit_covered seen block.
  Proof.
    intros seen b H. apply orb_true_iff in H. destruct H as [H | H].
    - apply orb_true_iff in H. destruct H as [H | H].
      + left. apply block_eqb_spec. exact H.
      + right; left. apply negb_true_iff. exact H.
    - right; right. exact H.
  Qed.

  (* If [visit] keeps the completed set completed and growing and covers the
     block visited, so does [visit_all] over [roots], covering every root. *)
  Lemma visit_all_spec : ∀ (visit : blockset -> block -> option blockset),
    (∀ (seen : blockset) (block : block) (completed_set : blockset),
       completed_blocks_accessible seen -> visit seen block = Some
         completed_set ->
       completed_blocks_accessible completed_set ∧ blockset_subset seen
         completed_set ∧ block_visit_covered completed_set block) ->
    ∀ (roots : list block) (seen completed_set : blockset),
      completed_blocks_accessible seen -> visit_all visit seen roots = Some
        completed_set ->
      completed_blocks_accessible completed_set
      ∧ blockset_subset seen completed_set
      ∧ ∀ (block : block), block ∈ roots -> block_visit_covered
        completed_set block.
  Proof.
    intros visit Hvisit roots. induction roots as [|b roots IH];
      intros seen out Hseen H; cbn [visit_all] in H.
    - injection H as <-. split; [ exact Hseen | ].
      split; [ intros x Hx; exact Hx | intros x Hx; contradiction ].
    - destruct (visit seen b) as [next |] eqn:Hv; [ | discriminate ].
      destruct (Hvisit _ _ _ Hseen Hv) as [Hnext [Hinc Hcover]].
      destruct (IH _ _ Hnext H) as [Hout [Hinc' Hcovers]].
      split; [ exact Hout | ]. split.
      + intros x Hx. apply Hinc', Hinc. exact Hx.
      + intros x [<- | Hx].
        * eapply block_visit_covered_subset; eassumption.
        * apply Hcovers. exact Hx.
  Qed.

  (* A successful [visit_acyclic] from [block] keeps the completed set
     completed and growing, and covers [block]: a block is added only once all
     its successors within the region are completed. *)
  Lemma visit_acyclic_spec :
    ∀ (fuel : list block) (active seen : blockset) (block : block)
      (completed_set : blockset),
    completed_blocks_accessible seen ->
    visit_acyclic fuel region cut active seen block = Some completed_set ->
    completed_blocks_accessible completed_set ∧ blockset_subset seen
      completed_set ∧ block_visit_covered completed_set block.
  Proof.
    induction fuel as [|x fuel IH]; intros active seen b out Hseen H;
      cbn [visit_acyclic] in H;
      destruct (block_eqb b cut || negb (set_mem b region) || set_mem b seen)
        eqn:Hskip.
    - injection H as <-. split; [ exact Hseen | ].
      split; [ intros a Ha; exact Ha | apply skipped_block_visit_covered; exact
        Hskip ].
    - destruct (set_mem b active); discriminate.
    - injection H as <-. split; [ exact Hseen | ].
      split; [ intros a Ha; exact Ha | apply skipped_block_visit_covered; exact
        Hskip ].
    - destruct (set_mem b active); [ discriminate | ].
      destruct (visit_all
        (visit_acyclic fuel region cut (set_add b active)) seen
        (all_successors b)) as [next |] eqn:Hnext; [ | discriminate ].
      injection H as <-.
      destruct (visit_all_spec _ (IH (set_add b active)) _ _ _ Hseen Hnext)
        as [Hdone [Hinc Hchildren]].
      split.
      + intros a Ha. apply set_mem_add_iff in Ha. destruct Ha as [-> | Ha].
        * constructor. intros s [_ [_ [Hs [Hcut Hedge]]]].
          destruct (Hchildren s Hedge) as [H | [H | H]].
          -- contradiction.
          -- rewrite Hs in H. discriminate.
          -- apply Hdone. exact H.
        * apply Hdone. exact Ha.
      + split.
        * intros a Ha. apply set_mem_add_iff. right. apply Hinc. exact Ha.
        * right; right. apply set_mem_add_iff. left. reflexivity.
  Qed.

End Acyclic.

(* A concrete argument of a Continue is among that terminator's uses. *)
Lemma continue_argument_operand : ∀ (continuation : continuation)
    (args : list Terminator.arg) (arg : value),
  (Terminator.Arg arg) ∈ args ->
  ∃ (uses : list value),
    uses ∈ (terminator_args (Terminator.Continue continuation args))
    ∧ arg ∈ uses.
Proof.
  intros continuation args arg Harg. unfold terminator_args.
  eexists. split; [left; reflexivity |].
  induction args as [ | [v |] args IH]; cbn in *; try contradiction.
  - destruct Harg as [Heq | Hin].
    + injection Heq as ->. left. reflexivity.
    + right. apply IH. exact Hin.
  - destruct Harg as [Heq | Hin]; [discriminate | apply IH; exact Hin].
Qed.

(* The indexed version used for back-edge parameter passing. *)
Lemma continue_argument_used : ∀ (continuation : continuation)
    (args : list Terminator.arg) (index : int) (arg : value),
  nth_int args index = Some (Terminator.Arg arg) ->
  ∃ (uses : list value),
    uses ∈ (terminator_args (Terminator.Continue continuation args))
    ∧ arg ∈ uses.
Proof.
  intros continuation args index arg Harg. apply continue_argument_operand.
  eapply nth_int_In. exact Harg.
Qed.

(* --- Goto arguments --- *)

(* [goto_args] reads the terminator. *)
Lemma goto_args_terminator :
  ∀ (target pred : block) (args : list Terminator.arg),
  goto_args target pred = Some args ->
  terminator pred = Terminator.Continue (Goto target) args.
Proof.
  intros target pred args H. unfold goto_args in H.
  destruct (terminator pred) as [ [goto | | | ] args' | | | ];
    try discriminate.
  destruct (block_eqb goto target && _) eqn:Hc; [ | discriminate ].
  injection H as <-. apply andb_true_iff in Hc. destruct Hc as [Hgoto _].
  apply block_eqb_spec in Hgoto. subst goto. reflexivity.
Qed.

(* An accepted Goto has one argument slot per target parameter. *)
Lemma goto_args_arity : ∀ (target source : block)
    (args : list Terminator.arg),
  goto_args target source = Some args ->
  same_length args (params target) = true.
Proof.
  intros target source args Hgoto. unfold goto_args in Hgoto.
  destruct (terminator source) as [[dest | | |] arguments | | |];
    try discriminate.
  destruct (block_eqb dest target && same_length arguments (params target))
    eqn:E; [ | discriminate].
  injection Hgoto as <-. apply andb_true_iff in E. exact (proj2 E).
Qed.

(* [arg_value] reads the argument list. *)
Lemma arg_value_nth :
  ∀ (args : list Terminator.arg) (index : int) (argument_value : value),
  arg_value args index = Some argument_value ->
  nth_int args index = Some (Terminator.Arg argument_value).
Proof.
  intros args i u H. unfold arg_value in H.
  destruct (nth_int args i) as [ [a | ] | ]; try discriminate.
  injection H as <-. reflexivity.
Qed.

(* --- Uses --- *)

(* An individual instruction operand inherits the checked use approval. *)
Lemma uses_only_instruction : ∀ (blocks : list block)
    (same : value -> bool) (approve_insn : instr -> Prop)
    (approve_term : block -> list value -> Prop) (b : block) (insn : instr)
    (arg : value),
  uses_only blocks same approve_insn approve_term -> b ∈ blocks ->
  insn ∈ (block_body b) -> arg ∈ (args_of_instr insn) -> same arg = true ->
  approve_insn insn.
Proof.
  intros blocks same approve_insn approve_term b insn arg Huses Hblock
    Hmember Harg Hsame.
  apply (proj1 (Huses b Hblock) insn Hmember).
  apply existsb_exists. exists arg. auto.
Qed.

(* [uses_only] with weaker approvals follows from [uses_only] with stronger
   ones. *)
Lemma uses_only_weaken :
  ∀ (blocks : list block) (same : value -> bool)
    (approve_insn weaker_insn_approval : instr -> Prop)
    (approve_term weaker_term_approval : block -> list value -> Prop),
  (∀ (instr : instr), approve_insn instr -> weaker_insn_approval instr) ->
  (∀ (block : block) (operands : list value),
     approve_term block operands -> weaker_term_approval block operands) ->
  uses_only blocks same approve_insn approve_term -> uses_only blocks same
    weaker_insn_approval weaker_term_approval.
Proof.
  intros blocks same P P' Q Q' HP HQ H bl Hbl.
  destruct (H bl Hbl) as [Hi Ht]. split.
  - intros i Hin Huse. apply HP, Hi; assumption.
  - intros arr Hin Huse. apply HQ, Ht; assumption.
Qed.

(* ======================================================================== *)
(* Part 4: theorems                                                         *)
(* ======================================================================== *)

(* The DFS certificate supplies accessibility directly, without passing through
   an infinite-trace interpretation. *)
Theorem acyclic_except_sound :
  ∀ (blocks : list block) (region : blockset) (cut : block),
  acyclic_except blocks region cut = true -> region_acyclic_except region cut.
Proof.
  intros blocks region cut H b Hmem Hnot. unfold acyclic_except in H.
  destruct (visit_all (visit_acyclic blocks region cut set_empty)
    set_empty blocks) as [seen |] eqn:Hv; [ | discriminate].
  assert (Hempty : completed_blocks_accessible region cut set_empty).
  { intros b' Hb. rewrite set_mem_empty in Hb. discriminate. }
  destruct (visit_all_spec region cut _
    (visit_acyclic_spec region cut blocks set_empty) _ _ _ Hempty Hv)
    as [Hdone _].
  rewrite set_for_all_spec in H. specialize (H _ Hmem).
  apply orb_true_iff in H. destruct H as [H | H].
  - apply block_eqb_spec in H. contradiction.
  - apply Hdone. exact H.
Qed.

(* What [all_uses_ok] checks, read back as [uses_only]: the approvals, as
   propositions, hold of every use. *)
Theorem all_uses_ok_spec :
  ∀ (blocks : list block) (same : value -> bool)
    (approve_instr : instr -> bool)
    (approve_term : block -> list value -> bool),
  all_uses_ok blocks same approve_instr approve_term = true ->
  uses_only blocks same
    (fun instr => approve_instr instr = true)
    (fun block operands => approve_term block operands = true).
Proof.
  intros blocks same approve_instr approve_term H bl Hbl.
  unfold all_uses_ok in H. rewrite forallb_forall in H.
  specialize (H bl Hbl). apply andb_true_iff in H. destruct H as [Hi Ht].
  rewrite forallb_forall in Hi, Ht.
  split.
  - intros i Hin Huse. specialize (Hi i Hin).
    rewrite Huse in Hi. exact Hi.
  - intros arr Hin Huse. specialize (Ht arr Hin).
    rewrite Huse in Ht. exact Ht.
Qed.

(* What [goto_passes_only_at] checks, read back as [passes_only_at]. *)
Theorem goto_passes_only_at_spec :
  ∀ (target : block) (argument_index : int) (argument_value : value)
    (block : block),
  goto_passes_only_at target argument_index argument_value block = true ->
    passes_only_at target argument_index argument_value block.
Proof.
  intros target k v bl H. unfold goto_passes_only_at in H. unfold
    passes_only_at.
  destruct (terminator bl) as [ [goto | | | ] args | | | ]; try discriminate.
  apply andb_true_iff in H. destruct H as [H Hcount].
  apply andb_true_iff in H. destruct H as [Hgoto Harg].
  apply block_eqb_spec in Hgoto. subst goto.
  destruct (arg_value args k) as [a | ] eqn:Hav; [ | discriminate ].
  apply value_eqb_spec in Harg. subst a.
  apply eqb_int in Hcount.
  exists args. split; [ reflexivity | split; [ exact Hav | exact Hcount ] ].
Qed.
