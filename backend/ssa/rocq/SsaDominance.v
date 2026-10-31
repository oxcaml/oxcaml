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

(* Dominance in an SSA graph, beyond the [dominates] test SsaModel.v takes from
   the compiler: strict dominance, the test that a block is entered from one
   block only, and what dominance means in terms of paths. The extraction of
   this file, SsaDominance.ml, is the implementation.

   See backend/ssa/loop/rocq/LoopComparisons.v for the Rocq reading guide and
   SsaModel.v for how the models see an SSA graph.
   As there, the file has four parts: (1) the definitions, (2) the intended
   meaning, (3) stepping-stone lemmas, (4) the theorems. Parts 1 and 2 are the
   part to check by eye.

   The results are the point-level dominance facts of part 3, which
   SsaExecution.v, SsaLiveness.v and the loop proofs use; the direction a
   dominating branch took is derived operationally in LoopGuardExecution.v from
   [singleton_predecessor_dominance].

   The trusted part is SsaModel.v: that [predecessors] lists the sources of the
   edges into a block, that [dominates d b] means every path from the entry to
   [b] passes through [d], and that the entry dominates every block (every
   block of a finished graph is reachable from it). The last two are
   [dominance_semantics] in part 2, which the theorems assume. *)

From Stdlib Require Import Bool List Lia PeanoNat.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlList.
From OxCamlSsa Require Import SsaModel SsaGraph.
Import ListNotations.

(* ======================================================================== *)
(* Part 1: the definitions                                                  *)
(* ======================================================================== *)

(* [dominator] dominates [block] and is not it. *)
Definition strictly_dominates (dominator block : block) : bool :=
  dominates dominator block && negb (block_eqb dominator block).

(* [pred] is the sole predecessor of [succ], which is not the graph's entry:
   control enters [succ] only from [pred]. The entry has a predecessor when the
   function calls itself in tail position ([Continue (Goto entry)]), yet is
   also entered from outside; on a finished graph, where every block is
   reachable from the entry, a block that dominates its only predecessor can
   only be the entry, which the first test excludes. *)
Definition sole_predecessor (pred succ : block) : bool :=
  negb (dominates succ pred)
  && match predecessors succ with
     | [only_pred] => block_eqb only_pred pred
     | [] | _ :: _ :: _ => false
     end.

(* ======================================================================== *)
(* Part 2: the intended meaning                                             *)
(* ======================================================================== *)

(* [block_path_from first p last]: [p] is a path from [first] to [last] along
   the graph's edges, listing its blocks from [first] to [last]. An edge [pred
   -> succ] is [pred] being a predecessor of [succ]. *)
Inductive block_path_from : block -> list block -> block -> Prop :=
  | Block_path_one : ∀ (block : block),
      block_path_from block [block] block
  | Block_path_cons : ∀ (pred succ : block) (rest : list block)
      (last : block),
      pred ∈ (predecessors succ) ->
      block_path_from succ (succ :: rest) last ->
      block_path_from pred (pred :: succ :: rest) last.

(* What the compiler's [dominates], written [▷], means, [entry] being
   the graph's entry block: every path from the entry to [target] passes
   through each dominator of [target], and the entry dominates every block,
   each being reachable from it. Nothing is assumed of the entry's
   predecessors: a self-recursive tail call is a back edge to the entry. *)
Record dominance_semantics (entry : block) : Prop :=
  { dominates_spec : ∀ (dominator target : block),
      dominator ▷ target ->
      ∀ (visited_blocks : list block),
      block_path_from entry visited_blocks target ->
      dominator ∈ visited_blocks;
    entry_dominates : ∀ (block : block), entry ▷ block }.

Arguments dominates_spec {entry}.
Arguments entry_dominates {entry}.

(* A program point is a block and the number of instructions still to execute.
   The entry point has the full body remaining; point zero is the terminator.
   Result definitions become available after their instruction, while
   parameters are available at the block entry. *)
Definition program_point : Type := block * nat.

Definition block_entry_point (target : block) : program_point :=
  (target, length (block_body target)).

Definition program_point_eqb (lhs rhs : program_point) : bool :=
  block_eqb (fst lhs) (fst rhs) && Nat.eqb (snd lhs) (snd rhs).

(* Instruction edges advance through a block. Terminator edges use the same
   successor observation as block dominance, including exceptional successors
   even though the restricted semantics cannot take them. *)
Inductive program_point_edge : program_point -> program_point -> Prop :=
  | Program_point_instruction : ∀ (b : block) (insns_left : nat),
      insns_left < length (block_body b) ->
      program_point_edge (b, S insns_left) (b, insns_left)
  | Program_point_transfer : ∀ (source target : block),
      target ∈ (all_successors source) ->
      program_point_edge (source, 0) (block_entry_point target).

Inductive program_point_path_from
    : program_point -> list program_point -> program_point -> Prop :=
  | Program_point_path_one : ∀ (position : program_point),
      program_point_path_from position [position] position
  | Program_point_path_cons : ∀ (first next last : program_point)
      (rest : list program_point),
      program_point_edge first next ->
      program_point_path_from next (next :: rest) last ->
      program_point_path_from first (first :: next :: rest) last.

Definition program_point_dominates (entry : block)
    (dominator target : program_point) : Prop :=
  ∀ (points : list program_point),
    program_point_path_from (block_entry_point entry) points target ->
    dominator ∈ points.

Definition program_point_reachable (entry : block)
    (target : program_point) : Prop :=
  ∃ (points : list program_point),
    program_point_path_from (block_entry_point entry) points target.

(* ======================================================================== *)
(* Part 3: lemmas used by the proofs                                        *)
(* ======================================================================== *)

(* Point equality compares the block identity and instruction position. *)
Lemma program_point_eqb_spec : ∀ (lhs rhs : program_point),
  program_point_eqb lhs rhs = true <-> lhs = rhs.
Proof.
  intros [b n] [b' n']. unfold program_point_eqb; cbn.
  rewrite andb_true_iff, block_eqb_spec, Nat.eqb_eq.
  split; [intros [-> ->]; reflexivity | intros H; inversion H; auto].
Qed.

(* Every path contains its endpoint, so each point dominates itself. *)
Lemma program_point_dominates_refl : ∀ (entry : block)
  (position : program_point),
  program_point_dominates entry position position.
Proof.
  intros entry position points Hpath. induction Hpath;
    [left; reflexivity | right; exact IHHpath].
Qed.

(* A point path can be extended at either end by one graph edge. *)
Lemma program_point_path_prepend : ∀ (first next last : program_point)
    (points : list program_point),
  program_point_edge first next -> program_point_path_from next points last ->
  program_point_path_from first (first :: points) last.
Proof.
  intros first next last points Hedge Hpath.
  inversion Hpath; subst; apply Program_point_path_cons; assumption.
Qed.

Lemma program_point_path_snoc : ∀ (first last next : program_point)
    (points : list program_point),
  program_point_path_from first points last -> program_point_edge last next ->
  program_point_path_from first (points ++ [next]) next.
Proof.
  intros first last next points Hpath. induction Hpath; intros Hedge; cbn.
  - apply Program_point_path_cons; [exact Hedge | constructor].
  - apply Program_point_path_cons; [assumption | apply IHHpath; exact Hedge].
Qed.

(* Join two paths without requiring a self-edge at their shared point. *)
Lemma program_point_path_join : ∀ (first middle last : program_point)
    (prefix suffix : list program_point),
  program_point_path_from first prefix middle -> program_point_path_from middle
    suffix last ->
  ∃ (joined : list program_point), program_point_path_from first joined
    last ∧
    ∀ (member : program_point), member ∈ joined ->
      member ∈ prefix ∨ member ∈ suffix.
Proof.
  intros first middle last prefix suffix Hprefix Hsuffix.
  induction Hprefix.
  - exists suffix. split; [exact Hsuffix | auto].
  - destruct (IHHprefix Hsuffix) as [joined [Hjoined Hmembers]].
    exists (first :: joined). split.
    + eapply program_point_path_prepend; eassumption.
    + intros member [<- | Hin]; [left; left; reflexivity |].
      destruct (Hmembers member Hin); [left; right | right]; assumption.
Qed.

(* Keep the suffix starting at the last occurrence of a selected point. *)
Lemma program_point_path_last_visit : ∀ (first last : program_point)
    (points : list program_point),
  program_point_path_from first points last -> ∀ (selected :
    program_point),
  selected ∈ points -> ∃ (tail : list program_point),
    program_point_path_from selected (selected :: tail) last ∧ selected ∉ tail.
Proof.
  intros first last points Hpath. induction Hpath;
    intros selected Hin.
  - destruct Hin as [<- | []]. exists []. split; [constructor | tauto].
  - destruct (existsb (program_point_eqb selected) (next :: rest)) eqn:Hlater.
    + apply existsb_exists in Hlater. destruct Hlater as [p [Hp Heq]].
      apply program_point_eqb_spec in Heq. subst p. apply IHHpath. exact Hp.
    + destruct Hin as [<- | Hin].
      * exists (next :: rest). split; [econstructor; eassumption |].
        intros Hin. assert (existsb (program_point_eqb first)
          (next :: rest) = true).
        { apply existsb_exists. exists first. split; [exact Hin |].
          apply program_point_eqb_spec. reflexivity. }
        congruence.
      * assert (existsb (program_point_eqb selected) (next :: rest) = true).
        { apply existsb_exists. exists selected. split; [exact Hin |].
          apply program_point_eqb_spec. reflexivity. }
        congruence.
Qed.

(* A path avoiding a region's entry point after its start cannot leave and
   re-enter a region closed under all other incoming edges. *)
Lemma program_point_path_region_suffix : ∀ (inside : program_point ->
  Prop)
    (cut : program_point),
  (∀ (before after : program_point), program_point_edge before after ->
     inside after -> after <> cut -> inside before) ->
  ∀ (first last : program_point) (points : list program_point),
  program_point_path_from first points last -> inside last ->
  cut ∉ (tl points) -> Forall inside points.
Proof.
  intros inside cut Hclosed first last points Hpath.
  induction Hpath; intros Hlast Havoid.
  - constructor; [exact Hlast | constructor].
  - assert (Htail : Forall inside (next :: rest)).
    { apply IHHpath; [exact Hlast |]. cbn in *. tauto. }
    constructor; [ | exact Htail].
    eapply Hclosed; [exact H | inversion Htail; assumption |].
    intros Heq. subst next. apply Havoid. left. reflexivity.
Qed.

(* Truncate a path at its first visit to the destination. *)
Lemma program_point_path_first_visit : ∀ (first last : program_point)
    (points : list program_point),
  program_point_path_from first points last ->
  ∃ (prefix : list program_point),
    program_point_path_from first (prefix ++ [last]) last ∧ last ∉ prefix.
Proof.
  intros first last points Hpath. induction Hpath.
  - exists []. split; [constructor | cbn; tauto].
  - destruct (program_point_eqb first last) eqn:E.
    + apply program_point_eqb_spec in E. subst first.
      exists []. split; [constructor | cbn; tauto].
    + destruct IHHpath as [prefix [Hp Hnot]].
      exists (first :: prefix). split.
      * cbn. eapply program_point_path_prepend; eassumption.
      * intros [Heq | Hin]; [ | contradiction].
        subst first. assert (program_point_eqb last last = true)
          by (apply program_point_eqb_spec; reflexivity). congruence.
Qed.

(* A member of an avoiding prefix can be reached without leaving that prefix.
   This is the path-splicing fact needed for antisymmetry. *)
Lemma program_point_path_prefix_avoiding : ∀ (first last : program_point)
    (points : list program_point),
  program_point_path_from first points last ->
  ∀ (prefix suffix : list program_point) (member avoid : program_point),
  points = prefix ++ suffix -> avoid ∉ prefix -> member ∈ prefix ->
  ∃ (before_member : list program_point),
    program_point_path_from first before_member member ∧ avoid ∉ before_member.
Proof.
  intros first last points Hpath. induction Hpath;
    intros prefix suffix member avoid Heq Havoid Hin;
    destruct prefix as [ | head prefix]; [contradiction | | contradiction |].
  - cbn in Heq. injection Heq as <- Htail.
    destruct prefix; [ | discriminate]. cbn in Hin.
    destruct Hin as [<- | []]. exists [position]. split; [constructor |].
    exact Havoid.
  - cbn in Heq. injection Heq as <- Htail.
    destruct Hin as [<- | Hin].
    + exists [first]. split; [constructor | cbn in *; tauto].
    + destruct (IHHpath prefix suffix member avoid Htail
        ltac:(cbn in Havoid; tauto) Hin) as [before_member [Hp Hnot]].
      exists (first :: before_member). split.
      * eapply program_point_path_prepend; eassumption.
      * cbn in *. tauto.
Qed.

(* Mutual domination of reachable points means they are the same point.
   Reachability prevents unreachable graph fragments making this vacuous. *)
Theorem program_point_dominance_antisymmetric : ∀ (entry : block)
    (lhs rhs : program_point),
  program_point_reachable entry rhs -> program_point_dominates entry lhs rhs ->
  program_point_dominates entry rhs lhs -> lhs = rhs.
Proof.
  intros entry lhs rhs [points Hpath] Hlhs Hrhs.
  destruct (program_point_eqb lhs rhs) eqn:E;
    [apply program_point_eqb_spec; exact E |].
  destruct (program_point_path_first_visit _ _ _ Hpath)
    as [prefix [Hp Hnot]].
  pose proof (Hlhs _ Hp) as Hin. apply in_app_or in Hin.
  destruct Hin as [Hin | [Heq | []]].
  - destruct (program_point_path_prefix_avoiding _ _ _ Hp prefix [rhs] lhs rhs
      eq_refl Hnot Hin) as [before_lhs [Hbefore Havoid]].
    exfalso. apply Havoid, Hrhs, Hbefore.
  - subst lhs. assert (program_point_eqb rhs rhs = true)
      by (apply program_point_eqb_spec; reflexivity). congruence.
Qed.

(* A dominator of a successor also dominates its predecessor unless it is the
   successor itself, where a definition may just have become live. *)
Lemma program_point_dominates_predecessor : ∀ (entry : block)
    (dominator before after : program_point),
  program_point_dominates entry dominator after -> dominator <> after ->
  program_point_edge before after ->
  program_point_dominates entry dominator before.
Proof.
  intros entry dominator before after Hdom Hneq Hedge points Hpath.
  pose proof (Hdom _ (program_point_path_snoc _ _ _ _ Hpath Hedge)) as Hin.
  apply in_app_or in Hin. destruct Hin as [Hin | [Heq | []]];
    [exact Hin | congruence].
Qed.

(* A definition in another block that dominates this block's terminator also
   dominates its entry: advancing through the body never encounters that other
   definition. *)
Lemma other_block_dominates_entry : ∀ (entry b : block)
    (dominator : program_point),
  fst dominator <> b -> program_point_dominates entry dominator (b, 0%nat) ->
  program_point_dominates entry dominator (block_entry_point b).
Proof.
  intros entry b dominator Hother Hdom.
  assert (forall (remaining : nat), remaining <= length (block_body b) ->
    program_point_dominates entry dominator (b, remaining)) as Hthrough.
  { intros remaining. induction remaining as [ | remaining IH]; intros Hlen.
    - exact Hdom.
    - eapply program_point_dominates_predecessor.
      + apply IH. lia.
      + intros Heq. apply (f_equal fst) in Heq. cbn in Heq. contradiction.
      + constructor. lia. }
  apply Hthrough. lia.
Qed.

(* A visited point has a prefix path contained in the original path. *)
Lemma program_point_path_member : ∀ (first last : program_point)
  (points : list program_point),
  program_point_path_from first points last -> ∀ (member : program_point),
    member ∈ points ->
  ∃ (prefix : list program_point),
    program_point_path_from first prefix member
    ∧ ∀ (visited : program_point), visited ∈ prefix -> visited ∈ points.
Proof.
  intros first last points Hpath. induction Hpath; intros member Hin.
  - destruct Hin as [<- | []]. exists [position]. split; [constructor | auto].
  - destruct Hin as [<- | Hin].
    + exists [first]. split; [constructor | cbn; tauto].
    + destruct (IHHpath member Hin) as [prefix [Hp Hmembers]].
      exists (first :: prefix). split.
      * eapply program_point_path_prepend; eassumption.
      * intros visited [<- | Hvisited]; [left; reflexivity |].
        right. apply Hmembers. exact Hvisited.
Qed.

(* Dominating a dominator is enough to dominate its target. *)
Theorem program_point_dominance_transitive : ∀ (entry : block)
    (first middle last : program_point),
  program_point_dominates entry first middle -> program_point_dominates entry
    middle last ->
  program_point_dominates entry first last.
Proof.
  intros entry first middle last Hfirst Hmiddle points Hpath.
  destruct (program_point_path_member _ _ _ Hpath middle (Hmiddle _ Hpath))
    as [prefix [Hprefix Hmembers]].
  apply Hmembers, Hfirst, Hprefix.
Qed.

(* A block path can be extended by one incoming edge. *)
Lemma block_path_prepend : ∀ (first next last : block)
  (blocks : list block),
  first ∈ (predecessors next) -> block_path_from next blocks last ->
  block_path_from first (first :: blocks) last.
Proof.
  intros first next last blocks Hedge Hpath.
  inversion Hpath; subst; apply Block_path_cons; assumption.
Qed.

(* Project a program-point path to a block path. Every block other than the
   starting block was entered at its full-body entry point. *)
Lemma program_point_path_to_blocks : control_edges -> ∀ (first last :
  program_point)
    (points : list program_point),
  program_point_path_from first points last ->
  ∃ (blocks : list block), block_path_from (fst first) blocks (fst last)
    ∧ ∀ (b : block), b ∈ blocks ->
         b = fst first ∨ (block_entry_point b) ∈ points.
Proof.
  intros Hedges first last points Hpath. induction Hpath.
  - exists [fst position]. split; [constructor |].
    intros b [<- | []]. left. reflexivity.
  - destruct IHHpath as [blocks [Hblocks Hmembers]].
    destruct H; cbn in *.
    + exists blocks. split; [exact Hblocks |].
      intros block Hblock. destruct (Hmembers block Hblock) as [Hfirst | Hin].
      * left. exact Hfirst.
      * right. right. exact Hin.
    + exists (source :: blocks). split.
      * eapply block_path_prepend; [ | exact Hblocks].
        apply (proj2 (predecessor_successor Hedges source target)). exact H.
      * intros block [<- | Hblock]; [left; reflexivity |].
        destruct (Hmembers block Hblock) as [-> | Hin].
        -- right. right. left. reflexivity.
        -- right. right. exact Hin.
Qed.

(* Counting down through a block gives a path to its terminator without
   visiting any other block. *)
Lemma block_body_program_point_path : ∀ (b : block) (remaining : nat),
  remaining <= length (block_body b) ->
  ∃ (points : list program_point), program_point_path_from (b, remaining)
    points (b, 0)
    ∧ ∀ (position : program_point),
       position ∈ points -> fst position = b.
Proof.
  intros b remaining. induction remaining as [ | remaining IH]; intros Hlen.
  - exists [(b, 0)]. split; [constructor |].
    intros position [<- | []]. reflexivity.
  - destruct (IH ltac:(lia)) as [points [Hpath Hmembers]].
    exists ((b, S remaining) :: points). split.
    + eapply program_point_path_prepend; [constructor; lia | exact Hpath].
    + intros position [<- | Hin]; [reflexivity | apply Hmembers; exact Hin].
Qed.

(* Expand a block path to instruction-level points. This direction is needed to
   use completeness of the compiler's block-dominance query. *)
Lemma block_path_to_program_points : control_edges ->
  ∀ (first last : block) (blocks : list block),
  block_path_from first blocks last ->
  ∃ (points : list program_point),
    program_point_path_from (block_entry_point first) points (last, 0)
    ∧ ∀ (position : program_point), position ∈ points ->
         (fst position) ∈ blocks.
Proof.
  intros Hedges first last blocks Hpath. induction Hpath.
  - destruct (block_body_program_point_path block (length (block_body block))
      ltac:(lia)) as [points [Hpoints Hmembers]].
    exists points. split; [exact Hpoints |].
    intros position Hin. left. symmetry. apply Hmembers. exact Hin.
  - destruct IHHpath as [suffix [Hsuffix Hsuffix_members]].
    destruct (block_body_program_point_path pred (length (block_body pred))
      ltac:(lia)) as [prefix [Hprefix Hprefix_members]].
    assert (Htransfer : program_point_edge (pred, 0) (block_entry_point succ)).
    { constructor. apply (proj1 (predecessor_successor Hedges pred succ)).
      exact H. }
    pose proof (program_point_path_prepend _ _ _ _ Htransfer Hsuffix) as Htail.
    destruct (program_point_path_join _ _ _ _ _ Hprefix Htail)
      as [points [Hpoints Hmembers]].
    exists points. split; [exact Hpoints |]. intros position Hin.
    destruct (Hmembers position Hin)
      as [Hprefix_member | [<- | Hsuffix_member]].
    + left. symmetry. apply Hprefix_members. exact Hprefix_member.
    + left. reflexivity.
    + right. apply Hsuffix_members. exact Hsuffix_member.
Qed.

(* Entry-point dominance implies ordinary block dominance. *)
Theorem block_dominance_of_program_point : control_edges ->
  ∀ (entry dominator target : block),
  program_point_dominates entry (block_entry_point dominator) (target, 0) ->
  ∀ (blocks : list block),
  block_path_from entry blocks target -> dominator ∈ blocks.
Proof.
  intros Hedges entry dominator target Hdom blocks Hpath.
  destruct (block_path_to_program_points Hedges _ _ _ Hpath)
    as [points [Hpoints Hmembers]].
  exact (Hmembers _ (Hdom points Hpoints)).
Qed.

(* Semantic block dominance lifts to dominance of the block's entry point,
   preserving within-block instruction ordering. *)
Theorem program_point_dominance_of_block : control_edges ->
  ∀ (entry dominator : block) (target : program_point),
  (∀ (blocks : list block), block_path_from entry blocks (fst target) ->
     dominator ∈ blocks) ->
  program_point_dominates entry (block_entry_point dominator) target.
Proof.
  intros Hedges entry dominator target Hdom points Hpath.
  destruct (program_point_path_to_blocks Hedges _ _ _ Hpath)
    as [blocks [Hblocks Hmembers]].
  destruct (Hmembers dominator (Hdom blocks Hblocks)) as [Heq | Hin];
    [ | exact Hin].
  cbn in Heq. subst dominator. inversion Hpath; subst; left; reflexivity.
Qed.

(* Only a terminator edge can enter the full-body point of a block. *)
Lemma program_point_edge_to_entry : control_edges -> ∀ (before :
  program_point)
    (target : block),
  program_point_edge before (block_entry_point target) ->
  snd before = 0 ∧ (fst before) ∈ (predecessors target).
Proof.
  intros Hedges before target Hedge. inversion Hedge; subst.
  - unfold block_entry_point in *. cbn in *. lia.
  - split; [reflexivity |].
    apply (proj2 (predecessor_successor Hedges source target)). assumption.
Qed.

(* A non-entry block with one predecessor is dominated, at instruction
   granularity, by that predecessor's terminator. *)
Lemma singleton_predecessor_dominance : control_edges ->
  ∀ (entry source target : block),
  target <> entry -> predecessors target = [source] ->
  program_point_dominates entry (source, 0) (block_entry_point target).
Proof.
  intros Hedges entry source target Hnotentry Hpreds points Hpath.
  assert (Hpath_property : forall (first : program_point)
    (points : list program_point),
    program_point_path_from first points (block_entry_point target) ->
    first = block_entry_point target ∨ (source, 0) ∈ points).
  { clear Hpath points. intros first block_path_from Hrun.
    remember (block_entry_point target) as last.
    induction Hrun; subst.
    - left. reflexivity.
    - destruct (IHHrun eq_refl) as [Heq | Hin].
      + right. left.
        rewrite Heq in H.
        destruct (program_point_edge_to_entry Hedges _ _ H) as [Hcount Hpred].
        rewrite Hpreds in Hpred. destruct Hpred as [Hsource | []].
        destruct first as [b n]. cbn in *. subst. reflexivity.
      + right. right. exact Hin. }
  destruct (Hpath_property _ _ Hpath) as [Heq | Hin]; [ | exact Hin].
  apply (f_equal fst) in Heq. cbn in Heq. congruence.
Qed.

(* A path contains its endpoint. *)
Lemma block_path_last : ∀ (first : block) (visited_blocks : list block)
  (last : block),
  block_path_from first visited_blocks last -> last ∈ visited_blocks.
Proof.
  intros first p last H. induction H; cbn; auto.
Qed.

(* Following one more edge extends a path at its end. *)
Lemma block_path_snoc :
  ∀ (first : block) (visited_blocks : list block) (last next : block),
  block_path_from first visited_blocks last -> last ∈ (predecessors next) ->
  block_path_from first (visited_blocks ++ [next]) next.
Proof.
  intros first p last next H. induction H; intros Hedge; cbn.
  - apply Block_path_cons; [exact Hedge | constructor].
  - apply Block_path_cons; [assumption | apply IHblock_path_from; exact Hedge].
Qed.
