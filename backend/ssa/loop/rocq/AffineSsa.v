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

(* The linearization of SSA integer values into affine forms, and the affine
   facts implied by the branches dominating a block. The extraction of this
   file, AffineSsa.ml next to it, is the implementation.

   See the reading guide at the top of LoopComparisons.v for the Rocq
   vocabulary and the layout of the shared files; AffineExpr.v for the
   expressions and ranges this file builds, and LoopComparisons.v for the facts
   it derives from comparisons. As there, this file has four parts: (1) the
   definitions, (2) the intended meaning, (3) stepping-stone lemmas, (4) the
   theorems. Parts 1 and 2 are the part to check by eye.

   [expr_of_value] walks the value graph itself: a [value] (SsaModel.v) is the
   compiler's [Ssa.Value.t], carrying the operation that defines it and its
   arguments, and the walk matches on the integer operations it understands.
   Two things about the shape of the code:

   - The graph is shared, and a walk of it as a tree can be exponentially
     larger than the graph. So the walk has a budget of nodes to expand,
     [max_nodes] per linearization, carried in the context as [budget] and
     spent before descending into the arguments of a node. A value beyond the
     budget becomes an atom, like a value defined by an operation not
     understood here; an atom is always sound.

   - The context [ctx] (the atom interner and the budget) and the [side] list
     of facts are values, passed to each function and returned with its result,
     rather than mutable state, so that each function is a pure function of its
     arguments.

   [guards_at] walks the chain of immediate dominators of a block, which
   SsaModel.v provides as [strict_dominators], reading each dominator's
   terminator.

   The results, in part 4, say that the facts gathered here are true: under the
   valuation that reads every atom as the machine value of the SSA value it
   stands for ([agrees]),

   - [expr_of_value_sound]: the expression built for a value evaluates to the
     value's machine value exactly, its range (when known) contains that value,
     and its atomized shifts and ors are [atom_operations_consistent] in the
     sense of AffineExpr.v;
   - [linearize_sound]: the affine form of a value has the value's machine
     value, and the side facts it pushes are true;
   - [comparison_facts_sound]: when the comparison a branch tests came out as
     the branch taken requires, the facts derived from it are true; and they
     are well-formed, as [FourierMotzkin.entails] needs;
   - [guards_at_sound]: when each branch [guards_at] takes a fact from went the
     way it must have on every path to the block ([guards_taken]), the facts
     gathered are all true, and well-formed.

   Contexts only grow ([extends]), and whatever holds under every valuation
   agreeing with a context holds under every valuation agreeing with a later
   one; so the facts and side facts of all the [comparison_facts] calls
   [guards_at] makes hold together under the one valuation that agrees with the
   final context, which exists ([agrees_exists]). That is the valuation under
   which [FourierMotzkin.entails_sound] then makes an entailed goal true of the
   machine values (see LoopTermination.v).

   The theorems carry one hypothesis about resources: the atom counter must not
   wrap. A linearization allocates at most [linearize_atom_bound] atoms (one
   per node expanded, and two more), so this only asks that the counter start
   that far below [max_int].

   Trusted parts, beyond those of AffineExpr.v: what the recognised operations
   compute, which is [SsaSemantics.eval_operation] ([op_semantics] below asks
   that the valuation satisfy it), that [Ssa.Value.equal] only identifies
   values with the same machine value, and that the dominators and terminators
   SsaModel.v reports are the graph's. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlInt64 OxCamlList
  IntArith.
From OxCamlBackend Require Import IntegerComparison Operation.
From OxCamlSsa Require Import SsaModel SsaDominance SsaSemantics.
From OxCamlLoop Require Import FourierMotzkin LoopComparisons AffineExpr.
Import ListNotations.

(* ======================================================================== *)
(* Part 1: the definitions                                                  *)
(* ======================================================================== *)

Local Open Scope sint63_scope.

(* SSA values ([value], with [value_eqb] for [Ssa.Value.equal]) are those of
   SsaModel.v; the operations are those of Operation.v. *)

(* --- The context: atom interner and node budget --- *)

(* The atoms allocated so far, each with its value; the next atom to allocate;
   and the nodes [expr_of_value] may still expand. *)
Record ctx := { atoms : list (int * value); next : int; budget : int }.

(* The nodes one linearization may expand. *)
Definition max_nodes : int := 256.

(* The empty context, with a full budget. *)
Definition new_ctx : ctx :=
  {| atoms := []; next := zero; budget := max_nodes |}.

(* The context with a fresh budget, and with [k] nodes of it spent. *)
Definition refill_budget (c : ctx) : ctx :=
  {| atoms := c.(atoms); next := c.(next); budget := max_nodes |}.

Definition spend_budget (c : ctx) (k : int) : ctx :=
  {| atoms := c.(atoms); next := c.(next); budget := c.(budget) - k |}.

(* The atom of [v], allocated if [v] has none yet. *)
Definition intern (c : ctx) (v : value) : int * ctx :=
  match find (fun '(_, w) => value_eqb v w) c.(atoms) with
  | Some (id, _) => (id, c)
  | None =>
    let id := c.(next) in
    (id, {| atoms := (id, v) :: c.(atoms); next := id + one;
            budget := c.(budget) |})
  end.

(* --- Recognition into [AffineExpr.t] --- *)

(* The shifts expanded are by fewer than this many bits. *)
Definition max_shift_bits : int := 16.

(* The helpers of [expr_of_value]: [atomize] makes a value an atom with the
   full range; [keep_if_range_known] keeps a node only while its range is
   known. *)
Definition atomize (c : ctx) (v : value)
    : (AffineExpr.t * option Range.t) * ctx :=
  let '(id, c) := intern c v in ((AffineExpr.Atom id, Some Range.full), c).

Definition keep_if_range_known (c : ctx) (v : value) (node : AffineExpr.t)
    (range : option Range.t) : (AffineExpr.t * option Range.t) * ctx :=
  match range with
  | Some _ => ((node, range), c)
  | None => atomize c v
  end.

(* The range operations, on ranges that may be unknown. *)
Definition add_optional_ranges (a b : option Range.t) : option Range.t :=
  match a, b with Some a, Some b => Range.add a b | _, _ => None end.

Definition sub_optional_ranges (a b : option Range.t) : option Range.t :=
  match a, b with Some a, Some b => Range.sub a b | _, _ => None end.

Definition scale_optional_range (k : int)
  (r : option Range.t) : option Range.t :=
  match r with Some r => Range.scale k r | None => None end.

(* The expression of a value, with its range when one is known, from the
   operation defining it and, recursively, its arguments; the budget pays for
   each argument before it is walked. A value defined by an operation not
   understood here, one whose range is lost, or one beyond the budget becomes
   an atom ([atomize]). The argument of a logical shift is not walked. Sound
   for a well-formed context ([ctx_wf], part 2) whose atom counter has room for
   the budget: [expr_of_value_sound]. *)
Fixpoint expr_of_value (c : ctx) (v : value)
    : (AffineExpr.t * option Range.t) * ctx :=
  match v with
  | Res _ (Const_int n) _ _ =>
    let n := Int64.of_nativeint n in
    if int64_is_int n
    then
      let k := Int64.to_int n in
      ((AffineExpr.Const k, Some (Range.const k)), c)
    else atomize c v
  | Res _ (Intop Iadd) [a; b] _ =>
    if two <=? c.(budget)
    then
      let c := spend_budget c two in
      let '((ea, ra), c) := expr_of_value c a in
      let '((eb, rb), c) := expr_of_value c b in
      keep_if_range_known c v (AffineExpr.Add ea eb)
        (add_optional_ranges ra rb)
    else atomize c v
  | Res _ (Intop Isub) [a; b] _ =>
    if two <=? c.(budget)
    then
      let c := spend_budget c two in
      let '((ea, ra), c) := expr_of_value c a in
      let '((eb, rb), c) := expr_of_value c b in
      keep_if_range_known c v (AffineExpr.Sub ea eb)
        (sub_optional_ranges ra rb)
    else atomize c v
  | Res _ (Intop_imm Iadd k) [a] _ =>
    if one <=? c.(budget)
    then
      let c := spend_budget c one in
      let '((ea, ra), c) := expr_of_value c a in
      keep_if_range_known c v (AffineExpr.Add ea (AffineExpr.Const k))
        (add_optional_ranges ra (Some (Range.const k)))
    else atomize c v
  | Res _ (Intop_imm Isub k) [a] _ =>
    if one <=? c.(budget)
    then
      let c := spend_budget c one in
      let '((ea, ra), c) := expr_of_value c a in
      keep_if_range_known c v (AffineExpr.Sub ea (AffineExpr.Const k))
        (sub_optional_ranges ra (Some (Range.const k)))
    else atomize c v
  | Res _ (Intop_imm Ilsl k) [a] _ =>
    if (zero <=? k) && (k <? max_shift_bits) && (one <=? c.(budget))
    then
      let c := spend_budget c one in
      let '((ea, ra), c) := expr_of_value c a in
      keep_if_range_known c v (AffineExpr.Scale (one << k) ea)
        (scale_optional_range (one << k) ra)
    else atomize c v
  | Res _ (Intop_imm Iasr k) [a] _ =>
    if (zero <=? k) && (k <? max_shift_bits) && (one <=? c.(budget))
    then
      let c := spend_budget c one in
      match expr_of_value c a with
      | ((ea, Some _), c) =>
        let '(id, c) := intern c v in
        ((AffineExpr.Shr_atom id ea k, Some (Range.shr_signed k)), c)
      | ((_, None), c) => atomize c v
      end
    else atomize c v
  | Res _ (Intop_imm Ilsr k) [_] _ =>
    if (one <=? k) && (k <? word_bits)
    then
      let '(id, c) := intern c v in
      ((AffineExpr.Lsr_atom id k, Some (Range.shr_logical k)), c)
    else atomize c v
  | Res _ (Intop_imm Ior m) [a] _ =>
    if (zero <=? m) && (one <=? c.(budget))
    then
      let c := spend_budget c one in
      match expr_of_value c a with
      | ((ea, Some r), c) =>
        let '(id, c) := intern c v in
        ((AffineExpr.Or_atom id ea m, Range.or_mask m r), c)
      | ((_, None), c) => atomize c v
      end
    else atomize c v
  | _ => atomize c v
  end.

(* --- Linearization --- *)

(* [existing_facts] with each fact of [new_facts] not already in it pushed on
   the front. *)
Definition push_sides (existing_facts new_facts : list Affine.t)
    : list Affine.t :=
  OxCamlList.fold_left
    (fun existing_facts fact => if existsb (Affine.equal fact) existing_facts
      then existing_facts else fact :: existing_facts)
    existing_facts new_facts.

(* The affine form of a value, walked with a fresh budget, with the side facts
   of its linearization added to [side]; an atom for the value itself when
   [to_affine] overflows. Sound under the conditions of [expr_of_value]:
   [linearize_sound]. *)
Definition linearize (c : ctx) (side : list Affine.t) (v : value)
    : (Affine.t * list Affine.t) * ctx :=
  let '((e, _), c) := expr_of_value (refill_budget c) v in
  match to_affine e with
  | Some (form, sides) => ((form, push_sides side sides), c)
  | None => let '(id, c) := intern c v in ((Affine.var id, side), c)
  end.

(* --- Guard facts from dominating branches --- *)

(* The facts a comparison implies when it holds ([negate = false]) or fails
   ([negate = true]); its operands are linearized in turn, [a] first. A value
   that is not a comparison yields no facts. *)
Definition comparison_facts (c : ctx) (side : list Affine.t) (negate : bool)
    (cond : value) : (list Affine.t * list Affine.t) * ctx :=
  match cond with
  | Res _ (Intop (Icomp cmp)) [a; b] _ =>
    let '((la, side), c) := linearize c side a in
    let '((lb, side), c) := linearize c side b in
    ((facts negate cmp la lb, side), c)
  | Res _ (Intop_imm (Icomp cmp) k) [a] _ =>
    let '((la, side), c) := linearize c side a in
    ((facts negate cmp la (Affine.constant k), side), c)
  | _ => (([], side), c)
  end.

(* [guards_at] walks the strict dominators of [target], which SsaModel.v
   provides as the list [strict_dominators target]. For one dominator,
   [guard_test] gives the comparison it branches on and whether the fact is to
   be negated, when the successor it branches to towards [target] dominates
   [target] and is entered only from the dominator ([sole_predecessor],
   SsaDominance.v), so that every path to [target] takes that branch;
   [guard_facts] is the body of the walk, with the facts so far and the [side]
   and [ctx] of [comparison_facts] passed along as [st]. *)
Definition guard_test (target dominator : block) : option (value * bool) :=
  match terminator dominator with
  | Terminator.Switch index [ifnot; ifso] =>
    if dominates ifso target && sole_predecessor dominator ifso
    then Some (index, false)
    else if dominates ifnot target && sole_predecessor dominator ifnot
    then Some (index, true)
    else None
  | _ => None
  end.

Definition guard_facts (target : block)
    (st : (list Affine.t * list Affine.t) * ctx) (dominator : block)
    : (list Affine.t * list Affine.t) * ctx :=
  let '((acc, side), c) := st in
  match guard_test target dominator with
  | Some (index, negate) =>
    let '((fs, side), c) := comparison_facts c side negate index in
    ((fs ++ acc, side), c)
  | None => ((acc, side), c)
  end.

(* The facts holding on entry to [target], from the branches that dominate it.
   Sound for a well-formed context whose atom counter has room for the guards:
   [guards_at_sound]. *)
Definition guards_at (c : ctx) (side : list Affine.t) (target : block)
    : (list Affine.t * list Affine.t) * ctx :=
  OxCamlList.fold_left (guard_facts target) (([], side), c)
    (strict_dominators target).

(* ======================================================================== *)
(* Part 2: the intended meaning                                             *)
(* ======================================================================== *)

Local Open Scope Z_scope.

(* The context's invariant: each atom appears once, all are below the next one
   to be allocated, and the budget is not negative. *)
Definition ctx_wf (c : ctx) : Prop :=
  NoDup (map fst c.(atoms))
  ∧ (∀ (id : int) (v : value),
        (id, v) ∈ c.(atoms) -> to_Z id < to_Z c.(next))
  ∧ 0 <= to_Z c.(budget).

(* A context only gains atoms. *)
Definition extends (initial_ctx extended_ctx : ctx) : Prop :=
  ∀ (p : int * value), p ∈ initial_ctx.(atoms) -> p ∈ extended_ctx.(atoms).

(* The atoms one linearization may allocate: one per node expanded, one for the
   value itself and one when its affine form overflows; those the facts of one
   comparison may allocate, with two operands linearized; and those [guards_at]
   may allocate for one dominator, for a list of dominators, and for the whole
   walk. *)
Definition linearize_atom_bound : Z := to_Z max_nodes + 2.

Definition comparison_atom_bound : Z := 2 * linearize_atom_bound.

Definition guard_atom_bound (target dominator : block) : Z :=
  match guard_test target dominator with
  | Some _ => comparison_atom_bound
  | None => 0
  end.

Definition guard_list_atom_bound (target : block)
  (dominators : list block) : Z :=
  fold_right (fun dominator acc => guard_atom_bound target dominator + acc) 0
    dominators.

Definition guards_atom_bound (target : block) : Z :=
  guard_list_atom_bound target (strict_dominators target).

Section Semantics.
  (* The machine value of each SSA value: a 64-bit register, in one execution
     of the function. [Ssa.Value.equal] identifies SSA values, which then have
     the same machine value. *)
  Variable value_words : value -> Z.
  Hypothesis value_words_bounded : ∀ (v : value), in_int64 (value_words
    v).
  Hypothesis value_words_respect_equality :
    ∀ (v w : value),
    value_eqb v w = true -> value_words v = value_words w.

  (* The recognised operations compute what [SsaSemantics.eval_operation] says
     on the machine values of their arguments: there is one arithmetic
     semantics in the development, and this is a reading of it. It covers every
     [modeled_operation], comparisons included, though the affine theorems only
     need the operands of a comparison (see [comparison_matches_branch]). *)
  Definition op_semantics (v : value) : Prop :=
    match v with
    | Res _ op args _ =>
      modeled_operation op args = true ->
      eval_operation op (map value_words args) = Some (value_words v)
    | Block_param _ _ => True
    end.

  (* Every value is computed as its operation says. *)
  Hypothesis value_words_follow_operations :
    ∀ (v : value), op_semantics v.

  (* The equations the proofs below use, one per recognised shape, as they
     follow from [op_semantics] and the register bound: add, subtract and left
     shift wrap at 64 bits; the arithmetic right shift divides rounding towards
     minus infinity; the logical right shift divides the unsigned reading of
     its argument, read back as a signed register; or is the bitwise or of
     two's-complement values. Shifts by amounts outside the register width are
     not constrained. *)
  Definition op_equations (v : value) : Prop :=
    match v with
    | Res _ (Const_int n) _ _ => value_words v = Int64.to_Z (Int64.of_nativeint
      n)
    | Res _ (Intop Iadd) [a; b] _ => value_words v = wrap64 (value_words a +
      value_words b)
    | Res _ (Intop Isub) [a; b] _ => value_words v = wrap64 (value_words a -
      value_words b)
    | Res _ (Intop_imm Iadd k) [a] _ => value_words v = wrap64 (value_words a +
      to_Z k)
    | Res _ (Intop_imm Isub k) [a] _ => value_words v = wrap64 (value_words a -
      to_Z k)
    | Res _ (Intop_imm Ilsl k) [a] _ =>
      0 <= to_Z k <= 63 -> value_words v = wrap64 (value_words a * 2 ^ to_Z k)
    | Res _ (Intop_imm Iasr k) [a] _ =>
      0 <= to_Z k <= 63 -> value_words v = value_words a / 2 ^ to_Z k
    | Res _ (Intop_imm Ilsr k) [a] _ =>
      0 <= to_Z k <= 63 ->
      value_words v = wrap64 (unsigned64 (value_words a) / 2 ^ to_Z k)
    | Res _ (Intop_imm Ior m) [a] _ => value_words v = Z.lor (value_words a)
      (to_Z m)
    | _ => True
    end.

  Lemma value_word_equations : ∀ (v : value), op_equations v.
  Proof.
    intros v. pose proof (value_words_follow_operations v) as Hs.
    destruct v as [id op args i | b i]; [ | exact I].
    assert (Hbits : forall (k : int), 0 <= to_Z k <= 63 ->
      Z.leb 0 (to_Z k) && Z.leb (to_Z k) 63 = true).
    { intros k Hk. apply andb_true_iff. split; apply Z.leb_le; lia. }
    destruct op; try exact I; try (destruct op; try exact I);
      destruct args as [ | a [ | b [ | extra rest]]]; try exact I;
      cbn [op_semantics op_equations map modeled_operation eval_operation]
        in *;
      try (specialize (Hs eq_refl); injection Hs as Hs; symmetry; exact Hs).
    - (* Ior *)
      specialize (Hs eq_refl). injection Hs as Hs.
      rewrite <- Hs. apply wrap64_id, lor_in_int64.
      + apply value_words_bounded.
      + pose proof (int_bounds k). unfold in_int64, two63, two62 in *. lia.
    - (* Ilsl *)
      intros Hk. rewrite (Hbits k Hk) in Hs. specialize (Hs eq_refl).
      injection Hs as Hs. symmetry. exact Hs.
    - (* Ilsr *)
      intros Hk. rewrite (Hbits k Hk) in Hs. specialize (Hs eq_refl).
      injection Hs as Hs. symmetry. exact Hs.
    - (* Iasr *)
      intros Hk. rewrite (Hbits k Hk) in Hs. specialize (Hs eq_refl).
      injection Hs as Hs. rewrite <- Hs. apply wrap64_id.
      eapply in_range_implies_int64.
      eapply signed_shift_range_sound; [exact Hk | apply value_words_bounded].
  Qed.

  (* A valuation agrees with a context when it reads each atom as the machine
     value of the SSA value interned for it. *)
  Definition agrees (rho : valuation) (c : ctx) : Prop :=
    ∀ (id : int) (v : value),
      (id, v) ∈ c.(atoms) -> rho id = value_words v.

  (* The comparison a branch tests came out as the taken edge requires: true
     for the edge [comparison_facts] is called with [negate = false] for, false
     for the other. Values that are not comparisons yield no facts. *)
  Definition comparison_matches_branch (negate : bool)
    (comparison_result : value) : Prop :=
    match comparison_result with
    | Res _ (Intop (Icomp cmp)) [a; b] _ =>
      IntegerComparison.eval cmp (value_words a) (value_words b) = negb negate
    | Res _ (Intop_imm (Icomp cmp) k) [a] _ =>
      IntegerComparison.eval cmp (value_words a) (to_Z k) = negb negate
    | _ => True
    end.

  (* The branches [guards_at] takes facts from went the way it assumes: for
     each dominator of [target] whose branch [guard_test] accepts, the
     comparison tested came out as that branch requires. On entry to [target]
     this holds because every path to [target] took that branch
     (LoopGuardExecution.executed_guards_taken proves it of the actual
     execution); here it is an assumption on [value_words], which
     LoopTermination.v makes of the values at the entry to a loop. *)
  Definition guard_taken (target dominator : block) : Prop :=
    ∀ (index : value) (negate : bool),
      guard_test target dominator = Some (index, negate) ->
      comparison_matches_branch negate index.

  Definition guards_taken (target : block) : Prop :=
    ∀ (dominator : block),
      dominator ∈ (strict_dominators target) -> guard_taken target dominator.

(* ======================================================================== *)
(* Part 3: lemmas used by the proofs                                        *)
(* ======================================================================== *)

  (* A context extends itself, and extension is transitive. *)
  Lemma extends_refl : ∀ (c : ctx), extends c c.
  Proof. intros c p H. exact H. Qed.

  Lemma extends_trans : ∀ (initial_ctx middle_ctx final_ctx : ctx),
    extends initial_ctx middle_ctx -> extends middle_ctx final_ctx -> extends
      initial_ctx final_ctx.
  Proof. intros c1 c2 c3 H12 H23 p H. apply H23, H12, H. Qed.

  (* A valuation agreeing with an extension of a context agrees with the
     context. *)
  Lemma agrees_extends : ∀ (rho : valuation)
    (initial_ctx extended_ctx : ctx),
    extends initial_ctx extended_ctx -> agrees rho extended_ctx -> agrees rho
      initial_ctx.
  Proof. intros rho c c' He Ha id v H. apply Ha, He, H. Qed.

  (* The literals. *)
  Lemma to_Z_max_nodes : to_Z max_nodes = 256.
  Proof. reflexivity. Qed.

  Lemma to_Z_max_shift_bits : to_Z max_shift_bits = 16.
  Proof. reflexivity. Qed.

  Lemma linearize_atom_bound_eq : linearize_atom_bound = 258.
  Proof. reflexivity. Qed.

  Lemma comparison_atom_bound_eq : comparison_atom_bound = 516.
  Proof. reflexivity. Qed.

  (* The atom allowances are non-negative, and [guard_list_atom_bound] unfolds
     on lists. *)
  Lemma guard_atom_bound_nonnegative : ∀ (target dominator : block),
    0 <= guard_atom_bound target dominator.
  Proof.
    intros target d. unfold guard_atom_bound.
    destruct (guard_test target d) as [[index negate] | ];
      [ rewrite comparison_atom_bound_eq | ]; lia.
  Qed.

  Lemma guard_list_atom_bound_nil : ∀ (target : block),
    guard_list_atom_bound target [] = 0.
  Proof. reflexivity. Qed.

  Lemma guard_list_atom_bound_cons :
    ∀ (target dominator : block) (dominators : list block),
    guard_list_atom_bound target (dominator :: dominators)
    = guard_atom_bound target dominator + guard_list_atom_bound target
      dominators.
  Proof. reflexivity. Qed.

  Lemma guard_list_atom_bound_nonnegative : ∀ (target : block)
    (dominators : list block),
    0 <= guard_list_atom_bound target dominators.
  Proof.
    intros target doms. induction doms as [ | d doms IH ].
    - rewrite guard_list_atom_bound_nil. lia.
    - rewrite guard_list_atom_bound_cons. pose proof
      (guard_atom_bound_nonnegative target d). lia.
  Qed.

  (* --- The context --- *)

  Lemma ctx_wf_budget : ∀ (c : ctx), ctx_wf c -> 0 <= to_Z c.(budget).
  Proof. intros c [_ [_ H]]. exact H. Qed.

  Lemma new_ctx_wf : ctx_wf new_ctx.
  Proof.
    split; [ constructor | ]. split; [ intros id v Hin; destruct Hin | ].
    cbn [budget new_ctx]. rewrite to_Z_max_nodes. lia.
  Qed.

  (* [refill_budget] keeps the atoms and the counter and sets the budget. *)
  Lemma refill_budget_spec : ∀ (c : ctx),
    ctx_wf c ->
    ctx_wf (refill_budget c) ∧ extends c (refill_budget c)
    ∧ to_Z (refill_budget c).(next) = to_Z c.(next)
    ∧ to_Z (refill_budget c).(budget) = to_Z max_nodes.
  Proof.
    intros c [Hnd [Hlt _]]. unfold refill_budget. cbn [atoms next budget].
    refine (conj (conj Hnd (conj Hlt _)) (conj _ (conj eq_refl eq_refl))).
    - cbn [budget]. rewrite to_Z_max_nodes. lia.
    - intros p Hp. exact Hp.
  Qed.

  (* [spend_budget c node_count], when the budget has [node_count] nodes, keeps
     the atoms and the counter and lowers the budget by [node_count]. *)
  Lemma spend_budget_spec : ∀ (c : ctx) (node_count : int),
    ctx_wf c -> 0 <= to_Z node_count ->
    (node_count <=? c.(budget))%sint63 = true ->
    ctx_wf (spend_budget c node_count) ∧ extends c (spend_budget c node_count)
    ∧ to_Z (spend_budget c node_count).(next) = to_Z c.(next)
    ∧ to_Z (spend_budget c node_count).(budget)
       = to_Z c.(budget) - to_Z node_count.
  Proof.
    intros c k [Hnd [Hlt Hb]] Hk Hle. apply leb_int in Hle.
    unfold spend_budget. cbn [atoms next budget].
    assert (Hsub : to_Z (c.(budget) - k)%sint63 = to_Z c.(budget) - to_Z k).
    { apply Sint63.to_Z_sub. rewrite to_Z_min_int, to_Z_max_int.
      pose proof (int_bounds c.(budget)). pose proof (int_bounds k). lia. }
    refine (conj (conj Hnd (conj Hlt _)) (conj _ (conj eq_refl Hsub))).
    - cbn [budget]. rewrite Hsub. lia.
    - intros p Hp. exact Hp.
  Qed.

  (* --- [intern] --- *)

  (* [intern] returns an atom standing for [value_ref]: the context stays
     well-formed, grows by at most one atom and keeps its budget, and under any
     agreeing valuation the atom reads as [value_ref]'s machine value. *)
  Lemma intern_spec : ∀ (initial_ctx : ctx) (value_ref : value)
    (id : int) (final_ctx : ctx),
    ctx_wf initial_ctx -> to_Z initial_ctx.(next) < ocaml_max_int ->
    intern initial_ctx value_ref = (id, final_ctx) ->
    ctx_wf final_ctx ∧ extends initial_ctx final_ctx
    ∧ to_Z final_ctx.(next) <= to_Z initial_ctx.(next) + 1
    ∧ to_Z final_ctx.(budget) = to_Z initial_ctx.(budget)
    ∧ ∀ (rho : valuation), agrees rho final_ctx ->
         rho id = value_words value_ref.
  Proof.
    intros c v id c' [Hnd [Hlt Hb]] Hnext H. unfold intern in H.
    destruct (find _ c.(atoms)) as [ [id0 w] | ] eqn:Hf.
    - apply find_some in Hf as [Hin Heq]. injection H as <- <-.
      refine (conj (conj Hnd (conj Hlt Hb))
                (conj (extends_refl c) (conj _ (conj eq_refl _))));
        [ lia | ].
      intros rho Ha. rewrite (Ha id0 w Hin). symmetry. apply
        value_words_respect_equality.
      exact Heq.
    - injection H as <- <-. unfold ctx_wf, extends, agrees.
      cbn [atoms next budget].
      assert (Hsucc : to_Z (c.(next) + one)%sint63 = to_Z c.(next) + 1).
      { rewrite Sint63.to_Z_add.
        - rewrite to_Z_one. reflexivity.
        - rewrite to_Z_min_int, to_Z_max_int, to_Z_one.
          pose proof (int_bounds c.(next)). unfold ocaml_max_int in Hnext.
          lia. }
      refine (conj (conj _ (conj _ Hb)) (conj _ (conj _ (conj eq_refl _)))).
      + cbn [map fst]. constructor; [ | exact Hnd ].
        intros Hin. apply in_map_iff in Hin as [[id0 w] [Hfst Hin]].
        cbn [fst] in Hfst. subst id0. pose proof (Hlt _ _ Hin). lia.
      + intros id0 w [ Hin | Hin ].
        * injection Hin as <- _. lia.
        * pose proof (Hlt _ _ Hin). lia.
      + intros p Hp. right. exact Hp.
      + lia.
      + intros rho Ha. apply Ha. left. reflexivity.
  Qed.

  (* [atomize] returns an atom standing for [value_ref] with the full range:
     the context stays well-formed, keeps its budget and gains at most one
     atom, and under any agreeing valuation the atom reads as [value_ref]'s
     machine value, which is in the range. *)
  Lemma atomize_spec :
    ∀ (initial_ctx : ctx) (value_ref : value) (expr : AffineExpr.t)
      (optional_range : option Range.t)
      (final_ctx : ctx),
    ctx_wf initial_ctx -> to_Z initial_ctx.(next) < ocaml_max_int ->
    atomize initial_ctx value_ref = ((expr, optional_range), final_ctx) ->
    ctx_wf final_ctx ∧ extends initial_ctx final_ctx ∧ to_Z final_ctx.(next)
      <= to_Z initial_ctx.(next) + 1
    ∧ to_Z final_ctx.(budget) = to_Z initial_ctx.(budget)
    ∧ ∀ (rho : valuation), agrees rho final_ctx ->
         AffineExpr.eval rho expr = value_words value_ref ∧
           atom_operations_consistent rho expr
         ∧ ∀ (known_range : Range.t), optional_range = Some known_range
           -> in_range known_range (value_words value_ref).
  Proof.
    intros c v e r c' Hwf Hnext H. unfold atomize in H.
    destruct (intern c v) as [id c1] eqn:Hi. injection H as <- <- <-.
    destruct (intern_spec c v id c1 Hwf Hnext Hi)
      as [Hwf1 [He1 [Hn1 [Hb1 Hid]]]].
    refine (conj Hwf1 (conj He1 (conj Hn1 (conj Hb1 _)))).
    intros rho Ha. cbn [AffineExpr.eval atom_operations_consistent].
    split; [ apply Hid; exact Ha | ].
    split; [ exact I | ].
    intros rg Hrg. injection Hrg as <-. apply full_range_sound. apply
      value_words_bounded.
  Qed.

  (* --- [push_sides] --- *)

  (* Every fact of the result comes from one of the two lists ... *)
  Lemma push_sides_In : ∀ (new_facts existing_facts : list Affine.t)
    (fact : Affine.t),
    fact ∈ (push_sides existing_facts new_facts) -> fact ∈ existing_facts ∨
      fact ∈ new_facts.
  Proof.
    intros sides. unfold push_sides, OxCamlList.fold_left.
    induction sides as [ | g sides IH ]; cbn [List.fold_left]; intros side f H.
    - left. exact H.
    - destruct (existsb (Affine.equal g) side).
      + destruct (IH side f H) as [ Hs | Hs ]; [ left | right; right ];
          assumption.
      + destruct (IH (g :: side) f H) as [ [ Hs | Hs ] | Hs ].
        * right. left. exact Hs.
        * left. exact Hs.
        * right. right. exact Hs.
  Qed.

  (* ... so a property of every fact of both holds of every fact of the result.
     *)
  Lemma push_sides_Forall :
    ∀ (P : Affine.t -> Prop) (existing_facts new_facts : list Affine.t),
    Forall P existing_facts -> Forall P new_facts -> Forall P (push_sides
      existing_facts new_facts).
  Proof.
    intros P side sides Hside Hsides.
    rewrite Forall_forall in Hside, Hsides |- *. intros f Hf.
    destruct (push_sides_In sides side f Hf) as [ H | H ];
      [ apply Hside | apply Hsides ]; exact H.
  Qed.

  (* --- The valuation agreeing with a context --- *)

  (* Every well-formed context has an agreeing valuation: its atoms are
     distinct, so each can be given the machine value of its SSA value. *)
  Theorem agrees_exists :
    ∀ (c : ctx), ctx_wf c -> ∃ rho, agrees rho c.
  Proof.
    intros c [Hnd _].
    exists (fun id => match assoc_opt id c.(atoms) with
                      | Some v => value_words v
                      | None => 0
                      end).
    intros id v Hin. cbv beta. rewrite (assoc_opt_NoDup _ id v Hnd Hin).
    reflexivity.
  Qed.

  (* --- Induction over values --- *)

  (* Induction over a value and, through the argument lists, the values it is
     computed from. *)
  Fixpoint value_ind' (P : value -> Prop)
      (Hres : ∀ id op args i, Forall P args -> P (Res id op args i))
      (Hparam : ∀ block i, P (Block_param block i)) (v : value) : P v :=
    match v with
    | Res id op args i =>
      Hres id op args i
        ((fix args_ind (l : list value) : Forall P l :=
            match l with
            | [] => Forall_nil P
            | a :: l => Forall_cons a (value_ind' P Hres Hparam a) (args_ind l)
            end) args)
    | Block_param block i => Hparam block i
    end.

(* ======================================================================== *)
(* Part 4: the theorems                                                     *)
(* ======================================================================== *)

  (* The fall-through of a case to [atomize]: close the goal from
     [atomize_spec], given the context's well-formedness and its extension from
     the initial context. *)
  Ltac finish_atomized Hwf Hext H :=
    let A := fresh "A" in let B := fresh "B" in let C := fresh "C" in
    let D := fresh "D" in let E := fresh "E" in let F := fresh "F" in
    pose proof (ctx_wf_budget _ Hwf) as F;
    destruct (atomize_spec _ _ _ _ _ Hwf ltac:(lia) H) as [A [B [C [D E]]]];
    refine (conj A (conj (extends_trans _ _ _ Hext B) (conj _ E))); lia.

  (* Turn a true shift-amount guard into bounds on the amount. *)
  Ltac shift_guard Hk :=
    apply andb_prop in Hk; destruct Hk as [Hk0 Hk1];
    apply leb_int in Hk0; apply ltb_int in Hk1;
    rewrite ?to_Z_zero, ?to_Z_one in Hk0;
    rewrite ?to_Z_max_shift_bits, ?to_Z_word_bits in Hk1.

  (* Pay for the arguments of a node: read [spend_budget_spec] for a true
     budget test. *)
  Ltac pay k Hwf Hle Hwf1 He1 Hn1 Hb1 :=
    destruct (spend_budget_spec _ k Hwf ltac:(rewrite ?to_Z_one, ?to_Z_two;
      lia) Hle)
      as [Hwf1 [He1 [Hn1 Hb1]]];
    rewrite ?to_Z_one, ?to_Z_two in Hb1.

  (* The expression built for a value, under any valuation agreeing with the
     context it leaves: it evaluates to the value's machine value exactly; its
     atomized shifts and ors are consistent; and if a range was found, the
     machine value is in it. The context stays well-formed and only grows, and
     the atoms it gains are paid for by the budget, but for one. *)
  Theorem expr_of_value_sound :
    ∀ (value_ref : value) (initial_ctx : ctx) (expr : AffineExpr.t)
      (optional_range : option Range.t)
      (final_ctx : ctx),
    ctx_wf initial_ctx ->
    to_Z initial_ctx.(next) + to_Z initial_ctx.(budget) + 1 <= ocaml_max_int ->
    expr_of_value initial_ctx value_ref
      = ((expr, optional_range), final_ctx) ->
    ctx_wf final_ctx ∧ extends initial_ctx final_ctx
    ∧ to_Z final_ctx.(next) + to_Z final_ctx.(budget)
       <= to_Z initial_ctx.(next) + to_Z initial_ctx.(budget) + 1
    ∧ ∀ (rho : valuation), agrees rho final_ctx ->
         AffineExpr.eval rho expr = value_words value_ref ∧
           atom_operations_consistent rho expr
         ∧ ∀ (known_range : Range.t), optional_range = Some known_range
           -> in_range known_range (value_words value_ref).
  Proof.
    intros v. induction v as [id op args i IHargs | b i] using value_ind';
      intros c e r c' Hwf Hn H;
      [ | cbn [expr_of_value] in H; finish_atomized Hwf (extends_refl c) H ].
    pose proof (value_word_equations (Res id op args i)) as Hs.
    destruct op as [ | | | n | ? | ? | ? | ? | ? | ? | ? | ? | ? | ? ? ? | iop
                   | ? | iop k | ? | ? ? | ? | ? | ? | ? | | | | ? | ? | | | |
                   | | ? ];
      try solve [ cbn [expr_of_value] in H;
                  finish_atomized Hwf (extends_refl c) H ].
    - (* Const_int *)
      cbn [expr_of_value] in H. cbn [op_equations] in Hs.
      destruct (int64_is_int (Int64.of_nativeint n)) eqn:Hf.
      + injection H as <- <- <-.
        refine (conj Hwf (conj (extends_refl c) (conj _ _))); [ lia | ].
        intros rho _. cbn [AffineExpr.eval atom_operations_consistent].
        pose proof (int64_is_int_spec _ Hf) as Hk.
        split; [ rewrite Hk, Hs; reflexivity | ].
        split; [ exact I | ].
        intros rg Hrg. injection Hrg as <-. rewrite Hs, <- Hk.
        apply constant_range_sound.
      + finish_atomized Hwf (extends_refl c) H.
    - (* Intop *)
      destruct iop;
        try solve [ cbn [expr_of_value] in H;
                    finish_atomized Hwf (extends_refl c) H ];
        destruct args as [ | a [ | b [ | ? ? ] ] ];
        try solve [ cbn [expr_of_value] in H;
                    finish_atomized Hwf (extends_refl c) H ];
        cbn [expr_of_value] in H; cbn [op_equations] in Hs;
        pose proof (Forall_inv IHargs) as IHa;
        pose proof (Forall_inv (Forall_inv_tail IHargs)) as IHb;
        destruct (two <=? c.(budget))%sint63 eqn:Hle;
        [ | finish_atomized Hwf (extends_refl c) H
          | | finish_atomized Hwf (extends_refl c) H ];
        pay two Hwf Hle Hwf1 He1 Hn1 Hb1;
        destruct (expr_of_value (spend_budget c two) a) as [[ea ra] ca] eqn:Ha;
        destruct (expr_of_value ca b) as [[eb rb] cb] eqn:Hb;
        destruct (IHa _ ea ra ca Hwf1 ltac:(lia) Ha)
          as [Hwfa [Hea [Hna Hrho_a]]];
        destruct (IHb ca eb rb cb Hwfa ltac:(lia) Hb)
          as [Hwfb [Heb [Hnb Hrho_b]]];
        unfold keep_if_range_known in H.
      + (* Iadd *)
        destruct (add_optional_ranges ra rb) as [ rg | ] eqn:Hr.
        * injection H as <- <- <-.
          unfold add_optional_ranges in Hr.
          destruct ra as [ rga | ]; [ | discriminate ].
          destruct rb as [ rgb | ]; [ | discriminate ].
          refine (conj Hwfb
                    (conj (extends_trans _ _ _ He1
                             (extends_trans _ _ _ Hea Heb))
                       (conj _ _)));
            [ lia | ].
          intros rho Hrho.
          destruct (Hrho_b rho Hrho) as [Eb [Cb Rb]].
          destruct (Hrho_a rho (agrees_extends _ _ _ Heb Hrho))
            as [Ea [Ca Ra]].
          specialize (Ra rga eq_refl). specialize (Rb rgb eq_refl).
          pose proof (range_add_sound _ _ _ _ _ Hr Ra Rb) as Hsum.
          pose proof (in_range_implies_int64 _ _ Hsum) as Hfits.
          rewrite Hs, (wrap64_id _ Hfits).
          cbn [AffineExpr.eval atom_operations_consistent].
          split; [ rewrite Ea, Eb; reflexivity | ].
          split; [ exact (conj Ca Cb) | ].
          intros rg' Hrg. injection Hrg as <-. exact Hsum.
        * finish_atomized Hwfb
            (extends_trans _ _ _ He1 (extends_trans _ _ _ Hea Heb)) H.
      + (* Isub *)
        destruct (sub_optional_ranges ra rb) as [ rg | ] eqn:Hr.
        * injection H as <- <- <-.
          unfold sub_optional_ranges in Hr.
          destruct ra as [ rga | ]; [ | discriminate ].
          destruct rb as [ rgb | ]; [ | discriminate ].
          refine (conj Hwfb
                    (conj (extends_trans _ _ _ He1
                             (extends_trans _ _ _ Hea Heb))
                       (conj _ _)));
            [ lia | ].
          intros rho Hrho.
          destruct (Hrho_b rho Hrho) as [Eb [Cb Rb]].
          destruct (Hrho_a rho (agrees_extends _ _ _ Heb Hrho))
            as [Ea [Ca Ra]].
          specialize (Ra rga eq_refl). specialize (Rb rgb eq_refl).
          pose proof (range_sub_sound _ _ _ _ _ Hr Ra Rb) as Hdiff.
          pose proof (in_range_implies_int64 _ _ Hdiff) as Hfits.
          rewrite Hs, (wrap64_id _ Hfits).
          cbn [AffineExpr.eval atom_operations_consistent].
          split; [ rewrite Ea, Eb; reflexivity | ].
          split; [ exact (conj Ca Cb) | ].
          intros rg' Hrg. injection Hrg as <-. exact Hdiff.
        * finish_atomized Hwfb
            (extends_trans _ _ _ He1 (extends_trans _ _ _ Hea Heb)) H.
    - (* Intop_imm *)
      destruct iop;
        try solve [ cbn [expr_of_value] in H;
                    finish_atomized Hwf (extends_refl c) H ];
        destruct args as [ | a [ | ? ? ] ];
        try solve [ cbn [expr_of_value] in H;
                    finish_atomized Hwf (extends_refl c) H ];
        cbn [expr_of_value] in H; cbn [op_equations] in Hs;
        pose proof (Forall_inv IHargs) as IHa.
      + (* Iadd *)
        destruct (one <=? c.(budget))%sint63 eqn:Hle;
          [ | finish_atomized Hwf (extends_refl c) H ].
        pay one Hwf Hle Hwf1 He1 Hn1 Hb1.
        destruct (expr_of_value (spend_budget c one) a) as [[ea ra] ca] eqn:Ha.
        destruct (IHa _ ea ra ca Hwf1 ltac:(lia) Ha)
          as [Hwfa [Hea [Hna Hrho_a]]].
        unfold keep_if_range_known in H.
        destruct (add_optional_ranges ra (Some (Range.const k))) as [ rg | ]
          eqn:Hr.
        * injection H as <- <- <-.
          unfold add_optional_ranges in Hr. destruct ra as [ rga | ]; [ |
            discriminate ].
          refine (conj Hwfa (conj (extends_trans _ _ _ He1 Hea) (conj _ _)));
            [ lia | ].
          intros rho Hrho.
          destruct (Hrho_a rho Hrho) as [Ea [Ca Ra]].
          specialize (Ra rga eq_refl).
          pose proof (range_add_sound _ _ _ _ _ Hr Ra (constant_range_sound k))
            as Hsum.
          pose proof (in_range_implies_int64 _ _ Hsum) as Hfits.
          rewrite Hs, (wrap64_id _ Hfits).
          cbn [AffineExpr.eval atom_operations_consistent].
          split; [ rewrite Ea; reflexivity | ].
          split; [ exact (conj Ca I) | ].
          intros rg' Hrg. injection Hrg as <-. exact Hsum.
        * finish_atomized Hwfa (extends_trans _ _ _ He1 Hea) H.
      + (* Isub *)
        destruct (one <=? c.(budget))%sint63 eqn:Hle;
          [ | finish_atomized Hwf (extends_refl c) H ].
        pay one Hwf Hle Hwf1 He1 Hn1 Hb1.
        destruct (expr_of_value (spend_budget c one) a) as [[ea ra] ca] eqn:Ha.
        destruct (IHa _ ea ra ca Hwf1 ltac:(lia) Ha)
          as [Hwfa [Hea [Hna Hrho_a]]].
        unfold keep_if_range_known in H.
        destruct (sub_optional_ranges ra (Some (Range.const k))) as [ rg | ]
          eqn:Hr.
        * injection H as <- <- <-.
          unfold sub_optional_ranges in Hr. destruct ra as [ rga | ]; [ |
            discriminate ].
          refine (conj Hwfa (conj (extends_trans _ _ _ He1 Hea) (conj _ _)));
            [ lia | ].
          intros rho Hrho.
          destruct (Hrho_a rho Hrho) as [Ea [Ca Ra]].
          specialize (Ra rga eq_refl).
          pose proof (range_sub_sound _ _ _ _ _ Hr Ra (constant_range_sound k))
            as Hdiff.
          pose proof (in_range_implies_int64 _ _ Hdiff) as Hfits.
          rewrite Hs, (wrap64_id _ Hfits).
          cbn [AffineExpr.eval atom_operations_consistent].
          split; [ rewrite Ea; reflexivity | ].
          split; [ exact (conj Ca I) | ].
          intros rg' Hrg. injection Hrg as <-. exact Hdiff.
        * finish_atomized Hwfa (extends_trans _ _ _ He1 Hea) H.
      + (* Ior *)
        destruct ((zero <=? k) && (one <=? c.(budget)))%sint63 eqn:Hg;
          [ | finish_atomized Hwf (extends_refl c) H ].
        apply andb_prop in Hg. destruct Hg as [Hm Hle].
        apply leb_int in Hm. rewrite to_Z_zero in Hm.
        pay one Hwf Hle Hwf1 He1 Hn1 Hb1.
        destruct (expr_of_value (spend_budget c one) a) as [[ea ra] ca] eqn:Ha.
        destruct (IHa _ ea ra ca Hwf1 ltac:(lia) Ha)
          as [Hwfa [Hea [Hna Hrho_a]]].
        pose proof (ctx_wf_budget _ Hwfa) as Hba.
        destruct ra as [ rga | ].
        * destruct (intern ca (Res id (Intop_imm Ior k) [a] i))
            as [atom c1] eqn:Hi.
          injection H as <- <- <-.
          destruct (intern_spec ca _ atom c1 Hwfa ltac:(lia) Hi)
            as [Hwf1' [He1' [Hn1' [Hb1' Hid]]]].
          refine (conj Hwf1'
                    (conj (extends_trans _ _ _ He1
                             (extends_trans _ _ _ Hea He1'))
                       (conj _ _)));
            [ lia | ].
          intros rho Hrho.
          destruct (Hrho_a rho (agrees_extends _ _ _ He1' Hrho))
            as [Ea [Ca Ra]].
          pose proof (Hid rho Hrho) as Eid. specialize (Ra rga eq_refl).
          cbn [AffineExpr.eval atom_operations_consistent].
          split; [ exact Eid | ].
          split.
          { split; [ exact Ca | ]. split; [ exact Hm | ].
            rewrite Eid, Ea. exact Hs. }
          { intros rg Hrg. rewrite Hs. exact (range_or_mask_sound _ _ _ _ Hrg
            Ra). }
        * finish_atomized Hwfa (extends_trans _ _ _ He1 Hea) H.
      + (* Ilsl *)
        destruct ((zero <=? k) && (k <? max_shift_bits)
                  && (one <=? c.(budget)))%sint63 eqn:Hg;
          [ | finish_atomized Hwf (extends_refl c) H ].
        apply andb_prop in Hg. destruct Hg as [Hk Hle]. shift_guard Hk.
        pay one Hwf Hle Hwf1 He1 Hn1 Hb1.
        destruct (expr_of_value (spend_budget c one) a) as [[ea ra] ca] eqn:Ha.
        destruct (IHa _ ea ra ca Hwf1 ltac:(lia) Ha)
          as [Hwfa [Hea [Hna Hrho_a]]].
        unfold keep_if_range_known in H. destruct (scale_optional_range _ ra)
          as [ rg | ] eqn:Hr.
        * injection H as <- <- <-.
          unfold scale_optional_range in Hr. destruct ra as [ rga | ]; [ |
            discriminate ].
          refine (conj Hwfa (conj (extends_trans _ _ _ He1 Hea) (conj _ _)));
            [ lia | ].
          intros rho Hrho.
          destruct (Hrho_a rho Hrho) as [Ea [Ca Ra]].
          specialize (Ra rga eq_refl).
          pose proof (range_scale_sound _ _ _ _ Hr Ra) as Hprod.
          rewrite to_Z_lsl_one in Hprod by lia.
          pose proof (in_range_implies_int64 _ _ Hprod) as Hfits.
          rewrite Z.mul_comm in Hfits.
          specialize (Hs ltac:(lia)).
          rewrite Hs, (wrap64_id _ Hfits).
          cbn [AffineExpr.eval atom_operations_consistent].
          split; [ rewrite to_Z_lsl_one by lia; rewrite Ea; lia | ].
          split; [ exact Ca | ].
          intros rg' Hrg. injection Hrg as <-. rewrite Z.mul_comm. exact Hprod.
        * finish_atomized Hwfa (extends_trans _ _ _ He1 Hea) H.
      + (* Ilsr *)
        destruct ((one <=? k) && (k <? word_bits))%sint63 eqn:Hk;
          [ | finish_atomized Hwf (extends_refl c) H ].
        shift_guard Hk.
        pose proof (ctx_wf_budget _ Hwf) as Hbc.
        destruct (intern c (Res id (Intop_imm Ilsr k) [a] i))
          as [atom c1] eqn:Hi.
        injection H as <- <- <-.
        destruct (intern_spec c _ atom c1 Hwf ltac:(lia) Hi)
          as [Hwf1 [He1 [Hn1 [Hb1 Hid]]]].
        refine (conj Hwf1 (conj He1 (conj _ _))); [ lia | ].
        intros rho Hrho. pose proof (Hid rho Hrho) as Eid.
        specialize (Hs ltac:(lia)).
        rewrite wrap64_id in Hs.
        2: { apply in_range_implies_int64 with (range := Range.shr_logical k).
             apply logical_shift_range_sound; [ lia | apply value_words_bounded
               ]. }
        cbn [AffineExpr.eval atom_operations_consistent].
        split; [ exact Eid | ].
        split.
        { split; [ lia | ]. exists (value_words a). split; [ apply
          value_words_bounded | ].
          rewrite Eid. exact Hs. }
        { intros rg Hrg. injection Hrg as <-. rewrite Hs.
          apply logical_shift_range_sound; [ lia | apply value_words_bounded ].
            }
      + (* Iasr *)
        destruct ((zero <=? k) && (k <? max_shift_bits)
                  && (one <=? c.(budget)))%sint63 eqn:Hg;
          [ | finish_atomized Hwf (extends_refl c) H ].
        apply andb_prop in Hg. destruct Hg as [Hk Hle]. shift_guard Hk.
        pay one Hwf Hle Hwf1 He1 Hn1 Hb1.
        destruct (expr_of_value (spend_budget c one) a) as [[ea ra] ca] eqn:Ha.
        destruct (IHa _ ea ra ca Hwf1 ltac:(lia) Ha)
          as [Hwfa [Hea [Hna Hrho_a]]].
        pose proof (ctx_wf_budget _ Hwfa) as Hba.
        destruct ra as [ rga | ].
        * destruct (intern ca (Res id (Intop_imm Iasr k) [a] i))
            as [atom c1] eqn:Hi.
          injection H as <- <- <-.
          destruct (intern_spec ca _ atom c1 Hwfa ltac:(lia) Hi)
            as [Hwf1' [He1' [Hn1' [Hb1' Hid]]]].
          refine (conj Hwf1'
                    (conj (extends_trans _ _ _ He1
                             (extends_trans _ _ _ Hea He1'))
                       (conj _ _)));
            [ lia | ].
          intros rho Hrho.
          destruct (Hrho_a rho (agrees_extends _ _ _ He1' Hrho))
            as [Ea [Ca Ra]].
          pose proof (Hid rho Hrho) as Eid.
          specialize (Hs ltac:(lia)).
          cbn [AffineExpr.eval atom_operations_consistent].
          split; [ exact Eid | ].
          split.
          { split; [ exact Ca | ]. split; [ lia | ]. split.
            - rewrite Ea. apply value_words_bounded.
            - rewrite Eid, Ea. exact Hs. }
          { intros rg Hrg. injection Hrg as <-. rewrite Hs.
            apply signed_shift_range_sound; [ lia | apply value_words_bounded
              ]. }
        * finish_atomized Hwfa (extends_trans _ _ _ He1 Hea) H.
  Qed.

  (* The affine form of a value has the value's machine value, and the side
     facts pushed are true; both are well-formed. *)
  Theorem linearize_sound :
    ∀ (value_ref : value) (initial_ctx : ctx)
      (initial_side_facts : list Affine.t) (form : Affine.t)
      (final_side_facts : list Affine.t) (final_ctx : ctx),
    ctx_wf initial_ctx ->
    to_Z initial_ctx.(next) + linearize_atom_bound <= ocaml_max_int ->
    linearize initial_ctx initial_side_facts value_ref
      = ((form, final_side_facts), final_ctx) ->
    ctx_wf final_ctx ∧ extends initial_ctx final_ctx
    ∧ to_Z final_ctx.(next) <= to_Z initial_ctx.(next) + linearize_atom_bound
    ∧ wf form ∧ (Forall wf initial_side_facts -> Forall wf final_side_facts)
    ∧ ∀ (rho : valuation), agrees rho final_ctx ->
         denote rho form = value_words value_ref
         ∧ (holds_all rho initial_side_facts ->
             holds_all rho final_side_facts).
  Proof.
    intros v c side f side' c' Hwf Hn H. unfold linearize in H.
    unfold holds_all. rewrite linearize_atom_bound_eq in *.
    destruct (refill_budget_spec c Hwf) as [Hwfr [Her [Hnr Hbr]]].
    rewrite to_Z_max_nodes in Hbr.
    destruct (expr_of_value (refill_budget c) v) as [[e r] ce] eqn:He.
    destruct (expr_of_value_sound v (refill_budget c) e r ce Hwfr ltac:(lia)
      He)
      as [Hwfe [Hee [Hne Hrho_e]]].
    pose proof (ctx_wf_budget _ Hwfe) as Hbe.
    destruct (to_affine e) as [ [form sides] | ] eqn:Hta.
    - injection H as <- <- <-.
      destruct (to_affine_wf e form sides Hta) as [Hwff Hwfs].
      refine (conj Hwfe (conj (extends_trans _ _ _ Her Hee)
                (conj _ (conj Hwff (conj _ _)))));
        [ lia | | ].
      + intros Hside. apply push_sides_Forall; assumption.
      + intros rho Hrho.
        destruct (Hrho_e rho Hrho) as [Ee [Ce _]].
        destruct (to_affine_sound rho e form sides Hta) as [Ed Hsides].
        split; [ rewrite Ed; exact Ee | ].
        intros Hside. apply push_sides_Forall; [ exact Hside | ].
        apply Hsides. exact Ce.
    - destruct (intern ce v) as [atom c1] eqn:Hi. injection H as <- <- <-.
      destruct (intern_spec ce v atom c1 Hwfe ltac:(lia) Hi)
        as [Hwf1 [He1 [Hn1 [_ Hid]]]].
      refine (conj Hwf1
                (conj (extends_trans _ _ _ Her (extends_trans _ _ _ Hee He1))
                   (conj _ (conj (var_wf atom) (conj _ _)))));
        [ lia | intros Hside; exact Hside | ].
      intros rho Hrho. split; [ rewrite var_denote; apply Hid; exact Hrho | ].
      intros Hside. exact Hside.
  Qed.

  (* A value that is not a comparison yields no facts: close the goal of
     [comparison_facts_sound] for it. *)
  Ltac no_facts Hwf H :=
    cbn [comparison_facts] in H; injection H as <- <- <-;
    refine (conj Hwf (conj (extends_refl _)
              (conj _ (conj (Forall_nil _) (conj _ _)))));
    [ lia
    | intros Hside; exact Hside
    | intros rho _; split;
      [ intros Hside; exact Hside | intros _; constructor ] ].

  (* The facts derived from a comparison hold when the comparison came out as
     the branch taken requires; they, and the side facts, are well-formed. *)
  Theorem comparison_facts_sound :
    ∀ (comparison_result : value) (initial_ctx : ctx)
      (initial_side_facts : list Affine.t) (negate : bool)
      (derived_facts final_side_facts : list Affine.t) (final_ctx : ctx),
    ctx_wf initial_ctx ->
    to_Z initial_ctx.(next) + comparison_atom_bound <= ocaml_max_int ->
    comparison_facts initial_ctx initial_side_facts negate comparison_result =
      ((derived_facts, final_side_facts), final_ctx) ->
    ctx_wf final_ctx ∧ extends initial_ctx final_ctx
    ∧ to_Z final_ctx.(next) <= to_Z initial_ctx.(next) + comparison_atom_bound
    ∧ Forall wf derived_facts
    ∧ (Forall wf initial_side_facts -> Forall wf final_side_facts)
    ∧ ∀ (rho : valuation), agrees rho final_ctx ->
         (holds_all rho initial_side_facts -> holds_all rho final_side_facts)
         ∧ (comparison_matches_branch negate comparison_result ->
             holds_all rho derived_facts).
  Proof.
    intros cond c side negate fs side' c' Hwf Hn H.
    rewrite comparison_atom_bound_eq in *. pose proof linearize_atom_bound_eq
      as Hla.
    destruct cond as [id op args i | bl i]; [ | no_facts Hwf H ].
    destruct op as [ | | | ? | ? | ? | ? | ? | ? | ? | ? | ? | ? | ? ? ? | iop
                   | ? | iop k | ? | ? ? | ? | ? | ? | ? | | | | ? | ? | | | |
                   | | ? ];
      try solve [ no_facts Hwf H ];
      destruct iop; try solve [ no_facts Hwf H ];
      destruct args as [ | a [ | b [ | ? ? ] ] ]; try solve [ no_facts Hwf H ];
      cbn [comparison_facts] in H; cbn [comparison_matches_branch].
    - (* Intop (Icomp cmp), [a; b] *)
      destruct (linearize c side a) as [[la side1] ca] eqn:Hlina.
      destruct (linearize ca side1 b) as [[lb side2] cb] eqn:Hlinb.
      injection H as <- <- <-.
      destruct (linearize_sound a c side la side1 ca Hwf ltac:(lia) Hlina)
        as [Hwfa [Hea [Hna [Hwfla [Hwfs1 Hrho_a]]]]].
      destruct (linearize_sound b ca side1 lb side2 cb Hwfa ltac:(lia) Hlinb)
        as [Hwfb [Heb [Hnb [Hwflb [Hwfs2 Hrho_b]]]]].
      refine (conj Hwfb (conj (extends_trans _ _ _ Hea Heb)
                (conj _ (conj (facts_wf _ _ _ _ Hwfla Hwflb) (conj _ _)))));
        [ lia | | ].
      + intros Hside. apply Hwfs2, Hwfs1, Hside.
      + intros rho Hrho.
        destruct (Hrho_b rho Hrho) as [Eb Sb].
        destruct (Hrho_a rho (agrees_extends _ _ _ Heb Hrho)) as [Ea Sa].
        split; [ intros Hside; apply Sb, Sa, Hside | ].
        intros Ht. unfold holds_all, holds.
        apply facts_sound; [ exact Hwfla | exact Hwflb | ].
        cbv zeta. rewrite Ea, Eb, Ht. destruct negate; reflexivity.
    - (* Intop_imm (Icomp cmp) k, [a] *)
      destruct (linearize c side a) as [[la side1] ca] eqn:Hlina.
      injection H as <- <- <-.
      destruct (linearize_sound a c side la side1 ca Hwf ltac:(lia) Hlina)
        as [Hwfa [Hea [Hna [Hwfla [Hwfs1 Hrho_a]]]]].
      refine (conj Hwfa (conj Hea
                (conj _ (conj (facts_wf _ _ _ _ Hwfla (constant_wf k))
                   (conj Hwfs1 _)))));
        [ lia | ].
      intros rho Hrho. destruct (Hrho_a rho Hrho) as [Ea Sa].
      split; [ exact Sa | ].
      intros Ht. unfold holds_all, holds.
      apply facts_sound; [ exact Hwfla | apply constant_wf | ].
      cbv zeta. rewrite Ea, constant_denote, Ht. destruct negate; reflexivity.
  Qed.

  (* The walk of [guards_at] over any list of dominators, from any accumulated
     facts: it keeps the context well-formed and growing, the facts and side
     facts well-formed, and, under a valuation agreeing with the final context,
     the side facts true and the facts true when the branches went as assumed.
     *)
  Lemma guards_fold_sound :
    ∀ (target : block) (dominators : list block)
      (initial_guard_facts initial_side_facts : list Affine.t)
      (initial_ctx : ctx)
      (derived_guard_facts final_side_facts : list Affine.t) (final_ctx : ctx),
    ctx_wf initial_ctx ->
    to_Z initial_ctx.(next) + guard_list_atom_bound target dominators <=
      ocaml_max_int ->
    List.fold_left (guard_facts target) dominators
      ((initial_guard_facts, initial_side_facts), initial_ctx)
      = ((derived_guard_facts, final_side_facts), final_ctx) ->
    ctx_wf final_ctx ∧ extends initial_ctx final_ctx
    ∧ to_Z final_ctx.(next)
       <= to_Z initial_ctx.(next) + guard_list_atom_bound target dominators
    ∧ (Forall wf initial_guard_facts -> Forall wf derived_guard_facts)
    ∧ (Forall wf initial_side_facts -> Forall wf final_side_facts)
    ∧ ∀ (rho : valuation), agrees rho final_ctx ->
         (holds_all rho initial_side_facts -> holds_all rho final_side_facts)
         ∧ ((∀ (dominator : block),
                dominator ∈ dominators -> guard_taken target dominator) ->
             holds_all rho initial_guard_facts ->
             holds_all rho derived_guard_facts).
  Proof.
    intros target doms. induction doms as [ | d doms IH ];
      intros acc side c fs side' c' Hwf Hn H;
      cbn [List.fold_left] in H.
    - injection H as <- <- <-. rewrite guard_list_atom_bound_nil in *.
      refine (conj Hwf (conj (extends_refl c) (conj _ (conj _ (conj _ _)))));
        [ lia | tauto | tauto | ].
      intros rho _. split; [ tauto | ]. intros _ Hacc. exact Hacc.
    - rewrite guard_list_atom_bound_cons in *.
      pose proof (guard_list_atom_bound_nonnegative target doms) as Hsum.
      pose proof (guard_atom_bound_nonnegative target d) as Hgd.
      assert (Hstep : exists acc1 side1 c1,
        guard_facts target ((acc, side), c) d = ((acc1, side1), c1)
        ∧ ctx_wf c1 ∧ extends c c1
        ∧ to_Z c1.(next) <= to_Z c.(next) + guard_atom_bound target d
        ∧ (Forall wf acc -> Forall wf acc1)
        ∧ (Forall wf side -> Forall wf side1)
        ∧ forall rho, agrees rho c1 ->
             (holds_all rho side -> holds_all rho side1)
             ∧ (guard_taken target d -> holds_all rho acc ->
                 holds_all rho acc1)).
      { unfold guard_facts, guard_atom_bound in *.
        destruct (guard_test target d) as [[index negate] | ] eqn:Hg.
        - destruct (comparison_facts c side negate index)
            as [[fs1 side1] c1] eqn:Hcf.
          exists (fs1 ++ acc), side1, c1. split; [ reflexivity | ].
          destruct (comparison_facts_sound index c side negate fs1 side1 c1 Hwf
                      ltac:(lia) Hcf)
            as [Hwf1 [He1 [Hn1 [Hwffs [Hwfs1 Hrho1]]]]].
          refine (conj Hwf1 (conj He1 (conj Hn1 (conj _ (conj Hwfs1 _)))));
            [ intros Hacc; apply Forall_app; split; assumption | ].
          intros rho Hrho. destruct (Hrho1 rho Hrho) as [S1 T1].
          split; [ exact S1 | ].
          intros Htaken Hacc. unfold holds_all in *. apply Forall_app.
          split; [ apply T1, (Htaken index negate Hg) | exact Hacc ].
        - exists acc, side, c. split; [ reflexivity | ].
          refine (conj Hwf (conj (extends_refl c)
                    (conj _ (conj _ (conj _ _)))));
            [ lia | tauto | tauto | ].
          intros rho _. split; [ tauto | ]. intros _ Hacc. exact Hacc. }
      destruct Hstep
        as [acc1 [side1 [c1 [Est [Hwf1 [He1 [Hn1 [Hacc1 [Hside1 Hrho1]]]]]]]]].
      rewrite Est in H.
      destruct (IH acc1 side1 c1 fs side' c' Hwf1 ltac:(lia) H)
        as [Hwf' [He' [Hn' [Hacc' [Hside' Hrho']]]]].
      refine (conj Hwf' (conj (extends_trans _ _ _ He1 He')
                (conj _ (conj _ (conj _ _)))));
        [ lia | tauto | tauto | ].
      intros rho Hrho. destruct (Hrho' rho Hrho) as [S' A'].
      destruct (Hrho1 rho (agrees_extends _ _ _ He' Hrho)) as [S1 A1].
      split; [ intros Hs; apply S', S1, Hs | ].
      intros Htaken Hacc.
      apply A'; [ intros d' Hd'; apply Htaken; right; exact Hd' | ].
      apply A1; [ apply Htaken; left; reflexivity | exact Hacc ].
  Qed.

  (* The facts [guards_at] gathers at [target] are well-formed, and true under
     a valuation agreeing with the final context when the dominating branches
     went as assumed; the side facts are kept true. *)
  Theorem guards_at_sound :
    ∀ (target : block) (initial_ctx : ctx)
      (initial_side_facts derived_guard_facts final_side_facts : list Affine.t)
      (final_ctx : ctx),
    ctx_wf initial_ctx ->
    to_Z initial_ctx.(next) + guards_atom_bound target <= ocaml_max_int ->
    guards_at initial_ctx initial_side_facts target
      = ((derived_guard_facts, final_side_facts), final_ctx) ->
    ctx_wf final_ctx ∧ extends initial_ctx final_ctx
    ∧ to_Z final_ctx.(next)
       <= to_Z initial_ctx.(next) + guards_atom_bound target
    ∧ Forall wf derived_guard_facts
    ∧ (Forall wf initial_side_facts -> Forall wf final_side_facts)
    ∧ ∀ (rho : valuation), agrees rho final_ctx ->
         (holds_all rho initial_side_facts -> holds_all rho final_side_facts)
         ∧ (guards_taken target -> holds_all rho derived_guard_facts).
  Proof.
    intros target c side fs side' c' Hwf Hn H.
    unfold guards_at, OxCamlList.fold_left in H.
    destruct (guards_fold_sound target (strict_dominators target) [] side c
                fs side' c' Hwf Hn H)
      as [Hwf' [He [Hn' [Hfs [Hside Hrho]]]]].
    refine (conj Hwf' (conj He (conj Hn' (conj (Hfs (Forall_nil _))
              (conj Hside _))))).
    intros rho Hrho'. destruct (Hrho rho Hrho') as [S T].
    split; [ exact S | ].
    intros Htaken. apply T; [ exact Htaken | exact (Forall_nil _) ].
  Qed.

End Semantics.
