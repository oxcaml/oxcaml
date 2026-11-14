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
   file, AffineSsa.ml next to it, is the implementation; it needs no review.
   What the forms and facts mean, and the proofs, are in AffineSsaProofs.v.
   AffineExpr.v has the expressions and ranges this file builds, and
   LoopComparisons.v the facts it derives from comparisons.

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

   [linearize_goal] is the linearization of a proof goal for the bounds-check
   entailment (BoundsCheckElim.v): sums, differences, immediate additions,
   left shifts and the target's scaled additions are decomposed whether or
   not their results are known to fit a register, so its form has the value's
   machine value only modulo 2^64.

   Trusted parts, beyond those of AffineExpr.v: what the recognised operations
   compute, which is [SsaSemanticsSpec.eval_operation], that [Ssa.Value.equal]
   only identifies values with the same machine value, and that the dominators
   and terminators SsaModel.v reports are the graph's. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlInt64 OxCamlList
  IntArith.
From OxCamlBackend Require Import IntegerComparison Operation.
From OxCamlSsa Require Import SsaModel SsaDominance SsaSemanticsSpec.
From OxCamlLoop Require Import FourierMotzkin LoopComparisons AffineExpr.
Import ListNotations.

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
   an atom ([atomize]). The argument of a logical shift is not walked. A
   target-specific operation is understood through the target's linear
   reading of it (Operation.specific_linear): a scaled addition, like the
   re-tagging of a loop counter. Sound for a well-formed context ([ctx_wf] in
   AffineSsaProofs.v) whose atom counter has room for the budget:
   [expr_of_value_sound], there. *)
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
  | Res _ (Specific s) [a] _ =>
    match specific_linear s with
    | Some ([ma], k) =>
      if one <=? c.(budget)
      then
        let c := spend_budget c one in
        let '((ea, ra), c) := expr_of_value c a in
        keep_if_range_known c v
          (AffineExpr.Add (AffineExpr.Scale ma ea) (AffineExpr.Const k))
          (add_optional_ranges (scale_optional_range ma ra)
            (Some (Range.const k)))
      else atomize c v
    | _ => atomize c v
    end
  | Res _ (Specific s) [a; b] _ =>
    match specific_linear s with
    | Some ([ma; mb], k) =>
      if two <=? c.(budget)
      then
        let c := spend_budget c two in
        let '((ea, ra), c) := expr_of_value c a in
        let '((eb, rb), c) := expr_of_value c b in
        keep_if_range_known c v
          (AffineExpr.Add
            (AffineExpr.Add (AffineExpr.Scale ma ea) (AffineExpr.Scale mb eb))
            (AffineExpr.Const k))
          (add_optional_ranges
            (add_optional_ranges (scale_optional_range ma ra)
              (scale_optional_range mb rb))
            (Some (Range.const k)))
      else atomize c v
    | _ => atomize c v
    end
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

(* --- Linearization for proof goals --- *)

(* [goal_atom] makes a value a plain atom. *)
Definition goal_atom (c : ctx) (v : value) : AffineExpr.t * ctx :=
  let '(id, c) := intern c v in (AffineExpr.Atom id, c).

(* The expression of a value as a proof goal sees it: sums, differences,
   immediate additions, left shifts and the target's scaled additions are
   decomposed whether or not their results are known to fit a register,
   constants are literals and everything else is an atom. The expression has
   the machine value only modulo 2^64 ([expr_of_value_goal_sound] in
   AffineSsaProofs.v): sound for a goal that itself pins the value into
   register range (the bounds-check goals of BoundsCheckElim.v), not for a
   fact. The budget pays for each argument as in [expr_of_value]. *)
Fixpoint expr_of_value_goal (c : ctx) (v : value) : AffineExpr.t * ctx :=
  match v with
  | Res _ (Const_int n) _ _ =>
    let n := Int64.of_nativeint n in
    if int64_is_int n
    then (AffineExpr.Const (Int64.to_int n), c)
    else goal_atom c v
  | Res _ (Intop Iadd) [a; b] _ =>
    if two <=? c.(budget)
    then
      let c := spend_budget c two in
      let '(ea, c) := expr_of_value_goal c a in
      let '(eb, c) := expr_of_value_goal c b in
      (AffineExpr.Add ea eb, c)
    else goal_atom c v
  | Res _ (Intop Isub) [a; b] _ =>
    if two <=? c.(budget)
    then
      let c := spend_budget c two in
      let '(ea, c) := expr_of_value_goal c a in
      let '(eb, c) := expr_of_value_goal c b in
      (AffineExpr.Sub ea eb, c)
    else goal_atom c v
  | Res _ (Intop_imm Iadd k) [a] _ =>
    if one <=? c.(budget)
    then
      let c := spend_budget c one in
      let '(ea, c) := expr_of_value_goal c a in
      (AffineExpr.Add ea (AffineExpr.Const k), c)
    else goal_atom c v
  | Res _ (Intop_imm Isub k) [a] _ =>
    if one <=? c.(budget)
    then
      let c := spend_budget c one in
      let '(ea, c) := expr_of_value_goal c a in
      (AffineExpr.Sub ea (AffineExpr.Const k), c)
    else goal_atom c v
  | Res _ (Intop_imm Ilsl k) [a] _ =>
    if (zero <=? k) && (k <? max_shift_bits) && (one <=? c.(budget))
    then
      let c := spend_budget c one in
      let '(ea, c) := expr_of_value_goal c a in
      (AffineExpr.Scale (one << k) ea, c)
    else goal_atom c v
  | Res _ (Specific s) [a] _ =>
    match specific_linear s with
    | Some ([ma], k) =>
      if one <=? c.(budget)
      then
        let c := spend_budget c one in
        let '(ea, c) := expr_of_value_goal c a in
        (AffineExpr.Add (AffineExpr.Scale ma ea) (AffineExpr.Const k), c)
      else goal_atom c v
    | _ => goal_atom c v
    end
  | Res _ (Specific s) [a; b] _ =>
    match specific_linear s with
    | Some ([ma; mb], k) =>
      if two <=? c.(budget)
      then
        let c := spend_budget c two in
        let '(ea, c) := expr_of_value_goal c a in
        let '(eb, c) := expr_of_value_goal c b in
        (AffineExpr.Add
          (AffineExpr.Add (AffineExpr.Scale ma ea) (AffineExpr.Scale mb eb))
          (AffineExpr.Const k), c)
      else goal_atom c v
    | _ => goal_atom c v
    end
  | _ => goal_atom c v
  end.

(* The affine form of a value as a proof goal, walked with a fresh budget; an
   atom for the value itself when [to_affine] overflows. There are no side
   facts: the shifts and ors that have them are atoms here. Sound modulo 2^64:
   [linearize_goal_sound] in AffineSsaProofs.v. *)
Definition linearize_goal (c : ctx) (v : value) : Affine.t * ctx :=
  let '(e, c) := expr_of_value_goal (refill_budget c) v in
  match to_affine e with
  | Some (form, _) => (form, c)
  | None => let '(id, c) := intern c v in (Affine.var id, c)
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
