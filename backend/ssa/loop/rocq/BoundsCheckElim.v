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

(* Which array bounds checks to remove: [find_eliminations], the decision of
   the bounds-check elimination pass. The extraction of this file,
   BoundsCheckElim.ml, is the implementation; it needs no review.
   bounds_check_elim.ml in the parent directory wraps it, builds the table of
   defining blocks ([op_def]) it is given, and rewrites the graph at the
   (block, target) pairs it returns, which is not modelled.

   What is claimed of the decision is BoundsCheckExecutionSpec.v, which is to
   be reviewed: in every execution reaching a reported block, its branch is
   taken towards the target returned. What the analysis certifies of a check
   it removes, read back from the decision, is BoundsCheckElimProofs.v, which
   needs no review.

   A bounds check is a two-way branch on an unsigned comparison [index <u
   limit] (or [<=u], or either with the operands exchanged), which is how the
   compiler tests [0 <= index < length] in one comparison. The check passes
   when the facts known on entry to its block entail [0 <= index] and [index
   <= limit - 1] (or [index <= limit]): the facts are those of the branches
   dominating the block (AffineSsa.guards_at), the side facts of the
   linearizations (for an array length computed from the header word, its
   range), and the certified bounds of loop counters (InductionRange.v).
   The index is linearized as a proof goal (AffineSsa.linearize_goal), that
   is, modulo 2^64: the two goals pin it between 0 and a register value, so
   its affine form is its machine value after all. The limit is linearized
   exactly, so no bound on it is needed; in particular the assumption that an
   array's length is at most [Max_wosize] is not used: when the length is read
   from the header word, the range of the logical shift is a side fact of
   its linearization, and otherwise the facts have to bound it.

   The global applicability gate is that of DeleteEmptyLoops.v
   ([graph_supported]). *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlInt64 OxCamlList.
From OxCamlBackend Require Import IntegerComparison Operation.
From OxCamlSsa Require Import SsaModel SsaGraph SsaSemantics.
From OxCamlLoop Require Import FourierMotzkin AffineSsa NaturalLoop
  InductionVar DeleteEmptyLoops InductionRange.
Import ListNotations.

Local Open Scope sint63_scope.

(* The other operand of a check: a value or a literal. *)
Inductive limit : Type :=
  | Value_limit (v : value)
  | Const_limit (k : int).

(* The unsigned comparison a branch tests, read as [index <u limit] or [index
   <=u limit]: [a <u b] and [a <=u b] as they are, [a >u b] and [a >=u b] with
   their operands exchanged. *)
Definition unsigned_check (cond : value)
    : option (integer_comparison * value * limit) :=
  match cond with
  | Res _ (Intop (Icomp cmp)) [a; b] _ =>
    match cmp with
    | Cult | Cule => Some (cmp, a, Value_limit b)
    | Cugt | Cuge => Some (swap cmp, b, Value_limit a)
    | Ceq | Cne | Clt | Cgt | Cle | Cge => None
    end
  | Res _ (Intop_imm (Icomp cmp) k) [a] _ =>
    match cmp with
    | Cult | Cule => Some (cmp, a, Const_limit k)
    | Ceq | Cne | Clt | Cgt | Cle | Cge | Cugt | Cuge => None
    end
  | _ => None
  end.

(* The goals whose entailment makes the check pass: [0 <= index] and [index <=
   limit - 1] for [<u], [index <= limit] for [<=u]; none when a form
   overflows. *)
Definition check_goals (cmp : integer_comparison) (index limit : Affine.t)
    : option (list Affine.t) :=
  let* room := Affine.sub_checked limit index in
  match cmp with
  | Cult =>
    let* room := Affine.add_const_checked room minus_one in
    Some [index; room]
  | Cule => Some [index; room]
  | _ => None
  end.

(* The affine form of the limit, exact, with the side facts of a value's
   linearization added to [side]. *)
Definition limit_form (c : ctx) (side : list Affine.t) (l : limit)
    : (Affine.t * list Affine.t) * ctx :=
  match l with
  | Value_limit v => linearize c side v
  | Const_limit k => ((Affine.constant k, side), c)
  end.

(* Whether the check [cond] that [b] branches on always passes. *)
Definition check_passes (certificates : list range_certificate) (b : block)
    (cond : value) : bool :=
  match unsigned_check cond with
  | Some (cmp, index, limit) =>
    let '(index_form, c) := linearize_goal new_ctx index in
    let '((limit_form, side), c) := limit_form c [] limit in
    let '((guards, side), c) := guards_at c side b in
    let '(ranges, _) := range_facts certificates b c in
    match check_goals cmp index_form limit_form with
    | Some goals => forallb (entails (ranges ++ guards ++ side)) goals
    | None => false
    end
  | None => false
  end.

(* The check of [b], as the pair of [b] and the target its branch always
   takes, when [b] ends in a two-way branch that always passes. *)
Definition eliminate_check (certificates : list range_certificate)
    (b : block) : option (block * block) :=
  match terminator b with
  | Terminator.Switch cond [_; ifso] =>
    if check_passes certificates b cond then Some (b, ifso) else None
  | _ => None
  end.

(* The checks to remove, as (block, target) pairs, given the table of defining
   blocks, the graph's entry block and its block list. The counter
   certificates are used only when there are few enough of them for the
   affine atom counter. *)
Definition find_eliminations (op_def : op_def) (entry : block)
    (blocks : list block) : list (block * block) :=
  if graph_supported blocks
  then
    let certificates := find_certificates op_def entry blocks in
    let certificates :=
      if length_at_most graph_block_limit certificates then certificates
      else []
    in
    filter_map (eliminate_check certificates) blocks
  else [].

Local Close Scope sint63_scope.
