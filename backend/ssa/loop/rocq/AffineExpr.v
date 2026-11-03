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

(* The affine view of a machine-integer value: a small expression type, its
   linearization into affine forms with side facts, and the value ranges that
   certify a linearization exact. The extraction of this file, AffineExpr.ml
   next to it, is the implementation; it needs no review. What the expressions
   and ranges mean, and the proofs, are in AffineExprProofs.v. FourierMotzkin.v
   has the model of [Affine] that this one builds on.

   [to_affine] returns an [option]: [None] where a coefficient would wrap in
   one of the checked [Affine] operations (see OxCamlOption.v).

   Trusted parts, beyond those of FourierMotzkin.v: the [Int64] shifts and
   negation, the exactness of [Int64.to_int] on values that fit an [int]
   (axioms in OxCamlInt64.v), and the extraction of [lsl] (OxCamlExtract.v). *)

From Stdlib Require Import ZArith Lia Bool List Zbitwise.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlInt64 OxCamlList
  IntArith.
From OxCamlLoop Require Import FourierMotzkin.
Import ListNotations.

Local Open Scope sint63_scope.

(* The expression recognised in an integer value: constants, atoms, sums,
   differences and scalings, and the right shifts and ors, which stand as atoms
   ([Shr_atom], [Lsr_atom], [Or_atom]) with facts about their ranges;
   [to_affine] says what each stands for. *)
Inductive t : Type :=
  | Const (c : int)
  | Atom (id : int)
  | Add (a b : t)
  | Sub (a b : t)
  | Scale (k : int) (a : t)
  | Shr_atom (atom : int) (arg : t) (bits : int)
  | Lsr_atom (atom : int) (bits : int)
  | Or_atom (atom : int) (arg : t) (mask : int).

Module Range.
  (* A closed interval of 64-bit values. *)
  Record t := { lo : Int64.t; hi : Int64.t }.

  (* The range of any register; of a constant; of an arithmetic right shift by
     [bits] of any register; and of a logical right shift by [bits] of any
     register (the full range for a shift by zero or less). *)
  Definition full : t := {| lo := Int64.min_int; hi := Int64.max_int |}.

  Definition const (c : int) : t :=
    let c := Int64.of_int c in {| lo := c; hi := c |}.

  Definition shr_signed (bits : int) : t :=
    {| lo := Int64.shift_right Int64.min_int bits;
       hi := Int64.shift_right Int64.max_int bits |}.

  Definition shr_logical (bits : int) : t :=
    if bits <=? zero
    then full
    else {| lo := Int64.zero;
            hi := Int64.shift_right_logical Int64.minus_one bits |}.

  (* The checked 64-bit arithmetic of IntArith.v (rocq/). *)
  Definition add64 : Int64.t -> Int64.t -> option Int64.t := add_ovf64.
  Definition mul64 : Int64.t -> Int64.t -> option Int64.t := mul_ovf64.

  (* Each operation answers [None] when a bound would overflow. *)
  Definition add (a b : t) : option t :=
    match add64 a.(lo) b.(lo), add64 a.(hi) b.(hi) with
    | Some lo, Some hi => Some {| lo := lo; hi := hi |}
    | _, _ => None
    end.

  Definition neg (r : t) : option t :=
    if Int64.equal r.(lo) Int64.min_int
    then None
    else Some {| lo := Int64.neg r.(hi); hi := Int64.neg r.(lo) |}.

  (* [a - b] as [a + (- b)]. *)
  Definition sub (a b : t) : option t :=
    let* nb := neg b in add a nb.

  (* The range of [x lor mask] for [x] in [r] and a non-negative [mask]. *)
  Definition or_mask (mask : int) (r : t) : option t :=
    if mask <? zero
    then None
    else
      let* hi := add64 r.(hi) (Int64.of_int mask) in
      Some {| lo := r.(lo); hi := hi |}.

  Definition scale (k : int) (r : t) : option t :=
    if k =? zero
    then Some {| lo := Int64.zero; hi := Int64.zero |}
    else
      let k := Int64.of_int k in
      match mul64 k r.(lo), mul64 k r.(hi) with
      | Some a, Some b =>
        if Int64.compare a b <=? zero
        then Some {| lo := a; hi := b |}
        else Some {| lo := b; hi := a |}
      | _, _ => None
      end.
End Range.

(* The [64] of [64 - bits]: the width of a register. *)
Definition word_bits : int := 64.

(* The facts about the atom standing for a right shift by [bits] bits: the
   bounds of its result, when they are representable. *)
Definition shr_range_facts (signed : bool) (atom bits : int)
    : list Affine.t :=
  if signed
  then
    if bits <? one
    then []
    else
      let hi := Int64.to_int (Int64.shift_right Int64.max_int bits) in
      let upper :=
        Affine.add_const (Affine.scale minus_one (Affine.var atom)) hi
      in
      if two <=? bits
      then
        let lo := Int64.to_int (Int64.shift_right Int64.min_int bits) in
        [upper; Affine.add_const (Affine.var atom) (- lo)]
      else [upper]
  else if bits <? one
  then []
  else
    let lower := Affine.var atom in
    if two <=? bits
    then
      let hi := (one << (word_bits - bits)) - one in
      [Affine.add_const (Affine.scale minus_one (Affine.var atom)) hi; lower]
    else [lower].

(* The affine form of an expression, with the side facts about the atoms
   standing for its shifts and ors; each checked operation is a [let*] step,
   [None] on overflow. *)
Fixpoint to_affine (expr : t) : option (Affine.t * list Affine.t) :=
  match expr with
  | Const c => Some (Affine.constant c, [])
  | Atom id => Some (Affine.var id, [])
  | Add a b =>
    let* '(fa, sa) := to_affine a in
    let* '(fb, sb) := to_affine b in
    let* form := Affine.add_checked fa fb in
    Some (form, sa ++ sb)
  | Sub a b =>
    let* '(fa, sa) := to_affine a in
    let* '(fb, sb) := to_affine b in
    let* nfb := Affine.scale_checked minus_one fb in
    let* form := Affine.add_checked fa nfb in
    Some (form, sa ++ sb)
  | Scale k a =>
    let* '(fa, sa) := to_affine a in
    let* form := Affine.scale_checked k fa in
    Some (form, sa)
  | Shr_atom atom arg bits =>
    let* '(fa, sa) := to_affine arg in
    let v := Affine.var atom in
    let pow := one << bits in
    (* [fa - pow * v >= 0] *)
    let* m := Affine.scale_checked (- pow) v in
    let* s1 := Affine.add_checked fa m in
    (* [pow * v + (pow - 1) - fa >= 0] *)
    let* p := Affine.scale_checked pow v in
    let* pc := Affine.add_const_checked p (pow - one) in
    let* nfa := Affine.scale_checked minus_one fa in
    let* s2 := Affine.add_checked pc nfa in
    Some (v, s1 :: s2 :: (shr_range_facts true atom bits ++ sa))
  | Lsr_atom atom bits =>
    Some (Affine.var atom, shr_range_facts false atom bits)
  | Or_atom atom arg mask =>
    let* '(fa, sa) := to_affine arg in
    let v := Affine.var atom in
    (* [v - fa >= 0] *)
    let* nfa := Affine.scale_checked minus_one fa in
    let* s1 := Affine.add_checked v nfa in
    (* [fa + mask - v >= 0] *)
    let* fm := Affine.add_const_checked fa mask in
    let* nv := Affine.scale_checked minus_one v in
    let* s2 := Affine.add_checked fm nv in
    Some (v, s1 :: s2 :: sa)
  end.
