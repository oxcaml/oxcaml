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

(* Models of the OxCaml [List] functions the compiler models use, over
   [int]-keyed association lists where a key is involved.

   Functions whose OxCaml callback may raise an exception come in a variant
   returning [option] (see OxCamlOption.v): [map_opt] is [List.map] and
   [filter_map_opt] is [List.filter_map], each returning [None] if any call of
   the callback would have raised. *)

From Stdlib Require Import ZArith Lia List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt.
Import ListNotations.

Notation "element ∈ elements" := (List.In element elements)
  (at level 70).
Notation "element ∉ elements" := (~ List.In element elements)
  (at level 70).

Local Open Scope sint63_scope.

(* [List.assoc_opt] on [int] keys. *)
Fixpoint assoc_opt {C} (id : int) (l : list (int * C)) : option C :=
  match l with
  | [] => None
  | (k, c) :: l => if id =? k then Some c else assoc_opt id l
  end.

(* The value for [id] in an association list, [dflt] if there is none. *)
Definition assoc_default {C} (dflt : C) (id : int) (l : list (int * C)) : C :=
  match assoc_opt id l with Some c => c | None => dflt end.

(* [List.filter_map]. *)
Fixpoint filter_map {A B} (f : A -> option B) (l : list A) : list B :=
  match l with
  | [] => []
  | x :: l =>
    match f x with
    | Some y => y :: filter_map f l
    | None => filter_map f l
    end
  end.

(* [List.concat_map]. *)
Definition concat_map {A B} (f : A -> list B) (l : list A) : list B :=
  flat_map f l.

(* [List.fold_left f init l]; Rocq's own [fold_left] takes [init] last. *)
Definition fold_left {A B} (f : A -> B -> A) (init : A) (l : list B) : A :=
  List.fold_left f l init.

(* How many elements of [l] [same] accepts. *)
Definition count_occurrences {A} (same : A -> bool) (l : list A) : int :=
  fold_left (fun n x => if same x then n + one else n) zero l.

(* [List.sort_uniq Int.compare]. Only what the proofs need is assumed: the
   result has no duplicates and the same members as the input. *)
Parameter sort_uniq : list int -> list int.
Axiom sort_uniq_NoDup : ∀ (l : list int), NoDup (sort_uniq l).
Axiom sort_uniq_In : ∀ (l : list int) (x : int),
  x ∈ (sort_uniq l) <-> x ∈ l.

(* [List.compare_length_with l n], the sign of [length l - n]. The models only
   use it to apply a size limit, on which nothing they claim depends, so no
   property of it is assumed. *)
Parameter compare_length_with : ∀ {A : Type}, list A -> int -> int.

(* A bounded length check without a potentially overflowing length count. The
   extracted recursion decrements a positive machine integer only. *)
Fixpoint length_at_most {A : Type} (limit : int) (xs : list A) : bool :=
  match xs with
  | [] => zero <=? limit
  | _ :: rest =>
    if zero <? limit then length_at_most (limit - one) rest else false
  end.

(* [List.map f l] where [f] may raise. *)
Fixpoint map_opt {A B} (f : A -> option B) (l : list A) : option (list B) :=
  match l with
  | [] => Some []
  | x :: l =>
    let* y := f x in
    let* l := map_opt f l in
    Some (y :: l)
  end.

(* [List.filter_map f l] where [f] may raise: the outer [option] is the
   exception, the inner one is [f]'s answer. *)
Fixpoint filter_map_opt {A B} (f : A -> option (option B)) (l : list A)
    : option (list B) :=
  match l with
  | [] => Some []
  | x :: l =>
    let* y := f x in
    let* l := filter_map_opt f l in
    Some (match y with Some y => y :: l | None => l end)
  end.

(* [arr.(i)] on an array held as a list: [None] where OxCaml would raise
   [Invalid_argument], i.e. for [i] outside [0 .. length - 1]. *)
Fixpoint nth_int {A} (l : list A) (i : int) : option A :=
  match l with
  | [] => None
  | x :: l => if i =? zero then Some x else nth_int l (i - one)
  end.

(* [Array.length a = Array.length b] on arrays held as lists. *)
Fixpoint same_length {A B} (l1 : list A) (l2 : list B) : bool :=
  match l1, l2 with
  | [], [] => true
  | _ :: l1, _ :: l2 => same_length l1 l2
  | [], _ :: _ | _ :: _, [] => false
  end.
