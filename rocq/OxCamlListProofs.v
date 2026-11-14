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

(* Facts about the list functions of OxCamlList.v that the proofs use: what
   [length_at_most], [same_length], [nth_int], [filter_map], [assoc_opt],
   [map_opt] and [assoc_default] answer, and how a filter changes a length.
   Nothing here needs review; Rocq checks the proofs. *)

From Stdlib Require Import ZArith Lia List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlIntProofs
  OxCamlList.
Import ListNotations.

Local Open Scope sint63_scope.

(* A successful bounded walk establishes the mathematical length bound. *)
Lemma length_at_most_spec : ∀ (A : Type) (xs : list A) (limit : int),
  length_at_most limit xs = true ->
  (Z.of_nat (length xs) <= to_Z limit)%Z.
Proof.
  intros A xs. induction xs as [|x rest IH]; intros limit Hcheck;
    cbn [length_at_most] in Hcheck.
  - apply leb_int in Hcheck. rewrite to_Z_zero in Hcheck. exact Hcheck.
  - destruct (zero <? limit) eqn:Hpositive; [|discriminate].
    apply ltb_int in Hpositive. rewrite to_Z_zero in Hpositive.
    specialize (IH _ Hcheck).
    rewrite Sint63.sub_spec, to_Z_one in IH.
    rewrite Sint63.cmod_small in IH.
    + cbn [length]. rewrite Nat2Z.inj_succ. lia.
    + pose proof (int_bounds limit). unfold two62 in *.
      change (-4611686018427387904 <= to_Z limit - 1
        < 4611686018427387904)%Z. lia.
Qed.

(* [same_length] is equality of lengths. *)
Lemma same_length_spec : ∀ {A B : Type} (l1 : list A) (l2 : list B),
  same_length l1 l2 = true <-> length l1 = length l2.
Proof.
  intros A B l1. induction l1 as [ | x l1 IH ]; intros [ | y l2 ]; simpl;
    [ tauto | split; discriminate | split; discriminate | ].
  rewrite IH. split; [ intros ->; reflexivity | intros H; injection H; auto ].
Qed.

(* Equal-length arrays have the same valid machine-int indices. *)
Lemma same_length_nth_int : ∀ {A B : Type} (xs : list A) (ys : list B)
    (index : int) (x : A),
  same_length xs ys = true -> nth_int xs index = Some x ->
  ∃ (y : B), nth_int ys index = Some y.
Proof.
  intros A B xs. induction xs as [ | x xs IH];
    intros [ | y ys] index value Hlength Hnth; try discriminate; cbn in *.
  destruct (index =? zero) eqn:E.
  - exists y. reflexivity.
  - eapply IH; eassumption.
Qed.

(* An element at some position is an element of the list. *)
Lemma nth_int_In : ∀ {A : Type} (l : list A) (i : int) (x : A),
  nth_int l i = Some x -> x ∈ l.
Proof.
  intros A l. induction l as [ | y l IH ]; intros i x H; simpl in H.
  - discriminate.
  - destruct (i =? zero).
    + injection H as ->. left. reflexivity.
    + right. exact (IH _ _ H).
Qed.

(* Equal suffix lengths identify the same element in two decompositions of a
   list. This lets SSA definitions be located by instruction position. *)
Lemma same_position_element : ∀ (A : Type)
    (prefix prefix' suffix suffix' : list A) (element element' : A),
  prefix ++ element :: suffix = prefix' ++ element' :: suffix' ->
  length suffix = length suffix' -> element = element'.
Proof.
  intros A prefix. induction prefix as [ | head prefix IH ];
    intros prefix' suffix suffix' element element' Heq Hlength;
    destruct prefix' as [ | head' prefix']; cbn in Heq.
  - congruence.
  - apply (f_equal (@length A)) in Heq. cbn in Heq.
    rewrite length_app in Heq. cbn in Heq. lia.
  - apply (f_equal (@length A)) in Heq. cbn in Heq.
    rewrite length_app in Heq. cbn in Heq. lia.
  - injection Heq as _ Htail. eapply IH; eassumption.
Qed.

(* Pointwise-related lists have related entries at the same machine-int index,
   whenever the entry in the first list exists. *)
Lemma Forall2_nth_int : ∀ {A B : Type} (R : A -> B -> Prop)
    (xs : list A) (ys : list B),
  Forall2 R xs ys -> ∀ (index : int) (x : A),
  nth_int xs index = Some x ->
  ∃ (y : B), nth_int ys index = Some y ∧ R x y.
Proof.
  intros A B R xs ys H. induction H; intros i z Hnth; cbn in *.
  - discriminate.
  - destruct (i =? zero) eqn:E.
    + injection Hnth as <-. exists y. auto.
    + apply IHForall2. exact Hnth.
Qed.

(* An element of [filter_map f l] is [f] of an element of [l]. *)
Lemma filter_map_In :
  ∀ {A B : Type} (f : A -> option B) (l : list A) (y : B),
  y ∈ (filter_map f l) -> ∃ x, x ∈ l ∧ f x = Some y.
Proof.
  intros A B f l y. induction l as [ | x l IH ]; simpl; intros H.
  - contradiction.
  - destruct (f x) as [ y' | ] eqn:Hx.
    + destruct H as [ -> | H ].
      * exists x. split; [ left; reflexivity | exact Hx ].
      * destruct (IH H) as [x' [Hin Hx']]. exists x'. auto.
    + destruct (IH H) as [x' [Hin Hx']]. exists x'. auto.
Qed.

(* [assoc_opt] finds an entry with the key ... *)
Lemma assoc_opt_In : ∀ {C : Type} (key : int) (entries : list (int * C))
  (entry_value : C),
  assoc_opt key entries = Some entry_value -> (key, entry_value) ∈ entries.
Proof.
  intros C id l c. induction l as [ | [k c'] l IH ]; simpl; intros H.
  - discriminate.
  - destruct (id =? k) eqn:E.
    + apply eqb_int in E. injection H as <-. subst. left. reflexivity.
    + right. apply IH. exact H.
Qed.

(* ... finds one whenever the key is present ... *)
Lemma assoc_opt_Some : ∀ {C : Type} (key : int)
  (entries : list (int * C)),
  key ∈ map fst entries ->
  ∃ entry_value, assoc_opt key entries = Some entry_value.
Proof.
  intros C id l. induction l as [ | [k c] l IH ]; simpl; intros H.
  - contradiction.
  - destruct (id =? k) eqn:E; [ exists c; reflexivity | ].
    destruct H as [ -> | H ]; [ rewrite eqb_int_refl in E; discriminate | ].
    apply IH. exact H.
Qed.

(* ... and none when it is absent. *)
Lemma assoc_opt_None : ∀ {C : Type} (key : int)
  (entries : list (int * C)),
  key ∉ (map fst entries) -> assoc_opt key entries = None.
Proof.
  intros C id l. induction l as [ | [k c] l IH ]; simpl; intros H.
  - reflexivity.
  - destruct (id =? k) eqn:E.
    + apply eqb_int in E. subst. exfalso. apply H. left. reflexivity.
    + apply IH. intros H'. apply H. right. exact H'.
Qed.

(* [map_opt] over an association list, mapping the values: the keys are
   unchanged ... *)
Lemma map_opt_keys :
  ∀ {C D : Type} (g : C -> option D) (entries : list (int * C))
    (mapped_entries : list (int * D)),
  map_opt (fun '(id, c) => let* d := g c in Some (id, d)) entries = Some
    mapped_entries ->
  map fst mapped_entries = map fst entries.
Proof.
  intros C D g l. induction l as [ | [k c] l IH ]; simpl; intros l' H.
  - injection H as <-. reflexivity.
  - destruct (g c) as [ d | ]; [ | discriminate ].
    destruct (map_opt _ l) as [ rest | ] eqn:Hr; [ | discriminate ].
    injection H as <-. simpl. f_equal. apply IH. reflexivity.
Qed.

(* ... and each value is mapped. *)
Lemma map_opt_assoc :
  ∀ {C D : Type} (g : C -> option D) (entries : list (int * C))
    (mapped_entries : list (int * D)),
  map_opt (fun '(id, c) => let* d := g c in Some (id, d)) entries = Some
    mapped_entries ->
  ∀ (id : int),
    match assoc_opt id entries with
    | Some c => ∃ d, g c = Some d ∧ assoc_opt id mapped_entries = Some d
    | None => assoc_opt id mapped_entries = None
    end.
Proof.
  intros C D g l. induction l as [ | [k c] l IH ]; simpl; intros l' H id.
  - injection H as <-. reflexivity.
  - destruct (g c) as [ d | ] eqn:Hg; [ | discriminate ].
    destruct (map_opt _ l) as [ rest | ] eqn:Hr; [ | discriminate ].
    injection H as <-. simpl. destruct (id =? k).
    + exists d. split; [ exact Hg | reflexivity ].
    + apply IH. reflexivity.
Qed.

(* [rev_append] has the elements of both lists. *)
Lemma in_rev_append : ∀ {A : Type} (l1 l2 : list A) (x : A),
  x ∈ (rev_append l1 l2) <-> x ∈ l1 ∨ x ∈ l2.
Proof.
  intros. rewrite rev_append_rev, in_app_iff, <- in_rev. reflexivity.
Qed.

(* --- [assoc_opt] and [assoc_default] --- *)

(* In a list whose keys are distinct, the lookup of a key finds its entry. *)
Lemma assoc_opt_NoDup :
  ∀ {C : Type} (entries : list (int * C)) (key : int) (entry_value : C),
  NoDup (map fst entries) -> (key, entry_value) ∈ entries -> assoc_opt key
    entries = Some entry_value.
Proof.
  intros C l id c. induction l as [ | [k c'] l IH ]; intros Hnd Hin.
  - contradiction.
  - cbn [map fst] in Hnd. inversion Hnd as [ | ? ? Hnin Hnd' ]. subst.
    cbn [assoc_opt]. destruct Hin as [ Heq | Hin ].
    + injection Heq as <- <-. rewrite eqb_int_refl. reflexivity.
    + destruct (id =? k)%sint63 eqn:E.
      * apply eqb_int in E. subst k. exfalso. apply Hnin.
        apply in_map_iff. exists (id, c). split; [ reflexivity | exact Hin ].
      * apply IH; assumption.
Qed.

(* [assoc_default] on a cons: the head's value if its key matches, else the
   lookup in the tail. *)
Lemma assoc_default_cons :
  ∀ {C : Type} (default : C) (key head_key : int) (head_value : C)
    (entries : list (int * C)),
  assoc_default default key ((head_key, head_value) :: entries)
  = if (key =? head_key)%sint63 then head_value
    else assoc_default default key entries.
Proof.
  intros C default key head_key head_value entries.
  unfold assoc_default. simpl.
  destruct (key =? head_key)%sint63; reflexivity.
Qed.

(* An absent key gives the default. *)
Lemma assoc_default_not_in :
  ∀ {C : Type} (default : C) (key : int) (entries : list (int * C)),
  key ∉ (map fst entries) -> assoc_default default key entries = default.
Proof.
  intros. unfold assoc_default. rewrite assoc_opt_None by assumption.
  reflexivity.
Qed.

(* --- [filter] --- *)

(* A filter keeps at most the list's elements. *)
Lemma length_filter_le : ∀ {A : Type} (f : A -> bool) (l : list A),
  (length (filter f l) <= length l)%nat.
Proof.
  intros A f l. induction l as [ | x l IH ]; [ reflexivity | ].
  simpl. destruct (f x); simpl; lia.
Qed.

(* A filter that accepts less keeps no more elements ... *)
Lemma filter_length_le : ∀ {A : Type} (f g : A -> bool) (l : list A),
  (∀ (y : A), f y = true -> g y = true) ->
  (length (filter f l) <= length (filter g l))%nat.
Proof.
  intros A f g l Hfg. induction l as [ | y l IH ]; [ reflexivity | ].
  simpl. destruct (f y) eqn:Hf.
  - rewrite (Hfg y Hf). simpl. lia.
  - destruct (g y); simpl; lia.
Qed.

(* ... and strictly fewer when it rejects an element of the list that the other
   accepts. *)
Lemma filter_length_lt : ∀ {A : Type} (f g : A -> bool) (l : list A)
  (x : A),
  (∀ (y : A), f y = true -> g y = true) ->
  x ∈ l -> g x = true -> f x = false ->
  (length (filter f l) < length (filter g l))%nat.
Proof.
  intros A f g l x Hfg Hin Hg Hf.
  induction l as [ | y l IH ]; [ contradiction | ].
  simpl. destruct Hin as [ -> | Hin ].
  - rewrite Hf, Hg. simpl. pose proof (filter_length_le f g l Hfg). lia.
  - destruct (f y) eqn:Hfy.
    + rewrite (Hfg y Hfy). simpl. apply IH in Hin. lia.
    + destruct (g y); simpl; apply IH in Hin; lia.
Qed.
