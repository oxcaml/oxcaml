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

(* Affine inequalities over interned integer atoms, with Fourier-Motzkin
   feasibility and entailment, for the loop analyses. The extraction of this
   file, FourierMotzkin.ml next to it, is the implementation.

   See the reading guide at the top of LoopComparisons.v for the Rocq
   vocabulary and the layout of the shared files. As there, this file has four
   parts: (1) the definitions, (2) the intended meaning, (3) stepping-stone
   lemmas, (4) the theorems. Parts 1 and 2 are the part to check by eye.

   Every function that can overflow returns an [option], [None] standing for
   the overflow (see OxCamlOption.v); nothing wraps silently.

   Trusted parts: the [Int64] model in OxCamlInt64.v, the [sort_uniq]
   assumptions in OxCamlList.v and the overflow tests of the checked arithmetic
   in IntArith.v (each is a few lines of OxCaml to review).

   The main results are [feasible_sound] and [entails_sound] in part 4: when
   [feasible] says "infeasible", no integer assignment satisfies the
   inequalities, and when [entails] says the goal follows from the facts, it
   does, for every integer assignment satisfying the facts. These are the
   directions the loop passes rely on; [feasible] answering [true] promises
   nothing (it is conservative on overflow and on size limits). *)

From Stdlib Require Import ZArith Lia Bool List Recdef.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlInt64 OxCamlList
  IntArith.
Import ListNotations.

(* ======================================================================== *)
(* Part 1: the definitions                                                  *)
(* ======================================================================== *)

Local Open Scope sint63_scope.

(* ---- [module Affine] -------------------------------------------------- *)

Module Affine.
  Record t := { const : int; terms : list (int * int) }.

  (* The form with no terms ([const] being the field). *)
  Definition constant (c : int) : t := {| const := c; terms := [] |}.

  (* The form of one atom. *)
  Definition var (id : int) : t := {| const := zero; terms := [(id, one)] |}.

  (* The coefficient of [id] in [f], zero when absent. *)
  Definition coeff (id : int) (f : t) : int := assoc_default zero id f.(terms).

  Fixpoint terms_equal (l1 l2 : list (int * int)) : bool :=
    match l1, l2 with
    | [], [] => true
    | (i1, c1) :: l1, (i2, c2) :: l2 =>
      (i1 =? i2) && (c1 =? c2) && terms_equal l1 l2
    | _, _ => false
    end.

  Definition equal (a b : t) : bool :=
    (a.(const) =? b.(const)) && terms_equal a.(terms) b.(terms).

  Definition add_const (f : t) (c : int) : t :=
    {| const := f.(const) + c; terms := f.(terms) |}.

  Definition scale (k : int) (f : t) : t :=
    if k =? zero
    then constant zero
    else
      {| const := f.(const) * k;
         terms := map (fun '(id, c) => (id, c * k)) f.(terms) |}.

  (* The checked operations: [None] when a coefficient or the constant would
     overflow. *)
  Definition scale_checked (k : int) (f : t) : option t :=
    if k =? zero
    then Some (constant zero)
    else
      let* c := mul_ovf f.(const) k in
      let* ts :=
        map_opt (fun '(id, c) => let* c := mul_ovf c k in Some (id, c))
          f.(terms)
      in
      Some {| const := c; terms := ts |}.

  Definition add_checked (a b : t) : option t :=
    let ids := sort_uniq (map fst a.(terms) ++ map fst b.(terms)) in
    let* ts :=
      filter_map_opt
        (fun id =>
          let* c := add_ovf (coeff id a) (coeff id b) in
          Some (if c =? zero then None else Some (id, c)))
        ids
    in
    let* c := add_ovf a.(const) b.(const) in
    Some {| const := c; terms := ts |}.

  Definition add_const_checked (f : t) (c : int) : option t :=
    let* k := add_ovf f.(const) c in
    Some {| const := k; terms := f.(terms) |}.

  Definition sub_checked (a b : t) : option t :=
    let* nb := scale_checked minus_one b in
    add_checked a nb.
End Affine.

(* ---- Resolvents ------------------------------------------------------- *)

(* The local [coeff] of [resolvent], over [Int64] coefficients. *)
Definition coeff64 := assoc_default Int64.zero.

(* The local [scale_to_int64] of [resolvent]. *)
Definition scale_to_int64 (factor : Int64.t) (form : Affine.t)
    : option (Int64.t * list (int * Int64.t)) :=
  let* c := mul_ovf64 factor (Int64.of_int form.(Affine.const)) in
  let* ts :=
    map_opt
      (fun '(id, c) =>
        let* c := mul_ovf64 factor (Int64.of_int c) in Some (id, c))
      form.(Affine.terms)
  in
  Some (c, ts).

(* The resolvent of [positive_form], in which the atom has the positive
   coefficient [positive_coeff], and [negative_form], in which it has the
   negative coefficient [- negative_coeff_magnitude]: the combination
   [negative_coeff_magnitude * positive_form + positive_coeff * negative_form],
   computed in [Int64], divided by the gcd of its coefficients and read back as
   [int]s. A resolvent with no terms is normalized to the constant 0 or -1.
   [None] on overflow. *)
Definition resolvent (positive_coeff negative_coeff_magnitude : int)
    (positive_form negative_form : Affine.t) : option Affine.t :=
  let cp64 := Int64.of_int positive_coeff in
  let cn64 := Int64.of_int negative_coeff_magnitude in
  match scale_to_int64 cn64 positive_form,
        scale_to_int64 cp64 negative_form with
  | Some (pc, pterms), Some (nc, nterms) =>
    let ids := sort_uniq (map fst pterms ++ map fst nterms) in
    let* ts :=
      filter_map_opt
        (fun id =>
          let* c := add_ovf64 (coeff64 id pterms) (coeff64 id nterms) in
          Some (if Int64.equal c Int64.zero then None else Some (id, c)))
        ids
    in
    let* c := add_ovf64 pc nc in
    match ts with
    | [] =>
      Some {| Affine.const :=
                if zero <=? Int64.compare c Int64.zero then zero
                else minus_one;
              Affine.terms := [] |}
    | _ :: _ =>
      let normalization_factor :=
        fold_left
          (fun normalization_factor '(_, c) =>
            gcd64 normalization_factor (Int64.abs c))
          Int64.zero ts
      in
      let normalization_factor :=
        if zero <? Int64.compare normalization_factor Int64.one
        then normalization_factor else Int64.one
      in
      let* k := to_int_ovf (floor_div64 c normalization_factor) in
      let* ts :=
        map_opt
          (fun '(id, c) =>
            let* c := to_int_ovf (Int64.div c normalization_factor) in
            Some (id, c))
          ts
      in
      Some {| Affine.const := k; Affine.terms := ts |}
    end
  | _, _ => None
  end.

(* ---- [feasible] and [entails] ----------------------------------------- *)

(* Size limits beyond which [feasible] gives up, answering "feasible", the safe
   side. One elimination step can produce up to a quarter of the squared number
   of inequalities as resolvents, so [max_ineqs] bounds the work of a step; the
   systems the loop analyses build have a few dozen inequalities at most.
   FourierMotzkinExtract.v repeats the two values as literals. *)
Definition max_atoms : int := 64.
Definition max_ineqs : int := 500.

(* One step of [feasible]: eliminate the atom [atom]. [None] when the system is
   over the size limit; an overflowing resolvent is dropped. *)
Definition eliminate_atom (atom : int) (ineqs : list Affine.t)
    : option (list Affine.t) :=
  if zero <? compare_length_with ineqs max_ineqs
  then None
  else
    let cf f := Affine.coeff atom f in
    let pos := filter (fun f => zero <? cf f) ineqs in
    let neg := filter (fun f => cf f <? zero) ineqs in
    let zero_coeff := filter (fun f => cf f =? zero) ineqs in
    let resolvents :=
      concat_map
        (fun p =>
          filter_map
            (fun n => let* cn := neg_ovf (cf n) in resolvent (cf p) cn p n)
            neg)
        pos
    in
    Some (rev_append zero_coeff resolvents).

(* Eliminate the atoms in turn, stopping at the first [None]. *)
Fixpoint eliminate_atoms (atoms : list int) (ineqs : list Affine.t)
    : option (list Affine.t) :=
  match atoms with
  | [] => Some ineqs
  | atom :: atoms => let* ineqs := eliminate_atom atom ineqs in eliminate_atoms
    atoms ineqs
  end.

(* Whether the system may have a solution: eliminate every atom and check that
   no constant form is negative. [true], the safe answer, over the size limits
   or on overflow. *)
Definition feasible (ineqs : list Affine.t) : bool :=
  let atoms :=
    sort_uniq (concat_map (fun f => map fst f.(Affine.terms)) ineqs) in
  if zero <? compare_length_with atoms max_atoms
  then true
  else
    match eliminate_atoms atoms ineqs with
    | Some reduced => forallb (fun f => zero <=? f.(Affine.const)) reduced
    | None => true
    end.

(* Whether [goal >= 0] follows from [facts]: the facts together with [goal <
   0], i.e. [- goal - 1 >= 0], are infeasible. *)
Definition entails (facts : list Affine.t) (goal : Affine.t) : bool :=
  let negated :=
    let* ng := Affine.scale_checked minus_one goal in
    Affine.add_const_checked ng minus_one
  in
  match negated with
  | Some ng => negb (feasible (ng :: facts))
  | None => false
  end.

(* ======================================================================== *)
(* Part 2: what the values mean                                             *)
(* ======================================================================== *)

Local Open Scope Z_scope.

(* An assignment of integers to atoms. Atoms stand for machine values, but
   nothing here depends on their range: the results hold for every integer
   assignment, so a fortiori for the machine ones. *)
Definition valuation := int -> Z.

(* [sum coeff * atom] over a term list, for any coefficient type that reads as
   an integer. *)
Definition denote_terms {C} (coefficient_value : C -> Z) (rho : valuation)
    (terms : list (int * C)) : Z :=
  fold_right (fun '(id, c) acc => coefficient_value c * rho id + acc) 0 terms.

(* The value of an affine form: [const + sum coeff * atom]. *)
Definition denote (rho : valuation) (f : Affine.t) : Z :=
  to_Z f.(Affine.const) + denote_terms to_Z rho f.(Affine.terms).

(* A form asserts that its value is non-negative. *)
Definition holds (rho : valuation) (f : Affine.t) : Prop := 0 <= denote rho f.

Definition holds_all (rho : valuation) (l : list Affine.t) : Prop :=
  Forall (holds rho) l.

(* The representation invariant: each atom appears at most once in a term list.
   Every constructor in [Affine] maintains it, so every form the loop passes
   build satisfies it. *)
Definition wf (f : Affine.t) : Prop := NoDup (map fst f.(Affine.terms)).

(* ======================================================================== *)
(* Part 3: lemmas used by the proofs                                        *)
(* ======================================================================== *)

(* --- The gcd fold of [resolvent] yields a common divisor of all coefficients.
   --- *)

(* The fold's result divides the initial accumulator ... *)
Lemma fold_gcd_divides_acc : ∀ (terms : list (int * Int64.t))
  (initial_gcd : Int64.t),
  (Int64.to_Z (fold_left (fun g '(_, coefficient) => gcd64 g (Int64.abs
    coefficient)) initial_gcd terms)
   | Int64.to_Z initial_gcd).
Proof.
  intros ts. unfold fold_left.
  induction ts as [ | [id c] ts IH ]; intros g0; simpl.
  - apply Z.divide_refl.
  - eapply Z.divide_trans; [ apply IH | apply gcd64_divides ].
Qed.

(* ... and every coefficient in the list. *)
Lemma fold_gcd_divides :
  ∀ (terms : list (int * Int64.t)) (initial_gcd : Int64.t) (id : int)
    (coefficient : Int64.t),
  (id, coefficient) ∈ terms ->
  (Int64.to_Z (fold_left (fun g '(_, coefficient) => gcd64 g (Int64.abs
    coefficient)) initial_gcd terms)
   | Int64.to_Z coefficient).
Proof.
  intros ts. unfold fold_left.
  induction ts as [ | [id' c'] ts IH ]; intros g0 id c Hin; simpl in *.
  - contradiction.
  - destruct Hin as [ E | Hin ].
    + injection E as -> ->.
      pose proof (fold_gcd_divides_acc ts (gcd64 g0 (Int64.abs c))) as Hacc.
      unfold fold_left in Hacc.
      eapply Z.divide_trans; [ exact Hacc | ].
      apply abs64_divide. apply gcd64_divides.
    + eapply IH. exact Hin.
Qed.

(* --- Sums over atoms. --- *)

(* [sum f id] over a list of atoms. *)
Definition sum_ids (f : int -> Z) (ids : list int) : Z :=
  fold_right (fun id acc => f id + acc) 0 ids.

(* Sums unfold on cons, depend only on the summand at each atom, vanish when
   every summand does, and distribute over addition and scaling. *)
Lemma sum_ids_cons : ∀ (f : int -> Z) (id : int) (ids : list int),
  sum_ids f (id :: ids) = f id + sum_ids f ids.
Proof. reflexivity. Qed.

Lemma sum_ids_ext : ∀ (f g : int -> Z) (ids : list int),
  (∀ (id : int), id ∈ ids -> f id = g id) ->
  sum_ids f ids = sum_ids g ids.
Proof.
  intros f g ids H. induction ids as [ | id ids IH ]; [ reflexivity | ].
  rewrite !sum_ids_cons. rewrite H by (left; reflexivity).
  rewrite IH; [ reflexivity | ]. intros. apply H. right. assumption.
Qed.

Lemma sum_ids_zero : ∀ (f : int -> Z) (ids : list int),
  (∀ (id : int), id ∈ ids -> f id = 0) -> sum_ids f ids = 0.
Proof.
  intros f ids H. induction ids as [ | id ids IH ]; [ reflexivity | ].
  rewrite sum_ids_cons, H by (left; reflexivity).
  rewrite IH; [ reflexivity | ]. intros. apply H. right. assumption.
Qed.

Lemma sum_ids_add : ∀ (f g : int -> Z) (ids : list int),
  sum_ids (fun id => f id + g id) ids = sum_ids f ids + sum_ids g ids.
Proof.
  intros f g ids. induction ids as [ | id ids IH ]; [ reflexivity | ].
  rewrite !sum_ids_cons, IH. cbv beta. ring.
Qed.

Lemma sum_ids_scale : ∀ (k : Z) (f : int -> Z) (ids : list int),
  sum_ids (fun id => k * f id) ids = k * sum_ids f ids.
Proof.
  intros k f ids. induction ids as [ | id ids IH ]; [ simpl; ring | ].
  rewrite !sum_ids_cons, IH. cbv beta. ring.
Qed.

(* Splitting off the contribution of one atom [id0] that occurs exactly once in
   [ids]. *)
Lemma sum_ids_split :
  ∀ (f g : int -> Z) (ids : list int) (id0 : int) (v : Z),
  NoDup ids -> id0 ∈ ids ->
  (∀ (id : int), id <> id0 -> f id = g id) -> f id0 = v + g id0 ->
  sum_ids f ids = v + sum_ids g ids.
Proof.
  intros f g ids id0 v Hnd Hin Hfg Hv.
  induction ids as [ | id ids IH ]; [ contradiction | ].
  apply NoDup_cons_iff in Hnd as [Hnotin Hnd].
  rewrite !sum_ids_cons.
  destruct Hin as [ -> | Hin ].
  - rewrite Hv. rewrite (sum_ids_ext f g ids); [ lia | ].
    intros id' Hid'. apply Hfg. intros ->. contradiction.
  - rewrite (Hfg id) by (intros ->; contradiction).
    rewrite (IH Hnd Hin). lia.
Qed.

(* The value of a term list unfolds on cons. *)
Lemma denote_terms_cons :
  ∀ {C : Type} (coefficient_value : C -> Z) (rho : valuation) (id : int)
    (c : C)
    (terms : list (int * C)),
  denote_terms coefficient_value rho ((id, c) :: terms)
  = coefficient_value c * rho id + denote_terms coefficient_value rho terms.
Proof. reflexivity. Qed.

(* The value of a duplicate-free term list is the sum, over any duplicate-free
   list of atoms covering it, of coefficient times atom. *)
Lemma sum_ids_coeff :
  ∀ {C : Type} (coefficient_value : C -> Z) (dflt : C) (rho : valuation)
    (terms : list (int * C)) (ids : list int),
  coefficient_value dflt = 0 -> NoDup (map fst terms) -> NoDup ids ->
  incl (map fst terms) ids ->
  sum_ids (fun id => coefficient_value (assoc_default dflt id terms) * rho id)
    ids
  = denote_terms coefficient_value rho terms.
Proof.
  intros C cZ dflt rho terms.
  induction terms as [ | [k c] terms IH ]; intros ids Hd Hnd Hnd_ids Hincl.
  - apply sum_ids_zero. intros id _. unfold assoc_default. simpl.
    rewrite Hd. ring.
  - apply NoDup_cons_iff in Hnd as [Hk Hnd].
    rewrite denote_terms_cons.
    rewrite <- (IH ids Hd Hnd Hnd_ids)
      by (intros x Hx; apply Hincl; right; exact Hx).
    apply (sum_ids_split _ _ ids k).
    + exact Hnd_ids.
    + apply Hincl. left. reflexivity.
    + intros id Hne. cbv beta. rewrite assoc_default_cons.
      destruct (id =? k)%sint63 eqn:E; [ apply eqb_int in E; contradiction | ].
      reflexivity.
    + cbv beta. rewrite assoc_default_cons, eqb_int_refl.
      rewrite (assoc_default_not_in dflt k terms Hk), Hd. ring.
Qed.

(* A duplicate-free term list all of whose coefficients are zero has value
   zero. *)
Lemma denote_terms_zero : ∀ (rho : valuation) (terms : list (int * int)),
  NoDup (map fst terms) ->
  (∀ (id : int), id ∈ (map fst terms) ->
     to_Z (assoc_default zero id terms) = 0) ->
  denote_terms to_Z rho terms = 0.
Proof.
  intros rho terms Hnd Hz.
  rewrite <- (sum_ids_coeff to_Z zero rho terms (map fst terms));
    [ | reflexivity | exact Hnd | exact Hnd | intros x Hx; exact Hx ].
  apply sum_ids_zero. intros id Hid. rewrite Hz by exact Hid. ring.
Qed.

(* --- Merging term lists. --- *)

(* The shape shared by [Affine.add_checked] and [resolvent]: for each atom a
   checked coefficient, dropped when zero. *)
Definition merge_terms {C} (f : int -> option C) (is_zero : C -> bool)
    (ids : list int) : option (list (int * C)) :=
  filter_map_opt
    (fun id =>
      let* c := f id in
      Some (if is_zero c then None else Some (id, c)))
    ids.

(* [merge_terms] unfolds on cons: [None] if the coefficient fails, else the
   entry is kept unless its coefficient is zero. *)
Lemma merge_terms_cons :
  ∀ {C : Type} (compute_coefficient : int -> option C)
    (is_zero : C -> bool) (id : int)
    (ids : list int),
  merge_terms compute_coefficient is_zero (id :: ids)
  = (let* c := compute_coefficient id in
     let* rest := merge_terms compute_coefficient is_zero ids in
     Some (if is_zero c then rest else (id, c) :: rest)).
Proof.
  intros C f z id ids. unfold merge_terms. simpl.
  destruct (f id) as [ c | ]; [ | reflexivity ].
  destruct (filter_map_opt _ ids); [ | reflexivity ].
  destruct (z c); reflexivity.
Qed.

(* The atoms of a merged list are among those given ... *)
Lemma merge_terms_keys :
  ∀ {C : Type} (compute_coefficient : int -> option C)
    (is_zero : C -> bool) (ids : list int)
    (merged_terms : list (int * C)),
  merge_terms compute_coefficient is_zero ids = Some merged_terms ->
  ∀ (id : int), id ∈ (map fst merged_terms) -> id ∈ ids.
Proof.
  intros C f z ids. induction ids as [ | id ids IH ]; intros ts H id' Hin.
  - injection H as <-. contradiction.
  - rewrite merge_terms_cons in H.
    destruct (f id) as [ c | ]; [ | discriminate ].
    destruct (merge_terms f z ids) as [ rest | ] eqn:Hr; [ | discriminate ].
    injection H as <-. destruct (z c).
    + right. eapply IH; [ reflexivity | exact Hin ].
    + simpl in Hin. destruct Hin as [ -> | Hin ]; [ left; reflexivity | ].
      right. eapply IH; [ reflexivity | exact Hin ].
Qed.

(* ... and distinct when those are. *)
Lemma merge_terms_NoDup :
  ∀ {C : Type} (compute_coefficient : int -> option C)
    (is_zero : C -> bool) (ids : list int)
    (merged_terms : list (int * C)),
  NoDup ids ->
  merge_terms compute_coefficient is_zero ids = Some merged_terms ->
  NoDup (map fst merged_terms).
Proof.
  intros C f z ids. induction ids as [ | id ids IH ]; intros ts Hnd H.
  - injection H as <-. constructor.
  - apply NoDup_cons_iff in Hnd as [Hnot Hnd].
    rewrite merge_terms_cons in H.
    destruct (f id) as [ c | ]; [ | discriminate ].
    destruct (merge_terms f z ids) as [ rest | ] eqn:Hr; [ | discriminate ].
    injection H as <-. destruct (z c).
    + eapply IH; [ exact Hnd | reflexivity ].
    + simpl. constructor; [ | eapply IH; [ exact Hnd | reflexivity  ] ].
      intros Hin. apply Hnot. eapply merge_terms_keys; [ exact Hr | exact Hin
        ].
Qed.

(* For every atom given, the merge computed a coefficient, which the merged
   list holds, unless it is zero, in which case the atom is absent. *)
Lemma merge_terms_coefficient :
  ∀ {C : Type} (compute_coefficient : int -> option C)
    (is_zero : C -> bool) (default_coefficient : C)
    (ids : list int) (merged_terms : list (int * C)),
  NoDup ids ->
  merge_terms compute_coefficient is_zero ids = Some merged_terms ->
  ∀ (id : int), id ∈ ids ->
    ∃ c, compute_coefficient id = Some c
      ∧ assoc_default default_coefficient id merged_terms = (if is_zero c then
        default_coefficient else c)
      ∧ (id ∈ (map fst merged_terms) <-> is_zero c = false).
Proof.
  intros C f z dflt ids. induction ids as [ | id ids IH ];
    intros ts Hnd H id' Hin;
    [ contradiction | ].
  apply NoDup_cons_iff in Hnd as [Hnot Hnd].
  rewrite merge_terms_cons in H.
  destruct (f id) as [ c | ] eqn:Hf; [ | discriminate ].
  destruct (merge_terms f z ids) as [ rest | ] eqn:Hr; [ | discriminate ].
  injection H as <-.
  assert (Hrest : id ∉ (map fst rest))
    by (intros Hin'; apply Hnot; eapply merge_terms_keys; [ exact Hr | exact
      Hin' ]).
  destruct Hin as [ -> | Hin ].
  - exists c. split; [ exact Hf | ]. destruct (z c) eqn:Hz.
    + split; [ apply assoc_default_not_in; exact Hrest | ].
      split; [ intros Hin'; contradiction | discriminate ].
    + split; [ rewrite assoc_default_cons, eqb_int_refl; reflexivity | ].
      split; [ reflexivity | intros _; left; reflexivity ].
  - destruct (IH rest Hnd eq_refl id' Hin) as [c' [Hf' [Hc' Hin']]].
    exists c'. split; [ exact Hf' | ].
    assert (Hne : id' <> id) by (intros ->; contradiction).
    destruct (z c).
    + split; [ exact Hc' | exact Hin' ].
    + split.
      * rewrite assoc_default_cons.
        destruct (id' =? id)%sint63 eqn:E;
          [ apply eqb_int in E; contradiction | exact Hc' ].
      * simpl. split.
        { intros [ E | Hin'' ]; [ congruence | apply Hin'; exact Hin'' ]. }
        { intros Hz. right. apply Hin'. exact Hz. }
Qed.

(* The value of a merged term list is the sum over the atoms of the merged
   coefficients. *)
Lemma merge_terms_denote :
  ∀ {C : Type} (coefficient_value : C -> Z)
    (compute_coefficient : int -> option C) (is_zero : C -> bool)
    (default_coefficient : C) (rho : valuation)
      (merged_coefficient_value : int -> Z) (ids : list int)
    (merged_terms : list (int * C)),
  coefficient_value default_coefficient = 0 -> (∀ (c : C), is_zero c =
    true -> coefficient_value c = 0) -> NoDup ids ->
  (∀ (id : int) (c : C), id ∈ ids -> compute_coefficient id = Some c ->
    coefficient_value c = merged_coefficient_value id) ->
  merge_terms compute_coefficient is_zero ids = Some merged_terms ->
  denote_terms coefficient_value rho merged_terms = sum_ids (fun id =>
    merged_coefficient_value id * rho id) ids.
Proof.
  intros C cZ f z dflt rho fz ids ts Hd Hz Hnd Hf H.
  rewrite <- (sum_ids_coeff cZ dflt rho ts ids);
    [ | exact Hd | eapply merge_terms_NoDup; eauto | exact Hnd
     | intros x Hx; eapply merge_terms_keys; eauto].
  apply sum_ids_ext. intros id Hid.
  destruct (merge_terms_coefficient f z dflt ids ts Hnd H id Hid) as [c [Hc
    [Hcoeff _]]].
  rewrite Hcoeff. rewrite <- (Hf id c Hid Hc).
  destruct (z c) eqn:Hzc;
    [ rewrite Hd, (Hz c Hzc); reflexivity | reflexivity ].
Qed.

(* --- The [Affine] operations. --- *)

Lemma denote_record :
  ∀ (rho : valuation) (c : int) (ts : list (int * int)),
  denote rho {| Affine.const := c; Affine.terms := ts |}
  = to_Z c + denote_terms to_Z rho ts.
Proof. reflexivity. Qed.

(* A constant form has the constant's value, and is well-formed. *)
Lemma constant_denote : ∀ (rho : valuation) (c : int),
  denote rho (Affine.constant c) = to_Z c.
Proof.
  intros. unfold Affine.constant. rewrite denote_record.
  change (denote_terms to_Z rho []) with 0. ring.
Qed.

Lemma constant_wf : ∀ (c : int), wf (Affine.constant c).
Proof. intros. constructor. Qed.

(* The form of an atom has the atom's value, and is well-formed. *)
Lemma var_denote : ∀ (rho : valuation) (id : int),
  denote rho (Affine.var id) = rho id.
Proof.
  intros. unfold Affine.var. rewrite denote_record, denote_terms_cons.
  change (denote_terms to_Z rho []) with 0. rewrite to_Z_zero, to_Z_one. ring.
Qed.

Lemma var_wf : ∀ (id : int), wf (Affine.var id).
Proof. intros. unfold wf. simpl. constructor; [ tauto | constructor ]. Qed.

(* Scaling every coefficient, when none overflows, scales the value. *)
Lemma scale_terms_denote :
  ∀ (rho : valuation) (factor : int)
    (terms scaled_terms : list (int * int)),
  map_opt (fun '(id, c) => let* c := mul_ovf c factor in Some (id, c)) terms
  = Some scaled_terms ->
  denote_terms to_Z rho scaled_terms = to_Z factor * denote_terms to_Z rho
    terms.
Proof.
  intros rho k terms. induction terms as [ | [id c0] terms IH ]; intros ts Hts.
  - injection Hts as <-. change (denote_terms to_Z rho []) with 0. ring.
  - cbn [map_opt] in Hts.
    destruct (mul_ovf c0 k) as [ d | ] eqn:Hd; [ | discriminate ].
    destruct (map_opt _ terms) as [ rest | ] eqn:Hr; [ | discriminate ].
    injection Hts as <-. rewrite !denote_terms_cons, (IH rest eq_refl).
    apply mul_ovf_spec in Hd. rewrite Hd. ring.
Qed.

(* [scale_checked factor form], when it answers, has [factor] times the value
   of [form], no new atoms, and stays well-formed. *)
Lemma scale_checked_spec :
  ∀ (rho : valuation) (factor : int) (form scaled_form : Affine.t),
  Affine.scale_checked factor form = Some scaled_form ->
  denote rho scaled_form = to_Z factor * denote rho form
  ∧ (∀ (id : int), id ∈ (map fst scaled_form.(Affine.terms)) ->
        id ∈ (map fst form.(Affine.terms)))
  ∧ (wf form -> wf scaled_form).
Proof.
  intros rho k f f' H. unfold Affine.scale_checked in H.
  destruct (k =? zero)%sint63 eqn:Hk.
  - apply eqb_int in Hk. subst k. injection H as <-.
    rewrite constant_denote, to_Z_zero.
    split; [ ring | ].
    split; [ intros id Hin; contradiction | intros _; constructor ].
  - destruct (mul_ovf _ k) as [ c | ] eqn:Hc; [ | discriminate ].
    destruct (map_opt _ _) as [ ts | ] eqn:Hts; [ | discriminate ].
    injection H as <-.
    pose proof (map_opt_keys _ _ _ Hts) as Hkeys.
    pose proof (scale_terms_denote rho k _ _ Hts) as Hdt.
    apply mul_ovf_spec in Hc.
    unfold denote, wf. cbn [Affine.const Affine.terms]. rewrite Hc, Hdt, Hkeys.
    split; [ ring | split; tauto ].
Qed.

(* [add_checked], when it answers, has the sum of the values, atoms from either
   side, and is well-formed. *)
Lemma add_checked_spec : ∀ (rho : valuation)
  (left_form right_form sum_form : Affine.t),
  wf left_form -> wf right_form -> Affine.add_checked left_form right_form =
    Some sum_form ->
  denote rho sum_form = denote rho left_form + denote rho right_form
  ∧ (∀ (id : int), id ∈ (map fst sum_form.(Affine.terms)) ->
        id ∈ (map fst left_form.(Affine.terms)) ∨ id ∈ (map fst
          right_form.(Affine.terms)))
  ∧ wf sum_form.
Proof.
  intros rho a b c Ha Hb H. unfold Affine.add_checked in H.
  set (ids := sort_uniq (map fst a.(Affine.terms) ++ map fst b.(Affine.terms)))
    in *.
  assert (Hnd : NoDup ids) by apply sort_uniq_NoDup.
  assert (Hids : forall x, x ∈ ids <->
                   x ∈ (map fst a.(Affine.terms))
                   ∨ x ∈ (map fst b.(Affine.terms)))
    by (intros x; unfold ids; rewrite sort_uniq_In, in_app_iff; reflexivity).
  change (filter_map_opt _ ids)
    with (merge_terms
            (fun id => add_ovf (Affine.coeff id a) (Affine.coeff id b))
            (fun c => (c =? zero)%sint63) ids) in H.
  destruct (merge_terms _ _ ids) as [ ts | ] eqn:Hts; [ | discriminate ].
  destruct (add_ovf _ _) as [ k | ] eqn:Hk; [ | discriminate ].
  injection H as <-. apply add_ovf_spec in Hk.
  assert (Hdt : denote_terms to_Z rho ts
                = denote_terms to_Z rho a.(Affine.terms)
                  + denote_terms to_Z rho b.(Affine.terms)).
  { rewrite (merge_terms_denote to_Z
               (fun id => add_ovf (Affine.coeff id a) (Affine.coeff id b))
               (fun c => (c =? zero)%sint63) zero rho
               (fun id => to_Z (Affine.coeff id a) + to_Z (Affine.coeff id b))
               ids ts).
    - rewrite (sum_ids_ext _
                 (fun id => to_Z (Affine.coeff id a) * rho id
                            + to_Z (Affine.coeff id b) * rho id))
        by (intros; ring).
      rewrite sum_ids_add. unfold Affine.coeff.
      rewrite !(sum_ids_coeff to_Z zero rho)
        by first [ reflexivity | assumption
                 | (intros x Hx; apply Hids; tauto) ].
      reflexivity.
    - reflexivity.
    - intros c0 Hc0. apply eqb_int in Hc0. subst c0. reflexivity.
    - exact Hnd.
    - intros id c0 _ Hc0. apply add_ovf_spec in Hc0. exact Hc0.
    - exact Hts. }
  unfold denote, wf. cbn [Affine.const Affine.terms]. rewrite Hk, Hdt.
  split; [ ring | split ].
  - intros id Hin. apply Hids. eapply merge_terms_keys; [ exact Hts | exact Hin
    ].
  - eapply merge_terms_NoDup; [ exact Hnd | exact Hts ].
Qed.

(* [add_const_checked], when it answers, adds the constant to the value and
   keeps the terms. *)
Lemma add_const_checked_spec :
  ∀ (rho : valuation) (form : Affine.t) (constant : int)
    (result_form : Affine.t),
  Affine.add_const_checked form constant = Some result_form ->
  denote rho result_form = denote rho form + to_Z constant
  ∧ result_form.(Affine.terms) = form.(Affine.terms)
  ∧ (wf form -> wf result_form).
Proof.
  intros rho f k f' H. unfold Affine.add_const_checked in H.
  destruct (add_ovf _ _) as [ c | ] eqn:Hc; [ | discriminate ].
  injection H as <-. apply add_ovf_spec in Hc.
  unfold denote, wf. cbn [Affine.const Affine.terms]. rewrite Hc.
  split; [ ring | split; [ reflexivity | tauto  ] ].
Qed.

(* [sub_checked], when it answers, has the difference of the values, atoms from
   either side, and is well-formed. *)
Lemma sub_checked_spec : ∀ (rho : valuation)
  (left_form right_form difference_form : Affine.t),
  wf left_form -> wf right_form -> Affine.sub_checked left_form right_form =
    Some difference_form ->
  denote rho difference_form = denote rho left_form - denote rho right_form
  ∧ (∀ (id : int), id ∈ (map fst difference_form.(Affine.terms)) ->
        id ∈ (map fst left_form.(Affine.terms)) ∨ id ∈ (map fst
          right_form.(Affine.terms)))
  ∧ wf difference_form.
Proof.
  intros rho a b c Ha Hb H. unfold Affine.sub_checked in H.
  destruct (Affine.scale_checked minus_one b) as [ nb | ] eqn:Hnb;
    [ | discriminate ].
  destruct (scale_checked_spec rho _ _ _ Hnb) as [Hd [Hkeys Hwf]].
  rewrite to_Z_minus_one in Hd.
  destruct (add_checked_spec rho _ _ _ Ha (Hwf Hb) H) as [Hd' [Hkeys' Hwf']].
  split; [ | split ].
  - rewrite Hd', Hd. ring.
  - intros id Hin. destruct (Hkeys' id Hin) as [ Hin' | Hin' ];
      [ left; exact Hin' | right; apply Hkeys; exact Hin' ].
  - exact Hwf'.
Qed.

(* --- Resolvents. --- *)

(* [scale_to_int64 factor form] multiplies the constant and every coefficient
   of [form] by [factor], exactly. *)
Lemma scale_to_int64_spec :
  ∀ (factor : Int64.t) (form : Affine.t) (scaled_constant : Int64.t)
    (scaled_terms : list (int * Int64.t)),
  scale_to_int64 factor form = Some (scaled_constant, scaled_terms) ->
  Int64.to_Z scaled_constant = Int64.to_Z factor * to_Z form.(Affine.const)
  ∧ map fst scaled_terms = map fst form.(Affine.terms)
  ∧ (∀ (id : int), Int64.to_Z (coeff64 id scaled_terms)
                         = Int64.to_Z factor * to_Z (Affine.coeff id form)).
Proof.
  unfold scale_to_int64. intros k f c ts H.
  destruct (mul_ovf64 _ _) as [ c' | ] eqn:Hc; [ | discriminate ].
  destruct (map_opt _ _) as [ ts' | ] eqn:Hts; [ | discriminate ].
  injection H as <- <-.
  split; [ | split ].
  - apply mul_ovf64_spec in Hc. rewrite Hc, Int64.of_int_spec. reflexivity.
  - eapply map_opt_keys. exact Hts.
  - intros id. unfold coeff64, Affine.coeff, assoc_default.
    pose proof (map_opt_assoc _ _ _ Hts id) as Ha. revert Ha.
    destruct (assoc_opt id f.(Affine.terms)) as [ c0 | ]; intro Ha.
    + destruct Ha as [d [Hd ->]]. apply mul_ovf64_spec in Hd.
      rewrite Hd, Int64.of_int_spec. reflexivity.
    + rewrite Ha, Int64.to_Z_zero, to_Z_zero. ring.
Qed.

(* What a resolvent is: a form [resolvent_form] whose coefficients are, up to a
   positive factor [normalization_factor], the combination
   [negative_coeff_magnitude * positive_form + positive_coeff * negative_form],
   and which holds whenever that combination is non-negative. *)
Lemma resolvent_spec : ∀ (positive_coeff negative_coeff_magnitude : int)
    (positive_form negative_form resolvent_form : Affine.t),
  wf positive_form -> wf negative_form ->
  resolvent positive_coeff negative_coeff_magnitude
    positive_form negative_form = Some resolvent_form ->
  wf resolvent_form
  ∧ (∀ (id : int), id ∈ (map fst resolvent_form.(Affine.terms)) ->
        id ∈ (map fst positive_form.(Affine.terms))
        ∨ id ∈ (map fst negative_form.(Affine.terms)))
  ∧ (∃ normalization_factor, 0 < normalization_factor ∧
        ∀ (id : int),
          normalization_factor * to_Z (Affine.coeff id resolvent_form)
          = to_Z negative_coeff_magnitude
              * to_Z (Affine.coeff id positive_form)
            + to_Z positive_coeff * to_Z (Affine.coeff id negative_form))
  ∧ (∀ (rho : valuation),
        0 <= to_Z negative_coeff_magnitude * denote rho positive_form
             + to_Z positive_coeff * denote rho negative_form ->
        0 <= denote rho resolvent_form).
Proof.
  intros cp cn p n r Hp Hn H. unfold resolvent in H.
  destruct (scale_to_int64 (Int64.of_int cn) p) as [ [pc pterms] | ] eqn:Hsp;
    [ | discriminate ].
  destruct (scale_to_int64 (Int64.of_int cp) n) as [ [nc nterms] | ] eqn:Hsn;
    [ | discriminate ].
  destruct (scale_to_int64_spec _ _ _ _ Hsp) as [Hpc [Hpkeys Hpcoeff]].
  destruct (scale_to_int64_spec _ _ _ _ Hsn) as [Hnc [Hnkeys Hncoeff]].
  rewrite Int64.of_int_spec in Hpc, Hnc, Hpcoeff, Hncoeff.
  set (ids := sort_uniq (map fst pterms ++ map fst nterms)) in *.
  assert (Hnd : NoDup ids) by apply sort_uniq_NoDup.
  assert (Hids : forall x, x ∈ ids <->
                   x ∈ (map fst p.(Affine.terms))
                   ∨ x ∈ (map fst n.(Affine.terms))).
  { intros x. unfold ids.
    rewrite sort_uniq_In, in_app_iff, Hpkeys, Hnkeys. reflexivity. }
  change (filter_map_opt _ ids)
    with (merge_terms
            (fun id => add_ovf64 (coeff64 id pterms) (coeff64 id nterms))
            (fun c => Int64.equal c Int64.zero) ids) in H.
  destruct (merge_terms _ _ ids) as [ ts | ] eqn:Hts; [ | discriminate ].
  destruct (add_ovf64 pc nc) as [ c | ] eqn:Hc; [ | discriminate ].
  apply add_ovf64_spec in Hc. rewrite Hpc, Hnc in Hc.
  (* The merged coefficient of each atom. *)
  set (m := fun id => to_Z cn * to_Z (Affine.coeff id p)
                      + to_Z cp * to_Z (Affine.coeff id n)).
  assert (Hm : forall id, id ∈ ids ->
            Int64.to_Z (coeff64 id ts) = m id
            ∧ (id ∈ (map fst ts) <-> m id <> 0)).
  { intros id Hid.
    destruct (merge_terms_coefficient _ _ Int64.zero ids ts Hnd Hts id Hid)
      as [c0 [Hc0 [Hcoeff Hin]]].
    apply add_ovf64_spec in Hc0. rewrite Hpcoeff, Hncoeff in Hc0.
    unfold coeff64. rewrite Hcoeff, Hin.
    destruct (Int64.equal c0 Int64.zero) eqn:Hz.
    - apply Int64.equal_spec in Hz. rewrite Int64.to_Z_zero in Hz.
      split; [ rewrite Int64.to_Z_zero; unfold m; lia | ].
      split; [ discriminate | intros Hne; exfalso; apply Hne; unfold m; lia ].
    - apply Int64.equal_false in Hz. rewrite Int64.to_Z_zero in Hz.
      split; [ unfold m; lia | ].
      split; [ intros _; unfold m; lia | reflexivity ]. }
  assert (Hm0 : forall id, id ∉ ids -> m id = 0 ∧ id ∉ (map fst ts)).
  { intros id Hid. split.
    - unfold m, Affine.coeff. rewrite !assoc_default_not_in;
        [ rewrite to_Z_zero; ring | | ];
        intros Hin; apply Hid; apply Hids; tauto.
    - intros Hin. apply Hid. eapply merge_terms_keys; [ exact Hts | exact Hin
      ]. }
  (* The combination, as a sum over [ids]. *)
  assert (Hsum : forall rho,
            to_Z cn * denote rho p + to_Z cp * denote rho n
            = Int64.to_Z c + sum_ids (fun id => m id * rho id) ids).
  { intros rho. unfold denote. rewrite Hc.
    rewrite (sum_ids_ext _
               (fun id => to_Z cn * (to_Z (Affine.coeff id p) * rho id)
                          + to_Z cp * (to_Z (Affine.coeff id n) * rho id)))
      by (intros; unfold m; ring).
    rewrite sum_ids_add, !sum_ids_scale. unfold Affine.coeff.
    rewrite !(sum_ids_coeff to_Z zero rho)
      by first [ reflexivity | assumption | (intros x Hx; apply Hids; tauto) ].
    ring. }
  destruct ts as [ | t0 ts' ].
  - (* No atom left: a constant inequality, decided by the sign of [c]. *)
    injection H as <-.
    assert (Hm_all : forall id, m id = 0).
    { intros id. destruct (in_dec int_eq_dec id ids) as [ Hin | Hnin ].
      - destruct (Hm id Hin) as [_ Hiff].
        destruct (Z.eq_dec (m id) 0) as [ E | NE ]; [ exact E | ].
        exfalso. apply Hiff. exact NE.
      - apply Hm0. exact Hnin. }
    split; [ | split; [ | split ] ].
    + constructor.
    + intros id Hin. contradiction.
    + exists 1. split; [ lia | ]. intros id.
      specialize (Hm_all id). unfold m in Hm_all. rewrite Hm_all.
      unfold Affine.coeff, assoc_default. cbn [Affine.terms assoc_opt].
      rewrite to_Z_zero. ring.
    + intros rho Hpos. rewrite Hsum in Hpos.
      rewrite sum_ids_zero in Hpos by (intros; rewrite Hm_all; ring).
      assert (Hge : (zero <=? Int64.compare c Int64.zero)%sint63 = true)
        by (apply Int64.compare_ge; rewrite Int64.to_Z_zero; lia).
      unfold denote. rewrite Hge. cbn [Affine.const Affine.terms].
      change (denote_terms to_Z rho []) with 0. rewrite to_Z_zero. lia.
  - (* Normalise by the gcd of the coefficients. *)
    cbv zeta in H.
    set (g0 := fold_left (fun g '(_, c) => gcd64 g (Int64.abs c)) Int64.zero
                 (t0 :: ts')) in H.
    set (g := if (zero <? Int64.compare g0 Int64.one)%sint63 then g0
              else Int64.one) in H.
    assert (Hg : 0 < Int64.to_Z g
                 ∧ forall id c0, (id, c0) ∈ (t0 :: ts') ->
                      (Int64.to_Z g | Int64.to_Z c0)).
    { unfold g. destruct (zero <? Int64.compare g0 Int64.one)%sint63 eqn:Hgt.
      - apply Int64.compare_gt in Hgt. rewrite Int64.to_Z_one in Hgt.
        split; [ lia | ]. intros id c0 Hin. unfold g0.
        eapply fold_gcd_divides. exact Hin.
      - rewrite Int64.to_Z_one. split; [ lia | ]. intros. apply Z.divide_1_l. }
    clearbody g.
    destruct (to_int_ovf (floor_div64 c g)) as [ k | ] eqn:Hk; [ | discriminate
      ].
    destruct (map_opt _ (t0 :: ts')) as [ rts | ] eqn:Hrts; [ | discriminate ].
    injection H as <-.
    apply to_int_ovf_spec in Hk. rewrite floor_div64_spec in Hk by lia.
    pose proof (map_opt_keys _ _ _ Hrts) as Hkeys.
    assert (Hcoeff : forall id,
              Int64.to_Z g
              * to_Z (Affine.coeff id
                        {| Affine.const := k; Affine.terms := rts |})
              = m id).
    { intros id. unfold Affine.coeff, assoc_default. cbn [Affine.terms].
      pose proof (map_opt_assoc _ _ _ Hrts id) as Ha. revert Ha.
      destruct (assoc_opt id (t0 :: ts')) as [ c0 | ] eqn:Ha0; intro Ha.
      - destruct Ha as [d [Hd Hd']]. rewrite Hd'.
        apply to_int_ovf_spec in Hd. rewrite Hd.
        assert (Hdiv : (Int64.to_Z g | Int64.to_Z c0))
          by (apply (proj2 Hg id c0); apply assoc_opt_In; exact Ha0).
        rewrite div_exact64 by first [ lia | exact Hdiv ].
        destruct Hdiv as [q Hq]. rewrite Hq, Z.div_mul by lia.
        assert (Hin : id ∈ ids).
        { eapply merge_terms_keys; [ exact Hts | ].
          apply assoc_opt_In in Ha0. apply (in_map fst) in Ha0. exact Ha0. }
        destruct (Hm id Hin) as [Hm1 _].
        unfold coeff64, assoc_default in Hm1. rewrite Ha0 in Hm1.
        rewrite <- Hm1, Hq. ring.
      - rewrite Ha, to_Z_zero.
        assert (Hnot : id ∉ (map fst (t0 :: ts'))).
        { intros Hin. destruct (assoc_opt_Some id (t0 :: ts') Hin) as [c0 E].
          congruence. }
        destruct (in_dec int_eq_dec id ids) as [ Hin | Hnin ].
        + destruct (Hm id Hin) as [_ Hiff].
          destruct (Z.eq_dec (m id) 0) as [ E | NE ]; [ rewrite E; ring | ].
          exfalso. apply Hnot. apply Hiff. exact NE.
        + destruct (Hm0 id Hnin) as [E _]. rewrite E. ring. }
    split; [ | split; [ | split ] ].
    + unfold wf. cbn [Affine.terms]. rewrite Hkeys.
      eapply merge_terms_NoDup; [ exact Hnd | exact Hts ].
    + intros id Hin. cbn [Affine.terms] in Hin. rewrite Hkeys in Hin.
      apply Hids.
      eapply merge_terms_keys; [ exact Hts | exact Hin ].
    + exists (Int64.to_Z g). split; [ lia | exact Hcoeff ].
    + intros rho Hpos. rewrite Hsum in Hpos.
      assert (Hdt : sum_ids (fun id => m id * rho id) ids
                    = Int64.to_Z g * denote_terms to_Z rho rts).
      { rewrite <- (sum_ids_coeff to_Z zero rho rts ids);
          [ | reflexivity | | exact Hnd | ].
        - rewrite <- sum_ids_scale. apply sum_ids_ext. intros id _.
          rewrite <- (Hcoeff id). unfold Affine.coeff. cbn [Affine.terms].
          ring.
        - rewrite Hkeys. eapply merge_terms_NoDup; [ exact Hnd | exact Hts ].
        - intros x Hx. rewrite Hkeys in Hx.
          eapply merge_terms_keys; [ exact Hts | exact Hx ]. }
      unfold denote. cbn [Affine.const Affine.terms]. rewrite Hk.
      rewrite Hdt in Hpos.
      assert (Hlow : - denote_terms to_Z rho rts
                     <= Int64.to_Z c / Int64.to_Z g).
      { apply Z.div_le_lower_bound; [ lia | ].
        replace (Int64.to_Z g * - denote_terms to_Z rho rts)
          with (- (Int64.to_Z g * denote_terms to_Z rho rts)) by ring.
        lia. }
      lia.
Qed.

(* ======================================================================== *)
(* Part 4: soundness of [feasible] and [entails]                            *)
(* ======================================================================== *)

(* --- Every constructor of [Affine] builds a well-formed form. --- *)

Lemma add_const_wf : ∀ (f : Affine.t) (c : int),
  wf f -> wf (Affine.add_const f c).
Proof. intros f c H. exact H. Qed.

Lemma scale_wf : ∀ (k : int) (f : Affine.t),
  wf f -> wf (Affine.scale k f).
Proof.
  intros k f H. unfold wf, Affine.scale.
  destruct (k =? zero)%sint63; [ constructor | ].
  cbn [Affine.terms]. rewrite map_map.
  erewrite map_ext; [ exact H | ]. intros [id c]. reflexivity.
Qed.

(* --- One elimination step. --- *)

(* What an elimination establishes of its output [output_inequalities] from its
   input [input_inequalities], [eliminated] holding of the atoms it removed:
   the forms are well-formed and their atoms among the input's; every
   eliminated atom, and every atom whose coefficient was zero throughout the
   input, has a zero coefficient in every output; and truth is preserved: a
   valuation satisfying the input satisfies the output. *)
Definition elimination_result (eliminated : int -> Prop)
  (input_inequalities output_inequalities : list Affine.t)
    : Prop :=
  Forall wf output_inequalities
  ∧ (∀ (covering_atoms : list int),
        (∀ (f : Affine.t),
           f ∈ input_inequalities -> incl (map fst f.(Affine.terms))
             covering_atoms) ->
        ∀ (f : Affine.t), f ∈ output_inequalities -> incl (map fst
          f.(Affine.terms)) covering_atoms)
  ∧ (∀ (id : int),
        eliminated id
        ∨ (∀ (f : Affine.t),
              f ∈ input_inequalities -> to_Z (Affine.coeff id f) = 0) ->
        ∀ (f : Affine.t), f ∈ output_inequalities -> to_Z (Affine.coeff
          id f) = 0)
  ∧ (∀ (rho : valuation), holds_all rho input_inequalities -> holds_all
    rho output_inequalities).

(* [eliminate_atom atom], when it answers, is an elimination step in the sense
   of [elimination_result], the atom eliminated being [atom]. *)
Lemma eliminate_atom_spec : ∀ (atom : int)
  (input_inequalities output_inequalities : list Affine.t),
  Forall wf input_inequalities -> eliminate_atom atom input_inequalities = Some
    output_inequalities ->
  elimination_result (fun id => id = atom) input_inequalities
    output_inequalities.
Proof.
  intros v ineqs out Hwf H. unfold eliminate_atom in H.
  destruct (zero <? compare_length_with ineqs max_ineqs)%sint63;
    [ discriminate | ].
  injection H as <-. cbv beta zeta.
  rewrite Forall_forall in Hwf.
  set (pos := filter (fun f => (zero <? Affine.coeff v f)%sint63) ineqs).
  set (neg := filter (fun f => (Affine.coeff v f <? zero)%sint63) ineqs).
  set (zc := filter (fun f => (Affine.coeff v f =? zero)%sint63) ineqs).
  set (res := concat_map _ pos).
  (* Each output is an input with a zero coefficient of [v], or the resolvent
     of an input pair with coefficients of opposite signs. *)
  assert (Hcase : forall f, f ∈ (rev_append zc res) ->
            (f ∈ ineqs ∧ Affine.coeff v f = zero)
            ∨ (exists p n cn, p ∈ ineqs ∧ n ∈ ineqs
                  ∧ 0 < to_Z (Affine.coeff v p)
                  ∧ to_Z (Affine.coeff v n) < 0
                  ∧ to_Z cn = - to_Z (Affine.coeff v n)
                  ∧ resolvent (Affine.coeff v p) cn p n = Some f)).
  { intros f Hin. apply in_rev_append in Hin as [ Hin | Hin ].
    - left. apply filter_In in Hin as [Hin Hz]. apply eqb_int in Hz. auto.
    - right. unfold res, concat_map in Hin.
      apply in_flat_map in Hin as [p [Hp Hin]].
      apply filter_In in Hp as [Hp Hpos]. apply ltb_int in Hpos.
      apply filter_map_In in Hin as [n [Hn Hres]]. cbv beta in Hres.
      apply filter_In in Hn as [Hn Hneg]. apply ltb_int in Hneg.
      rewrite to_Z_zero in Hpos, Hneg.
      destruct (neg_ovf (Affine.coeff v n)) as [ cn | ] eqn:Hcn;
        [ | discriminate ].
      cbv iota in Hres. apply neg_ovf_spec in Hcn.
      exists p, n, cn. tauto. }
  unfold elimination_result. split; [ | split; [ | split ] ].
  - apply Forall_forall. intros f Hin.
    destruct (Hcase f Hin)
      as [ [Hin' _] | [p [n [cn [Hp [Hn [_ [_ [_ Hres]]]]]]]] ].
    + apply Hwf. exact Hin'.
    + exact (proj1 (resolvent_spec _ _ _ _ _ (Hwf p Hp) (Hwf n Hn) Hres)).
  - intros S HS f Hin id Hid.
    destruct (Hcase f Hin)
      as [ [Hin' _] | [p [n [cn [Hp [Hn [_ [_ [_ Hres]]]]]]]] ].
    + apply (HS f Hin' id Hid).
    + destruct (resolvent_spec _ _ _ _ _ (Hwf p Hp) (Hwf n Hn) Hres)
        as [_ [Hkeys _]].
      destruct (Hkeys id Hid) as [ Hid' | Hid' ].
      * apply (HS p Hp id Hid').
      * apply (HS n Hn id Hid').
  - intros id [ -> | Hz ] f Hin.
    + destruct (Hcase f Hin)
        as [ [_ Hz] | [p [n [cn [Hp [Hn [Hpos [Hneg [Hcn Hres]]]]]]]] ].
      * rewrite Hz. apply to_Z_zero.
      * destruct (resolvent_spec _ _ _ _ _ (Hwf p Hp) (Hwf n Hn) Hres)
          as [_ [_ [[g [Hg Hcoeff]] _]]].
        specialize (Hcoeff v). rewrite Hcn in Hcoeff.
        assert (E : g * to_Z (Affine.coeff v f) = 0) by (rewrite Hcoeff; ring).
        apply Z.mul_eq_0 in E as [ E | E ]; lia.
    + destruct (Hcase f Hin)
        as [ [Hin' _] | [p [n [cn [Hp [Hn [_ [_ [_ Hres]]]]]]]] ].
      * apply Hz. exact Hin'.
      * destruct (resolvent_spec _ _ _ _ _ (Hwf p Hp) (Hwf n Hn) Hres)
          as [_ [_ [[g [Hg Hcoeff]] _]]].
        specialize (Hcoeff id). rewrite (Hz p Hp), (Hz n Hn) in Hcoeff.
        assert (E : g * to_Z (Affine.coeff id f) = 0) by lia.
        apply Z.mul_eq_0 in E as [ E | E ]; lia.
  - intros rho Hholds. unfold holds_all in *. rewrite Forall_forall in *.
    intros f Hin.
    destruct (Hcase f Hin)
      as [ [Hin' _] | [p [n [cn [Hp [Hn [Hpos [Hneg [Hcn Hres]]]]]]]] ].
    + apply Hholds. exact Hin'.
    + destruct (resolvent_spec _ _ _ _ _ (Hwf p Hp) (Hwf n Hn) Hres)
        as [_ [_ [_ Hsound]]].
      apply Hsound. rewrite Hcn.
      pose proof (Hholds p Hp). pose proof (Hholds n Hn). unfold holds in *.
      apply Z.add_nonneg_nonneg; apply Z.mul_nonneg_nonneg; lia.
Qed.

(* --- Eliminating every atom in turn. --- *)

(* The same, for a sequence of eliminations: every atom of the sequence is
   eliminated. *)
Lemma eliminate_atoms_spec : ∀ (atoms : list int)
  (input_inequalities output_inequalities : list Affine.t),
  Forall wf input_inequalities -> eliminate_atoms atoms input_inequalities =
    Some output_inequalities ->
  elimination_result (fun id => id ∈ atoms) input_inequalities
    output_inequalities.
Proof.
  induction atoms as [ | v atoms IH ]; intros ineqs out Hwf H;
    unfold elimination_result.
  - injection H as <-. split; [ exact Hwf | split; [ | split ] ].
    + intros S HS f Hin. apply HS. exact Hin.
    + intros id [ Hid | Hz ] f Hin; [ contradiction | apply Hz; exact Hin ].
    + intros rho Hh. exact Hh.
  - cbn [eliminate_atoms] in H.
    destruct (eliminate_atom v ineqs) as [ mid | ] eqn:He; [ | discriminate ].
    destruct (eliminate_atom_spec v ineqs mid Hwf He) as [Hwf1 [Hkeys1 [Hz1
      Hh1]]].
    destruct (IH mid out Hwf1 H) as [Hwf2 [Hkeys2 [Hz2 Hh2]]].
    split; [ exact Hwf2 | split; [ | split ] ].
    + intros S HS. apply Hkeys2. apply Hkeys1. exact HS.
    + intros id Hid f Hin. apply (Hz2 id); [ | exact Hin ].
      destruct Hid as [ [ -> | Hid ] | Hz ].
      * right. apply Hz1. left. reflexivity.
      * left. exact Hid.
      * right. apply Hz1. right. exact Hz.
    + intros rho Hh. apply Hh2. apply Hh1. exact Hh.
Qed.

(* --- The theorems. --- *)

(* [feasible] never wrongly reports an infeasible system: if it answers
   [false], no integer valuation of the atoms satisfies every inequality. (It
   may answer [true] for an infeasible system: on the size limits, on an
   overflow, or because Fourier-Motzkin elimination is only complete over the
   rationals.) *)
Theorem feasible_sound : ∀ (ineqs : list Affine.t),
  Forall wf ineqs -> feasible ineqs = false ->
  ∀ (rho : valuation), ~ holds_all rho ineqs.
Proof.
  intros ineqs Hwf Hf rho Hholds. unfold feasible in Hf. cbv zeta in Hf.
  set (atoms := sort_uniq _) in Hf.
  destruct (zero <? compare_length_with atoms max_atoms)%sint63;
    [ discriminate | ].
  destruct (eliminate_atoms atoms ineqs) as [ reduced | ] eqn:He; [ |
    discriminate ].
  cbv iota in Hf.
  destruct (eliminate_atoms_spec atoms ineqs reduced Hwf He)
    as [Hwf' [Hkeys [Hzero Hpres]]].
  specialize (Hpres rho Hholds). unfold holds_all in Hpres.
  rewrite Forall_forall in Hwf', Hpres.
  rewrite <- not_true_iff_false in Hf. apply Hf.
  apply forallb_forall. intros f Hin.
  assert (Hatoms : incl (map fst f.(Affine.terms)) atoms).
  { apply (Hkeys atoms); [ | exact Hin ]. intros f0 Hf0 x Hx.
    unfold atoms. apply sort_uniq_In. unfold concat_map. apply in_flat_map.
    exists f0. split; [ exact Hf0 | exact Hx ]. }
  assert (Hd : denote_terms to_Z rho f.(Affine.terms) = 0).
  { apply denote_terms_zero; [ apply Hwf'; exact Hin | ].
    intros id Hid. exact (Hzero id (or_introl (Hatoms id Hid)) f Hin). }
  specialize (Hpres f Hin). unfold holds, denote in Hpres. rewrite Hd in Hpres.
  apply leb_int. rewrite to_Z_zero. lia.
Qed.

(* [entails] never wrongly reports an entailment: if it answers [true], every
   integer valuation satisfying the facts satisfies the goal. *)
Theorem entails_sound : ∀ (facts : list Affine.t) (goal : Affine.t),
  Forall wf facts -> wf goal -> entails facts goal = true ->
  ∀ (rho : valuation), holds_all rho facts -> 0 <= denote rho goal.
Proof.
  intros facts goal Hwf Hwfg He rho Hfacts.
  unfold entails in He. cbv zeta in He.
  destruct (Affine.scale_checked minus_one goal) as [ ng | ] eqn:Hng;
    [ | discriminate ].
  cbv iota in He.
  destruct (Affine.add_const_checked ng minus_one) as [ ng' | ] eqn:Hng';
    [ | discriminate ].
  cbv iota in He. apply negb_true_iff in He.
  destruct (scale_checked_spec rho _ _ _ Hng) as [Hd1 [_ Hwf1]].
  destruct (add_const_checked_spec rho _ _ _ Hng') as [Hd2 [_ Hwf2]].
  rewrite to_Z_minus_one in Hd1, Hd2.
  assert (Hwf' : Forall wf (ng' :: facts))
    by (constructor; [ auto | exact Hwf ]).
  pose proof (feasible_sound (ng' :: facts) Hwf' He rho) as Hinf.
  destruct (Z_le_gt_dec 0 (denote rho goal)) as [ Hle | Hgt ]; [ exact Hle | ].
  exfalso. apply Hinf. constructor; [ | exact Hfacts ].
  unfold holds. rewrite Hd2, Hd1. lia.
Qed.

