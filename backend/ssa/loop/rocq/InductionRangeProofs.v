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

(* What the loop-counter certificates of InductionRange.v mean, and the proofs
   that [find_certificates] only returns certificates that hold. Nothing here
   needs review; Rocq checks the proofs.

   The results are [find_certificates_spec]: every certificate returned
   satisfies [certificate_spec]; [argument_at_most_sound]: when the check
   passed, the value passed is at most the bound under any reading of the
   values on which the branches dominating the predecessor went as taken;
   [no_wrap_at_sound]: when the check passed, the counter plus the step is at
   most [max_int] under any such reading on which the counter is within its
   bounds; and [range_facts_sound]: the facts [range_facts] builds for a block
   hold under every valuation that agrees with the context, when the counters
   of the certificates whose loop contains the block are within their bounds.
   The operational meaning of a certificate, that the counter register is
   within the bounds in every state of the loop body an execution reaches, is
   InductionRangeExecutionProofs.counter_bounded. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlIntProofs
  OxCamlInt64 OxCamlInt64Proofs OxCamlList OxCamlListProofs IntArith
  IntArithProofs.
From OxCamlBackend Require Import IntegerComparison IntegerComparisonProofs
  Operation.
From OxCamlSsa Require Import SsaModel SsaGraphSpec SsaGraph SsaGraphProofs
  SsaDominanceSpec SsaDominance SsaDominanceProofs.
From OxCamlLoop Require Import FourierMotzkin FourierMotzkinProofs AffineSsa
  AffineSsaProofs NaturalLoop NaturalLoopProofs InductionVar
  InductionVarProofs InductionRange.
Import ListNotations.

(* ======================================================================== *)
(* The intended meaning                                                     *)
(* ======================================================================== *)

Local Open Scope Z_scope.

(* [pred] passes, in its [Goto] to [header], a literal at least [lower] for the
   parameter at [index]. *)
Definition passes_literal_at_least (header : block) (index lower : int)
    (pred : block) : Prop :=
  ∃ (args : list Terminator.arg) (id : instr_id) (n : nativeint)
    (operands : list value) (result_index : int),
    goto_args header pred = Some args
    ∧ arg_value args index
       = Some (Res id (Const_int n) operands result_index)
    ∧ to_Z lower <= Int64.to_Z (Int64.of_nativeint n).

(* [v] is defined in a block that dominates [header] without being it: what
   [defined_above] checks, read back. *)
Definition defined_above_loop (op_def : op_def) (header : block) (v : value)
    : Prop :=
  match v with
  | Res id _ _ _ =>
    ∃ (d : block),
      op_def_find op_def id = Some d ∧ d ▷ header ∧ d <> header
  | Block_param d _ => d ▷ header ∧ d <> header
  end.

(* [u] is an operand of a comparison some block of [blocks] branches on, so it
   has a definition in the graph. *)
Definition switch_operand (blocks : list block) (u : value) : Prop :=
  ∃ (g : block) (id : instr_id) (cmp : integer_comparison) (x y : value)
    (result_index : int) (targets : list block),
    g ∈ blocks
    ∧ terminator g
       = Terminator.Switch (Res id (Intop (Icomp cmp)) [x; y] result_index)
           targets
    ∧ (u = x ∨ u = y).

(* What holds of a certificate: its counter is a basic induction variable
   (InductionVar.v) of a loop NaturalLoop.v finds, with a positive literal
   step; the loop's header is not the entry block; every predecessor of the
   header that is not a back-edge source passes a literal at least [lower] for
   the counter; the upper bound, when there is one, is defined above the loop,
   is an operand of a comparison branched on in the graph, and every
   predecessor of the header passes a value [argument_at_most] accepts for
   it; and at every back-edge source [no_wrap_at] passed. *)
Definition certificate_spec (op_def : op_def) (entry : block)
    (blocks : list block) (cert : range_certificate) : Prop :=
  let loop := cert.(biv).(InductionVar.loop) in
  loop ∈ (find_loops blocks)
  ∧ biv_spec op_def loop cert.(biv)
  ∧ ∃ (step : int),
      signed_step cert.(biv) = Some step ∧ 0 < to_Z step
      ∧ loop.(header) <> entry
      ∧ (∀ (pred : block), pred ∈ (predecessors loop.(header)) ->
           pred ∉ loop.(back_edge_sources) ->
           passes_literal_at_least loop.(header) cert.(biv).(param_index)
             cert.(lower) pred)
      ∧ (∀ (u : value), cert.(upper) = Some u ->
           defined_above_loop op_def loop.(header) u
           ∧ switch_operand blocks u
           ∧ ∀ (pred : block), pred ∈ (predecessors loop.(header)) ->
               argument_at_most u loop.(header) cert.(biv).(param_index)
                 pred = true)
      ∧ (∀ (source : block), source ∈ loop.(back_edge_sources) ->
           no_wrap_at step cert.(lower) cert.(upper) loop.(header) source
             cert.(biv).(param_index) = true).

(* The atoms [range_facts] may allocate: for each certificate, one for the
   counter and a linearization of the upper bound. *)
Definition range_atom_bound (certificates : list range_certificate) : Z :=
  Z.of_nat (length certificates) * (1 + linearize_atom_bound).

(* The counter of [cert] is at least its [lower] and at most its [upper], as
   [value_words] reads the values. *)
Definition certificate_counter_in_range (value_words : value -> Z)
    (cert : range_certificate) : Prop :=
  to_Z cert.(lower)
    <= value_words (param cert.(biv).(InductionVar.loop).(header)
                      cert.(biv).(param_index))
  ∧ ∀ (u : value), cert.(upper) = Some u ->
      value_words (param cert.(biv).(InductionVar.loop).(header)
                     cert.(biv).(param_index))
      <= value_words u.

(* The counter of every certificate whose loop contains [target] is within
   its bounds, as [value_words] reads the values. *)
Definition counters_in_range (value_words : value -> Z)
    (certificates : list range_certificate) (target : block) : Prop :=
  ∀ (cert : range_certificate), cert ∈ certificates ->
    set_mem target cert.(biv).(InductionVar.loop).(body) = true ->
    certificate_counter_in_range value_words cert.

(* ======================================================================== *)
(* Lemmas                                                                   *)
(* ======================================================================== *)

(* What [literal_argument] answers: the argument passed is a literal of that
   value. *)
Lemma literal_argument_spec : ∀ (header : block) (index : int) (pred : block)
    (k : int),
  literal_argument header index pred = Some k ->
  ∃ (args : list Terminator.arg) (id : instr_id) (n : nativeint)
    (operands : list value) (result_index : int),
    goto_args header pred = Some args
    ∧ arg_value args index
       = Some (Res id (Const_int n) operands result_index)
    ∧ to_Z k = Int64.to_Z (Int64.of_nativeint n).
Proof.
  intros header index pred k H. unfold literal_argument in H.
  destruct (goto_args header pred) as [args | ] eqn:Hgoto; [ | discriminate ].
  cbn in H. destruct (arg_value args index) as [argument | ] eqn:Harg;
    [ | discriminate ].
  cbn in H. destruct argument as [id op operands result_index | ];
    [ | discriminate ].
  destruct op; try discriminate. cbv zeta in H.
  destruct (int64_is_int (Int64.of_nativeint n)) eqn:Hfits; [ | discriminate ].
  injection H as <-. exists args, id, n, operands, result_index.
  split; [ reflexivity | split; [ exact Harg | ] ].
  apply int64_is_int_spec. exact Hfits.
Qed.

(* The least of a list of literals is at most each of them. *)
Lemma fold_least_le : ∀ (literals : list int) (first : int) (k : int),
  k ∈ (first :: literals) ->
  to_Z (OxCamlList.fold_left
    (fun least k => if (k <? least)%sint63 then k else least) first literals)
  <= to_Z k.
Proof.
  intros literals. induction literals as [ | l literals IH ];
    intros first k Hk; unfold OxCamlList.fold_left in *; cbn.
  - destruct Hk as [<- | []]. lia.
  - assert (Hstep : ∀ (least : int),
      to_Z (if (l <? least)%sint63 then l else least) <= to_Z least
      ∧ to_Z (if (l <? least)%sint63 then l else least) <= to_Z l).
    { intros least. destruct (l <? least)%sint63 eqn:Hlt.
      - apply ltb_int in Hlt. lia.
      - split; [ lia | ]. destruct (Z_le_gt_dec (to_Z least) (to_Z l));
          [ lia | ]. exfalso. assert (Hcontra : (l <? least)%sint63 = true)
          by (apply ltb_int; lia). congruence. }
    pose proof (IH (if (l <? first)%sint63 then l else first)
      (if (l <? first)%sint63 then l else first) (or_introl eq_refl)) as Hle.
    destruct Hk as [Hk | [Hk | Hk]].
    + subst k. pose proof (proj1 (Hstep first)). lia.
    + subst k. pose proof (proj2 (Hstep first)). lia.
    + exact (IH _ k (or_intror Hk)).
Qed.

(* Every element [map_opt] succeeded on has its image in the result. *)
Lemma map_opt_image : ∀ {A B : Type} (f : A -> option B) (l : list A)
    (images : list B) (x : A),
  map_opt f l = Some images -> x ∈ l ->
  ∃ (y : B), f x = Some y ∧ y ∈ images.
Proof.
  intros A B f l. induction l as [ | a l IH ]; intros images x H Hx;
    [ contradiction | ].
  cbn in H. destruct (f a) as [b | ] eqn:Hb; [ | discriminate ].
  destruct (map_opt f l) as [rest | ] eqn:Hrest; [ | discriminate ].
  injection H as <-. destruct Hx as [<- | Hx].
  - exists b. split; [ exact Hb | left; reflexivity ].
  - destruct (IH rest x eq_refl Hx) as [y [Hy Hin]].
    exists y. split; [ exact Hy | right; exact Hin ].
Qed.

(* What [least_literal_argument] answers: every entry passes a literal at least
   it. *)
Lemma least_literal_argument_spec : ∀ (loop : natural_loop) (index : int)
    (lower : int),
  least_literal_argument loop index = Some lower ->
  ∀ (pred : block), pred ∈ (predecessors loop.(header)) ->
    pred ∉ loop.(back_edge_sources) ->
    passes_literal_at_least loop.(header) index lower pred.
Proof.
  intros loop index lower H pred Hpred Hnot.
  unfold least_literal_argument in H.
  destruct (map_opt _ (entry_predecessors loop)) as [literals | ] eqn:Hmap;
    [ | discriminate ].
  cbn in H. destruct literals as [ | first rest ]; [ discriminate | ].
  injection H as <-.
  assert (Hentry : pred ∈ (entry_predecessors loop)).
  { unfold entry_predecessors. cbv zeta. apply filter_In.
    split; [ exact Hpred | ]. apply negb_true_iff.
    destruct (set_mem pred (back_edge_set loop)) eqn:Hmem; [ | reflexivity ].
    exfalso. apply Hnot. apply back_edge_set_spec. exact Hmem. }
  destruct (map_opt_image _ _ _ _ Hmap Hentry) as [k [Hk Hin]].
  destruct (literal_argument_spec _ _ _ _ Hk)
    as [args [id [n [operands [result_index [Hgoto [Harg Hvalue]]]]]]].
  exists args, id, n, operands, result_index.
  split; [ exact Hgoto | split; [ exact Harg | ] ].
  rewrite <- Hvalue. apply fold_least_le. exact Hin.
Qed.

(* What [defined_above] checks, read back. *)
Lemma defined_above_spec : ∀ (op_def : op_def) (header : block) (v : value),
  defined_above op_def header v = true -> defined_above_loop op_def header v.
Proof.
  intros op_def header v H.
  assert (Hstrict : ∀ (d : block), strictly_dominates d header = true ->
    d ▷ header ∧ d <> header).
  { intros d Hs. unfold strictly_dominates in Hs.
    apply andb_true_iff in Hs. destruct Hs as [Hd Hne].
    split; [ exact Hd | ]. intros ->.
    rewrite block_eqb_refl in Hne. discriminate. }
  unfold defined_above in H. unfold defined_above_loop.
  destruct v as [id op args i | d i].
  - destruct (op_def_find op_def id) as [d | ]; [ | discriminate ].
    exists d. split; [ reflexivity | apply Hstrict; exact H ].
  - apply Hstrict. exact H.
Qed.

(* Every value [compared_with] lists is an operand of a comparison branched
   on in the graph. *)
Lemma compared_with_spec : ∀ (blocks : list block)
    (is_counter : value -> bool) (u : value),
  u ∈ (compared_with blocks is_counter) -> switch_operand blocks u.
Proof.
  intros blocks is_counter u H.
  unfold compared_with, concat_map in H. apply in_flat_map in H.
  destruct H as [g [Hg H]]. cbv beta in H.
  destruct (terminator g) as [ | cond targets | | ] eqn:Hterm;
    try contradiction.
  destruct cond as [id op args result_index | ]; try contradiction.
  destruct op; try contradiction. destruct op; try contradiction.
  destruct args as [ | x [ | y [ | ? ?]]]; try contradiction.
  destruct (is_counter x).
  - destruct H as [<- | []]. exists g, id. eexists.
    exists x, y, result_index, targets.
    split; [ exact Hg | split; [ exact Hterm | right; reflexivity ] ].
  - destruct (is_counter y); [ | contradiction ].
    destruct H as [<- | []]. exists g, id. eexists.
    exists x, y, result_index, targets.
    split; [ exact Hg | split; [ exact Hterm | left; reflexivity ] ].
Qed.

(* Every candidate is an operand of a comparison branched on in the graph. *)
Lemma upper_candidates_spec : ∀ (blocks : list block) (header : block)
    (index : int) (updates : list value) (u : value),
  u ∈ (upper_candidates blocks header index updates) ->
  switch_operand blocks u.
Proof.
  intros blocks header index updates u H.
  unfold upper_candidates in H. apply in_app_or in H.
  destruct H as [H | H]; eapply compared_with_spec; exact H.
Qed.

(* What [verify_upper] checks, read back. *)
Lemma verify_upper_spec : ∀ (op_def : op_def) (loop : natural_loop)
    (index : int) (u : value),
  verify_upper op_def loop index u = true ->
  defined_above_loop op_def loop.(header) u
  ∧ ∀ (pred : block), pred ∈ (predecessors loop.(header)) ->
      argument_at_most u loop.(header) index pred = true.
Proof.
  intros op_def loop index u H. unfold verify_upper in H.
  apply andb_true_iff in H. destruct H as [Habove Hpreds].
  split; [ apply defined_above_spec; exact Habove | ].
  intros pred Hpred. rewrite forallb_forall in Hpreds. apply Hpreds, Hpred.
Qed.

(* What [find_upper] answers: a verified candidate. *)
Lemma find_upper_spec : ∀ (op_def : op_def) (blocks : list block)
    (biv : InductionVar.biv) (u : value),
  find_upper op_def blocks biv = Some u ->
  defined_above_loop op_def biv.(InductionVar.loop).(header) u
  ∧ switch_operand blocks u
  ∧ ∀ (pred : block),
      pred ∈ (predecessors biv.(InductionVar.loop).(header)) ->
      argument_at_most u biv.(InductionVar.loop).(header) biv.(param_index)
        pred = true.
Proof.
  intros op_def blocks biv u H. unfold find_upper in H. cbv zeta in H.
  apply find_some in H. destruct H as [Hin Hverify].
  destruct (verify_upper_spec _ _ _ _ Hverify) as [Habove Hpreds].
  split; [ exact Habove | ].
  split; [ eapply upper_candidates_spec; exact Hin | exact Hpreds ].
Qed.

(* What [verify_certificate] checks, read back. *)
Lemma verify_certificate_spec : ∀ (entry : block) (biv : InductionVar.biv)
    (lower : int) (upper : option value),
  verify_certificate entry biv lower upper = true ->
  ∃ (step : int),
    signed_step biv = Some step ∧ 0 < to_Z step
    ∧ biv.(InductionVar.loop).(header) <> entry
    ∧ ∀ (source : block),
        source ∈ biv.(InductionVar.loop).(back_edge_sources) ->
        no_wrap_at step lower upper biv.(InductionVar.loop).(header) source
          biv.(param_index) = true.
Proof.
  intros entry biv lower upper H. unfold verify_certificate in H.
  cbv zeta in H.
  destruct (signed_step biv) as [step | ]; [ | discriminate ].
  apply andb_true_iff in H. destruct H as [H Hsources].
  apply andb_true_iff in H. destruct H as [Hpositive Hentry].
  exists step. split; [ reflexivity | ].
  split; [ rewrite zero_ltb_to_Z in Hpositive; apply Z.ltb_lt; exact Hpositive
    | ].
  split.
  - intros Heq. apply negb_true_iff in Hentry.
    rewrite <- block_eqb_spec in Heq. congruence.
  - intros source Hsource. rewrite forallb_forall in Hsources.
    apply Hsources. exact Hsource.
Qed.

(* The lower-bound fact about a form holds when the form's value is at least
   [lower]. *)
Lemma lower_facts_sound : ∀ (lower : int) (counter : Affine.t),
  wf counter ->
  Forall wf (lower_facts lower counter)
  ∧ ∀ (rho : valuation) (counter_word : Z),
      denote rho counter = counter_word -> to_Z lower <= counter_word ->
      holds_all rho (lower_facts lower counter).
Proof.
  intros lower counter Hwfc. unfold lower_facts.
  destruct (Affine.sub_checked counter (Affine.constant lower))
    as [fact | ] eqn:Hfact.
  - destruct (sub_checked_spec (fun _ => 0) _ _ _ Hwfc (constant_wf lower)
      Hfact) as [_ [_ Hwff]].
    split; [ constructor; [ exact Hwff | constructor ] | ].
    intros rho counter_word Hden Hlow. constructor; [ | constructor ].
    destruct (sub_checked_spec rho _ _ _ Hwfc (constant_wf lower) Hfact)
      as [Hd _].
    unfold holds. rewrite Hd, constant_denote, Hden. lia.
  - split; [ constructor | ]. intros. constructor.
Qed.

(* ======================================================================== *)
(* Theorems                                                                 *)
(* ======================================================================== *)

(* Every certificate [find_certificates] returns satisfies
   [certificate_spec]. *)
Theorem find_certificates_spec : ∀ (op_def : op_def) (entry : block)
    (blocks : list block) (cert : range_certificate),
  cert ∈ (find_certificates op_def entry blocks) ->
  certificate_spec op_def entry blocks cert.
Proof.
  intros op_def entry blocks cert H. unfold find_certificates in H.
  unfold concat_map in H. apply in_flat_map in H.
  destruct H as [[loop bivs] [Hin H]].
  apply filter_map_In in H. destruct H as [biv [Hbiv H]].
  destruct (least_literal_argument _ _) as [lower | ] eqn:Hlower;
    [ | discriminate ].
  cbv zeta in H.
  destruct (verify_certificate entry biv lower (find_upper op_def blocks biv))
    eqn:Hverify; [ | discriminate ].
  injection H as <-.
  destruct (InductionVarProofs.analyze_spec _ _ _ _ Hin) as [Hloop [_ Hspec]].
  pose proof (Hspec biv Hbiv) as Hbiv_spec.
  pose proof (proj1 Hbiv_spec) as Hsame.
  destruct (verify_certificate_spec _ _ _ _ Hverify)
    as [step [Hstep [Hpositive [Hentry Hsources]]]].
  unfold certificate_spec.
  cbn [InductionRange.biv InductionRange.lower InductionRange.upper].
  rewrite Hsame.
  split; [ exact Hloop | split; [ exact Hbiv_spec | ] ].
  exists step. split; [ exact Hstep | split; [ exact Hpositive | ] ].
  rewrite Hsame in Hentry, Hsources.
  split; [ exact Hentry | ].
  split.
  { intros pred Hpred Hnot. rewrite Hsame in Hlower.
    eapply least_literal_argument_spec; eassumption. }
  split; [ | exact Hsources ].
  intros u Hu. destruct (find_upper_spec _ _ _ _ Hu) as [Habove [Hused Hpreds]].
  rewrite Hsame in Habove, Hpreds.
  split; [ exact Habove | split; [ exact Hused | exact Hpreds ] ].
Qed.

Section Semantics.
  Variable value_words : value -> Z.
  Hypothesis value_words_bounded : ∀ (v : value), in_int64 (value_words v).
  Hypothesis value_words_respect_equality :
    ∀ (v w : value),
    value_eqb v w = true -> value_words v = value_words w.
  Hypothesis value_words_follow_operations :
    ∀ (v : value), op_semantics value_words v.

  (* The bound facts about a form whose value is [counter_word] hold under an
     agreeing valuation when that value is within the bounds. *)
  Lemma bound_facts_sound : ∀ (lower : int) (upper : option value)
      (counter : Affine.t) (counter_word : Z) (initial_ctx : ctx)
      (initial_side_facts facts final_side_facts : list Affine.t)
      (final_ctx : ctx),
    ctx_wf initial_ctx ->
    to_Z initial_ctx.(next) + linearize_atom_bound <= ocaml_max_int ->
    wf counter ->
    (∀ (rho : valuation), agrees value_words rho initial_ctx ->
       denote rho counter = counter_word) ->
    bound_facts initial_ctx initial_side_facts lower upper counter
      = ((facts, final_side_facts), final_ctx) ->
    ctx_wf final_ctx ∧ extends initial_ctx final_ctx
    ∧ to_Z final_ctx.(next) <= to_Z initial_ctx.(next) + linearize_atom_bound
    ∧ Forall wf facts
    ∧ (Forall wf initial_side_facts -> Forall wf final_side_facts)
    ∧ ∀ (rho : valuation), agrees value_words rho final_ctx ->
         (holds_all rho initial_side_facts -> holds_all rho final_side_facts)
         ∧ (to_Z lower <= counter_word ->
            (∀ (u : value), upper = Some u ->
               counter_word <= value_words u) ->
            holds_all rho facts).
  Proof.
    intros lower upper counter counter_word c side facts side' c' Hwf Hroom
      Hwfc Hcounter H.
    unfold bound_facts in H.
    destruct (lower_facts_sound lower counter Hwfc) as [Hwfl Hholdsl].
    destruct upper as [u | ].
    - destruct (linearize c side u) as [[bound side1] c1] eqn:Hlin.
      destruct (linearize_sound value_words value_words_bounded
        value_words_respect_equality value_words_follow_operations u c side
        bound side1 c1 Hwf Hroom Hlin)
        as [Hwf1 [Hext1 [Hnext1 [Hwfb [Hwfs1 Hrho1]]]]].
      assert (Hrest : ∀ (rho : valuation), agrees value_words rho c1 ->
        (holds_all rho side -> holds_all rho side1)
        ∧ (to_Z lower <= counter_word ->
           holds_all rho (lower_facts lower counter))).
      { intros rho Hagrees. split; [ exact (proj2 (Hrho1 rho Hagrees)) | ].
        intros Hlow. apply (Hholdsl rho counter_word); [ | exact Hlow ].
        apply Hcounter. eapply agrees_extends; eassumption. }
      destruct (Affine.sub_checked bound counter) as [fact | ] eqn:Hfact;
        injection H as <- <- <-.
      + destruct (sub_checked_spec (fun _ => 0) _ _ _ Hwfb Hwfc Hfact)
          as [_ [_ Hwff]].
        split; [ exact Hwf1 | ].
        split; [ exact Hext1 | split; [ exact Hnext1 | ] ].
        split; [ constructor; [ exact Hwff | exact Hwfl ] | ].
        split; [ exact Hwfs1 | ].
        intros rho Hagrees. destruct (Hrest rho Hagrees) as [Hsides Hlower].
        split; [ exact Hsides | ]. intros Hlow Hhigh.
        constructor; [ | apply Hlower; exact Hlow ].
        destruct (sub_checked_spec rho _ _ _ Hwfb Hwfc Hfact) as [Hd _].
        unfold holds. rewrite Hd, (proj1 (Hrho1 rho Hagrees)).
        rewrite (Hcounter rho (agrees_extends value_words _ _ _ Hext1 Hagrees)).
        specialize (Hhigh u eq_refl). lia.
      + split; [ exact Hwf1 | ].
        split; [ exact Hext1 | split; [ exact Hnext1 | ] ].
        split; [ exact Hwfl | split; [ exact Hwfs1 | ] ].
        intros rho Hagrees. destruct (Hrest rho Hagrees) as [Hsides Hlower].
        split; [ exact Hsides | ]. intros Hlow _. apply Hlower. exact Hlow.
    - injection H as <- <- <-.
      split; [ exact Hwf | split; [ apply extends_refl | ] ].
      split; [ rewrite linearize_atom_bound_eq; lia | ].
      split; [ exact Hwfl | split; [ auto | ] ].
      intros rho Hagrees. split; [ auto | ]. intros Hlow _.
      apply (Hholdsl rho counter_word); [ apply Hcounter; exact Hagrees
        | exact Hlow ].
  Qed.

  (* One certificate's facts hold under an agreeing valuation when its counter
     is within its bounds. *)
  Lemma certificate_facts_sound : ∀ (cert : range_certificate)
      (initial_ctx : ctx) (facts : list Affine.t) (final_ctx : ctx),
    ctx_wf initial_ctx ->
    to_Z initial_ctx.(next) + (1 + linearize_atom_bound) <= ocaml_max_int ->
    certificate_facts initial_ctx cert = (facts, final_ctx) ->
    ctx_wf final_ctx ∧ extends initial_ctx final_ctx
    ∧ to_Z final_ctx.(next)
       <= to_Z initial_ctx.(next) + (1 + linearize_atom_bound)
    ∧ Forall wf facts
    ∧ ∀ (rho : valuation), agrees value_words rho final_ctx ->
        certificate_counter_in_range value_words cert -> holds_all rho facts.
  Proof.
    intros cert c facts c' Hwf Hroom H. unfold certificate_facts in H.
    cbv zeta in H. pose proof linearize_atom_bound_eq as Hbound.
    destruct (intern c _) as [id c1] eqn:Hintern.
    destruct (intern_spec value_words value_words_respect_equality _ _ _ _
      Hwf ltac:(lia) Hintern) as [Hwf1 [Hext1 [Hnext1 [_ Hrho1]]]].
    destruct (bound_facts c1 [] cert.(lower) cert.(upper) (Affine.var id))
      as [[bounds side] c2] eqn:Hbounds.
    injection H as <- <-.
    destruct (bound_facts_sound cert.(lower) cert.(upper) (Affine.var id)
      (value_words (param cert.(biv).(InductionVar.loop).(header)
         cert.(biv).(param_index)))
      c1 [] bounds side c2 Hwf1 ltac:(lia) (var_wf id)
      ltac:(intros rho Hagrees; rewrite var_denote; exact (Hrho1 rho Hagrees))
      Hbounds) as [Hwf2 [Hext2 [Hnext2 [Hwfb [Hwfs Hrho2]]]]].
    split; [ exact Hwf2 | ].
    split; [ eapply extends_trans; eassumption | ].
    split; [ lia | ].
    split.
    { apply Forall_app. split; [ exact Hwfb | apply Hwfs; constructor ]. }
    intros rho Hagrees [Hlow Hhigh].
    destruct (Hrho2 rho Hagrees) as [Hsides Hfacts].
    unfold holds_all. apply Forall_app.
    split; [ apply Hfacts; [ exact Hlow | exact Hhigh ] | ].
    apply Hsides. constructor.
  Qed.

  (* The walk of [range_facts] from any starting point: the facts gathered so
     far are kept and the new ones hold under the same condition. *)
  Lemma range_facts_walk_sound :
    ∀ (all certificates : list range_certificate)
      (target : block) (initial_facts : list Affine.t) (initial_ctx : ctx)
      (facts : list Affine.t) (final_ctx : ctx),
    incl certificates all ->
    ctx_wf initial_ctx ->
    to_Z initial_ctx.(next)
      + Z.of_nat (length certificates) * (1 + linearize_atom_bound)
      <= ocaml_max_int ->
    Forall wf initial_facts ->
    (∀ (rho : valuation), agrees value_words rho initial_ctx ->
       counters_in_range value_words all target -> holds_all rho initial_facts)
    ->
    List.fold_left (add_range_fact target) certificates
      (initial_facts, initial_ctx) = (facts, final_ctx) ->
    ctx_wf final_ctx ∧ extends initial_ctx final_ctx
    ∧ to_Z final_ctx.(next)
       <= to_Z initial_ctx.(next)
          + Z.of_nat (length certificates) * (1 + linearize_atom_bound)
    ∧ Forall wf facts
    ∧ ∀ (rho : valuation), agrees value_words rho final_ctx ->
        counters_in_range value_words all target -> holds_all rho facts.
  Proof.
    intros all certificates target.
    induction certificates as [ | cert certificates IH ];
      intros facts0 c0 facts c' Hincl Hwf Hroom Hwf0 Hholds0 H;
      cbn [List.fold_left length] in *;
      rewrite linearize_atom_bound_eq in *.
    - injection H as <- <-.
      split; [ exact Hwf | split; [ apply extends_refl | ] ].
      split; [ lia | split; [ exact Hwf0 | exact Hholds0 ] ].
    - rewrite Nat2Z.inj_succ in Hroom |- *.
      assert (Hcert : cert ∈ all) by (apply Hincl; left; reflexivity).
      assert (Hincl' : incl certificates all)
        by (intros x Hx; apply Hincl; right; exact Hx).
      destruct (add_range_fact target (facts0, c0) cert) as [facts1 c1]
        eqn:Hstep.
      assert (Hcont :
        ctx_wf c1 -> extends c0 c1 ->
        to_Z c1.(next) <= to_Z c0.(next) + (1 + 258) ->
        Forall wf facts1 ->
        (∀ (rho : valuation), agrees value_words rho c1 ->
           counters_in_range value_words all target -> holds_all rho facts1)
        ->
        ctx_wf c' ∧ extends c0 c'
        ∧ to_Z c'.(next)
           <= to_Z c0.(next)
              + Z.succ (Z.of_nat (length certificates)) * (1 + 258)
        ∧ Forall wf facts
        ∧ ∀ (rho : valuation), agrees value_words rho c' ->
            counters_in_range value_words all target -> holds_all rho facts).
      { intros Hwf1 Hext1 Hnext1 Hwff1 Hholds1.
        destruct (IH facts1 c1 facts c' Hincl' Hwf1 ltac:(lia) Hwff1 Hholds1
          H) as [Hwf' [Hext' [Hnext' [Hwff Hholds]]]].
        split; [ exact Hwf' | ].
        split; [ eapply extends_trans; eassumption | ].
        split; [ lia | split; [ exact Hwff | exact Hholds ] ]. }
      unfold add_range_fact in Hstep.
      destruct (set_mem target cert.(biv).(InductionVar.loop).(body))
        eqn:Hmem.
      + destruct (certificate_facts c0 cert) as [new_facts c2] eqn:Hfacts.
        injection Hstep as <- <-.
        destruct (certificate_facts_sound _ _ _ _ Hwf
          ltac:(rewrite linearize_atom_bound_eq; lia) Hfacts)
          as [Hwf2 [Hext2 [Hnext2 [Hwff2 Hholds2]]]].
        rewrite linearize_atom_bound_eq in Hnext2.
        apply Hcont; [ exact Hwf2 | exact Hext2 | exact Hnext2
          | apply Forall_app; split; [ exact Hwff2 | exact Hwf0 ] | ].
        intros rho Hagrees Hcounters. unfold holds_all. apply Forall_app.
        split.
        * apply Hholds2; [ exact Hagrees | ]. apply Hcounters; assumption.
        * apply Hholds0; [ | exact Hcounters ].
          eapply agrees_extends; eassumption.
      + injection Hstep as <- <-.
        apply Hcont; [ exact Hwf | apply extends_refl | lia | exact Hwf0
          | exact Hholds0 ].
  Qed.

  (* When [argument_at_most] accepts a predecessor of the header, the value it
     passes is at most the bound, under any reading of the values on which
     the branches dominating the predecessor went as taken. *)
  Theorem argument_at_most_sound : ∀ (u : value) (header : block)
      (index : int) (pred : block) (args : list Terminator.arg)
      (argument : value),
    2 * linearize_atom_bound + guards_atom_bound pred <= ocaml_max_int ->
    argument_at_most u header index pred = true ->
    goto_args header pred = Some args ->
    arg_value args index = Some argument ->
    guards_taken value_words pred ->
    value_words argument <= value_words u.
  Proof.
    intros u header index pred args argument Hroom Hcheck Hargs Harg Htaken.
    unfold argument_at_most in Hcheck.
    rewrite Hargs in Hcheck. cbv beta iota in Hcheck.
    rewrite Harg in Hcheck. cbv beta iota in Hcheck.
    destruct (linearize new_ctx [] u) as [[bound side1] c1] eqn:Hlin_u.
    destruct (linearize c1 side1 argument) as [[passed side2] c2]
      eqn:Hlin_a.
    destruct (guards_at c2 side2 pred) as [[guards side3] c3] eqn:Hguards.
    destruct (Affine.sub_checked bound passed) as [goal | ] eqn:Hgoal;
      [ | discriminate ].
    assert (Hn0 : to_Z new_ctx.(next) = 0) by exact to_Z_zero.
    assert (Hgs : 0 <= guards_atom_bound pred)
      by apply guard_list_atom_bound_nonnegative.
    pose proof linearize_atom_bound_eq as Hbound.
    destruct (linearize_sound value_words value_words_bounded
      value_words_respect_equality value_words_follow_operations u new_ctx
      [] bound side1 c1 new_ctx_wf ltac:(lia) Hlin_u)
      as [Hwf1 [_ [Hn1 [Hwfb [Hwfs1 Hrho1]]]]].
    destruct (linearize_sound value_words value_words_bounded
      value_words_respect_equality value_words_follow_operations argument
      c1 side1 passed side2 c2 Hwf1 ltac:(lia) Hlin_a)
      as [Hwf2 [He12 [Hn2 [Hwfp [Hwfs2 Hrho2]]]]].
    destruct (guards_at_sound value_words value_words_bounded
      value_words_respect_equality value_words_follow_operations pred c2
      side2 guards side3 c3 Hwf2 ltac:(lia) Hguards)
      as [Hwf3 [He23 [_ [Hwfg [Hwfs3 Hrho3]]]]].
    destruct (agrees_exists value_words c3 Hwf3) as [rho Hrho].
    pose proof (agrees_extends value_words rho _ _ He23 Hrho) as Hrho2'.
    pose proof (agrees_extends value_words rho _ _ He12 Hrho2') as Hrho1'.
    destruct (Hrho1 rho Hrho1') as [Hdb Hs1].
    destruct (Hrho2 rho Hrho2') as [Hdp Hs2].
    destruct (Hrho3 rho Hrho) as [Hs3 Hg].
    destruct (sub_checked_spec rho _ _ _ Hwfb Hwfp Hgoal)
      as [Hdgoal [_ Hwfgoal]].
    assert (Hfacts : holds_all rho (guards ++ side3)).
    { unfold holds_all. apply Forall_app.
      split; [ apply Hg; exact Htaken | ].
      apply Hs3, Hs2, Hs1. constructor. }
    assert (Hwffacts : Forall wf (guards ++ side3)).
    { apply Forall_app. split; [ exact Hwfg | ].
      apply Hwfs3, Hwfs2, Hwfs1. constructor. }
    pose proof (entails_sound _ _ Hwffacts Hwfgoal Hcheck rho Hfacts)
      as Hresult.
    rewrite Hdgoal, Hdb, Hdp in Hresult. lia.
  Qed.

  (* When [no_wrap_at] accepts a back-edge source, the counter plus the step
     is at most [max_int] there, under any reading of the values on which the
     branches dominating the source went as taken and the counter is within
     its bounds. *)
  Theorem no_wrap_at_sound : ∀ (step lower : int) (upper : option value)
      (header source : block) (index : int),
    2 * linearize_atom_bound + guards_atom_bound source <= ocaml_max_int ->
    no_wrap_at step lower upper header source index = true ->
    guards_taken value_words source ->
    to_Z lower <= value_words (param header index) ->
    (∀ (u : value), upper = Some u ->
       value_words (param header index) <= value_words u) ->
    value_words (param header index) + to_Z step <= to_Z max_int.
  Proof.
    intros step lower upper header source index Hroom Hcheck Htaken Hlower
      Hupper.
    unfold no_wrap_at in Hcheck.
    destruct (linearize new_ctx [] (param header index))
      as [[counter side1] c1] eqn:Hlin.
    destruct (bound_facts c1 side1 lower upper counter)
      as [[bounds side2] c2] eqn:Hbounds.
    destruct (guards_at c2 side2 source) as [[guards side3] c3] eqn:Hguards.
    cbv zeta in Hcheck.
    destruct (Affine.add_const_checked counter step) as [lifted | ]
      eqn:Hlifted; [ | discriminate ].
    cbn in Hcheck.
    destruct (Affine.sub_checked (Affine.constant max_int) lifted)
      as [goal | ] eqn:Hgoal; [ | discriminate ].
    assert (Hn0 : to_Z new_ctx.(next) = 0) by exact to_Z_zero.
    assert (Hgs : 0 <= guards_atom_bound source)
      by apply guard_list_atom_bound_nonnegative.
    pose proof linearize_atom_bound_eq as Hbound.
    destruct (linearize_sound value_words value_words_bounded
      value_words_respect_equality value_words_follow_operations _ _ _ _ _ _
      new_ctx_wf ltac:(lia) Hlin) as [Hwf1 [_ [Hn1 [Hwfc [Hwfs1 Hrho1]]]]].
    destruct (bound_facts_sound lower upper counter
      (value_words (param header index)) c1 side1 bounds side2 c2 Hwf1
      ltac:(lia) Hwfc ltac:(intros rho Hr; exact (proj1 (Hrho1 rho Hr)))
      Hbounds) as [Hwf2 [He12 [Hn2 [Hwfb [Hwfs2 Hrho2]]]]].
    destruct (guards_at_sound value_words value_words_bounded
      value_words_respect_equality value_words_follow_operations source c2
      side2 guards side3 c3 Hwf2 ltac:(lia) Hguards)
      as [Hwf3 [He23 [_ [Hwfg [Hwfs3 Hrho3]]]]].
    destruct (agrees_exists value_words c3 Hwf3) as [rho Hrho].
    pose proof (agrees_extends value_words rho _ _ He23 Hrho) as Hrho2'.
    pose proof (agrees_extends value_words rho _ _ He12 Hrho2') as Hrho1'.
    destruct (Hrho1 rho Hrho1') as [Hden Hs1].
    destruct (Hrho2 rho Hrho2') as [Hs2 Hb].
    destruct (Hrho3 rho Hrho) as [Hs3 Hg].
    destruct (add_const_checked_spec rho _ _ _ Hlifted)
      as [Hdlifted [_ Hwflifted]].
    destruct (sub_checked_spec rho _ _ _ (constant_wf max_int)
      (Hwflifted Hwfc) Hgoal) as [Hdgoal [_ Hwfgoal]].
    assert (Hfacts : holds_all rho (bounds ++ guards ++ side3)).
    { unfold holds_all. apply Forall_app.
      split; [ apply Hb; [ exact Hlower | exact Hupper ] | ].
      apply Forall_app. split; [ apply Hg; exact Htaken | ].
      apply Hs3, Hs2, Hs1. constructor. }
    assert (Hwffacts : Forall wf (bounds ++ guards ++ side3)).
    { apply Forall_app. split; [ exact Hwfb | ].
      apply Forall_app. split; [ exact Hwfg | ].
      apply Hwfs3, Hwfs2, Hwfs1. constructor. }
    pose proof (entails_sound _ _ Hwffacts Hwfgoal Hcheck rho Hfacts)
      as Hresult.
    rewrite Hdgoal, Hdlifted, constant_denote, Hden in Hresult. lia.
  Qed.

  (* The facts [range_facts] builds for [target] are well-formed and hold under
     every valuation agreeing with the final context, when the counter of each
     certificate whose loop contains [target] is within its bounds. *)
  Theorem range_facts_sound : ∀ (certificates : list range_certificate)
      (target : block) (initial_ctx : ctx) (facts : list Affine.t)
      (final_ctx : ctx),
    ctx_wf initial_ctx ->
    to_Z initial_ctx.(next) + range_atom_bound certificates
      <= ocaml_max_int ->
    range_facts certificates target initial_ctx = (facts, final_ctx) ->
    ctx_wf final_ctx ∧ extends initial_ctx final_ctx
    ∧ to_Z final_ctx.(next)
       <= to_Z initial_ctx.(next) + range_atom_bound certificates
    ∧ Forall wf facts
    ∧ ∀ (rho : valuation), agrees value_words rho final_ctx ->
        counters_in_range value_words certificates target ->
        holds_all rho facts.
  Proof.
    intros certificates target c facts c' Hwf Hroom H.
    unfold range_facts, OxCamlList.fold_left in H.
    unfold range_atom_bound in Hroom |- *.
    apply (range_facts_walk_sound certificates certificates target [] c facts
      c' (incl_refl _) Hwf Hroom (Forall_nil _)); [ | exact H ].
    intros rho _ _. constructor.
  Qed.
End Semantics.
