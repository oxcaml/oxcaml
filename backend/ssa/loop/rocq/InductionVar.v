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

(* The basic induction variables of a natural loop: the header parameters that
   every back edge updates by the same loop-invariant step. The extraction of
   this file, InductionVar.ml, is the implementation; the table SsaModel.v
   declares as [op_def] is built by the pass (delete_empty_loops.ml).

   See the reading guide at the top of LoopComparisons.v for the Rocq
   vocabulary, SsaModel.v for how the models see an SSA graph, and
   NaturalLoop.v for the loops this file analyses. As there, the file has four
   parts: (1) the definitions, (2) the intended meaning, (3) stepping-stone
   lemmas, (4) the theorems. Review parts 1 and 2 and the theorem statements;
   Rocq checks the proofs.

   The header's parameters and the arguments of a [Goto] are lists here,
   indexed by [nth_int] (OxCamlList.v).

   The result, in part 4, is [analyze_spec]: every basic induction variable
   reported for a loop satisfies [biv_spec] (part 2), which spells out what was
   checked: the update value arriving over every back edge for that parameter
   is [self + c], [c + self] or [self - c] with the same loop-invariant [c] and
   the same sign, and the [update] list is exactly those values.
   LoopCounterExecution.v proves what such an update does to the parameter in
   actual instruction-by-instruction execution. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlList.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaGraph SsaDominance.
From OxCamlLoop Require Import NaturalLoop.
Import ListNotations.

(* ======================================================================== *)
(* Part 1: the definitions                                                  *)
(* ======================================================================== *)

Local Open Scope sint63_scope.

(* A value cannot change across the iterations of a loop when it is a constant
   (one of the [Const_*] operations), or is defined in a block that strictly
   dominates the loop's header: such a block is not in the loop's body (whose
   blocks the header dominates), so it does not run between two visits of the
   header. For a value used in the body this is the same as its defining block
   being outside the body, by SSA scoping. A [true] establishes
   [loop_invariant_value] (part 2). LoopInvariantExecution.v proves its
   operational meaning for available canonical definitions; it is not sound for
   arbitrary fabricated [Res] expressions merely borrowing an outside
   instruction's ID. *)
Definition is_loop_invariant (op_def : op_def) (header : block) (v : value)
    : bool :=
  match v with
  | Res _ (Const_int _ | Const_float32 _ | Const_float _
           | Const_symbol _ | Const_vec128 _ | Const_vec256 _
           | Const_vec512 _ | Const_mask _) _ _ => true
  | Res id _ _ _ =>
    match op_def_find op_def id with
    | None => false
    | Some block => strictly_dominates block header
    end
  | Block_param block _ => strictly_dominates block header
  end.

(* The per-iteration step: a literal, or a loop-invariant value. *)
Inductive biv_step : Type :=
  | Step_const (c : int)
  | Step_var (v : value).

(* Whether the step is added or subtracted. *)
Inductive biv_sign : Type :=
  | Add
  | Sub.

(* A basic induction variable: parameter [param_index] of the header of [loop],
   the values arriving for it over the back edges ([update]), and their common
   step and sign. *)
Record biv : Type :=
  { loop : natural_loop;
    param_index : int;
    update : list value;
    step : biv_step;
    sign : biv_sign }.

(* Equality of steps, and of signs. *)
Definition step_same (a b : biv_step) : bool :=
  match a, b with
  | Step_const x, Step_const y => x =? y
  | Step_var x, Step_var y => value_eqb x y
  | Step_const _, Step_var _ | Step_var _, Step_const _ => false
  end.

Definition sign_same (a b : biv_sign) : bool :=
  match a, b with
  | Add, Add | Sub, Sub => true
  | Add, Sub | Sub, Add => false
  end.

(* A literal step with its sign. A [-c] step is not representable when [c] is
   [min_int], so there is no signed step then. *)
Definition signed_step (biv : biv) : option int :=
  match biv.(step) with
  | Step_const c =>
    match biv.(sign) with
    | Add => Some c
    | Sub => if c =? min_int then None else Some (Uint63.opp c)
    end
  | Step_var _ => None
  end.

(* The step and sign of an update value: [self + c], [c + self] or [self - c],
   with [self] the parameter ([is_self]) and [c] a literal or a loop-invariant
   value. *)
Definition classify_update (is_self : value -> bool) (op_def : op_def)
    (header : block) (v : value) : option (biv_step * biv_sign) :=
  match v with
  | Res _ (Intop_imm Iadd c) [x] _ =>
    if is_self x then Some (Step_const c, Add) else None
  | Res _ (Intop_imm Isub c) [x] _ =>
    if is_self x then Some (Step_const c, Sub) else None
  | Res _ (Intop Iadd) [x; y] _ =>
    if is_self x && is_loop_invariant op_def header y
    then Some (Step_var y, Add)
    else if is_self y && is_loop_invariant op_def header x
    then Some (Step_var x, Add)
    else None
  | Res _ (Intop Isub) [x; y] _ =>
    if is_self x && is_loop_invariant op_def header y
    then Some (Step_var y, Sub)
    else None
  | _ => None
  end.

(* The back-edge sources as a set. *)
Definition back_edge_set (loop : natural_loop) : blockset :=
  OxCamlList.fold_left (fun set block => set_add block set) set_empty
    loop.(back_edge_sources).

(* The BIV for the parameter [index], if it is one: every back edge passes a
   value for it, and they classify alike. [source_args] pairs each back-edge
   source with the arguments of its [Goto] to the header. *)
Definition classify_param (loop : natural_loop) (op_def : op_def)
    (source_args : list (block * list Terminator.arg)) (index : int)
    : option biv :=
  let is_self := is_param loop.(header) index in
  let back_vals_opt :=
    map (fun '(_, args) => arg_value args index) source_args
  in
  if forallb is_some back_vals_opt
  then
    let back_vals := filter_map (fun v => v) back_vals_opt in
    match back_vals with
    | [] => None
    | first :: rest =>
      match classify_update is_self op_def loop.(header) first with
      | None => None
      | Some (step, sign) =>
        let agrees :=
          forallb
            (fun v =>
              match classify_update is_self op_def loop.(header) v with
              | Some (step', sign') =>
                step_same step step' && sign_same sign sign'
              | None => false
              end)
            rest
        in
        if agrees
        then
          Some {| loop := loop;
                  param_index := index;
                  update := back_vals;
                  step := step;
                  sign := sign |}
        else None
      end
    end
  else None.

(* The BIVs among [params], [index] being the position of the first of them in
   the header's parameters. *)
Fixpoint params_bivs (loop : natural_loop) (op_def : op_def)
    (source_args : list (block * list Terminator.arg)) (index : int)
    (params : list value) : list biv :=
  match params with
  | [] => []
  | _ :: params =>
    let rest := params_bivs loop op_def source_args (index + one) params in
    match classify_param loop op_def source_args index with
    | Some biv => biv :: rest
    | None => rest
    end
  end.

(* The BIVs of a loop: none unless every back edge is a [Goto] to the header
   with one argument per parameter. *)
Definition analyze_loop (op_def : op_def) (loop : natural_loop) : list biv :=
  let header := loop.(header) in
  let source_set := back_edge_set loop in
  let pred_args :=
    map (fun pred => (pred, goto_args header pred)) (predecessors header)
  in
  let all_back_edges_have_args :=
    forallb
      (fun '(pred, args) => negb (set_mem pred source_set) || is_some args)
      pred_args
  in
  if negb all_back_edges_have_args
  then []
  else
    let source_args :=
      filter_map
        (fun '(pred, args) =>
          if set_mem pred source_set
          then option_map (fun args => (pred, args)) args
          else None)
        pred_args
    in
    params_bivs loop op_def source_args zero (params header).

(* Every natural loop of the graph with its BIVs. *)
Definition analyze (op_def : op_def) (blocks : list block)
    : list (natural_loop * list biv) :=
  map (fun loop => (loop, analyze_loop op_def loop)) (find_loops blocks).

(* ======================================================================== *)
(* Part 2: the intended meaning                                             *)
(* ======================================================================== *)

(* A value that cannot change while the loop runs: a constant, or a value
   defined (an operation according to [op_def], a parameter by its block) in a
   block that dominates [header] without being it. *)
Definition loop_invariant_value (op_def : op_def) (header : block)
    (v : value) : Prop :=
  match v with
  | Res _ (Const_int _ | Const_float32 _ | Const_float _
           | Const_symbol _ | Const_vec128 _ | Const_vec256 _
           | Const_vec512 _ | Const_mask _) _ _ => True
  | Res id _ _ _ =>
    ∃ block,
      op_def_find op_def id = Some block
      ∧ block ▷ header ∧ block <> header
  | Block_param block _ => block ▷ header ∧ block <> header
  end.

(* Every update value of [biv] is [self + c], [c + self] or [self - c] for the
   header parameter [self] at [index] and a loop-invariant [c], with the step
   and sign [biv] records. *)
Definition updates_have_same_step_and_sign (op_def : op_def)
  (loop : natural_loop)
    (index : int) (biv : biv) : Prop :=
  ∀ (updated_value : value), updated_value ∈ biv.(update) ->
    ∃ step' sign',
      classify_update (is_param loop.(header) index) op_def loop.(header)
        updated_value
      = Some (step', sign')
      ∧ step_same biv.(step) step' = true
      ∧ sign_same biv.(sign) sign' = true.

(* Every back-edge source that is a predecessor of the header passes, in its
   [Goto] to the header, a value for the parameter of [biv], and that value is
   among [update]. *)
Definition every_back_edge_passes_an_update (loop : natural_loop) (biv : biv)
    : Prop :=
  ∀ (source : block),
    source ∈ loop.(back_edge_sources) ->
    source ∈ (predecessors loop.(header)) ->
    ∃ args updated_value,
      goto_args loop.(header) source = Some args
      ∧ arg_value args biv.(param_index) = Some updated_value
      ∧ updated_value ∈ biv.(update).

(* Every value of [update] is what some back-edge source passes, in its [Goto]
   to the header, for the parameter of [biv]. *)
Definition every_update_arrives_over_a_back_edge (loop : natural_loop)
    (biv : biv) : Prop :=
  ∀ (updated_value : value), updated_value ∈ biv.(update) ->
    ∃ source args,
      source ∈ loop.(back_edge_sources)
      ∧ goto_args loop.(header) source = Some args
      ∧ arg_value args biv.(param_index) = Some updated_value.

(* What holds of a BIV reported for the loop [loop]: it is for a parameter
   [param_index] of the header; every back-edge source that is a predecessor of
   the header passes a value for that parameter, and [update] holds exactly
   those values; and each of them is [self + c], [c + self] or [self - c] for
   the header parameter [self] and a loop-invariant [c], with the same [step]
   and [sign] as recorded. *)
Definition biv_spec (op_def : op_def) (loop : natural_loop) (biv : biv)
    : Prop :=
  biv.(InductionVar.loop) = loop
  ∧ biv.(update) <> []
  ∧ updates_have_same_step_and_sign op_def loop biv.(param_index) biv
  ∧ every_back_edge_passes_an_update loop biv
  ∧ every_update_arrives_over_a_back_edge loop biv.

(* ======================================================================== *)
(* Part 3: lemmas used by the proofs                                        *)
(* ======================================================================== *)

(* [step_same] and [sign_same] are reflexive. *)
Lemma step_same_refl : ∀ (step : biv_step), step_same step step = true.
Proof.
  intros [c | v]; cbn [step_same].
  - apply eqb_int. reflexivity.
  - apply value_eqb_spec. reflexivity.
Qed.

Lemma sign_same_refl : ∀ (sign : biv_sign), sign_same sign sign = true.
Proof. intros []; reflexivity. Qed.

(* [back_edge_set] holds exactly the back-edge sources. *)
Lemma back_edge_set_spec : ∀ (loop : natural_loop) (block : block),
  set_mem block (back_edge_set loop) = true
  <-> block ∈ loop.(back_edge_sources).
Proof.
  intros l b. unfold back_edge_set, OxCamlList.fold_left.
  generalize (back_edge_sources l). intros edges.
  (* The accumulator holds the edges seen so far. *)
  assert (H : forall s,
    set_mem b (List.fold_left (fun s b => set_add b s) edges s) = true
    <-> b ∈ edges ∨ set_mem b s = true).
  { induction edges as [ | e edges IH ]; intros s; cbn [List.fold_left].
    - cbn [In]. tauto.
    - rewrite IH, set_mem_add_iff. cbn [In]. intuition congruence. }
  rewrite H, set_mem_empty. intuition discriminate.
Qed.

(* Every recognized update directly reads the parameter it updates, whether its
   step is immediate or held in an invariant register. *)
Lemma classify_update_reads_self : ∀ (is_self : value -> bool)
    (defs : op_def) (header : block) (updated : value) (step : biv_step)
    (sign : biv_sign),
  classify_update is_self defs header updated = Some (step, sign) ->
  match updated with
  | Res _ _ args _ => ∃ (self : value),
      self ∈ args ∧ is_self self = true
  | Block_param _ _ => False
  end.
Proof.
  intros is_self defs header [id op args index | b index] step sign Hclass;
    [ | discriminate].
  destruct op; try discriminate; try (destruct op; try discriminate);
    destruct args as [ | a [ | b [ | extra rest]]]; try discriminate;
    cbn [classify_update] in Hclass;
    repeat match type of Hclass with
    | context [if ?test then _ else _] =>
      let Heq := fresh "Htest" in destruct test eqn:Heq
    end; try discriminate.
  all: repeat match goal with
    | Htest : (_ && _ = true) |- _ =>
      apply andb_true_iff in Htest; destruct Htest
    end; cbn; eauto.
Qed.

(* --- [classify_param] --- *)

(* When [forallb is_some] accepts the values at [index], every pair of
   [source_args] passes a value there. *)
Lemma all_argument_values_present :
  ∀ (source_args : list (block * list Terminator.arg)) (index : int),
  forallb is_some
    (map (fun '(_, args) => arg_value args index) source_args) = true ->
  ∀ (source : block) (args : list Terminator.arg),
  (source, args) ∈ source_args ->
  ∃ updated_value, arg_value args index = Some updated_value.
Proof.
  intros bpa index H p args Hin. rewrite forallb_forall in H.
  specialize (H (arg_value args index)).
  destruct (arg_value args index) as [u | ] eqn:E.
  - exists u. reflexivity.
  - exfalso.
    assert (H0 : (@None value)
              ∈ map (fun '(_, args) => arg_value args index) bpa).
    { apply in_map_iff. exists (p, args). split; [ exact E | exact Hin ]. }
    specialize (H H0). discriminate.
Qed.

(* The values collected at [index] are exactly those some back edge passes
   there. *)
Lemma back_edge_values_In :
  ∀ (source_args : list (block * list Terminator.arg)) (index : int)
    (updated_value : value),
  updated_value ∈ (filter_map (fun v => v)
          (map (fun '(_, args) => arg_value args index) source_args))
  <-> ∃ source args,
        (source, args) ∈ source_args
        ∧ arg_value args index = Some updated_value.
Proof.
  intros bpa index u. split.
  - intros H. apply filter_map_In in H. destruct H as [x [Hx Hu]].
    apply in_map_iff in Hx. destruct Hx as [[p args] [<- Hin]].
    exists p, args. split; [ exact Hin | exact Hu ].
  - intros [p [args [Hin Hu]]].
    induction bpa as [ | [p' args'] bpa IH ]; [ contradiction | ].
    cbn [map filter_map]. destruct Hin as [E | Hin].
    + injection E as <- <-. rewrite Hu. left. reflexivity.
    + destruct (arg_value args' index); [ right | ]; apply IH, Hin.
Qed.

(* What [classify_param] establishes of the BIV it returns: it is for this loop
   and [index]; its updates are non-empty, classify alike, and are exactly the
   values the back edges pass at [index]. *)
Lemma classify_param_spec :
  ∀ (loop : natural_loop) (op_def : op_def)
    (source_args : list (block * list Terminator.arg)) (index : int)
    (biv : biv),
  classify_param loop op_def source_args index = Some biv ->
  biv.(InductionVar.loop) = loop
  ∧ biv.(param_index) = index
  ∧ biv.(update) <> []
  ∧ updates_have_same_step_and_sign op_def loop index biv
  ∧ (∀ (source : block) (args : list Terminator.arg),
        (source, args) ∈ source_args ->
        ∃ updated_value,
          arg_value args index = Some updated_value
          ∧ updated_value ∈ biv.(update))
  ∧ (∀ (updated_value : value), updated_value ∈ biv.(update) ->
        ∃ source args,
          (source, args) ∈ source_args ∧ arg_value args index = Some
            updated_value).
Proof.
  intros l op_def bpa index b H. unfold classify_param in H. cbv zeta in H.
  destruct (forallb is_some _) eqn:Hall; [ | discriminate ].
  pose proof (all_argument_values_present _ _ Hall) as Hvals.
  set (back_vals := filter_map (fun v => v)
                      (map (fun '(_, args) => arg_value args index) bpa)) in *.
  destruct back_vals as [ | first rest ] eqn:Hbv; [ discriminate | ].
  destruct (classify_update _ _ _ first) as [[step0 sign0] | ] eqn:Hfirst;
    [ | discriminate ].
  destruct (forallb _ rest) eqn:Hagree; [ | discriminate ].
  injection H as <-. cbn [loop param_index update step sign].
  rewrite forallb_forall in Hagree.
  repeat split.
  - discriminate.
  - intros u [ -> | Hu ].
    + exists step0, sign0.
      split; [ exact Hfirst | ].
      split; [ apply step_same_refl | apply sign_same_refl ].
    + specialize (Hagree u Hu).
      destruct (classify_update _ _ _ u) as [[step' sign'] | ];
        [ | discriminate ].
      exists step', sign'. apply andb_true_iff in Hagree.
      split; [ reflexivity | exact Hagree ].
  - intros p args Hin. destruct (Hvals p args Hin) as [u Hu].
    exists u. split; [ exact Hu | ].
    rewrite <- Hbv. apply back_edge_values_In. exists p, args. split;
      assumption.
  - intros u Hu. rewrite <- Hbv in Hu. apply back_edge_values_In in Hu. exact
    Hu.
Qed.

(* At most one BIV per parameter. *)
Lemma params_bivs_length_le :
  ∀ (loop : natural_loop) (op_def : op_def)
    (source_args : list (block * list Terminator.arg)) (params : list value)
    (index : int),
  (length (params_bivs loop op_def source_args index params)
   <= length params)%nat.
Proof.
  intros l op_def bpa params. induction params as [ | x params IH ];
    intros index; cbn [params_bivs].
  - apply Nat.le_refl.
  - specialize (IH (index + one)%sint63).
    destruct (classify_param l op_def bpa index); cbn [length]; lia.
Qed.

(* With as many BIVs as parameters, every parameter position has one:
   [classify_param] yields at most one BIV per position, indexed by it. *)
Lemma params_bivs_cover :
  ∀ (loop : natural_loop) (op_def : op_def)
    (source_args : list (block * list Terminator.arg)) (params : list value)
    (first_parameter_index offset : int) (parameter : value),
  length (params_bivs loop op_def source_args first_parameter_index params) =
    length params ->
  nth_int params offset = Some parameter ->
  ∃ biv, biv ∈ (params_bivs loop op_def source_args first_parameter_index
    params)
    ∧ biv.(param_index) = (first_parameter_index + offset)%sint63.
Proof.
  intros l op_def bpa params. induction params as [ | x params IH ];
    intros index k v Hlen Hnth; cbn [params_bivs nth_int] in *.
  - discriminate.
  - destruct (classify_param l op_def bpa index) as [b | ] eqn:E.
    + destruct (k =? zero)%sint63 eqn:Hk.
      * apply eqb_int in Hk. subst k. exists b.
        split; [ left; reflexivity | ].
        rewrite add_zero_r.
        exact (proj1 (proj2 (classify_param_spec _ _ _ _ _ E))).
      * cbn [length] in Hlen. injection Hlen as Hlen.
        destruct (IH (index + one)%sint63 (k - one)%sint63 v Hlen Hnth)
          as [b' [Hb' Hidx]].
        exists b'. split; [ right; exact Hb' | ].
        rewrite Hidx. apply add_one_sub_one.
    + exfalso.
      pose proof
        (params_bivs_length_le l op_def bpa params (index + one)%sint63).
      cbn [length] in Hlen. lia.
Qed.

(* Every BIV [params_bivs] returns comes from [classify_param] at some index.
   *)
Lemma params_bivs_In :
  ∀ (loop : natural_loop) (op_def : op_def)
    (source_args : list (block * list Terminator.arg)) (params : list value)
    (index : int) (biv : biv),
  biv ∈ (params_bivs loop op_def source_args index params) ->
  ∃ index', classify_param loop op_def source_args index' = Some biv.
Proof.
  intros l op_def bpa params. induction params as [ | v params IH ];
    intros index b H; cbn [params_bivs] in H.
  - contradiction.
  - destruct (classify_param l op_def bpa index) as [b' | ] eqn:E.
    + destruct H as [ -> | H ].
      * exists index. exact E.
      * exact (IH _ _ H).
    + exact (IH _ _ H).
Qed.

(* --- [analyze_loop] --- *)

(* The pairs [analyze_loop] builds: a predecessor [pred] of the header appears
   with [args] exactly when [pred] is a back-edge source whose terminator is a
   [Goto] to the header passing [args]. *)
Lemma back_edge_arguments_In :
  ∀ (header : block) (source_set : blockset) (preds : list block)
    (pred : block) (args : list Terminator.arg),
  (pred, args) ∈ (filter_map
       (fun '(pred, args) =>
         if set_mem pred source_set
         then option_map (fun args => (pred, args)) args
         else None)
       (map (fun pred => (pred, goto_args header pred)) preds))
  <-> pred ∈ preds ∧ set_mem pred source_set = true
      ∧ goto_args header pred = Some args.
Proof.
  intros header back_set preds p args. induction preds as [ | q preds IH ].
  - cbn. tauto.
  - cbn [map filter_map]. destruct (set_mem q back_set) eqn:Hq.
    + destruct (goto_args header q) as [qargs | ] eqn:Hqa;
        cbn [option_map].
      * cbn [In]. rewrite IH. split.
        -- intros [E | H].
           ++ injection E as <- <-. split; [ left; reflexivity | ].
              split; [ exact Hq | exact Hqa ].
           ++ destruct H as [H1 [H2 H3]]. split; [ right; exact H1 | ].
              split; assumption.
        -- intros [[ -> | H1 ] [H2 H3]].
           ++ left. rewrite Hqa in H3. injection H3 as ->. reflexivity.
           ++ right. split; [ exact H1 | split; assumption ].
      * rewrite IH. cbn [In]. split.
        -- intros [H1 [H2 H3]]. split; [ right; exact H1 | split; assumption ].
        -- intros [[ -> | H1 ] [H2 H3]].
           ++ rewrite Hqa in H3. discriminate.
           ++ split; [ exact H1 | split; assumption ].
    + rewrite IH. cbn [In]. split.
      * intros [H1 [H2 H3]]. split; [ right; exact H1 | split; assumption ].
      * intros [[ -> | H1 ] [H2 H3]].
        -- rewrite Hq in H2. discriminate.
        -- split; [ exact H1 | split; assumption ].
Qed.

(* ======================================================================== *)
(* Part 4: theorems                                                         *)
(* ======================================================================== *)

(* What [is_loop_invariant] checks, read back. *)
Theorem is_loop_invariant_spec :
  ∀ (op_def : op_def) (header : block) (v : value),
  is_loop_invariant op_def header v = true ->
  loop_invariant_value op_def header v.
Proof.
  intros op_def header v H.
  unfold is_loop_invariant in H. unfold loop_invariant_value.
  assert (Hstrict : forall bl, strictly_dominates bl header = true ->
    dominates bl header = true ∧ bl <> header).
  { intros bl Hs. unfold strictly_dominates in Hs.
    apply andb_true_iff in Hs. destruct Hs as [Hd Hne].
    split; [ exact Hd | ]. intros ->.
    rewrite block_eqb_refl in Hne. discriminate. }
  destruct v as [id op args i | bl i].
  - destruct op; try exact I;
      destruct (op_def_find op_def id) as [bl | ]; try discriminate;
      (exists bl; split; [ reflexivity | apply Hstrict; exact H ]).
  - apply Hstrict. exact H.
Qed.

(* Every BIV [analyze_loop] reports satisfies [biv_spec]. *)
Theorem analyze_loop_spec :
  ∀ (op_def : op_def) (loop : natural_loop) (biv : biv),
  biv ∈ (analyze_loop op_def loop) -> biv_spec op_def loop biv.
Proof.
  intros op_def l b H. unfold analyze_loop in H. cbv zeta in H.
  destruct (negb (forallb _ _)) eqn:Hall; [ contradiction | ].
  apply negb_false_iff in Hall. rewrite forallb_forall in Hall.
  apply params_bivs_In in H. destruct H as [i Hi].
  destruct (classify_param_spec _ _ _ _ _ Hi)
    as [Hl [Hidx [Hne [Hupd [Hargs Hupd']]]]].
  unfold biv_spec, every_back_edge_passes_an_update,
    every_update_arrives_over_a_back_edge.
  rewrite Hidx.
  split; [ exact Hl | split; [ exact Hne | split; [ | split ] ] ].
  - exact Hupd.
  - intros s Hs Hpred.
    assert (Hset : set_mem s (back_edge_set l) = true)
      by (apply back_edge_set_spec; exact Hs).
    assert (Hsome : exists args, goto_args l.(header) s = Some args).
    { specialize (Hall (s, goto_args l.(header) s)).
      destruct (goto_args l.(header) s) as [args | ] eqn:E.
      - exists args. reflexivity.
      - exfalso.
        assert (Hin : (s, @None (list Terminator.arg))
                  ∈ map (fun p => (p, goto_args l.(header) p))
                      (predecessors l.(header))).
        { apply in_map_iff. exists s. split; [ rewrite E; reflexivity | ].
          exact Hpred. }
        specialize (Hall Hin). rewrite Hset in Hall. discriminate. }
    destruct Hsome as [args Hargs'].
    destruct (Hargs s args) as [u [Hu Hin]].
    { apply back_edge_arguments_In. split; [ exact Hpred | split; assumption ].
      }
    exists args, u. split; [ exact Hargs' | split; assumption ].
  - intros u Hu. destruct (Hupd' u Hu) as [s [args [Hin Hval]]].
    apply back_edge_arguments_In in Hin. destruct Hin as [_ [Hset Hpa]].
    apply back_edge_set_spec in Hset.
    exists s, args. split; [ exact Hset | split; assumption ].
Qed.

(* When a loop has as many BIVs as its header has parameters, every parameter
   position has one. *)
Theorem analyze_loop_cover :
  ∀ (op_def : op_def) (loop : natural_loop) (parameter_index : int)
    (parameter : value),
  length (analyze_loop op_def loop) = length (params loop.(header)) ->
  nth_int (params loop.(header)) parameter_index = Some parameter ->
  ∃ biv,
    biv ∈ analyze_loop op_def loop ∧ biv.(param_index) = parameter_index.
Proof.
  intros op_def l k v Hlen Hnth. unfold analyze_loop in *. cbv zeta in *.
  destruct (negb (forallb _ _)).
  - exfalso. cbn [length] in Hlen.
    destruct (params l.(header)); [ | discriminate ].
    cbn [nth_int] in Hnth. discriminate.
  - destruct (params_bivs_cover _ _ _ _ _ _ _ Hlen Hnth) as [b [Hb Hidx]].
    exists b. split; [ exact Hb | ]. rewrite Hidx. apply add_zero_l.
Qed.

(* Each pair [analyze] returns is a loop [find_loops] finds with the BIVs
   [analyze_loop] computes for it, each satisfying [biv_spec]. *)
Theorem analyze_spec :
  ∀ (op_def : op_def) (blocks : list block) (loop : natural_loop)
    (bivs : list biv),
  (loop, bivs) ∈ (analyze op_def blocks) ->
  loop ∈ (find_loops blocks) ∧ bivs = analyze_loop op_def loop
  ∧ ∀ (biv : biv), biv ∈ bivs -> biv_spec op_def loop biv.
Proof.
  intros op_def blocks l bivs H. unfold analyze in H.
  apply in_map_iff in H. destruct H as [l' [E Hl]]. injection E as <- <-.
  split; [ exact Hl | split; [ reflexivity | ] ].
  intros b Hb. apply analyze_loop_spec, Hb.
Qed.
