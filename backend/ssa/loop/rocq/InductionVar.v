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
   this file, InductionVar.ml, is the implementation; it needs no review. What
   a reported variable is, and the proofs, are in InductionVarProofs.v. The
   table SsaModel.v declares as [op_def] is built by the pass
   (delete_empty_loops.ml); NaturalLoop.v finds the loops this file analyses.

   The header's parameters and the arguments of a [Goto] are lists here,
   indexed by [nth_int] (OxCamlList.v). *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlList.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaGraph SsaDominance SsaSemanticsSpec.
From OxCamlLoop Require Import NaturalLoop.
Import ListNotations.

Local Open Scope sint63_scope.

(* A value cannot change across the iterations of a loop when it is a constant
   (one of the [Const_*] operations), or is defined in a block that strictly
   dominates the loop's header: such a block is not in the loop's body (whose
   blocks the header dominates), so it does not run between two visits of the
   header. For a value used in the body this is the same as its defining block
   being outside the body, by SSA scoping. A [true] establishes
   [loop_invariant_value] (InductionVarProofs.v).
   LoopInvariantExecutionProofs.v proves its operational meaning for available
   canonical definitions; it is not sound for arbitrary fabricated [Res]
   expressions merely borrowing an outside
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
