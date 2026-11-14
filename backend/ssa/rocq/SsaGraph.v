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

(* Checks on the blocks, terminators and values of an SSA graph that the loop
   analyses make: which value a Goto passes at a position, whether every use
   of a value in the graph is approved, whether a region is acyclic once a
   block is removed. The extraction of this file, SsaGraph.ml, is the
   implementation.

   This file needs no review: what each check establishes when it answers
   [true] is stated and proved in SsaGraphProofs.v, over SsaModel.v and
   SsaGraphSpec.v, and nothing else is relied on about it. *)

From Stdlib Require Import Bool List.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlList.
From OxCamlSsa Require Import SsaModel SsaGraphSpec.
Import ListNotations.

Local Open Scope sint63_scope.

(* [v] is parameter [index] of [block]. *)
Definition is_param (block : block) (index : int) (v : value) : bool :=
  value_eqb v (param block index).

(* [instr] is the instruction defining [v]. *)
Definition instr_defines_value (instr : instr) (v : value) : bool :=
  match instr, v with
  | Op id _ _ _, Res id' _ _ _ => instr_id_eqb id id'
  | _, _ => false
  end.

(* The values an instruction reads. *)
Definition args_of_instr (instr : instr) : list value :=
  match instr with
  | Op _ _ args _ => args
  | Push_trap | Pop_trap => []
  end.

(* [arg] passes the value [v]. *)
Definition arg_is (v : value) (arg : Terminator.arg) : bool :=
  match arg with
  | Terminator.Arg w => value_eqb w v
  | Terminator.Omitted_since_unused => false
  end.

(* The value an argument at position [index] passes, if any. *)
Definition arg_value (args : list Terminator.arg) (index : int)
    : option value :=
  match nth_int args index with
  | Some (Terminator.Arg v) => Some v
  | Some Terminator.Omitted_since_unused | None => None
  end.

(* The arguments [pred] passes to [target], when its terminator is a [Goto]
   there with one argument per parameter. *)
Definition goto_args (target pred : block) : option (list Terminator.arg) :=
  match terminator pred with
  | Terminator.Continue (Goto goto) args =>
    if block_eqb goto target && same_length args (params target)
    then Some args
    else None
  | _ => None
  end.

(* The terminator of [block] is a [Goto] to [target] passing [argument_value]
   as the argument at position [argument_index], and at no other position. *)
Definition goto_passes_only_at (target : block) (argument_index : int)
  (argument_value : value) (block : block)
    : bool :=
  match terminator block with
  | Terminator.Continue (Goto goto) args =>
    block_eqb goto target
    && (match arg_value args argument_index with
        | Some w => value_eqb w argument_value
        | None => false
        end)
    && (count_occurrences (arg_is argument_value) args =? one)
  | Terminator.Continue (Return | Raise _ | Unreachable) _
  | Terminator.Switch _ _ | Terminator.Call _ | Terminator.Invalid _ => false
  end.

(* Every use in the graph of a value [same] accepts is approved: by
   [approve_instr] for an instruction reading it, by [approve_term] for the
   block whose terminator reads it, given the list it is read in. *)
Definition all_uses_ok (blocks : list block) (same : value -> bool)
    (approve_instr : instr -> bool)
    (approve_term : block -> list value -> bool) : bool :=
  forallb
    (fun block =>
      forallb
        (fun instr =>
          negb (existsb same (args_of_instr instr)) || approve_instr instr)
        (block_body block)
      && forallb
           (fun operands =>
             negb (existsb same operands) || approve_term block operands)
           (terminator_args (terminator block)))
    blocks.

(* Visit roots from left to right, threading the completed set. *)
Fixpoint visit_all (visit : blockset -> block -> option blockset)
    (seen : blockset) (roots : list block) : option blockset :=
  match roots with
  | [] => Some seen
  | block :: roots =>
    let* seen := visit seen block in visit_all visit seen roots
  end.

(* A depth-first acyclicity check on [region] without [cut]. [active] holds the
   current path, so a cycle is rejected immediately; [seen] memoizes completed
   blocks, so shared successors are traversed only once. Fuel bounds recursive
   depth for Rocq; the graph's block list is enough for any acyclic region. *)
Fixpoint visit_acyclic (fuel : list block) (region : blockset) (cut : block)
    (active seen : blockset) (block : block) : option blockset :=
  if block_eqb block cut || negb (set_mem block region) || set_mem block seen
  then Some seen
  else if set_mem block active then None
  else
    match fuel with
    | [] => None
    | _ :: fuel =>
      let* seen := visit_all
        (visit_acyclic fuel region cut (set_add block active)) seen
        (all_successors block) in
      Some (set_add block seen)
    end.

(* Check every block of the region, not just the part reached from [cut]. The
   final coverage check prevents missing roots from hiding a cycle. *)
Definition acyclic_except (blocks : list block) (region : blockset)
    (cut : block) : bool :=
  match visit_all (visit_acyclic blocks region cut set_empty)
    set_empty blocks with
  | None => false
  | Some seen =>
    set_for_all (fun block => block_eqb block cut || set_mem block seen) region
  end.
