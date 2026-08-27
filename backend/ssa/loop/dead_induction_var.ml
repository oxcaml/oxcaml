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

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

(* See [dead_induction_var.mli] for the interface. *)

open Ssa.Export

type biv_result =
  { biv : Induction_var.biv;
    dead : bool
  }

type loop_result =
  { loop : Induction_var.loop;
    bivs : biv_result list;
    useless : bool
  }

(* Whether [i] is the [Op] defining (any result of) the value [v]. *)
let instr_defines_value (i : finished Instruction.t) (v : finished Value.t) :
    bool =
  match i, v with
  | Op { id; _ }, Res ({ id = id'; _ }, _) -> Instruction.Id.equal id id'
  | (Op _ | Push_trap _ | Pop_trap _), (Res _ | Block_param _) -> false

(* The shallow argument array of an instruction, i.e. every SSA value it
   directly references. *)
let args_of_instr (i : finished Instruction.t) : finished Value.t array =
  match i with Op { args; _ } -> args | Push_trap _ | Pop_trap _ -> [||]

(* All SSA-value arguments referenced by a terminator. [Continue]'s optional
   args are flattened, dropping the [Omitted_since_unused] markers. *)
let terminator_args (term : finished Terminator.t) : finished Value.t array list
    =
  match term with
  | Continue { args; _ } ->
    [ Array.to_list args
      |> List.filter_map (fun (arg : finished Terminator.arg) ->
          match arg with Arg v -> Some v | Omitted_since_unused -> None)
      |> Array.of_list ]
  | Switch { index; _ } -> [[| index |]]
  | Call { args; _ } -> [args]
  | Invalid { args; _ } -> [args]

let count_occurrences ~same arr =
  let n = ref 0 in
  Array.iter (fun x -> if same x then incr n) arr;
  !n

(* [all_uses_ok ssa ~same ~approve_instr ~approve_term] walks every instruction
   and every terminator and returns [true] iff every SSA-reference matching
   [same] is approved. For instruction consumers we consult [approve_instr]. For
   terminator references we consult [approve_term], which also receives the
   enclosing block so it can tell back-edge gotos from other control
   transfers. *)
let all_uses_ok (ssa : finished Ssa.graph) ~same ~approve_instr ~approve_term =
  let ok = ref true in
  List.iter
    (fun (bl : finished Block.t) ->
      Array.iter
        (fun (i : finished Instruction.t) ->
          if Array.exists same (args_of_instr i) && not (approve_instr i)
          then ok := false)
        (Block.body bl);
      List.iter
        (fun arr ->
          if Array.exists same arr && not (approve_term bl arr) then ok := false)
        (terminator_args (Block.terminator bl)))
    (Ssa.blocks ssa);
  !ok

let is_dead (ssa : finished Ssa.graph) (biv : Induction_var.biv) : bool =
  match Termination.find_exit_branch biv.loop with
  | None -> false
  | Some { condition = cond; continue_when_true = _; exit_target = _ } ->
    let header = biv.loop.header in
    let k = biv.param_index in
    let is_self = Induction_var.is_header_param header k in
    let back_preds = Induction_var.back_edge_set biv.loop in
    (* Uses of [self]: approved only when the consumer is the exit comparison or
       one of the update expressions. [self] must not appear as a direct
       argument of any terminator. *)
    let self_ok =
      all_uses_ok ssa ~same:is_self
        ~approve_instr:(fun i ->
          instr_defines_value i cond
          || List.exists (instr_defines_value i) biv.update)
        ~approve_term:(fun _ _ -> false)
    in
    if not self_ok
    then false
    else
      (* Uses of each back-edge update value: approved only as the argument of
         the exit comparison, or as the back-edge [Goto]'s argument at position
         [k] (and not at any other position of that goto). *)
      List.for_all
        (fun v ->
          all_uses_ok ssa ~same:(Value.equal v)
            ~approve_instr:(fun i -> instr_defines_value i cond)
            ~approve_term:(fun bl _arr ->
              Block.Set.mem bl back_preds
              &&
              match Block.terminator bl with
              | Continue { continuation = Goto goto; args } ->
                Block.equal goto header
                && Array.length args > k
                && (match args.(k) with
                  | Arg a -> Value.equal a v
                  | Omitted_since_unused -> false)
                && count_occurrences
                     ~same:(fun (arg : finished Terminator.arg) ->
                       match arg with
                       | Arg a -> Value.equal a v
                       | Omitted_since_unused -> false)
                     args
                   = 1
              | Continue { continuation = Return | Raise _ | Unreachable; _ }
              | Switch _ | Call _ | Invalid _ ->
                false))
        biv.update

let loop_is_useless (loop : Induction_var.loop) (bivs : biv_result list) =
  Array.length (Block.params loop.header) = List.length bivs
  && List.for_all (fun br -> br.dead) bivs

let analyze (ssa : finished Ssa.graph)
    (loops : (Induction_var.loop * Induction_var.biv list) list) :
    loop_result list =
  List.map
    (fun (loop, bivs) ->
      let bivs = List.map (fun biv -> { biv; dead = is_dead ssa biv }) bivs in
      let useless = loop_is_useless loop bivs in
      { loop; bivs; useless })
    loops
