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

(* See [termination.mli] for the interface. *)

open Ssa.Export
module Affine = Fourier_motzkin.Affine

type t =
  | Terminates
  | Unknown

type exit_branch =
  { condition : finished Value.t;
    continue_when_true : bool;
    exit_target : finished Block.t
  }

(* The exit branch must be the loop header's two-target [Switch] terminator
   ([targets.(0)] the false edge, [targets.(1)] the true edge), with exactly one
   of the two targets inside the loop and the other outside (if both are in or
   both are out, we give up). *)
let find_exit_branch (loop : Induction_var.loop) : exit_branch option =
  match[@warning "-fragile-match"] Block.terminator loop.header with
  | Switch { index; targets = [| ifnot; ifso |] } -> (
    let true_in = Block.Set.mem ifso loop.body in
    let false_in = Block.Set.mem ifnot loop.body in
    match true_in, false_in with
    | true, false ->
      Some { condition = index; continue_when_true = true; exit_target = ifnot }
    | false, true ->
      Some { condition = index; continue_when_true = false; exit_target = ifso }
    | true, true | false, false -> None)
  | _ -> None

(* Discharge {!Loop_comparisons.Terminates_if_bound_in_range}: the bound's
   machine value must satisfy [b <= max_int] (positive step) or [b >= -max_int]
   (negative step), which keeps the IV's increment from ever wrapping at 64 bits
   (see the argument in {!Loop_comparisons}). Literal bounds are checked
   directly; register bounds are proved by Fourier-Motzkin from the guards
   dominating the loop header — e.g. a bound compared against an array length
   (whose [lsr]-of-header shape yields a [<= 2^54 - 1] range fact), an untagged
   OCaml integer (whose [asr 1] shape is bounded by [max_int]), or any other
   dominating range check. *)
let bound_in_range ~step ~(header : finished Block.t)
    (bound : [`Value of finished Value.t | `Const of int]) : bool =
  match bound with
  | `Const k -> if step > 0 then k <= max_int else k >= -max_int
  | `Value v -> (
    let ctx = Affine_ssa.new_ctx () in
    let side = ref [] in
    let form = Affine_ssa.linearize ctx side v in
    let facts = Affine_ssa.guards_at ctx side header @ !side in
    match
      if step > 0
      then Affine.add_const_checked (Affine.scale_checked (-1) form) max_int
      else Affine.add_const_checked form max_int
    with
    | goal -> Fourier_motzkin.entails facts goal
    | exception Fourier_motzkin.Overflow -> false)

let biv_implies_termination ~op_def (biv : Induction_var.biv) : bool =
  match find_exit_branch biv.loop, Induction_var.signed_step biv with
  | None, _ | _, None -> false
  | Some exit_info, Some step -> (
    let header = biv.loop.header in
    let body = biv.loop.body in
    let is_self = Induction_var.is_header_param header biv.param_index in
    let is_iv_val v = is_self v || List.exists (Value.equal v) biv.update in
    (* The IV progresses monotonically, but that only forces the comparison to
       flip if the operand it is tested against stays put. A loop-variant other
       operand (e.g. a second counter in [while i < j]) can keep the comparison
       true forever, so we require it to be loop-invariant. *)
    let is_bound v = Induction_var.is_loop_invariant ~op_def body v in
    let extract =
      match[@warning "-fragile-match"] exit_info.condition with
      | Res ({ op = Intop_imm (Icomp cmp, k); args = [| x |]; _ }, _) ->
        if is_iv_val x then Some (cmp, true, `Const k) else None
      | Res ({ op = Intop (Icomp cmp); args = [| x; y |]; _ }, _) ->
        if is_iv_val x && is_bound y
        then Some (cmp, true, `Value y)
        else if is_iv_val y && is_bound x
        then Some (cmp, false, `Value x)
        else None
      | _ -> None
    in
    match extract with
    | None -> false
    | Some (cmp, iv_is_left, bound) -> (
      let continue_cmp =
        Loop_comparisons.oriented_continue_comparison ~iv_is_left
          ~continue_when_true:exit_info.continue_when_true cmp
      in
      match Loop_comparisons.continue_terminates ~step continue_cmp with
      | Terminates -> true
      | Terminates_if_bound_in_range -> bound_in_range ~step ~header bound
      | Unknown -> false))

let analyze ~op_def (_loop : Induction_var.loop) (bivs : Induction_var.biv list)
    : t =
  if List.exists (biv_implies_termination ~op_def) bivs
  then Terminates
  else Unknown
