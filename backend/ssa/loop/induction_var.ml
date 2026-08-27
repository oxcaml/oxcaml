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

(* See [induction_var.mli] for the interface. *)

open Ssa.Export

type loop = Natural_loop.loop =
  { header : finished Block.t;
    body : Block.Set.t;
    back_edges : finished Block.t list
  }

type op_def = (finished, finished Block.t) Instruction.Id.Tbl.t

let compute_op_def (ssa : finished Ssa.graph) : op_def =
  let tbl : op_def = Instruction.Id.Tbl.create 64 in
  List.iter
    (fun (bl : finished Block.t) ->
      Array.iter
        (fun (i : finished Instruction.t) ->
          match i with
          | Op { id; _ } -> Instruction.Id.Tbl.replace tbl id bl
          | Push_trap _ | Pop_trap _ -> ())
        (Block.body bl))
    (Ssa.blocks ssa);
  tbl

(* Is [v] the [Block_param] for parameter [index] of [block]? *)
let is_header_param (block : finished Block.t) (index : int)
    (v : finished Value.t) : bool =
  Value.equal v (Block.param block index)

(* A value is loop-invariant wrt [loop_body] if its defining block lies outside
   the body, or it is a compile-time constant. We do not try to hoist more
   generally. *)
let is_loop_invariant ~(op_def : op_def) (loop_body : Block.Set.t)
    (v : finished Value.t) : bool =
  match[@warning "-fragile-match"] v with
  | Res
      ( { op =
            ( Const_int _ | Const_float _ | Const_float32 _ | Const_symbol _
            | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ );
          _
        },
        _ ) ->
    true
  | Res ({ id; _ }, _) -> (
    match Instruction.Id.Tbl.find_opt op_def id with
    | None -> false
    | Some bl -> not (Block.Set.mem bl loop_body))
  | Block_param (block, _) -> not (Block.Set.mem block loop_body)

type step =
  | Step_const of int
  | Step_var of finished Value.t

type biv =
  { loop : loop;
    param_index : int;
    update : finished Value.t list;
    step : step;
    sign : [`Add | `Sub]
  }

let step_same a b =
  match a, b with
  | Step_const x, Step_const y -> Int.equal x y
  | Step_var x, Step_var y -> Value.equal x y
  | Step_const _, Step_var _ | Step_var _, Step_const _ -> false

let sign_same (a : [`Add | `Sub]) (b : [`Add | `Sub]) =
  match a, b with `Add, `Add | `Sub, `Sub -> true | (`Add | `Sub), _ -> false

(* The signed per-iteration step of a constant-step BIV. *)
let signed_step (biv : biv) : int option =
  match biv.step with
  | Step_const c -> Some (match biv.sign with `Add -> c | `Sub -> -c)
  | Step_var _ -> None

(* Given [is_self] = "is the header block_param", decide whether [v] is of the
   form [self + c], [c + self] or [self - c] for loop-invariant [c]. *)
let classify_update ~is_self ~op_def ~loop_body (v : finished Value.t) :
    (step * [`Add | `Sub]) option =
  match[@warning "-fragile-match"] v with
  | Res ({ op = Intop_imm (Iadd, c); args = [| x |]; _ }, _) when is_self x ->
    Some (Step_const c, `Add)
  | Res ({ op = Intop_imm (Isub, c); args = [| x |]; _ }, _) when is_self x ->
    Some (Step_const c, `Sub)
  | Res ({ op = Intop Iadd; args = [| x; y |]; _ }, _) ->
    if is_self x && is_loop_invariant ~op_def loop_body y
    then Some (Step_var y, `Add)
    else if is_self y && is_loop_invariant ~op_def loop_body x
    then Some (Step_var x, `Add)
    else None
  | Res ({ op = Intop Isub; args = [| x; y |]; _ }, _) ->
    if is_self x && is_loop_invariant ~op_def loop_body y
    then Some (Step_var y, `Sub)
    else None
  | _ -> None

let back_edge_set (loop : loop) : Block.Set.t =
  List.fold_left (fun s b -> Block.Set.add b s) Block.Set.empty loop.back_edges

(* Collect the argument array of [Continue (Goto header)] terminators targeting
   the header. Args are optional: an [Omitted_since_unused] marks a parameter
   the framework dropped as unused. *)
let pred_args_to_header ~(header : finished Block.t) (pred : finished Block.t) :
    finished Value.t option array option =
  match[@warning "-fragile-match"] Block.terminator pred with
  | Continue { continuation = Goto goto; args }
    when Block.equal goto header
         && Array.length args = Array.length (Block.params header) ->
    Some
      (Array.map
         (fun (arg : finished Terminator.arg) ->
           match arg with Arg v -> Some v | Omitted_since_unused -> None)
         args)
  | Continue _ | Switch _ | Call _ | Invalid _ -> None

let analyze_loop ~op_def (loop : loop) : biv list =
  let { header; body; back_edges = _ } = loop in
  let back_set = back_edge_set loop in
  let pred_args =
    List.map
      (fun p -> p, pred_args_to_header ~header p)
      (Block.predecessors header)
  in
  (* Every back edge must contribute a positional argument array; a back edge
     arriving some other way (e.g. as a [Call] continuation) passes values the
     analysis below would not see, so no parameter can be classified. *)
  let all_back_edges_have_args =
    List.for_all
      (fun ((p : finished Block.t), args) ->
        (not (Block.Set.mem p back_set)) || Option.is_some args)
      pred_args
  in
  if not all_back_edges_have_args
  then []
  else begin
    let back_pred_args =
      List.filter_map
        (fun ((p : finished Block.t), args) ->
          if Block.Set.mem p back_set
          then Option.map (fun args -> p, args) args
          else None)
        pred_args
    in
    let bivs = ref [] in
    Array.iteri
      (fun index _ ->
        let is_self = is_header_param header index in
        let back_vals_opt =
          List.map (fun (_, args) -> args.(index)) back_pred_args
        in
        if List.for_all Option.is_some back_vals_opt
        then
          let back_vals = List.map Option.get back_vals_opt in
          match back_vals with
          | [] -> ()
          | first :: rest -> (
            match classify_update ~is_self ~op_def ~loop_body:body first with
            | None -> ()
            | Some (step, sign) ->
              let agrees =
                List.for_all
                  (fun v ->
                    match
                      classify_update ~is_self ~op_def ~loop_body:body v
                    with
                    | Some (step', sign') ->
                      step_same step step' && sign_same sign sign'
                    | None -> false)
                  rest
              in
              if agrees
              then
                bivs
                  := { loop;
                       param_index = index;
                       update = back_vals;
                       step;
                       sign
                     }
                     :: !bivs))
      (Block.params header);
    List.rev !bivs
  end

let analyze ~op_def (ssa : finished Ssa.graph) : (loop * biv list) list =
  List.map
    (fun loop -> loop, analyze_loop ~op_def loop)
    (Natural_loop.find_loops ssa)
