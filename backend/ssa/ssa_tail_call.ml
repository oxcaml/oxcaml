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

open Ssa.Export
open Ssa_reducer

(** Detects [Call] terminators whose continuation does nothing but [Return] the
    call's results, and whose enclosing block has an empty trap stack. Rewrites
    them as a self-recursive [Continue (Goto entry)] back-edge (when the callee
    is the current function by name) or a tail [Call] (continuation [Return]).

    The per-call predicates (empty trap stack, [stack_offsets_zero], no stack
    check for self-calls) are equivalent to the checks in
    [Cfg_selectgen.emit_tail_apply]. The detection, however, is structural
    rather than syntactic: any call whose continuation block merely returns the
    results unchanged is accepted, which covers shapes such as
    [Clet (x, Capply ..., Cvar x)] in tail position that [Cfg_selectgen]
    compiles as a normal call followed by a return. Such shapes would show up as
    a [Cfg_compare] mismatch under [-ssa-validate]; they do not seem to be
    produced in practice. *)
module Tail_call_reducer = struct
  open! Context
  include Default_reducer (Default_analyzer)

  let returns_args_unchanged (block : finished Block.t) : bool =
    match[@warning "-fragile-match"] Block.terminator block with
    | Continue { continuation = Return; args } ->
      (* Skipping the block must not lose any observable effect; effect-free
         instructions (including debug-info markers) are fine since their
         results are not returned. *)
      Array.for_all
        (fun instr -> not (Instruction.has_side_effect instr))
        (Block.body block)
      && Array.equal Value.equal args (Block.params block)
    | _ -> false

  let stack_offsets_zero (call_op : Ssa.call_op) (args : finished Value.t array)
      (ret_ty : Cmm.machtype) : bool =
    let real_args =
      match call_op with
      | Indirect _ -> Array.sub args 1 (Array.length args - 1)
      | Direct _ | External _ | Probe _ -> args
    in
    let arg_types = Array.map Value.typ real_args in
    let _, stack_ofs_args = Proc.loc_arguments arg_types in
    let _, stack_ofs_res = Proc.loc_results_call ret_ty in
    stack_ofs_args = 0 && stack_ofs_res = 0

  let visit_terminator () ctx (block : finished Block.t) (c : Cursor.t) =
    match[@warning "-fragile-match"] Block.terminator block with
    | Call
        ({ op = (Direct _ | Indirect _) as call_op;
           args;
           continuation = Goto cont;
           may_raise = _;
           nontail = false
         } as operation)
      when List.is_empty (Block.block_end_trap_stack block)
           && returns_args_unchanged cont -> (
      let dbg = Block.terminator_dbg block in
      match call_op with
      | Direct func
        when String.equal func.sym_name
               (Ssa.function_info (in_graph ctx)).sym_name ->
        (* A self-recursive tail call is a back-edge to the entry block. Build
           it over input values/blocks and let [map_terminator] translate it. *)
        Cursor.finish_block ctx c ~dbg
          (map_terminator ctx
             (Continue { continuation = Goto (Ssa.entry (in_graph ctx)); args }));
        Emitted_replacement ()
      | (Direct _ | Indirect _)
        when stack_offsets_zero call_op args (Block.params_machtype cont) ->
        Cursor.finish_block ctx c ~dbg
          (map_terminator ctx (Call { operation with continuation = Return }));
        Emitted_replacement ()
      | Direct _ | Indirect _ -> For_next_reducer
      | External _ | Probe _ -> assert false)
    | _ -> For_next_reducer
end

module Runner = Make_run (Tail_call_reducer)

let run ~keep_unused_ops ssa = Runner.run ~keep_unused_ops ssa
