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

(* Extraction directives for SsaModel.v: which type and function of the
   compiler stand for each type and observation of the model. As in
   OxCamlExtract.v (rocq/), nothing here is proved; the mapping is trusted by
   inspection.

   The inductive types standing for the compiler's values, instructions and
   terminators are matched through the function given as the last argument of
   their [Extract Inductive], which reads the compiler's data (turning an array
   into a list on the way). The models never build such data, and could not, as
   the compiler's types are private, so the constructor names listed are never
   printed. *)

From Stdlib Require Import Extraction.
From OxCamlSsa Require Import SsaModel.

(* The types and their equalities. *)
Extract Constant block => "Ssa.finished Ssa.Block.t".
Extract Constant instr_id => "Ssa.finished Ssa.Instruction.Id.t".
Extract Inductive value => "Ssa.finished Ssa.Value.t"
  [ "Ssa.Instruction.Res" "Ssa.Instruction.Block_param" ]
  "(fun f_res f_param (v : Ssa.finished Ssa.Value.t) ->
      match v with
      | Ssa.Instruction.Res ({ Ssa.Instruction.id; op; args; _ }, i) ->
        f_res id op (Array.to_list args) i
      | Ssa.Instruction.Block_param (b, i) -> f_param b i)".
Extract Inductive instr => "Ssa.finished Ssa.Instruction.t"
  [ "Ssa.Instruction.Op" "Ssa.Instruction.Push_trap"
    "Ssa.Instruction.Pop_trap" ]
  "(fun f_op f_push_trap f_pop_trap (i : Ssa.finished Ssa.Instruction.t) ->
      match i with
      | Ssa.Instruction.Op { Ssa.Instruction.id; op; args; typ; _ } ->
        f_op id op (Array.to_list args) (Array.length typ)
      | Ssa.Instruction.Push_trap _ -> f_push_trap ()
      | Ssa.Instruction.Pop_trap _ -> f_pop_trap ())".
Extract Inlined Constant block_eqb => "Ssa.Block.equal".
Extract Inlined Constant value_eqb => "Ssa.Value.equal".
Extract Inlined Constant instr_id_eqb => "Ssa.Instruction.Id.equal".

(* Sets of blocks. *)
Extract Constant blockset => "Ssa.Block.Set.t".
Extract Inlined Constant set_empty => "Ssa.Block.Set.empty".
Extract Inlined Constant set_singleton => "Ssa.Block.Set.singleton".
Extract Inlined Constant set_add => "Ssa.Block.Set.add".
Extract Inlined Constant set_mem => "Ssa.Block.Set.mem".
Extract Inlined Constant set_for_all => "Ssa.Block.Set.for_all".

(* Blocks. *)
Extract Inlined Constant predecessors => "Ssa.Block.predecessors".
Extract Inlined Constant all_successors => "Ssa.Block.all_successors".
Extract Inlined Constant dominates => "Ssa.Block.dominates".
Extract Inlined Constant strict_dominators => "Ssa.Block.strict_dominators".
Extract Constant params => "fun b -> Array.to_list (Ssa.Block.params b)".
Extract Inlined Constant param => "Ssa.Block.param".
Extract Constant block_body => "fun b -> Array.to_list (Ssa.Block.body b)".

(* Terminators: [continuation] and [Terminator.arg] are the compiler's own
   types, whose constructors must be listed in the same order as in
   backend/ssa/ssa.mli; [Terminator.t] is matched through its function. *)
Extract Constant raise_kind => "Lambda.raise_kind".
Extract Inductive continuation => "Ssa.finished Ssa.continuation"
  [ "Ssa.Goto" "Ssa.Return" "Ssa.Raise" "Ssa.Unreachable" ].
Extract Inductive Terminator.arg => "Ssa.finished Ssa.Terminator.arg"
  [ "Ssa.Terminator.Arg" "Ssa.Terminator.Omitted_since_unused" ].
Extract Inductive Terminator.t => "Ssa.finished Ssa.Terminator.t"
  [ "Ssa.Terminator.Continue" "Ssa.Terminator.Switch" "Ssa.Terminator.Call"
    "Ssa.Terminator.Invalid" ]
  "(fun f_continue f_switch f_call f_invalid
        (t : Ssa.finished Ssa.Terminator.t) ->
      match t with
      | Ssa.Terminator.Continue { continuation; args } ->
        f_continue continuation (Array.to_list args)
      | Ssa.Terminator.Switch { index; targets } ->
        f_switch index (Array.to_list targets)
      | Ssa.Terminator.Call { args; _ } -> f_call (Array.to_list args)
      | Ssa.Terminator.Invalid { args; _ } -> f_invalid (Array.to_list args))".
Extract Inlined Constant terminator => "Ssa.Block.terminator".

(* The table the pass builds (delete_empty_loops.ml). *)
Extract Constant op_def =>
  "(Ssa.finished, Ssa.finished Ssa.Block.t) Ssa.Instruction.Id.Tbl.t".
Extract Inlined Constant op_def_find => "Ssa.Instruction.Id.Tbl.find_opt".
