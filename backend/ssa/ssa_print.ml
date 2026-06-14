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

(** Block- and graph-level pretty-printers for a finished {!Ssa.graph}. The
    instruction, terminator and block-id printers themselves live on the
    [Ssa.Block], [Ssa.Instruction] and [Ssa.Terminator] modules. *)

open Ssa.Export

type block = finished Block.t

let print_typed_params ppf (blk : block) =
  Array.iteri
    (fun i p ->
      if i > 0 then Format.fprintf ppf ", ";
      Value.print ppf p;
      Format.fprintf ppf " : %a" Printcmm.machtype_component (Value.typ p))
    (Block.params blk)

let print_block_header ppf (blk : block) =
  Format.fprintf ppf "%a(%a)" Block.print_id blk print_typed_params blk;
  Format.fprintf ppf " [dominator=%a depth=%d]" Block.print_id
    (Block.immediate_dominator blk)
    (Block.dominator_depth blk);
  match Block.predecessors blk with
  | [] -> ()
  | preds ->
    Format.fprintf ppf " <- %a"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
         Block.print_id)
      preds

let print_block ppf (blk : block) =
  Format.fprintf ppf "%a@." print_block_header blk;
  Array.iter
    (fun bi -> Format.fprintf ppf "  %a@." Instruction.print bi)
    (Block.body blk);
  Format.fprintf ppf "  %a" Terminator.print (Block.terminator blk);
  (match Block.exn_successor blk with
  | None -> ()
  | Some h -> Format.fprintf ppf " exn_successor=%a" Block.print_id h);
  Format.fprintf ppf "@.@."

let print_parameter ppf (var, typ) =
  Format.fprintf ppf "%a : %a" Backend_var.With_provenance.print var
    Printcmm.machtype typ

let print ppf (g : Ssa.finished Ssa.graph) =
  let function_info = Ssa.function_info g in
  Format.fprintf ppf "SSA for %s(%a) : %a@.@." function_info.sym_name
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
       print_parameter)
    function_info.parameters Printcmm.machtype function_info.ret_type;
  List.iter (print_block ppf) (Ssa.blocks g);
  Format.fprintf ppf "@."
