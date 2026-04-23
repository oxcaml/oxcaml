(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2026 Jane Street Group LLC                                       *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]

let equiv_addressing_mode subst
    (left : Arch.addressing_mode) (right : Arch.addressing_mode) : bool =
  match left, right with
  | Arch.Ibased (left_asm_sym, left_displ),
    Arch.Ibased (right_asm_sym, right_displ) ->
    let left_sym =
      { Cmm.sym_name = Asm_targets.Asm_symbol.encode left_asm_sym;
        sym_global =
          Arch.asm_to_cmm_global
            (Asm_targets.Asm_symbol.visibility left_asm_sym)
      }
    in
    let right_sym =
      { Cmm.sym_name = Asm_targets.Asm_symbol.encode right_asm_sym;
        sym_global =
          Arch.asm_to_cmm_global
            (Asm_targets.Asm_symbol.visibility right_asm_sym)
      }
    in
    Cmm.equal_symbol
      (Cfg_equiv_subst.subst_symbol subst left_sym) right_sym
    && Int.equal left_displ right_displ
  | Arch.Iindexed _, Arch.Iindexed _ ->
    Arch.equal_addressing_mode left right
  | (Arch.Ibased _ | Arch.Iindexed _), _ -> false

(* CR xclerc: rework this function. *)
let equiv_specific_operation _subst
    (left : Arch.specific_operation) (right : Arch.specific_operation) : bool =
  Arch.equal_specific_operation left right
