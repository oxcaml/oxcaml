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

(* The predicates below intentionally destructure each constructor and record
   field, and avoid delegating to the [Arch.equal_*] helpers. The point is to
   force a compile error if the architecture types grow a new constructor or
   field that needs to be routed through the substitution: a delegating call
   to [equal_*] would silently keep working and quietly miss the new case. For
   the same reason, we do not build a value on each side and then compare them
   with an existing equality function; instead we destructure the substituted
   value and check its components directly. *)

module Validated_mem_offset = Arm64_ast.Ast.DSL.Validated_mem_offset

let equiv_addressing_mode subst
    (left : Arch.addressing_mode) (right : Arch.addressing_mode) : bool =
  match left, right with
  | Arch.Ibased (left_asm_sym, left_displ),
    Arch.Ibased (right_asm_sym, right_displ) ->
    let { Cmm.sym_name = left_subst_name; sym_global = left_subst_global } =
      Cfg_equiv_subst.subst_symbol subst
        { Cmm.sym_name = Asm_targets.Asm_symbol.encode left_asm_sym;
          sym_global =
            Arch.asm_to_cmm_global
              (Asm_targets.Asm_symbol.visibility left_asm_sym)
        }
    in
    String.equal left_subst_name (Asm_targets.Asm_symbol.encode right_asm_sym)
    && Cmm.equal_is_global left_subst_global
         (Arch.asm_to_cmm_global
            (Asm_targets.Asm_symbol.visibility right_asm_sym))
    && Int.equal left_displ right_displ
  | Arch.Iindexed left_v, Arch.Iindexed right_v ->
    Int.equal
      (Validated_mem_offset.offset left_v)
      (Validated_mem_offset.offset right_v)
    && Int.equal
         (Validated_mem_offset.scale left_v)
         (Validated_mem_offset.scale right_v)
  | (Arch.Ibased _ | Arch.Iindexed _), _ -> false

let equiv_arith_operation
    (left : Arch.arith_operation) (right : Arch.arith_operation) =
  match left, right with
  | Arch.Ishiftadd, Arch.Ishiftadd -> true
  | Arch.Ishiftsub, Arch.Ishiftsub -> true
  | (Arch.Ishiftadd | Arch.Ishiftsub), _ -> false

let equiv_bswap_bitwidth
    (left : Arch.bswap_bitwidth) (right : Arch.bswap_bitwidth) =
  match left, right with
  | Arch.Sixteen, Arch.Sixteen -> true
  | Arch.Thirtytwo, Arch.Thirtytwo -> true
  | Arch.Sixtyfour, Arch.Sixtyfour -> true
  | (Arch.Sixteen | Arch.Thirtytwo | Arch.Sixtyfour), _ -> false

let equiv_specific_operation _subst
    (left : Arch.specific_operation) (right : Arch.specific_operation) : bool =
  match left, right with
  | Arch.Ifar_poll, Arch.Ifar_poll -> true
  | ( Arch.Ifar_alloc { bytes = left_bytes; dbginfo = _ },
      Arch.Ifar_alloc { bytes = right_bytes; dbginfo = _ } ) ->
    (* [dbginfo] is intentionally ignored, mirroring the [Alloc] case in
       [Cfg_equiv.equiv_operation]. *)
    Int.equal left_bytes right_bytes
  | ( Arch.Ishiftarith (left_arith, left_amount),
      Arch.Ishiftarith (right_arith, right_amount) ) ->
    equiv_arith_operation left_arith right_arith
    && Int.equal left_amount right_amount
  | Arch.Imuladd, Arch.Imuladd
  | Arch.Imulsub, Arch.Imulsub
  | Arch.Inegmulf, Arch.Inegmulf
  | Arch.Imuladdf, Arch.Imuladdf
  | Arch.Inegmuladdf, Arch.Inegmuladdf
  | Arch.Imulsubf, Arch.Imulsubf
  | Arch.Inegmulsubf, Arch.Inegmulsubf
  | Arch.Isqrtf, Arch.Isqrtf
  | Arch.Imove32, Arch.Imove32 ->
    true
  | ( Arch.Ibswap { bitwidth = left_bw },
      Arch.Ibswap { bitwidth = right_bw } ) ->
    equiv_bswap_bitwidth left_bw right_bw
  | Arch.Isignext left_n, Arch.Isignext right_n -> Int.equal left_n right_n
  | Arch.Isimd left_op, Arch.Isimd right_op ->
    (* [Simd.operation] is a flat enum of leaf constructors that carry only
       integers or other leaf data, so no substitution applies. Using
       [Simd.equal_operation] is acceptable here. *)
    Simd.equal_operation left_op right_op
  | Arch.Illvm_intrinsic left_s, Arch.Illvm_intrinsic right_s ->
    String.equal left_s right_s
  | ( ( Arch.Ifar_poll | Arch.Ifar_alloc _ | Arch.Ishiftarith _ | Arch.Imuladd
      | Arch.Imulsub | Arch.Inegmulf | Arch.Imuladdf | Arch.Inegmuladdf
      | Arch.Imulsubf | Arch.Inegmulsubf | Arch.Isqrtf | Arch.Ibswap _
      | Arch.Imove32 | Arch.Isignext _ | Arch.Isimd _
      | Arch.Illvm_intrinsic _ ),
      _ ) ->
    false
