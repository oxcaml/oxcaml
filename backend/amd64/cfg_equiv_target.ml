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
  | Arch.Ibased (left_name, left_glob, left_displ),
    Arch.Ibased (right_name, right_glob, right_displ) ->
    let left_sym =
      { Cmm.sym_name = left_name;
        sym_global = Arch.arch_to_cmm_global left_glob
      }
    in
    let right_sym =
      { Cmm.sym_name = right_name;
        sym_global = Arch.arch_to_cmm_global right_glob
      }
    in
    Cmm.equal_symbol (Cfg_equiv_subst.subst_symbol subst left_sym) right_sym
    && Int.equal left_displ right_displ
  | Arch.Iindexed left_displ, Arch.Iindexed right_displ ->
    Int.equal left_displ right_displ
  | Arch.Iindexed2 left_displ, Arch.Iindexed2 right_displ ->
    Int.equal left_displ right_displ
  | Arch.Iscaled (left_scale, left_displ),
    Arch.Iscaled (right_scale, right_displ) ->
    Int.equal left_scale right_scale && Int.equal left_displ right_displ
  | Arch.Iindexed2scaled (left_scale, left_displ),
    Arch.Iindexed2scaled (right_scale, right_displ) ->
    Int.equal left_scale right_scale && Int.equal left_displ right_displ
  | ( Arch.Ibased _ | Arch.Iindexed _ | Arch.Iindexed2 _ | Arch.Iscaled _
    | Arch.Iindexed2scaled _ ),
    _ ->
    false

(* CR xclerc: we may use the existing equality function defined in [Arch]. *)
let equiv_bswap_bitwidth
    (left : Arch.bswap_bitwidth) (right : Arch.bswap_bitwidth) =
  match left, right with
  | Arch.Sixteen, Arch.Sixteen -> true
  | Arch.Thirtytwo, Arch.Thirtytwo -> true
  | Arch.Sixtyfour, Arch.Sixtyfour -> true
  | (Arch.Sixteen | Arch.Thirtytwo | Arch.Sixtyfour), _ -> false

(* CR xclerc: we may use the existing equality function defined in [Arch]. *)
let equiv_float_operation
    (left : Arch.float_operation) (right : Arch.float_operation) =
  match left, right with
  | Arch.Ifloatadd, Arch.Ifloatadd -> true
  | Arch.Ifloatsub, Arch.Ifloatsub -> true
  | Arch.Ifloatmul, Arch.Ifloatmul -> true
  | Arch.Ifloatdiv, Arch.Ifloatdiv -> true
  | (Arch.Ifloatadd | Arch.Ifloatsub | Arch.Ifloatmul | Arch.Ifloatdiv), _ ->
    false

(* CR xclerc: we may use the existing equality function defined in [Arch]. *)
let equiv_prefetch_temporal_locality_hint
    (left : Arch.prefetch_temporal_locality_hint)
    (right : Arch.prefetch_temporal_locality_hint) =
  match left, right with
  | Arch.Nonlocal, Arch.Nonlocal -> true
  | Arch.Low, Arch.Low -> true
  | Arch.Moderate, Arch.Moderate -> true
  | Arch.High, Arch.High -> true
  | (Arch.Nonlocal | Arch.Low | Arch.Moderate | Arch.High), _ -> false

let equiv_specific_operation subst
    (left : Arch.specific_operation) (right : Arch.specific_operation) : bool =
  let equiv_addr = equiv_addressing_mode subst in
  match left, right with
  | Arch.Ilea left_addr, Arch.Ilea right_addr ->
    equiv_addr left_addr right_addr
  | Arch.Istore_int (left_n, left_addr, left_b),
    Arch.Istore_int (right_n, right_addr, right_b) ->
    Nativeint.equal left_n right_n
    && equiv_addr left_addr right_addr
    && Bool.equal left_b right_b
  | Arch.Ioffset_loc (left_n, left_addr),
    Arch.Ioffset_loc (right_n, right_addr) ->
    Int.equal left_n right_n && equiv_addr left_addr right_addr
  | Arch.Ifloatarithmem (left_width, left_fop, left_addr),
    Arch.Ifloatarithmem (right_width, right_fop, right_addr) ->
    Cmm.equal_float_width left_width right_width
    && equiv_float_operation left_fop right_fop
    && equiv_addr left_addr right_addr
  | Arch.Ibswap { bitwidth = left_bw }, Arch.Ibswap { bitwidth = right_bw } ->
    equiv_bswap_bitwidth left_bw right_bw
  | Arch.Isextend32, Arch.Isextend32 -> true
  | Arch.Izextend32, Arch.Izextend32 -> true
  | Arch.Irdtsc, Arch.Irdtsc -> true
  | Arch.Irdpmc, Arch.Irdpmc -> true
  | Arch.Ilfence, Arch.Ilfence -> true
  | Arch.Isfence, Arch.Isfence -> true
  | Arch.Imfence, Arch.Imfence -> true
  | Arch.Ipackf32, Arch.Ipackf32 -> true
  | Arch.Icldemote left_addr, Arch.Icldemote right_addr ->
    equiv_addr left_addr right_addr
  | Arch.Iprefetch
      { is_write = left_is_write;
        locality = left_locality;
        addr = left_addr
      },
    Arch.Iprefetch
      { is_write = right_is_write;
        locality = right_locality;
        addr = right_addr
      } ->
    Bool.equal left_is_write right_is_write
    && equiv_prefetch_temporal_locality_hint left_locality right_locality
    && equiv_addr left_addr right_addr
  | Arch.Isimd left_op, Arch.Isimd right_op ->
    (* CR xclerc: rework this case. *)
    Simd.equal_operation left_op right_op
  | Arch.Isimd_mem (left_op, left_addr),
    Arch.Isimd_mem (right_op, right_addr) ->
    (* CR xclerc: rework this case. *)
    Simd.Mem.equal_operation left_op right_op
    && equiv_addr left_addr right_addr
  | Arch.Illvm_intrinsic left_s, Arch.Illvm_intrinsic right_s ->
    String.equal left_s right_s
  | ( Arch.Ilea _ | Arch.Istore_int _ | Arch.Ioffset_loc _
    | Arch.Ifloatarithmem _ | Arch.Ibswap _ | Arch.Isextend32 | Arch.Izextend32
    | Arch.Irdtsc | Arch.Irdpmc | Arch.Ilfence | Arch.Isfence | Arch.Imfence
    | Arch.Ipackf32 | Arch.Isimd _ | Arch.Isimd_mem _ | Arch.Icldemote _
    | Arch.Iprefetch _ | Arch.Illvm_intrinsic _ ),
    _ ->
    false
