(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Max Slater, Jane Street                           *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-42"]

(* amd64 extension *)
type ext =
  | SSE
  | SSE2
  | SSE3
  | SSSE3
  | SSE4_1
  | SSE4_2
  | POPCNT
  | LZCNT
  | PCLMULQDQ
  | BMI
  | BMI2
  | AVX
  | AVX2
  | F16C
  | FMA
  | AVX512F
  | AVX512DQ
  | AVX512CD
  | AVX512BW
  | AVX512VL

(* Fixed machine register location *)
type reg =
  | RAX
  | RDI
  | RCX
  | RDX
  | XMM0

(* Flexible register or memory location *)
type temp =
  | R8
  | R16
  | R32
  | R64
  | M8
  | M16
  | M32
  | M64
  | M128
  | M256
  | M512
  | MM
  | XMM
  | YMM
  | ZMM
  | K (* AVX512 mask register (k0-k7) *)
  | VM32X (* R64 base + i32x4 offset *)
  | VM32Y (* R64 base + i32x8 offset *)
  | VM32Z (* R64 base + i32x16 offset *)
  | VM64X (* R64 base + i64x2 offset *)
  | VM64Y (* R64 base + i64x4 offset *)
  | VM64Z (* R64 base + i64x8 offset *)

(* Possible argument location *)
type loc =
  | Pin of reg
  | Temp of temp array (* All allowed locations *)

(* Possible argument encoding within an instruction *)
type loc_enc =
  | RM_r
  | RM_rm
  | Vex_v
  | Mask
  | Implicit
  | Immediate

type arg =
  { loc : loc;
    enc : loc_enc
  }

type res =
  | Res_none (* No result *)
  | Arg of int array (* Results are modified argument operand(s). *)
  | Res of arg array (* Separate operand(s) for result. *)

type legacy_prefix =
  | Prx_none
  | Prx_66
  | Prx_F2
  | Prx_F3

type legacy_rex =
  | Rex_none
  | Rex
  | Rex_w

type legacy_escape =
  | Esc_none
  | Esc_0F
  | Esc_0F38
  | Esc_0F3A

type vex_map =
  | Vexm_0F
  | Vexm_0F38
  | Vexm_0F3A

type evex_length =
  | L128
  | L256
  | L512

type evex_rounding =
  | Rnd_near
  | Rnd_down
  | Rnd_up
  | Rnd_zero

type evex_ll =
  | Ll_len of evex_length
  | Ll_round of evex_rounding

type prefix =
  | Legacy of
      { prefix : legacy_prefix;
        rex : legacy_rex;
        escape : legacy_escape;
        operand_size_override : bool
      }
  | Vex of
      { vex_m : vex_map;
        vex_w : bool;
        vex_l : bool;
        vex_p : legacy_prefix
      }
  | Evex of
      { evex_m : vex_map;
        evex_w : bool;
        evex_b : bool;
        evex_ll : evex_ll;
        evex_p : legacy_prefix;
        evex_z : bool
      }

type rm_reg =
  | Reg
  | Spec of int

type enc =
  { prefix : prefix;
    rm_reg : rm_reg;
    opcode : int
  }

type imm =
  | Imm_none
  | Imm_reg
  | Imm_spec

(* CR-someday gyorsh: restructure to avoid 'id and make the backend independent
   of simdgen, backend should only depend on the result of simdgen. *)
type 'id instr =
  { id : 'id;
    ext : ext array; (* Multiple extensions may be required. *)
    args : arg array;
    res : res;
    imm : imm;
    mnemonic : string;
    enc : enc
  }

let instr_expects_mask instr =
  Array.exists
    (fun (arg : arg) ->
      match arg.enc with
      | Mask -> true
      | RM_r | RM_rm | Vex_v | Implicit | Immediate -> false)
    instr.args

let instr_is_evex (instr : _ instr) =
  match instr.enc.prefix with Evex _ -> true | Legacy _ | Vex _ -> false

let equal_ext ext0 ext1 =
  match ext0, ext1 with
  | SSE, SSE
  | SSE2, SSE2
  | SSE3, SSE3
  | SSSE3, SSSE3
  | SSE4_1, SSE4_1
  | SSE4_2, SSE4_2
  | POPCNT, POPCNT
  | LZCNT, LZCNT
  | PCLMULQDQ, PCLMULQDQ
  | BMI, BMI
  | BMI2, BMI2
  | AVX, AVX
  | AVX2, AVX2
  | F16C, F16C
  | FMA, FMA
  | AVX512F, AVX512F
  | AVX512DQ, AVX512DQ
  | AVX512CD, AVX512CD
  | AVX512BW, AVX512BW
  | AVX512VL, AVX512VL ->
    true
  | ( ( SSE | SSE2 | SSE3 | SSSE3 | SSE4_1 | SSE4_2 | POPCNT | LZCNT | PCLMULQDQ
      | BMI | BMI2 | AVX | AVX2 | F16C | FMA | AVX512F | AVX512DQ | AVX512CD
      | AVX512BW | AVX512VL ),
      _ ) ->
    false

let equal_reg reg0 reg1 =
  match reg0, reg1 with
  | RAX, RAX | RDI, RDI | RCX, RCX | RDX, RDX | XMM0, XMM0 -> true
  | (RAX | RDI | RCX | RDX | XMM0), _ -> false

let equal_temp temp0 temp1 =
  match temp0, temp1 with
  | R8, R8
  | R16, R16
  | R32, R32
  | R64, R64
  | M8, M8
  | M16, M16
  | M32, M32
  | M64, M64
  | M128, M128
  | M256, M256
  | M512, M512
  | MM, MM
  | XMM, XMM
  | YMM, YMM
  | ZMM, ZMM
  | K, K
  | VM32X, VM32X
  | VM32Y, VM32Y
  | VM32Z, VM32Z
  | VM64X, VM64X
  | VM64Y, VM64Y
  | VM64Z, VM64Z ->
    true
  | ( ( R8 | R16 | R32 | R64 | M8 | M16 | M32 | M64 | M128 | M256 | M512 | MM
      | XMM | YMM | ZMM | K | VM32X | VM32Y | VM32Z | VM64X | VM64Y | VM64Z ),
      _ ) ->
    false

let equal_loc loc0 loc1 =
  match loc0, loc1 with
  | Pin reg0, Pin reg1 -> equal_reg reg0 reg1
  | Temp temp0, Temp temp1 ->
    Array.length temp0 = Array.length temp1
    && Array.for_all2 equal_temp temp0 temp1
  | (Pin _ | Temp _), _ -> false

let equal_loc_enc enc0 enc1 =
  match enc0, enc1 with
  | RM_r, RM_r
  | RM_rm, RM_rm
  | Vex_v, Vex_v
  | Mask, Mask
  | Implicit, Implicit
  | Immediate, Immediate ->
    true
  | (RM_r | RM_rm | Vex_v | Mask | Implicit | Immediate), _ -> false

let equal_arg (arg0 : arg) (arg1 : arg) =
  equal_loc arg0.loc arg1.loc && equal_loc_enc arg0.enc arg1.enc

let equal_legacy_prefix prefix0 prefix1 =
  match prefix0, prefix1 with
  | Prx_none, Prx_none | Prx_66, Prx_66 | Prx_F2, Prx_F2 | Prx_F3, Prx_F3 ->
    true
  | (Prx_none | Prx_66 | Prx_F2 | Prx_F3), _ -> false

let equal_legacy_rex rex0 rex1 =
  match rex0, rex1 with
  | Rex_none, Rex_none | Rex, Rex | Rex_w, Rex_w -> true
  | (Rex_none | Rex | Rex_w), _ -> false

let equal_legacy_escape escape0 escape1 =
  match escape0, escape1 with
  | Esc_none, Esc_none
  | Esc_0F, Esc_0F
  | Esc_0F38, Esc_0F38
  | Esc_0F3A, Esc_0F3A ->
    true
  | (Esc_none | Esc_0F | Esc_0F38 | Esc_0F3A), _ -> false

let equal_vex_map map0 map1 =
  match map0, map1 with
  | Vexm_0F, Vexm_0F | Vexm_0F38, Vexm_0F38 | Vexm_0F3A, Vexm_0F3A -> true
  | (Vexm_0F | Vexm_0F38 | Vexm_0F3A), _ -> false

let equal_evex_length length0 length1 =
  match length0, length1 with
  | L128, L128 | L256, L256 | L512, L512 -> true
  | (L128 | L256 | L512), _ -> false

let equal_evex_rounding rounding0 rounding1 =
  match rounding0, rounding1 with
  | Rnd_near, Rnd_near
  | Rnd_down, Rnd_down
  | Rnd_up, Rnd_up
  | Rnd_zero, Rnd_zero ->
    true
  | (Rnd_near | Rnd_down | Rnd_up | Rnd_zero), _ -> false

let equal_evex_ll ll0 ll1 =
  match ll0, ll1 with
  | Ll_len length0, Ll_len length1 -> equal_evex_length length0 length1
  | Ll_round rounding0, Ll_round rounding1 ->
    equal_evex_rounding rounding0 rounding1
  | (Ll_len _ | Ll_round _), _ -> false

let equal_prefix prefix0 prefix1 =
  match prefix0, prefix1 with
  | ( Legacy
        { prefix = prefix0;
          rex = rex0;
          escape = escape0;
          operand_size_override = operand_size_override0
        },
      Legacy
        { prefix = prefix1;
          rex = rex1;
          escape = escape1;
          operand_size_override = operand_size_override1
        } ) ->
    equal_legacy_prefix prefix0 prefix1
    && equal_legacy_rex rex0 rex1
    && equal_legacy_escape escape0 escape1
    && Bool.equal operand_size_override0 operand_size_override1
  | ( Vex { vex_m = vex_m0; vex_w = vex_w0; vex_l = vex_l0; vex_p = vex_p0 },
      Vex { vex_m = vex_m1; vex_w = vex_w1; vex_l = vex_l1; vex_p = vex_p1 } )
    ->
    equal_vex_map vex_m0 vex_m1
    && Bool.equal vex_w0 vex_w1 && Bool.equal vex_l0 vex_l1
    && equal_legacy_prefix vex_p0 vex_p1
  | ( Evex
        { evex_m = evex_m0;
          evex_w = evex_w0;
          evex_b = evex_b0;
          evex_ll = evex_ll0;
          evex_p = evex_p0;
          evex_z = evex_z0
        },
      Evex
        { evex_m = evex_m1;
          evex_w = evex_w1;
          evex_b = evex_b1;
          evex_ll = evex_ll1;
          evex_p = evex_p1;
          evex_z = evex_z1
        } ) ->
    equal_vex_map evex_m0 evex_m1
    && Bool.equal evex_w0 evex_w1 && Bool.equal evex_b0 evex_b1
    && equal_evex_ll evex_ll0 evex_ll1
    && equal_legacy_prefix evex_p0 evex_p1
    && Bool.equal evex_z0 evex_z1
  | (Legacy _ | Vex _ | Evex _), _ -> false

let equal_rm_reg rm_reg0 rm_reg1 =
  match rm_reg0, rm_reg1 with
  | Reg, Reg -> true
  | Spec spec0, Spec spec1 -> Int.equal spec0 spec1
  | (Reg | Spec _), _ -> false

let equal_enc enc0 enc1 =
  equal_prefix enc0.prefix enc1.prefix
  && equal_rm_reg enc0.rm_reg enc1.rm_reg
  && enc0.opcode = enc1.opcode

let equal_imm imm0 imm1 =
  match imm0, imm1 with
  | Imm_none, Imm_none | Imm_reg, Imm_reg | Imm_spec, Imm_spec -> true
  | (Imm_none | Imm_reg | Imm_spec), _ -> false

let equal_res res0 res1 =
  match res0, res1 with
  | Res_none, Res_none -> true
  | Arg arg1, Arg arg2 ->
    Array.length arg1 = Array.length arg2 && Array.for_all2 Int.equal arg1 arg2
  | Res res1, Res res2 ->
    Array.length res1 = Array.length res2 && Array.for_all2 equal_arg res1 res2
  | (Res_none | Arg _ | Res _), _ -> false

let temp_is_reg = function
  | R8 | R16 | R32 | R64 | MM | XMM | YMM | ZMM | K -> true
  | M8 | M16 | M32 | M64 | M128 | M256 | M512 | VM32X | VM32Y | VM32Z | VM64X
  | VM64Y | VM64Z ->
    false

let temp_is_vm = function
  | VM32X | VM32Y | VM32Z | VM64X | VM64Y | VM64Z -> true
  | R8 | R16 | R32 | R64 | MM | XMM | YMM | ZMM | K | M8 | M16 | M32 | M64
  | M128 | M256 | M512 ->
    false

let loc_allows_reg = function
  | Pin _ -> true
  | Temp temps -> Array.exists temp_is_reg temps

let loc_allows_mem = function
  | Pin _ -> false
  | Temp temps -> Array.exists (fun temp -> not (temp_is_reg temp)) temps

let loc_is_pinned = function Pin reg -> Some reg | Temp _ -> None

let loc_reg_count = function
  | Temp ts when Array.exists temp_is_vm ts -> 2
  | Temp _ | Pin _ -> 1

let unarized_reg_index args arg_idx =
  let idx = ref 0 in
  for i = 0 to arg_idx - 1 do
    idx := !idx + loc_reg_count args.(i).loc
  done;
  !idx

let arg_is_implicit ({ enc; _ } : arg) =
  match enc with
  | Implicit -> true
  | Immediate | RM_r | RM_rm | Vex_v | Mask -> false

let ext_to_string : ext -> string = function
  | SSE -> "SSE"
  | SSE2 -> "SSE2"
  | SSE3 -> "SSE3"
  | SSSE3 -> "SSSE3"
  | SSE4_1 -> "SSE4_1"
  | SSE4_2 -> "SSE4_2"
  | POPCNT -> "POPCNT"
  | LZCNT -> "LZCNT"
  | PCLMULQDQ -> "PCLMULQDQ"
  | BMI -> "BMI"
  | BMI2 -> "BMI2"
  | AVX -> "AVX"
  | AVX2 -> "AVX2"
  | F16C -> "F16C"
  | FMA -> "FMA"
  | AVX512F -> "AVX512F"
  | AVX512DQ -> "AVX512DQ"
  | AVX512CD -> "AVX512CD"
  | AVX512BW -> "AVX512BW"
  | AVX512VL -> "AVX512VL"

let exts_to_string exts =
  Array.map ext_to_string exts |> Array.to_list |> String.concat ", "

module Layout = struct
  type reg =
    | R8
    | R16
    | R32
    | R64
    | R128
    | R256
    | R512

  type mem =
    | M8
    | M16
    | M32
    | M64
    | M128
    | M256
    | M512
    | M32X
    | M64X
    | M32Y
    | M64Y
    | M32Z
    | M64Z
end

let loc_register_width = function
  | Pin _ -> None
  | Temp temps ->
    let width = ref None in
    let set w =
      assert (Option.is_none !width);
      width := Some w
    in
    Array.iter
      (function
        | R8 -> set Layout.R8
        | R16 -> set Layout.R16
        | R32 -> set Layout.R32
        | R64 | MM -> set Layout.R64
        | XMM -> set Layout.R128
        | YMM -> set Layout.R256
        | ZMM -> set Layout.R512
        | K | M8 | M16 | M32 | M64 | M128 | M256 | M512 | VM32X | VM32Y | VM32Z
        | VM64X | VM64Y | VM64Z ->
          ())
      temps;
    !width

let loc_memory_width = function
  | Pin _ -> assert false
  | Temp temps ->
    let width = ref None in
    let set w =
      assert (Option.is_none !width);
      width := Some w
    in
    Array.iter
      (function
        | M8 -> set Layout.M8
        | M16 -> set Layout.M16
        | M32 -> set Layout.M32
        | M64 -> set Layout.M64
        | M128 -> set Layout.M128
        | M256 -> set Layout.M256
        | M512 -> set Layout.M512
        | VM32X -> set Layout.M32X
        | VM32Y -> set Layout.M32Y
        | VM32Z -> set Layout.M32Z
        | VM64X -> set Layout.M64X
        | VM64Y -> set Layout.M64Y
        | VM64Z -> set Layout.M64Z
        | R8 | R16 | R32 | R64 | MM | XMM | YMM | ZMM | K -> ())
      temps;
    Option.get !width
