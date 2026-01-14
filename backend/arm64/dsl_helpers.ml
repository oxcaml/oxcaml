(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024--2025 Jane Street Group LLC                              *
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

[@@@ocaml.warning "+a-40-41-42"]

(** Helper functions that convert compiler [Reg.t] values to ARM64 operands. *)

open Misc
open Arch
open Reg
open! Int_replace_polymorphic_compare
module Ast = Arm64_ast.Ast
module Element_to_GP = Ast.Element_to_GP
module Smov_element_to_GP = Ast.Smov_element_to_GP
module L = Asm_targets.Asm_label
module S = Asm_targets.Asm_symbol

(* See [Proc.int_reg_name]. *)
let[@ocamlformat "disable"] int_reg_name_to_arch_index =
[| 0; 1; 2; 3; 4; 5; 6; 7;    (* 0 - 7 *)
   8; 9; 10; 11; 12; 13; 14; 15; (* 8 - 15 *)
   19; 20; 21; 22; 23; 24; 25;   (* 16 - 22 *)
   26; 27; 28;                   (* 23 - 25 *)
   16; 17; |]
(* 26 - 27 *)

let reg_name_to_arch_index (reg_class : Reg_class.t) (name_index : int) =
  match reg_class with
  | Reg_class.Int64 (* general-purpose registers *) ->
    int_reg_name_to_arch_index.(name_index)
  | Reg_class.Float128 (* neon registers *) -> name_index

let reg_index reg =
  match reg with
  | { loc = Reg r; typ; _ } ->
    let reg_class = Reg_class.of_machtype typ in
    let name_index = r - Reg_class.first_available_register reg_class in
    reg_name_to_arch_index reg_class name_index
  | { loc = Stack _ | Unknown; _ } -> fatal_error "Dsl_helpers.reg_index"

let assert_vec128_or_valx2 ~fname reg =
  match reg.typ with
  | Vec128 | Valx2 -> ()
  | Val | Int | Addr | Float | Float32 | Vec256 | Vec512 ->
    Misc.fatal_errorf "%s: expected Vec128/Valx2 register, got %a" fname
      Printreg.reg reg

let reg_v2s reg =
  assert_vec128_or_valx2 ~fname:"reg_v2s" reg;
  Ast.DSL.reg_v2s (reg_index reg)

let reg_v4s reg =
  assert_vec128_or_valx2 ~fname:"reg_v4s" reg;
  Ast.DSL.reg_v4s (reg_index reg)

let reg_v2d reg =
  assert_vec128_or_valx2 ~fname:"reg_v2d" reg;
  Ast.DSL.reg_v2d (reg_index reg)

let reg_v16b reg =
  assert_vec128_or_valx2 ~fname:"reg_v16b" reg;
  Ast.DSL.reg_v16b (reg_index reg)

let reg_v8h reg =
  assert_vec128_or_valx2 ~fname:"reg_v8h" reg;
  Ast.DSL.reg_v8h (reg_index reg)

let reg_v8b reg =
  assert_vec128_or_valx2 ~fname:"reg_v8b" reg;
  Ast.DSL.reg_v8b (reg_index reg)

let reg_v4h reg =
  assert_vec128_or_valx2 ~fname:"reg_v4h" reg;
  Ast.DSL.reg_v4h (reg_index reg)

(* Operand tuple helpers for SIMD instructions *)
let v4s_v4s_v4s i =
  reg_v4s i.Linear.res.(0), reg_v4s i.Linear.arg.(0), reg_v4s i.Linear.arg.(1)

let v2d_v2d_v2d i =
  reg_v2d i.Linear.res.(0), reg_v2d i.Linear.arg.(0), reg_v2d i.Linear.arg.(1)

let v8h_v8h_v8h i =
  reg_v8h i.Linear.res.(0), reg_v8h i.Linear.arg.(0), reg_v8h i.Linear.arg.(1)

let v16b_v16b_v16b i =
  ( reg_v16b i.Linear.res.(0),
    reg_v16b i.Linear.arg.(0),
    reg_v16b i.Linear.arg.(1) )

let v4s_v4s i = reg_v4s i.Linear.res.(0), reg_v4s i.Linear.arg.(0)

let v2d_v2d i = reg_v2d i.Linear.res.(0), reg_v2d i.Linear.arg.(0)

let v8h_v8h i = reg_v8h i.Linear.res.(0), reg_v8h i.Linear.arg.(0)

let v16b_v16b i = reg_v16b i.Linear.res.(0), reg_v16b i.Linear.arg.(0)

let mem_symbol_reg base ~(reloc : [`Twelve] Ast.Symbol.same_unit_or_reloc)
    ?offset sym =
  let symbol = Ast.Symbol.create_symbol reloc ?offset sym in
  Ast.DSL.mem_symbol ~base ~symbol

let mem_label base ~(reloc : [`Twelve] Ast.Symbol.same_unit_or_reloc) ?offset
    lbl =
  let symbol = Ast.Symbol.create_label reloc ?offset lbl in
  Ast.DSL.mem_symbol ~base ~symbol

let mem base = Ast.DSL.mem ~base

(* See .mli for why this returns [`X] and not [`X | `SP]. *)
let gp_reg_of_reg r : [`GP of [`X]] Ast.Reg.t =
  let index = reg_index r in
  match r.typ with
  | Val | Int | Addr -> Ast.Reg.reg_x index
  | Float | Float32 | Vec128 | Valx2 | Vec256 | Vec512 ->
    Misc.fatal_errorf "gp_reg_of_reg: expected integer register, got %a"
      Printreg.reg r

let mem_offset_reg r offset =
  let base = gp_reg_of_reg r in
  Ast.DSL.mem_offset ~base ~offset

let addressing addr r =
  match addr with
  | Iindexed ofs -> mem_offset_reg r ofs
  | Ibased (s, ofs) ->
    assert (not !Clflags.dlcode);
    (* see cfg_selection.ml *)
    let base = gp_reg_of_reg r in
    if S.is_local s
    then
      (* Local symbols are defined as labels (not linker symbols) to avoid ELF
         visibility issues, so references must also use labels. *)
      let lbl =
        L.create_label_for_local_symbol Asm_targets.Asm_section.Data s
      in
      mem_label base ~reloc:(Needs_reloc LOWER_TWELVE) ~offset:ofs lbl
    else mem_symbol_reg base ~reloc:(Needs_reloc LOWER_TWELVE) ~offset:ofs s

let stack ~stack_offset ~contains_calls ~num_stack_slots (r : Reg.t) =
  let slot_offset loc stack_class =
    let offset =
      Proc.slot_offset loc ~stack_class ~stack_offset:!stack_offset
        ~fun_contains_calls:!contains_calls ~fun_num_stack_slots:num_stack_slots
    in
    match offset with
    | Bytes_relative_to_stack_pointer n -> n
    | Bytes_relative_to_domainstate_pointer _ ->
      Misc.fatal_errorf "Not a stack slot"
  in
  let reg_domain_state_ptr =
    Proc.phys_reg Int 25
    (* x28 *)
  in
  match r.loc with
  | Stack (Domainstate n) ->
    let ofs = n + (Domainstate.(idx_of_field Domain_extra_params) * 8) in
    mem_offset_reg reg_domain_state_ptr ofs
  | Stack ((Local _ | Incoming _ | Outgoing _) as s) ->
    let ofs = slot_offset s (Stack_class.of_machtype r.typ) in
    Ast.DSL.mem_offset ~base:Ast.Reg.sp (* SP *) ~offset:ofs
  | Reg _ | Unknown ->
    Misc.fatal_errorf "Dsl_helpers.stack: register %a not on stack" Printreg.reg
      r

let reg_x reg =
  let index = reg_index reg in
  match reg.typ with
  | Val | Int | Addr ->
    if index = 31
    then Misc.fatal_error "reg_x: register 31 (SP) not valid here"
    else Ast.DSL.reg_op (Ast.Reg.reg_x index)
  | Float | Float32 | Vec128 | Valx2 | Vec256 | Vec512 ->
    Misc.fatal_errorf "reg_x: expected integer register, got %a" Printreg.reg
      reg

let reg_w reg =
  let index = reg_index reg in
  match reg.typ with
  | Val | Int | Addr ->
    if index = 31
    then Misc.fatal_error "reg_w: register 31 not valid here"
    else Ast.DSL.reg_op (Ast.Reg.reg_w index)
  | Float | Float32 | Vec128 | Valx2 | Vec256 | Vec512 ->
    Misc.fatal_errorf "reg_w: expected integer register, got %a" Printreg.reg
      reg

let reg_d reg =
  let index = reg_index reg in
  match reg.typ with
  | Float | Vec128 | Valx2 ->
    (* Vec128/Valx2 allowed for scalar extraction from vectors *)
    Ast.DSL.reg_op (Ast.Reg.reg_d index)
  | Val | Int | Addr | Float32 | Vec256 | Vec512 ->
    Misc.fatal_errorf "reg_d: expected Float or Vec128/Valx2 register, got %a"
      Printreg.reg reg

(* Like [reg_d] but accepts Float, Float32, Vec128 and Valx2 registers. Used for
   reinterpret casts where we want to treat any of these as D registers. *)
let reg_d_of_float_reg reg =
  let index = reg_index reg in
  match reg.typ with
  | Float | Float32 | Vec128 | Valx2 -> Ast.DSL.reg_op (Ast.Reg.reg_d index)
  | Val | Int | Addr | Vec256 | Vec512 ->
    Misc.fatal_errorf
      "reg_d_of_float_reg: expected Float, Float32, or Vec128/Valx2 register, \
       got %a"
      Printreg.reg reg

let reg_s reg =
  let index = reg_index reg in
  match reg.typ with
  | Float32 | Vec128 | Valx2 ->
    (* Vec128/Valx2 allowed for scalar extraction from vectors *)
    Ast.DSL.reg_op (Ast.Reg.reg_s index)
  | Val | Int | Addr | Float | Vec256 | Vec512 ->
    Misc.fatal_errorf "reg_s: expected Float32 or Vec128/Valx2 register, got %a"
      Printreg.reg reg

let reg_q_operand reg =
  let index = reg_index reg in
  match reg.typ with
  | Vec128 | Valx2 -> Ast.DSL.reg_op (Ast.Reg.reg_q index)
  | Val | Int | Addr | Float | Float32 | Vec256 | Vec512 ->
    Misc.fatal_errorf "reg_q_operand: expected Vec128/Valx2 register, got %a"
      Printreg.reg reg

let reg_v2d_operand reg =
  let index = reg_index reg in
  match reg.typ with
  | Vec128 | Valx2 -> Ast.DSL.reg_op (Ast.Reg.reg_v2d index)
  | Val | Int | Addr | Float | Float32 | Vec256 | Vec512 ->
    Misc.fatal_errorf "reg_v2d_operand: expected Vec128/Valx2 register, got %a"
      Printreg.reg reg

let reg_v16b_operand reg =
  let index = reg_index reg in
  match reg.typ with
  | Vec128 | Valx2 -> Ast.DSL.reg_op (Ast.Reg.reg_v16b index)
  | Val | Int | Addr | Float | Float32 | Vec256 | Vec512 ->
    Misc.fatal_errorf "reg_v16b_operand: expected Vec128/Valx2 register, got %a"
      Printreg.reg reg

type scalar_fp_regs_3 =
  | S_regs :
      ([`Reg of [`Neon of [`Scalar of [`S]]]] Ast.Operand.t as 'op) * 'op * 'op
      -> scalar_fp_regs_3
  | D_regs :
      ([`Reg of [`Neon of [`Scalar of [`D]]]] Ast.Operand.t as 'op) * 'op * 'op
      -> scalar_fp_regs_3

let reg_fp_operand_3 r1 r2 r3 =
  let index1 = reg_index r1 in
  let index2 = reg_index r2 in
  let index3 = reg_index r3 in
  match r1.typ, r2.typ, r3.typ with
  | Float32, Float32, Float32 ->
    S_regs
      ( Ast.DSL.reg_op (Ast.Reg.reg_s index1),
        Ast.DSL.reg_op (Ast.Reg.reg_s index2),
        Ast.DSL.reg_op (Ast.Reg.reg_s index3) )
  | Float, Float, Float ->
    D_regs
      ( Ast.DSL.reg_op (Ast.Reg.reg_d index1),
        Ast.DSL.reg_op (Ast.Reg.reg_d index2),
        Ast.DSL.reg_op (Ast.Reg.reg_d index3) )
  | ( (Float32 | Float | Val | Int | Addr | Vec128 | Valx2 | Vec256 | Vec512),
      _,
      _ ) ->
    Misc.fatal_errorf
      "reg_fp_operand_3: expected all Float or all Float32 registers, got: %a, \
       %a, %a"
      Printreg.reg r1 Printreg.reg r2 Printreg.reg r3 ()

type scalar_fp_regs_4 =
  | S_regs :
      ([`Reg of [`Neon of [`Scalar of [`S]]]] Ast.Operand.t as 'op)
      * 'op
      * 'op
      * 'op
      -> scalar_fp_regs_4
  | D_regs :
      ([`Reg of [`Neon of [`Scalar of [`D]]]] Ast.Operand.t as 'op)
      * 'op
      * 'op
      * 'op
      -> scalar_fp_regs_4

let reg_fp_operand_4 r1 r2 r3 r4 : scalar_fp_regs_4 =
  let index1 = reg_index r1 in
  let index2 = reg_index r2 in
  let index3 = reg_index r3 in
  let index4 = reg_index r4 in
  match r1.typ, r2.typ, r3.typ, r4.typ with
  | Float32, Float32, Float32, Float32 ->
    S_regs
      ( Ast.DSL.reg_op (Ast.Reg.reg_s index1),
        Ast.DSL.reg_op (Ast.Reg.reg_s index2),
        Ast.DSL.reg_op (Ast.Reg.reg_s index3),
        Ast.DSL.reg_op (Ast.Reg.reg_s index4) )
  | Float, Float, Float, Float ->
    D_regs
      ( Ast.DSL.reg_op (Ast.Reg.reg_d index1),
        Ast.DSL.reg_op (Ast.Reg.reg_d index2),
        Ast.DSL.reg_op (Ast.Reg.reg_d index3),
        Ast.DSL.reg_op (Ast.Reg.reg_d index4) )
  | ( (Float32 | Float | Val | Int | Addr | Vec128 | Valx2 | Vec256 | Vec512),
      _,
      _,
      _ ) ->
    Misc.fatal_errorf
      "reg_fp_operand_4: expected all Float or all Float32 registers, got: %a, \
       %a, %a, %a"
      Printreg.reg r1 Printreg.reg r2 Printreg.reg r3 Printreg.reg r4 ()
