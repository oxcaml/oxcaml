(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                        The OxCaml developers                           *)
(*                                                                        *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

(* Emission of ARM assembly code, 64-bit mode *)

(* Correctness: carefully consider any use of [Config], [Clflags],
   [Oxcaml_flags] and shared variables. For details, see [asmgen.mli]. *)

open Misc
open Arch
open Proc
open Reg
open! Operation
open Linear
open Emitaux
module I = Arm64_ast.Ast.Instruction_name
module D = Asm_targets.Asm_directives
module S = Asm_targets.Asm_symbol
module L = Asm_targets.Asm_label
open! Int_replace_polymorphic_compare

(* Convert Cmm.is_global to Asm_symbol.visibility *)
let visibility_of_cmm_global : Cmm.is_global -> S.visibility = function
  | Cmm.Global -> S.Global
  | Cmm.Local -> S.Local

(* Create symbol from Cmm.symbol, preserving visibility *)
let symbol_of_cmm_symbol (s : Cmm.symbol) : S.t =
  S.create ~visibility:(visibility_of_cmm_global s.sym_global) s.sym_name

(* Binary emitter for JIT mode *)
let jit_emitter : Arm64_binary_emitter.Binary_emitter.t option ref = ref None

(* Tradeoff between code size and code speed *)

let fastcode_flag = ref true

(* Names for special regs *)

let reg_domain_state_ptr = phys_reg Int 25 (* x28 *)

let reg_trap_ptr = phys_reg Int 23 (* x26 *)

let reg_alloc_ptr = phys_reg Int 24 (* x27 *)

let reg_tmp1 = phys_reg Int 26 (* x16 *)

let reg_x8 = phys_reg Int 8 (* x8 *)

let reg_stack_arg_begin = phys_reg Int 17 (* x20 *)

let reg_stack_arg_end = phys_reg Int 18 (* x21 *)

(** Turn a Linear label into an assembly label. The section is checked against the
    section tracked by [D] when emitting label definitions. *)
let label_to_asm_label (l : label) ~(section : Asm_targets.Asm_section.t) : L.t
    =
  L.create_int section (Label.to_int l)

(* CR sdolan: Support local symbol definitions & references on arm64 *)

(* Layout of the stack frame *)

let stack_offset = ref 0

let num_stack_slots = Stack_class.Tbl.make 0

let prologue_required = ref false

let contains_calls = ref false

let initial_stack_offset () =
  Proc.initial_stack_offset ~contains_calls:!contains_calls ~num_stack_slots

let frame_size () =
  Proc.frame_size ~stack_offset:!stack_offset ~contains_calls:!contains_calls
    ~num_stack_slots

let slot_offset loc stack_class =
  let offset =
    Proc.slot_offset loc ~stack_class ~stack_offset:!stack_offset
      ~fun_contains_calls:!contains_calls ~fun_num_stack_slots:num_stack_slots
  in
  match offset with
  | Bytes_relative_to_stack_pointer n -> n
  | Bytes_relative_to_domainstate_pointer _ ->
    Misc.fatal_errorf "Not a stack slot"

(* This module builds on [Arm64_ast.Ast.DSL] to provide functions that work on
   normal [Reg.t] values. *)
module DSL : sig
  include module type of struct
    include Arm64_ast.Ast.DSL
  end

  module Cond : module type of struct
    include Arm64_ast.Ast.Cond
  end

  module Float_cond : module type of struct
    include Arm64_ast.Ast.Float_cond
  end

  val reg_w : Reg.t -> [`Reg of [`GP of [`W]]] Arm64_ast.Ast.Operand.t

  val reg_x : Reg.t -> [`Reg of [`GP of [`X]]] Arm64_ast.Ast.Operand.t

  val reg_d :
    Reg.t -> [`Reg of [`Neon of [`Scalar of [`D]]]] Arm64_ast.Ast.Operand.t

  (** Like [reg_d] but accepts both Float and Float32 registers. Used for
      reinterpret casts where we want to treat both as D registers. *)
  val reg_d_of_float_reg :
    Reg.t -> [`Reg of [`Neon of [`Scalar of [`D]]]] Arm64_ast.Ast.Operand.t

  val reg_s :
    Reg.t -> [`Reg of [`Neon of [`Scalar of [`S]]]] Arm64_ast.Ast.Operand.t

  val reg_v2s :
    Reg.t ->
    [`Reg of [`Neon of [`Vector of [`V2S] * [`S]]]] Arm64_ast.Ast.Operand.t

  val reg_v4s :
    Reg.t ->
    [`Reg of [`Neon of [`Vector of [`V4S] * [`S]]]] Arm64_ast.Ast.Operand.t

  val reg_v2d :
    Reg.t ->
    [`Reg of [`Neon of [`Vector of [`V2D] * [`D]]]] Arm64_ast.Ast.Operand.t

  val reg_v16b :
    Reg.t ->
    [`Reg of [`Neon of [`Vector of [`V16B] * [`B]]]] Arm64_ast.Ast.Operand.t

  val reg_v8h :
    Reg.t ->
    [`Reg of [`Neon of [`Vector of [`V8H] * [`H]]]] Arm64_ast.Ast.Operand.t

  val reg_v8b :
    Reg.t ->
    [`Reg of [`Neon of [`Vector of [`V8B] * [`B]]]] Arm64_ast.Ast.Operand.t

  val reg_v4h :
    Reg.t ->
    [`Reg of [`Neon of [`Vector of [`V4H] * [`H]]]] Arm64_ast.Ast.Operand.t

  (** Operand tuple helpers for SIMD instructions *)

  val v4s_v4s_v4s :
    Linear.instruction ->
    [`Reg of [`Neon of [`Vector of [`V4S] * [`S]]]] Arm64_ast.Ast.Operand.t
    * [`Reg of [`Neon of [`Vector of [`V4S] * [`S]]]] Arm64_ast.Ast.Operand.t
    * [`Reg of [`Neon of [`Vector of [`V4S] * [`S]]]] Arm64_ast.Ast.Operand.t

  val v2d_v2d_v2d :
    Linear.instruction ->
    [`Reg of [`Neon of [`Vector of [`V2D] * [`D]]]] Arm64_ast.Ast.Operand.t
    * [`Reg of [`Neon of [`Vector of [`V2D] * [`D]]]] Arm64_ast.Ast.Operand.t
    * [`Reg of [`Neon of [`Vector of [`V2D] * [`D]]]] Arm64_ast.Ast.Operand.t

  val v8h_v8h_v8h :
    Linear.instruction ->
    [`Reg of [`Neon of [`Vector of [`V8H] * [`H]]]] Arm64_ast.Ast.Operand.t
    * [`Reg of [`Neon of [`Vector of [`V8H] * [`H]]]] Arm64_ast.Ast.Operand.t
    * [`Reg of [`Neon of [`Vector of [`V8H] * [`H]]]] Arm64_ast.Ast.Operand.t

  val v16b_v16b_v16b :
    Linear.instruction ->
    [`Reg of [`Neon of [`Vector of [`V16B] * [`B]]]] Arm64_ast.Ast.Operand.t
    * [`Reg of [`Neon of [`Vector of [`V16B] * [`B]]]] Arm64_ast.Ast.Operand.t
    * [`Reg of [`Neon of [`Vector of [`V16B] * [`B]]]] Arm64_ast.Ast.Operand.t

  val v4s_v4s :
    Linear.instruction ->
    [`Reg of [`Neon of [`Vector of [`V4S] * [`S]]]] Arm64_ast.Ast.Operand.t
    * [`Reg of [`Neon of [`Vector of [`V4S] * [`S]]]] Arm64_ast.Ast.Operand.t

  val v2d_v2d :
    Linear.instruction ->
    [`Reg of [`Neon of [`Vector of [`V2D] * [`D]]]] Arm64_ast.Ast.Operand.t
    * [`Reg of [`Neon of [`Vector of [`V2D] * [`D]]]] Arm64_ast.Ast.Operand.t

  val v8h_v8h :
    Linear.instruction ->
    [`Reg of [`Neon of [`Vector of [`V8H] * [`H]]]] Arm64_ast.Ast.Operand.t
    * [`Reg of [`Neon of [`Vector of [`V8H] * [`H]]]] Arm64_ast.Ast.Operand.t

  val v16b_v16b :
    Linear.instruction ->
    [`Reg of [`Neon of [`Vector of [`V16B] * [`B]]]] Arm64_ast.Ast.Operand.t
    * [`Reg of [`Neon of [`Vector of [`V16B] * [`B]]]] Arm64_ast.Ast.Operand.t

  val reg_q_operand :
    Reg.t -> [`Reg of [`Neon of [`Scalar of [`Q]]]] Arm64_ast.Ast.Operand.t

  val reg_v2d_operand :
    Reg.t ->
    [`Reg of [`Neon of [`Vector of [`V2D] * [`D]]]] Arm64_ast.Ast.Operand.t

  val reg_v16b_operand :
    Reg.t ->
    [`Reg of [`Neon of [`Vector of [`V16B] * [`B]]]] Arm64_ast.Ast.Operand.t

  val imm_six : int -> [`Imm of [`Six]] Arm64_ast.Ast.Operand.t

  val reg_s7 : [`Reg of [`Neon of [`Scalar of [`S]]]] Arm64_ast.Ast.Operand.t

  val emit_mem_symbol :
    reloc:[`Twelve] Arm64_ast.Ast.Symbol.same_unit_or_reloc ->
    ?offset:int ->
    Reg.t ->
    S.t ->
    [`Mem of [> `Offset_sym]] Arm64_ast.Ast.Operand.t

  val emit_mem_label :
    reloc:[`Twelve] Arm64_ast.Ast.Symbol.same_unit_or_reloc ->
    ?offset:int ->
    Reg.t ->
    L.t ->
    [`Mem of [> `Offset_sym]] Arm64_ast.Ast.Operand.t

  val mem : Reg.t -> [`Mem of [> `Base_reg]] Arm64_ast.Ast.Operand.t

  val addressing :
    addressing_mode ->
    Reg.t ->
    [`Mem of [> `Offset_imm | `Offset_unscaled | `Offset_sym]]
    Arm64_ast.Ast.Operand.t

  val stack :
    Reg.t ->
    [`Mem of [> `Offset_imm | `Offset_unscaled]] Arm64_ast.Ast.Operand.t

  val label :
    ?offset:int ->
    'w Arm64_ast.Ast.Symbol.same_unit_or_reloc ->
    L.t ->
    [`Imm of [`Sym of 'w]] Arm64_ast.Ast.Operand.t

  val symbol :
    ?offset:int ->
    'w Arm64_ast.Ast.Symbol.same_unit_or_reloc ->
    S.t ->
    [`Imm of [`Sym of 'w]] Arm64_ast.Ast.Operand.t

  type scalar_fp_regs_3 = private
    | S_regs :
        ([`Reg of [`Neon of [`Scalar of [`S]]]] Arm64_ast.Ast.Operand.t as 'a)
        * 'a
        * 'a
        -> scalar_fp_regs_3
    | D_regs :
        ([`Reg of [`Neon of [`Scalar of [`D]]]] Arm64_ast.Ast.Operand.t as 'a)
        * 'a
        * 'a
        -> scalar_fp_regs_3

  type scalar_fp_regs_4 = private
    | S_regs :
        ([`Reg of [`Neon of [`Scalar of [`S]]]] Arm64_ast.Ast.Operand.t as 'a)
        * 'a
        * 'a
        * 'a
        -> scalar_fp_regs_4
    | D_regs :
        ([`Reg of [`Neon of [`Scalar of [`D]]]] Arm64_ast.Ast.Operand.t as 'a)
        * 'a
        * 'a
        * 'a
        -> scalar_fp_regs_4

  val reg_fp_operand_3 : Reg.t -> Reg.t -> Reg.t -> scalar_fp_regs_3

  val reg_fp_operand_4 : Reg.t -> Reg.t -> Reg.t -> Reg.t -> scalar_fp_regs_4

  module Reg : module type of struct
    include Arm64_ast.Ast.Reg
  end

  module Acc : sig
    include module type of struct
      include Arm64_ast.Ast.DSL.Acc
    end

    val labeled_ins1 :
      L.t ->
      (Arm64_ast.Ast.singleton, 'a) Arm64_ast.Ast.Instruction_name.t ->
      'a Arm64_ast.Ast.Operand.t ->
      unit

    val labeled_ins4 :
      L.t ->
      (Arm64_ast.Ast.quad, 'a * 'b * 'c * 'd) Arm64_ast.Ast.Instruction_name.t ->
      'a Arm64_ast.Ast.Operand.t
      * 'b Arm64_ast.Ast.Operand.t
      * 'c Arm64_ast.Ast.Operand.t
      * 'd Arm64_ast.Ast.Operand.t ->
      unit
  end
end = struct
  include Arm64_ast.Ast.DSL
  module Cond = Arm64_ast.Ast.Cond
  module Float_cond = Arm64_ast.Ast.Float_cond

  let imm_six n = Arm64_ast.Ast.DSL.imm_six n

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
    | { loc = Stack _ | Unknown; _ } -> fatal_error "Emit.reg"

  let reg_v2s reg = reg_v2s (reg_index reg)

  let reg_v4s reg = reg_v4s (reg_index reg)

  let reg_v2d reg = reg_v2d (reg_index reg)

  let reg_v16b reg = reg_v16b (reg_index reg)

  let reg_v8h reg = reg_v8h (reg_index reg)

  let reg_v8b reg = reg_v8b (reg_index reg)

  let reg_v4h reg = reg_v4h (reg_index reg)

  let reg_s7 = reg_s 7

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

  let label ?offset reloc lbl =
    Arm64_ast.Ast.DSL.symbol
      (Arm64_ast.Ast.Symbol.create_label reloc ?offset lbl)

  let symbol ?offset reloc s =
    Arm64_ast.Ast.DSL.symbol
      (Arm64_ast.Ast.Symbol.create_symbol reloc ?offset s)

  let emit_mem_symbol
      ~(reloc : [`Twelve] Arm64_ast.Ast.Symbol.same_unit_or_reloc) ?offset r sym
      =
    let index = reg_index r in
    let symbol = Arm64_ast.Ast.Symbol.create_symbol reloc ?offset sym in
    match r.typ with
    | Val | Int | Addr ->
      if index = 31
      then Arm64_ast.Ast.DSL.mem_symbol ~base:(Arm64_ast.Ast.Reg.sp ()) ~symbol
      else
        Arm64_ast.Ast.DSL.mem_symbol
          ~base:(Arm64_ast.Ast.Reg.reg_x index)
          ~symbol
    | Float | Float32 | Vec128 | Valx2 | Vec256 | Vec512 ->
      Misc.fatal_errorf
        "emit_mem_symbol: expected integer register for base, got %a"
        Printreg.reg r

  let emit_mem_label
      ~(reloc : [`Twelve] Arm64_ast.Ast.Symbol.same_unit_or_reloc) ?offset r
      label =
    let index = reg_index r in
    let symbol = Arm64_ast.Ast.Symbol.create_label reloc ?offset label in
    match r.typ with
    | Val | Int | Addr ->
      if index = 31
      then Arm64_ast.Ast.DSL.mem_symbol ~base:(Arm64_ast.Ast.Reg.sp ()) ~symbol
      else
        Arm64_ast.Ast.DSL.mem_symbol
          ~base:(Arm64_ast.Ast.Reg.reg_x index)
          ~symbol
    | Float | Float32 | Vec128 | Valx2 | Vec256 | Vec512 ->
      Misc.fatal_errorf
        "emit_mem_label: expected integer register for base, got %a"
        Printreg.reg r

  let mem r =
    let index = reg_index r in
    match r.typ with
    | Val | Int | Addr ->
      if index = 31
      then Arm64_ast.Ast.DSL.mem ~base:(Arm64_ast.Ast.Reg.sp ())
      else Arm64_ast.Ast.DSL.mem ~base:(Arm64_ast.Ast.Reg.reg_x index)
    | Float | Float32 | Vec128 | Valx2 | Vec256 | Vec512 ->
      Misc.fatal_errorf "mem: expected integer register for base, got %a"
        Printreg.reg r

  let emit_mem_offset r offset =
    let index = reg_index r in
    match r.typ with
    | Val | Int | Addr ->
      if index = 31
      then Arm64_ast.Ast.DSL.mem_offset ~base:(Arm64_ast.Ast.Reg.sp ()) ~offset
      else
        Arm64_ast.Ast.DSL.mem_offset
          ~base:(Arm64_ast.Ast.Reg.reg_x index)
          ~offset
    | Float | Float32 | Vec128 | Valx2 | Vec256 | Vec512 ->
      Misc.fatal_errorf
        "emit_mem_offset: expected integer register for base, got %a"
        Printreg.reg r

  let addressing addr r =
    match addr with
    | Iindexed ofs -> emit_mem_offset r ofs
    | Ibased (s, ofs) ->
      assert (not !Clflags.dlcode);
      (* see selection.ml *)
      emit_mem_symbol r ~reloc:(Needs_reloc LOWER_TWELVE) s ~offset:ofs

  let stack (r : Reg.t) =
    match r.loc with
    | Stack (Domainstate n) ->
      let ofs = n + (Domainstate.(idx_of_field Domain_extra_params) * 8) in
      emit_mem_offset reg_domain_state_ptr ofs
    | Stack ((Local _ | Incoming _ | Outgoing _) as s) ->
      let ofs = slot_offset s (Stack_class.of_machtype r.typ) in
      Arm64_ast.Ast.DSL.mem_offset ~base:(Arm64_ast.Ast.Reg.sp ()) ~offset:ofs
    | Reg _ | Unknown ->
      Misc.fatal_errorf "Emit.stack: register %a not on stack" Printreg.reg r

  let reg_x reg =
    let index = reg_index reg in
    match reg.typ with
    | Val | Int | Addr ->
      (* XXX is this the correct check for SP? What about XZR etc? *)
      if index = 31
      then Misc.fatal_error "DSL.reg_x: register 31 (SP) not valid here"
      else Arm64_ast.Ast.DSL.reg_op (Arm64_ast.Ast.Reg.reg_x index)
    | Float | Float32 | Vec128 | Valx2 | Vec256 | Vec512 ->
      Misc.fatal_errorf "reg_x: expected integer register, got %a" Printreg.reg
        reg

  let reg_w reg =
    let index = reg_index reg in
    match reg.typ with
    | Val | Int | Addr ->
      if index = 31
      then Misc.fatal_error "DSL.reg_w: register 31 not valid here"
      else Arm64_ast.Ast.DSL.reg_op (Arm64_ast.Ast.Reg.reg_w index)
    | Float | Float32 | Vec128 | Valx2 | Vec256 | Vec512 ->
      Misc.fatal_errorf "reg_w: expected integer register, got %a" Printreg.reg
        reg

  let reg_d reg =
    let index = reg_index reg in
    match reg.typ with
    | Float | Vec128 | Valx2 ->
      (* Vec128/Valx2 allowed for scalar extraction from vectors *)
      Arm64_ast.Ast.DSL.reg_op (Arm64_ast.Ast.Reg.reg_d index)
    | Val | Int | Addr | Float32 | Vec256 | Vec512 ->
      Misc.fatal_errorf "reg_d: expected Float or Vec128/Valx2 register, got %a"
        Printreg.reg reg

  (* Like [reg_d] but accepts both Float and Float32 registers. Used for
     reinterpret casts where we want to treat both as D registers. *)
  let reg_d_of_float_reg reg =
    let index = reg_index reg in
    match reg.typ with
    | Float | Float32 | Vec128 | Valx2 ->
      Arm64_ast.Ast.DSL.reg_op (Arm64_ast.Ast.Reg.reg_d index)
    | Val | Int | Addr | Vec256 | Vec512 ->
      Misc.fatal_errorf
        "reg_d_of_float_reg: expected Float or Float32 register, got %a"
        Printreg.reg reg

  let reg_s reg =
    let index = reg_index reg in
    match reg.typ with
    | Float32 | Vec128 | Valx2 ->
      (* Vec128/Valx2 allowed for scalar extraction from vectors *)
      Arm64_ast.Ast.DSL.reg_op (Arm64_ast.Ast.Reg.reg_s index)
    | Val | Int | Addr | Float | Vec256 | Vec512 ->
      Misc.fatal_errorf
        "reg_s: expected Float32 or Vec128/Valx2 register, got %a" Printreg.reg
        reg

  let reg_q_operand reg =
    let index = reg_index reg in
    match reg.typ with
    | Vec128 | Valx2 -> Arm64_ast.Ast.DSL.reg_op (Arm64_ast.Ast.Reg.reg_q index)
    | Val | Int | Addr | Float | Float32 | Vec256 | Vec512 ->
      Misc.fatal_errorf "reg_q_operand: expected Vec128/Valx2 register, got %a"
        Printreg.reg reg

  let reg_v2d_operand reg =
    let index = reg_index reg in
    match reg.typ with
    | Vec128 | Valx2 ->
      Arm64_ast.Ast.DSL.reg_op (Arm64_ast.Ast.Reg.reg_v2d index)
    | Val | Int | Addr | Float | Float32 | Vec256 | Vec512 ->
      Misc.fatal_errorf
        "reg_v2d_operand: expected Vec128/Valx2 register, got %a" Printreg.reg
        reg

  let reg_v16b_operand reg =
    let index = reg_index reg in
    match reg.typ with
    | Vec128 | Valx2 ->
      Arm64_ast.Ast.DSL.reg_op (Arm64_ast.Ast.Reg.reg_v16b index)
    | Val | Int | Addr | Float | Float32 | Vec256 | Vec512 ->
      Misc.fatal_errorf
        "reg_v16b_operand: expected Vec128/Valx2 register, got %a" Printreg.reg
        reg

  module Operand = Arm64_ast.Ast.Operand
  module Reg = Arm64_ast.Ast.Reg

  type scalar_fp_regs_3 =
    | S_regs :
        ([`Reg of [`Neon of [`Scalar of [`S]]]] Operand.t as 'op) * 'op * 'op
        -> scalar_fp_regs_3
    | D_regs :
        ([`Reg of [`Neon of [`Scalar of [`D]]]] Operand.t as 'op) * 'op * 'op
        -> scalar_fp_regs_3

  let reg_fp_operand_3 r1 r2 r3 =
    let index1 = reg_index r1 in
    let index2 = reg_index r2 in
    let index3 = reg_index r3 in
    match r1.typ, r2.typ, r3.typ with
    | Float32, Float32, Float32 ->
      S_regs
        ( Arm64_ast.Ast.DSL.reg_op (Arm64_ast.Ast.Reg.reg_s index1),
          Arm64_ast.Ast.DSL.reg_op (Arm64_ast.Ast.Reg.reg_s index2),
          Arm64_ast.Ast.DSL.reg_op (Arm64_ast.Ast.Reg.reg_s index3) )
    | Float, Float, Float ->
      D_regs
        ( Arm64_ast.Ast.DSL.reg_op (Arm64_ast.Ast.Reg.reg_d index1),
          Arm64_ast.Ast.DSL.reg_op (Arm64_ast.Ast.Reg.reg_d index2),
          Arm64_ast.Ast.DSL.reg_op (Arm64_ast.Ast.Reg.reg_d index3) )
    | ( (Float32 | Float | Val | Int | Addr | Vec128 | Valx2 | Vec256 | Vec512),
        _,
        _ ) ->
      Misc.fatal_errorf
        "reg_fp_operand_3: expected all Float or all Float32 registers, got: \
         %a, %a, %a"
        Printreg.reg r1 Printreg.reg r2 Printreg.reg r3 ()

  type scalar_fp_regs_4 =
    | S_regs :
        ([`Reg of [`Neon of [`Scalar of [`S]]]] Operand.t as 'op)
        * 'op
        * 'op
        * 'op
        -> scalar_fp_regs_4
    | D_regs :
        ([`Reg of [`Neon of [`Scalar of [`D]]]] Operand.t as 'op)
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
        ( reg_op (Reg.reg_s index1),
          reg_op (Reg.reg_s index2),
          reg_op (Reg.reg_s index3),
          reg_op (Reg.reg_s index4) )
    | Float, Float, Float, Float ->
      D_regs
        ( reg_op (Reg.reg_d index1),
          reg_op (Reg.reg_d index2),
          reg_op (Reg.reg_d index3),
          reg_op (Reg.reg_d index4) )
    | ( (Float32 | Float | Val | Int | Addr | Vec128 | Valx2 | Vec256 | Vec512),
        _,
        _,
        _ ) ->
      Misc.fatal_errorf
        "reg_fp_operand_4: expected all Float or all Float32 registers, got: \
         %a, %a, %a, %a"
        Printreg.reg r1 Printreg.reg r2 Printreg.reg r3 Printreg.reg r4 ()

  module Acc = struct
    include Arm64_ast.Ast.DSL.Acc

    let labeled_ins1 lbl instr op =
      D.define_label lbl;
      ins1 instr op

    let labeled_ins4 lbl instr ops =
      D.define_label lbl;
      ins4 instr ops
  end
end

module A = DSL.Acc

let runtime_function name =
  DSL.symbol (Needs_reloc CALL26) (S.create_global name)

let local_label lbl = DSL.label Same_section_and_unit lbl

(* SIMD instruction handling *)

let simd_instr_size (op : Simd.operation) =
  match op with
  | Min_scalar_f64 | Max_scalar_f64 -> 2
  | Min_scalar_f32 | Max_scalar_f32 -> 2
  | Cntq_u16 -> 2
  | Round_f32 _ | Round_f64 _ | Roundq_f32 _ | Roundq_f64 _ | Round_f32_s64
  | Round_f64_s64 | Zip1q_s8 | Zip2q_s8 | Zip1q_s16 | Zip2q_s16 | Zip1_f32
  | Zip1q_f32 | Zip2q_f32 | Zip1q_f64 | Zip2q_f64 | Addq_f32 | Subq_f32
  | Mulq_f32 | Divq_f32 | Minq_f32 | Maxq_f32 | Addq_f64 | Subq_f64 | Mulq_f64
  | Divq_f64 | Minq_f64 | Maxq_f64 | Recpeq_f32 | Sqrtq_f32 | Rsqrteq_f32
  | Sqrtq_f64 | Rsqrteq_f64 | Cvtq_s32_f32 | Cvtnq_s32_f32 | Cvtq_f32_s32
  | Cvt_f64_f32 | Cvt_f32_f64 | Paddq_f32 | Fmin_f32 | Fmax_f32 | Fmin_f64
  | Fmax_f64 | Addq_s64 | Subq_s64 | Cmp_f32 _ | Cmpz_f32 _ | Cmpz_s32 _
  | Cmp_f64 _ | Cmpz_f64 _ | Cmp_s32 _ | Cmp_s64 _ | Cmpz_s64 _ | Mvnq_s32
  | Orrq_s32 | Andq_s32 | Eorq_s32 | Negq_s32 | Getq_lane_s32 _
  | Getq_lane_s64 _ | Mulq_s32 | Mulq_s16 | Addq_s32 | Subq_s32 | Minq_s32
  | Maxq_s32 | Minq_u32 | Maxq_u32 | Absq_s32 | Absq_s64 | Paddq_f64 | Paddq_s32
  | Paddq_s64 | Mvnq_s64 | Orrq_s64 | Andq_s64 | Eorq_s64 | Negq_s64 | Shlq_u32
  | Shlq_u64 | Shlq_s32 | Shlq_s64 | Shlq_n_u32 _ | Shlq_n_u64 _ | Shrq_n_u32 _
  | Shrq_n_u64 _ | Shrq_n_s32 _ | Shrq_n_s64 _ | Setq_lane_s32 _
  | Setq_lane_s64 _ | Dupq_lane_s32 _ | Dupq_lane_s64 _ | Cvtq_f64_s64
  | Cvtq_s64_f64 | Cvtnq_s64_f64 | Movl_s32 | Movl_u32 | Addq_s16 | Paddq_s16
  | Qaddq_s16 | Qaddq_u16 | Subq_s16 | Qsubq_s16 | Qsubq_u16 | Absq_s16
  | Minq_s16 | Maxq_s16 | Minq_u16 | Maxq_u16 | Mvnq_s16 | Orrq_s16 | Andq_s16
  | Eorq_s16 | Negq_s16 | Shlq_u16 | Shlq_s16 | Cmp_s16 _ | Cmpz_s16 _
  | Shlq_n_u16 _ | Shrq_n_u16 _ | Shrq_n_s16 _ | Getq_lane_s16 _
  | Setq_lane_s16 _ | Dupq_lane_s16 _ | Movn_s64 | Copyq_laneq_s64 _ | Addq_s8
  | Paddq_s8 | Qaddq_s8 | Qaddq_u8 | Subq_s8 | Qsubq_s8 | Qsubq_u8 | Absq_s8
  | Minq_s8 | Maxq_s8 | Minq_u8 | Maxq_u8 | Mvnq_s8 | Orrq_s8 | Andq_s8
  | Eorq_s8 | Negq_s8 | Cntq_u8 | Shlq_u8 | Shlq_s8 | Cmp_s8 _ | Cmpz_s8 _
  | Shlq_n_u8 _ | Shrq_n_u8 _ | Shrq_n_s8 _ | Getq_lane_s8 _ | Setq_lane_s8 _
  | Dupq_lane_s8 _ | Extq_u8 _ | Qmovn_high_s64 | Qmovn_s64 | Qmovn_high_s32
  | Qmovn_s32 | Qmovn_high_u32 | Qmovn_u32 | Qmovn_high_s16 | Qmovn_s16
  | Qmovn_high_u16 | Qmovn_u16 | Movn_high_s64 | Movn_high_s32 | Movn_s32
  | Movn_high_s16 | Movn_s16 | Mullq_s16 | Mullq_u16 | Mullq_high_s16
  | Mullq_high_u16 | Movl_s16 | Movl_u16 | Movl_s8 | Movl_u8 ->
    1

let simd_instr (op : Simd.operation) (i : Linear.instruction) =
  let module Lane_index = Arm64_ast.Ast.Neon_reg_name.Lane_index in
  (* Check register constraints for instructions that require res = arg0 *)
  (match[@ocaml.warning "-4"] op with
  | Copyq_laneq_s64 _ | Setq_lane_s8 _ | Setq_lane_s16 _ | Setq_lane_s32 _
  | Setq_lane_s64 _ ->
    if not (Reg.same_loc i.res.(0) i.arg.(0))
    then
      Misc.fatal_errorf
        "simd_instr: Instruction %s requires res and arg0 to be the same \
         register"
        (Simd.print_name op)
  | _ -> ());
  match[@ocaml.warning "-4"] op with
  (* Vector floating-point arithmetic - V4S *)
  | Addq_f32 -> A.ins3 FADD_vector (DSL.v4s_v4s_v4s i)
  | Subq_f32 -> A.ins3 FSUB_vector (DSL.v4s_v4s_v4s i)
  | Mulq_f32 -> A.ins3 FMUL_vector (DSL.v4s_v4s_v4s i)
  | Divq_f32 -> A.ins3 FDIV_vector (DSL.v4s_v4s_v4s i)
  (* Vector floating-point arithmetic - V2D *)
  | Addq_f64 -> A.ins3 FADD_vector (DSL.v2d_v2d_v2d i)
  | Subq_f64 -> A.ins3 FSUB_vector (DSL.v2d_v2d_v2d i)
  | Mulq_f64 -> A.ins3 FMUL_vector (DSL.v2d_v2d_v2d i)
  | Divq_f64 -> A.ins3 FDIV_vector (DSL.v2d_v2d_v2d i)
  (* Vector integer arithmetic - ADD *)
  | Addq_s64 -> A.ins3 ADD_vector (DSL.v2d_v2d_v2d i)
  | Addq_s32 -> A.ins3 ADD_vector (DSL.v4s_v4s_v4s i)
  | Addq_s16 -> A.ins3 ADD_vector (DSL.v8h_v8h_v8h i)
  | Addq_s8 -> A.ins3 ADD_vector (DSL.v16b_v16b_v16b i)
  (* Vector integer arithmetic - SUB *)
  | Subq_s64 -> A.ins3 SUB_vector (DSL.v2d_v2d_v2d i)
  | Subq_s32 -> A.ins3 SUB_vector (DSL.v4s_v4s_v4s i)
  | Subq_s16 -> A.ins3 SUB_vector (DSL.v8h_v8h_v8h i)
  | Subq_s8 -> A.ins3 SUB_vector (DSL.v16b_v16b_v16b i)
  (* Vector integer arithmetic - MUL *)
  | Mulq_s32 -> A.ins3 MUL_vector (DSL.v4s_v4s_v4s i)
  | Mulq_s16 -> A.ins3 MUL_vector (DSL.v8h_v8h_v8h i)
  (* Vector integer arithmetic - NEG *)
  | Negq_s64 -> A.ins2 NEG_vector (DSL.v2d_v2d i)
  | Negq_s32 -> A.ins2 NEG_vector (DSL.v4s_v4s i)
  | Negq_s16 -> A.ins2 NEG_vector (DSL.v8h_v8h i)
  | Negq_s8 -> A.ins2 NEG_vector (DSL.v16b_v16b i)
  (* Vector min/max - floating-point *)
  | Minq_f32 -> A.ins3 FMIN_vector (DSL.v4s_v4s_v4s i)
  | Minq_f64 -> A.ins3 FMIN_vector (DSL.v2d_v2d_v2d i)
  | Maxq_f32 -> A.ins3 FMAX_vector (DSL.v4s_v4s_v4s i)
  | Maxq_f64 -> A.ins3 FMAX_vector (DSL.v2d_v2d_v2d i)
  (* Vector min/max - signed integer *)
  | Minq_s32 -> A.ins3 SMIN_vector (DSL.v4s_v4s_v4s i)
  | Minq_s16 -> A.ins3 SMIN_vector (DSL.v8h_v8h_v8h i)
  | Minq_s8 -> A.ins3 SMIN_vector (DSL.v16b_v16b_v16b i)
  | Maxq_s32 -> A.ins3 SMAX_vector (DSL.v4s_v4s_v4s i)
  | Maxq_s16 -> A.ins3 SMAX_vector (DSL.v8h_v8h_v8h i)
  | Maxq_s8 -> A.ins3 SMAX_vector (DSL.v16b_v16b_v16b i)
  (* Vector min/max - unsigned integer *)
  | Minq_u32 -> A.ins3 UMIN_vector (DSL.v4s_v4s_v4s i)
  | Minq_u16 -> A.ins3 UMIN_vector (DSL.v8h_v8h_v8h i)
  | Minq_u8 -> A.ins3 UMIN_vector (DSL.v16b_v16b_v16b i)
  | Maxq_u32 -> A.ins3 UMAX_vector (DSL.v4s_v4s_v4s i)
  | Maxq_u16 -> A.ins3 UMAX_vector (DSL.v8h_v8h_v8h i)
  | Maxq_u8 -> A.ins3 UMAX_vector (DSL.v16b_v16b_v16b i)
  (* Vector logical operations - all use V16B *)
  | Orrq_s32 | Orrq_s64 | Orrq_s16 | Orrq_s8 ->
    A.ins3 ORR_vector (DSL.v16b_v16b_v16b i)
  | Andq_s32 | Andq_s64 | Andq_s16 | Andq_s8 ->
    A.ins3 AND_vector (DSL.v16b_v16b_v16b i)
  | Eorq_s32 | Eorq_s64 | Eorq_s16 | Eorq_s8 ->
    A.ins3 EOR_vector (DSL.v16b_v16b_v16b i)
  | Mvnq_s32 | Mvnq_s64 | Mvnq_s16 | Mvnq_s8 ->
    A.ins2 MVN_vector (DSL.v16b_v16b i)
  (* Vector absolute value *)
  | Absq_s64 -> A.ins2 ABS_vector (DSL.v2d_v2d i)
  | Absq_s32 -> A.ins2 ABS_vector (DSL.v4s_v4s i)
  | Absq_s16 -> A.ins2 ABS_vector (DSL.v8h_v8h i)
  | Absq_s8 -> A.ins2 ABS_vector (DSL.v16b_v16b i)
  (* Vector sqrt and reciprocal estimates *)
  | Sqrtq_f32 -> A.ins2 FSQRT_vector (DSL.v4s_v4s i)
  | Sqrtq_f64 -> A.ins2 FSQRT_vector (DSL.v2d_v2d i)
  | Rsqrteq_f32 -> A.ins2 FRSQRTE_vector (DSL.v4s_v4s i)
  | Rsqrteq_f64 -> A.ins2 FRSQRTE_vector (DSL.v2d_v2d i)
  | Recpeq_f32 -> A.ins2 FRECPE_vector (DSL.v4s_v4s i)
  (* Vector conversions *)
  | Cvtq_s32_f32 -> A.ins2 FCVTZS_vector (DSL.v4s_v4s i)
  | Cvtq_s64_f64 -> A.ins2 FCVTZS_vector (DSL.v2d_v2d i)
  | Cvtnq_s32_f32 -> A.ins2 FCVTNS_vector (DSL.v4s_v4s i)
  | Cvtnq_s64_f64 -> A.ins2 FCVTNS_vector (DSL.v2d_v2d i)
  | Cvtq_f32_s32 -> A.ins2 SCVTF_vector (DSL.v4s_v4s i)
  | Cvtq_f64_s64 -> A.ins2 SCVTF_vector (DSL.v2d_v2d i)
  | Cvt_f64_f32 ->
    A.ins2 FCVTL_vector (DSL.reg_v2d i.res.(0), DSL.reg_v2s i.arg.(0))
  | Cvt_f32_f64 ->
    A.ins2 FCVTN_vector (DSL.reg_v2s i.res.(0), DSL.reg_v2d i.arg.(0))
  (* Vector extend/narrow operations - signed extend *)
  | Movl_s32 -> A.ins2 SXTL (DSL.reg_v2d i.res.(0), DSL.reg_v2s i.arg.(0))
  | Movl_s16 -> A.ins2 SXTL (DSL.reg_v4s i.res.(0), DSL.reg_v4h i.arg.(0))
  | Movl_s8 -> A.ins2 SXTL (DSL.reg_v8h i.res.(0), DSL.reg_v8b i.arg.(0))
  (* Vector extend/narrow operations - unsigned extend *)
  | Movl_u32 -> A.ins2 UXTL (DSL.reg_v2d i.res.(0), DSL.reg_v2s i.arg.(0))
  | Movl_u16 -> A.ins2 UXTL (DSL.reg_v4s i.res.(0), DSL.reg_v4h i.arg.(0))
  | Movl_u8 -> A.ins2 UXTL (DSL.reg_v8h i.res.(0), DSL.reg_v8b i.arg.(0))
  (* Vector narrow operations *)
  | Movn_s64 -> A.ins2 XTN (DSL.reg_v2s i.res.(0), DSL.reg_v2d i.arg.(0))
  | Movn_s32 -> A.ins2 XTN (DSL.reg_v4h i.res.(0), DSL.reg_v4s i.arg.(0))
  | Movn_s16 -> A.ins2 XTN (DSL.reg_v8b i.res.(0), DSL.reg_v8h i.arg.(0))
  (* Vector narrow high operations - uses arg.(1) for source *)
  | Movn_high_s64 -> A.ins2 XTN2 (DSL.reg_v4s i.res.(0), DSL.reg_v2d i.arg.(1))
  | Movn_high_s32 -> A.ins2 XTN2 (DSL.reg_v8h i.res.(0), DSL.reg_v4s i.arg.(1))
  | Movn_high_s16 -> A.ins2 XTN2 (DSL.reg_v16b i.res.(0), DSL.reg_v8h i.arg.(1))
  (* Vector saturating narrow operations *)
  | Qmovn_s64 -> A.ins2 SQXTN (DSL.reg_v2s i.res.(0), DSL.reg_v2d i.arg.(0))
  | Qmovn_s32 -> A.ins2 SQXTN (DSL.reg_v4h i.res.(0), DSL.reg_v4s i.arg.(0))
  | Qmovn_s16 -> A.ins2 SQXTN (DSL.reg_v8b i.res.(0), DSL.reg_v8h i.arg.(0))
  | Qmovn_high_s64 ->
    A.ins2 SQXTN2 (DSL.reg_v4s i.res.(0), DSL.reg_v2d i.arg.(1))
  | Qmovn_high_s32 ->
    A.ins2 SQXTN2 (DSL.reg_v8h i.res.(0), DSL.reg_v4s i.arg.(1))
  | Qmovn_high_s16 ->
    A.ins2 SQXTN2 (DSL.reg_v16b i.res.(0), DSL.reg_v8h i.arg.(1))
  | Qmovn_u32 -> A.ins2 UQXTN (DSL.reg_v4h i.res.(0), DSL.reg_v4s i.arg.(0))
  | Qmovn_u16 -> A.ins2 UQXTN (DSL.reg_v8b i.res.(0), DSL.reg_v8h i.arg.(0))
  | Qmovn_high_u32 ->
    A.ins2 UQXTN2 (DSL.reg_v8h i.res.(0), DSL.reg_v4s i.arg.(1))
  | Qmovn_high_u16 ->
    A.ins2 UQXTN2 (DSL.reg_v16b i.res.(0), DSL.reg_v8h i.arg.(1))
  (* Vector pairwise operations *)
  | Paddq_f32 -> A.ins3 FADDP_vector (DSL.v4s_v4s_v4s i)
  | Paddq_f64 -> A.ins3 FADDP_vector (DSL.v2d_v2d_v2d i)
  | Paddq_s64 -> A.ins3 ADDP_vector (DSL.v2d_v2d_v2d i)
  | Paddq_s32 -> A.ins3 ADDP_vector (DSL.v4s_v4s_v4s i)
  | Paddq_s16 -> A.ins3 ADDP_vector (DSL.v8h_v8h_v8h i)
  | Paddq_s8 -> A.ins3 ADDP_vector (DSL.v16b_v16b_v16b i)
  (* Vector zip operations *)
  | Zip1_f32 ->
    A.ins3 ZIP1
      (DSL.reg_v2s i.res.(0), DSL.reg_v2s i.arg.(0), DSL.reg_v2s i.arg.(1))
  | Zip1q_s8 -> A.ins3 ZIP1 (DSL.v16b_v16b_v16b i)
  | Zip1q_s16 -> A.ins3 ZIP1 (DSL.v8h_v8h_v8h i)
  | Zip1q_f32 -> A.ins3 ZIP1 (DSL.v4s_v4s_v4s i)
  | Zip1q_f64 -> A.ins3 ZIP1 (DSL.v2d_v2d_v2d i)
  | Zip2q_s8 -> A.ins3 ZIP2 (DSL.v16b_v16b_v16b i)
  | Zip2q_s16 -> A.ins3 ZIP2 (DSL.v8h_v8h_v8h i)
  | Zip2q_f32 -> A.ins3 ZIP2 (DSL.v4s_v4s_v4s i)
  | Zip2q_f64 -> A.ins3 ZIP2 (DSL.v2d_v2d_v2d i)
  (* Vector shift operations - unsigned *)
  | Shlq_u64 -> A.ins3 USHL_vector (DSL.v2d_v2d_v2d i)
  | Shlq_u32 -> A.ins3 USHL_vector (DSL.v4s_v4s_v4s i)
  | Shlq_u16 -> A.ins3 USHL_vector (DSL.v8h_v8h_v8h i)
  | Shlq_u8 -> A.ins3 USHL_vector (DSL.v16b_v16b_v16b i)
  (* Vector shift operations - signed *)
  | Shlq_s64 -> A.ins3 SSHL_vector (DSL.v2d_v2d_v2d i)
  | Shlq_s32 -> A.ins3 SSHL_vector (DSL.v4s_v4s_v4s i)
  | Shlq_s16 -> A.ins3 SSHL_vector (DSL.v8h_v8h_v8h i)
  | Shlq_s8 -> A.ins3 SSHL_vector (DSL.v16b_v16b_v16b i)
  (* Vector multiply long *)
  | Mullq_s16 ->
    A.ins3 SMULL_vector
      (DSL.reg_v4s i.res.(0), DSL.reg_v4h i.arg.(0), DSL.reg_v4h i.arg.(1))
  | Mullq_u16 ->
    A.ins3 UMULL_vector
      (DSL.reg_v4s i.res.(0), DSL.reg_v4h i.arg.(0), DSL.reg_v4h i.arg.(1))
  | Mullq_high_s16 ->
    A.ins3 SMULL2_vector
      (DSL.reg_v4s i.res.(0), DSL.reg_v8h i.arg.(0), DSL.reg_v8h i.arg.(1))
  | Mullq_high_u16 ->
    A.ins3 UMULL2_vector
      (DSL.reg_v4s i.res.(0), DSL.reg_v8h i.arg.(0), DSL.reg_v8h i.arg.(1))
  (* Vector saturating arithmetic *)
  | Qaddq_s16 -> A.ins3 SQADD_vector (DSL.v8h_v8h_v8h i)
  | Qaddq_s8 -> A.ins3 SQADD_vector (DSL.v16b_v16b_v16b i)
  | Qaddq_u16 -> A.ins3 UQADD_vector (DSL.v8h_v8h_v8h i)
  | Qaddq_u8 -> A.ins3 UQADD_vector (DSL.v16b_v16b_v16b i)
  | Qsubq_s16 -> A.ins3 SQSUB_vector (DSL.v8h_v8h_v8h i)
  | Qsubq_s8 -> A.ins3 SQSUB_vector (DSL.v16b_v16b_v16b i)
  | Qsubq_u16 -> A.ins3 UQSUB_vector (DSL.v8h_v8h_v8h i)
  | Qsubq_u8 -> A.ins3 UQSUB_vector (DSL.v16b_v16b_v16b i)
  (* Vector shift by immediate *)
  | Shlq_n_u64 n ->
    let rd, rn = DSL.v2d_v2d i in
    A.ins3 SHL (rd, rn, DSL.imm_six n)
  | Shlq_n_u32 n ->
    let rd, rn = DSL.v4s_v4s i in
    A.ins3 SHL (rd, rn, DSL.imm_six n)
  | Shlq_n_u16 n ->
    let rd, rn = DSL.v8h_v8h i in
    A.ins3 SHL (rd, rn, DSL.imm_six n)
  | Shlq_n_u8 n ->
    let rd, rn = DSL.v16b_v16b i in
    A.ins3 SHL (rd, rn, DSL.imm_six n)
  | Shrq_n_u64 n ->
    let rd, rn = DSL.v2d_v2d i in
    A.ins3 USHR (rd, rn, DSL.imm_six n)
  | Shrq_n_u32 n ->
    let rd, rn = DSL.v4s_v4s i in
    A.ins3 USHR (rd, rn, DSL.imm_six n)
  | Shrq_n_u16 n ->
    let rd, rn = DSL.v8h_v8h i in
    A.ins3 USHR (rd, rn, DSL.imm_six n)
  | Shrq_n_u8 n ->
    let rd, rn = DSL.v16b_v16b i in
    A.ins3 USHR (rd, rn, DSL.imm_six n)
  | Shrq_n_s64 n ->
    let rd, rn = DSL.v2d_v2d i in
    A.ins3 SSHR (rd, rn, DSL.imm_six n)
  | Shrq_n_s32 n ->
    let rd, rn = DSL.v4s_v4s i in
    A.ins3 SSHR (rd, rn, DSL.imm_six n)
  | Shrq_n_s16 n ->
    let rd, rn = DSL.v8h_v8h i in
    A.ins3 SSHR (rd, rn, DSL.imm_six n)
  | Shrq_n_s8 n ->
    let rd, rn = DSL.v16b_v16b i in
    A.ins3 SSHR (rd, rn, DSL.imm_six n)
  (* Sign-extend lane to X register *)
  | Getq_lane_s32 { lane } ->
    let lane_idx = Lane_index.create lane in
    A.ins2 (SMOV lane_idx) (DSL.reg_x i.res.(0), DSL.reg_v4s i.arg.(0))
  | Getq_lane_s16 { lane } ->
    let lane_idx = Lane_index.create lane in
    A.ins2 (SMOV lane_idx) (DSL.reg_x i.res.(0), DSL.reg_v8h i.arg.(0))
  | Getq_lane_s8 { lane } ->
    let lane_idx = Lane_index.create lane in
    A.ins2 (SMOV lane_idx) (DSL.reg_x i.res.(0), DSL.reg_v16b i.arg.(0))
  (* Extract 64-bit lane *)
  | Getq_lane_s64 { lane } ->
    let lane_idx = Lane_index.create lane in
    A.ins2 (UMOV lane_idx) (DSL.reg_x i.res.(0), DSL.reg_v2d i.arg.(0))
  (* Extract bytes from two vectors *)
  | Extq_u8 n ->
    A.ins4 EXT
      ( DSL.reg_v16b i.res.(0),
        DSL.reg_v16b i.arg.(0),
        DSL.reg_v16b i.arg.(1),
        DSL.imm_six n )
  (* Compare against zero - floating-point *)
  | Cmpz_f32 c -> A.ins2 (FCM_zero c) (DSL.v4s_v4s i)
  | Cmpz_f64 c -> A.ins2 (FCM_zero c) (DSL.v2d_v2d i)
  (* Compare against zero - integer *)
  | Cmpz_s32 c ->
    A.ins2 (CM_zero (Simd_proc.simd_cond_to_ast_cond c)) (DSL.v4s_v4s i)
  | Cmpz_s64 c ->
    A.ins2 (CM_zero (Simd_proc.simd_cond_to_ast_cond c)) (DSL.v2d_v2d i)
  | Cmpz_s16 c ->
    A.ins2 (CM_zero (Simd_proc.simd_cond_to_ast_cond c)) (DSL.v8h_v8h i)
  | Cmpz_s8 c ->
    A.ins2 (CM_zero (Simd_proc.simd_cond_to_ast_cond c)) (DSL.v16b_v16b i)
  (* Float vector compare *)
  | Cmp_f32 ((EQ | GE | GT) as c) -> A.ins3 (FCM_register c) (DSL.v4s_v4s_v4s i)
  | Cmp_f64 ((EQ | GE | GT) as c) -> A.ins3 (FCM_register c) (DSL.v2d_v2d_v2d i)
  | Cmp_f32 (LT | LE | NE | CC | CS | LS | HI) ->
    Misc.fatal_error "Cmp_f32 LT/LE should be transformed in Simd_selection"
  | Cmp_f64 (LT | LE | NE | CC | CS | LS | HI) ->
    Misc.fatal_error "Cmp_f64 LT/LE should be transformed in Simd_selection"
  (* Scalar float min/max *)
  | Fmin_f32 ->
    A.ins3 FMIN (DSL.reg_s i.res.(0), DSL.reg_s i.arg.(0), DSL.reg_s i.arg.(1))
  | Fmax_f32 ->
    A.ins3 FMAX (DSL.reg_s i.res.(0), DSL.reg_s i.arg.(0), DSL.reg_s i.arg.(1))
  | Fmin_f64 ->
    A.ins3 FMIN (DSL.reg_d i.res.(0), DSL.reg_d i.arg.(0), DSL.reg_d i.arg.(1))
  | Fmax_f64 ->
    A.ins3 FMAX (DSL.reg_d i.res.(0), DSL.reg_d i.arg.(0), DSL.reg_d i.arg.(1))
  (* Scalar rounding *)
  | Round_f32 rm ->
    A.ins2
      (FRINT (Simd_proc.simd_rounding_to_ast_rounding rm))
      (DSL.reg_s i.res.(0), DSL.reg_s i.arg.(0))
  | Round_f64 rm ->
    A.ins2
      (FRINT (Simd_proc.simd_rounding_to_ast_rounding rm))
      (DSL.reg_d i.res.(0), DSL.reg_d i.arg.(0))
  (* Vector rounding *)
  | Roundq_f32 rm ->
    A.ins2
      (FRINT_vector (Simd_proc.simd_rounding_to_ast_rounding rm))
      (DSL.v4s_v4s i)
  | Roundq_f64 rm ->
    A.ins2
      (FRINT_vector (Simd_proc.simd_rounding_to_ast_rounding rm))
      (DSL.v2d_v2d i)
  (* Float to signed integer with rounding *)
  | Round_f32_s64 -> A.ins2 FCVTNS (DSL.reg_x i.res.(0), DSL.reg_s i.arg.(0))
  | Round_f64_s64 -> A.ins2 FCVTNS (DSL.reg_x i.res.(0), DSL.reg_d i.arg.(0))
  (* Integer vector compares *)
  | Cmp_s32 ((EQ | GE | GT) as c) ->
    A.ins3 (CM_register (Simd_proc.simd_cond_to_ast_cond c)) (DSL.v4s_v4s_v4s i)
  | Cmp_s64 ((EQ | GE | GT) as c) ->
    A.ins3 (CM_register (Simd_proc.simd_cond_to_ast_cond c)) (DSL.v2d_v2d_v2d i)
  | Cmp_s16 ((EQ | GE | GT) as c) ->
    A.ins3 (CM_register (Simd_proc.simd_cond_to_ast_cond c)) (DSL.v8h_v8h_v8h i)
  | Cmp_s8 ((EQ | GE | GT) as c) ->
    A.ins3
      (CM_register (Simd_proc.simd_cond_to_ast_cond c))
      (DSL.v16b_v16b_v16b i)
  | Cmp_s32 (LT | LE) | Cmp_s64 (LT | LE) | Cmp_s16 (LT | LE) | Cmp_s8 (LT | LE)
    ->
    (* XXX out of date message *)
    Misc.fatal_error "Cmp_s LT/LE should be transformed in Emit"
  (* Lane operations - insert from GP register *)
  | Setq_lane_s8 { lane } ->
    let lane_idx = Lane_index.create lane in
    A.ins2 (INS lane_idx) (DSL.reg_v16b i.res.(0), DSL.reg_w i.arg.(1))
  | Setq_lane_s16 { lane } ->
    let lane_idx = Lane_index.create lane in
    A.ins2 (INS lane_idx) (DSL.reg_v8h i.res.(0), DSL.reg_w i.arg.(1))
  | Setq_lane_s32 { lane } ->
    let lane_idx = Lane_index.create lane in
    A.ins2 (INS lane_idx) (DSL.reg_v4s i.res.(0), DSL.reg_w i.arg.(1))
  | Setq_lane_s64 { lane } ->
    let lane_idx = Lane_index.create lane in
    A.ins2 (INS lane_idx) (DSL.reg_v2d i.res.(0), DSL.reg_x i.arg.(1))
  (* Copy lane to lane *)
  | Copyq_laneq_s64 { src_lane; dst_lane } ->
    let lanes =
      Lane_index.Src_and_dest.create
        ~src:(Lane_index.create src_lane)
        ~dest:(Lane_index.create dst_lane)
    in
    A.ins2 (INS_V lanes) (DSL.reg_v2d i.res.(0), DSL.reg_v2d i.arg.(1))
  (* Duplicate lane *)
  | Dupq_lane_s8 { lane } ->
    let lane_idx = Lane_index.create lane in
    A.ins2 (DUP lane_idx) (DSL.v16b_v16b i)
  | Dupq_lane_s16 { lane } ->
    let lane_idx = Lane_index.create lane in
    A.ins2 (DUP lane_idx) (DSL.v8h_v8h i)
  | Dupq_lane_s32 { lane } ->
    let lane_idx = Lane_index.create lane in
    A.ins2 (DUP lane_idx) (DSL.v4s_v4s i)
  | Dupq_lane_s64 { lane } ->
    let lane_idx = Lane_index.create lane in
    A.ins2 (DUP lane_idx) (DSL.v2d_v2d i)
  (* Population count - CNT only works on 8-bit elements *)
  | Cntq_u8 -> A.ins2 CNT_vector (DSL.v16b_v16b i)
  (* Population count for 16-bit elements: CNT on bytes then UADDLP to sum
     pairs *)
  | Cntq_u16 ->
    let rd = DSL.reg_v8h i.res.(0) in
    let rs = DSL.reg_v16b i.arg.(0) in
    A.ins2 CNT_vector (rs, rs);
    A.ins2 UADDLP_vector (rd, rs)
  (* Two special cases for min/max scalar: generate a sequence that matches the
     weird semantics of amd64 instruction "minss", even when the flag [FPCR.AH]
     is not set. *)
  | Min_scalar_f32 ->
    let rd = DSL.reg_s i.res.(0) in
    let rn = DSL.reg_s i.arg.(0) in
    let rm = DSL.reg_s i.arg.(1) in
    A.ins2 FCMP (rn, rm);
    A.ins4 FCSEL (rd, rn, rm, DSL.cond MI)
  | Min_scalar_f64 ->
    let rd = DSL.reg_d i.res.(0) in
    let rn = DSL.reg_d i.arg.(0) in
    let rm = DSL.reg_d i.arg.(1) in
    A.ins2 FCMP (rn, rm);
    A.ins4 FCSEL (rd, rn, rm, DSL.cond MI)
  | Max_scalar_f32 ->
    let rd = DSL.reg_s i.res.(0) in
    let rn = DSL.reg_s i.arg.(0) in
    let rm = DSL.reg_s i.arg.(1) in
    A.ins2 FCMP (rn, rm);
    A.ins4 FCSEL (rd, rn, rm, DSL.cond GT)
  | Max_scalar_f64 ->
    let rd = DSL.reg_d i.res.(0) in
    let rn = DSL.reg_d i.arg.(0) in
    let rm = DSL.reg_d i.arg.(1) in
    A.ins2 FCMP (rn, rm);
    A.ins4 FCSEL (rd, rn, rm, DSL.cond GT)

(* Record live pointers at call points *)

let record_frame_label live dbg =
  let lbl = Cmm.new_label () in
  let live_offset = ref [] in
  Reg.Set.iter
    (function
      | { typ = Val; loc = Reg r; _ } ->
        live_offset := ((r lsl 1) + 1) :: !live_offset
      | { typ = Val; loc = Stack s; _ } as reg ->
        live_offset
          := slot_offset s (Stack_class.of_machtype reg.typ) :: !live_offset
      | { typ = Addr; _ } as r ->
        Misc.fatal_errorf "bad GC root %a" Printreg.reg r
      | { typ = Valx2; _ } as r ->
        (* CR mslater: (SIMD) arm64 *)
        Misc.fatal_errorf "Unexpected Valx2 type of reg %a" Printreg.reg r
      | { typ = Val; loc = Unknown; _ } as r ->
        Misc.fatal_errorf "Unknown location %a" Printreg.reg r
      | { typ = Int | Float | Float32 | Vec128; _ } -> ()
      | { typ = Vec256 | Vec512; _ } ->
        Misc.fatal_error "arm64: got 256/512 bit vector")
    live;
  (* CR sspies: Consider changing [record_frame_descr] to [Asm_label.t] instead
     of linear labels. *)
  record_frame_descr ~label:lbl ~frame_size:(frame_size ())
    ~live_offset:!live_offset dbg;
  label_to_asm_label ~section:Text lbl

let record_frame live dbg =
  let lbl = record_frame_label live dbg in
  D.define_label lbl

(* Record calls to the GC -- we've moved them out of the way *)

type gc_call =
  { gc_lbl: L.t;                      (* Entry label *)
    gc_return_lbl: L.t;               (* Where to branch after GC *)
    gc_frame_lbl: L.t }               (* Label of frame descriptor *)
[@@ocamlformat "disable"]

let call_gc_sites = ref ([] : gc_call list)

let emit_call_gc gc =
  A.labeled_ins1 gc.gc_lbl BL (runtime_function "caml_call_gc");
  A.labeled_ins1 gc.gc_frame_lbl B (local_label gc.gc_return_lbl)

(* Record calls to local stack reallocation *)

type local_realloc_call =
  { lr_lbl : L.t;
    lr_return_lbl : L.t;
    lr_dbg : Debuginfo.t
  }

let local_realloc_sites = ref ([] : local_realloc_call list)

let file_emitter ~file_num ~file_name =
  D.file ~file_num:(Some file_num) ~file_name

let emit_debug_info ?discriminator dbg =
  Emitaux.emit_debug_info_gen ?discriminator dbg file_emitter D.loc

let emit_local_realloc lr =
  D.define_label lr.lr_lbl;
  emit_debug_info lr.lr_dbg;
  A.ins1 BL (runtime_function "caml_call_local_realloc");
  A.ins1 B (local_label lr.lr_return_lbl)

(* Local stack reallocation *)

type stack_realloc =
  { sc_label : L.t; (* Label of the reallocation code. *)
    sc_return : L.t; (* Label to return to after reallocation. *)
    sc_max_frame_size_in_bytes : int (* Size for reallocation. *)
  }

let stack_realloc = ref (None : stack_realloc option)

let clear_stack_realloc () = stack_realloc := None

let emit_stack_realloc () =
  match !stack_realloc with
  | None -> ()
  | Some { sc_label; sc_return; sc_max_frame_size_in_bytes } ->
    D.define_label sc_label;
    (* Pass the desired frame size on the stack, since all of the
       argument-passing registers may be in use. *)
    A.ins_mov_imm (DSL.reg_x reg_tmp1)
      (DSL.imm_sixteen sc_max_frame_size_in_bytes);
    A.ins3 STP
      ( DSL.reg_x reg_tmp1,
        DSL.lr (),
        DSL.mem_pre_pair ~base:(DSL.Reg.sp ()) ~offset:(-16) );
    A.ins1 BL (runtime_function "caml_call_realloc_stack");
    A.ins3 LDP
      ( DSL.reg_x reg_tmp1,
        DSL.lr (),
        DSL.mem_post_pair ~base:(DSL.Reg.sp ()) ~offset:16 );
    A.ins1 B (local_label sc_return)

(* Names of various instructions *)

let cond_for_comparison : integer_comparison -> DSL.Cond.t = function
  | Ceq -> EQ
  | Cne -> NE
  | Cle -> LE
  | Cge -> GE
  | Clt -> LT
  | Cgt -> GT
  | Cule -> LS
  | Cuge -> CS
  | Cult -> CC
  | Cugt -> HI

let instr_for_bitwise_int_operation_shifted_register op : _ I.t =
  match[@ocaml.warning "-4"] op with
  | Iand -> AND_shifted_register
  | Ior -> ORR_shifted_register
  | Ixor -> EOR_shifted_register
  | _ ->
    Misc.fatal_errorf "instr_for_bitwise_int_operation: unexpected op %s"
      (Operation.string_of_integer_operation op)

let instr_for_bitwise_int_operation_immediate op : _ I.t =
  match[@ocaml.warning "-4"] op with
  | Iand -> AND_immediate
  | Ior -> ORR_immediate
  | Ixor -> EOR_immediate
  | _ ->
    Misc.fatal_errorf "instr_for_bitwise_int_operation: unexpected op %s"
      (Operation.string_of_integer_operation op)

let instr_for_shift_operation op : _ I.t =
  match[@ocaml.warning "-4"] op with
  | Ilsl -> LSLV
  | Ilsr -> LSRV
  | Iasr -> ASRV
  | _ ->
    Misc.fatal_errorf "instr_for_shift_operation: unexpected op %s"
      (Operation.string_of_integer_operation op)

let instr_for_arith_operation_shifted_register op : _ I.t =
  match[@ocaml.warning "-4"] op with
  | Iadd -> ADD_shifted_register
  | Isub -> SUB_shifted_register
  | _ ->
    Misc.fatal_errorf "instr_for_arith_operation: unexpected op %s"
      (Operation.string_of_integer_operation op)

(* Decompose an integer constant into four 16-bit shifted fragments. Omit the
   fragments that are equal to "default" (16 zeros or 16 ones). *)

let decompose_int default n =
  let rec decomp n pos =
    if pos >= 64
    then []
    else
      let frag = Nativeint.logand n 0xFFFFn
      and rem = Nativeint.shift_right_logical n 16 in
      if Nativeint.equal frag default
      then decomp rem (pos + 16)
      else (frag, pos) :: decomp rem (pos + 16)
  in
  decomp n 0

(* Load an integer constant into a register *)

let emit_movk dst (f, p) =
  A.ins3 MOVK
    ( DSL.reg_x dst,
      DSL.imm_sixteen (Nativeint.to_int f),
      DSL.shift ~kind:LSL ~amount:p )

let emit_intconst dst n =
  if Arm64_ast.Logical_immediates.is_logical_immediate n
  then A.ins3 ORR_immediate (DSL.reg_x dst, DSL.xzr (), DSL.bitmask n)
  else
    let dz = decompose_int 0x0000n n and dn = decompose_int 0xFFFFn n in
    if List.length dz <= List.length dn
    then (
      match dz with
      | [] -> A.ins_mov_reg (DSL.reg_x dst) (DSL.xzr ())
      | (f, p) :: l ->
        A.ins3 MOVZ
          ( DSL.reg_x dst,
            DSL.imm_sixteen (Nativeint.to_int f),
            DSL.optional_shift ~kind:LSL ~amount:p );
        List.iter (emit_movk dst) l)
    else
      match dn with
      | [] -> A.ins3 MOVN (DSL.reg_x dst, DSL.imm_sixteen 0, DSL.optional_none)
      | (f, p) :: l ->
        let nf = Nativeint.logxor f 0xFFFFn in
        A.ins3 MOVN
          ( DSL.reg_x dst,
            DSL.imm_sixteen (Nativeint.to_int nf),
            DSL.optional_shift ~kind:LSL ~amount:p );
        List.iter (emit_movk dst) l

let num_instructions_for_intconst n =
  if Arm64_ast.Logical_immediates.is_logical_immediate n
  then 1
  else
    let dz = decompose_int 0x0000n n and dn = decompose_int 0xFFFFn n in
    max 1 (min (List.length dz) (List.length dn))

(* Recognize float constants appropriate for FMOV dst, #fpimm instruction: "a
   normalized binary floating point encoding with 1 sign bit, 4 bits of fraction
   and a 3-bit exponent" *)

let is_immediate_float bits =
  let exp = (Int64.(to_int (shift_right_logical bits 52)) land 0x7FF) - 1023 in
  let mant = Int64.logand bits 0xF_FFFF_FFFF_FFFFL in
  exp >= -3 && exp <= 4
  && Int64.equal (Int64.logand mant 0xF_0000_0000_0000L) mant

let is_immediate_float32 bits =
  let exp = (Int32.(to_int (shift_right_logical bits 23)) land 0x7F) - 63 in
  let mant = Int32.logand bits 0x7F_FFFFl in
  exp >= -3 && exp <= 4 && Int32.equal (Int32.logand mant 0x78_0000l) mant

(* Adjust sp (up or down) by the given byte amount *)

let emit_stack_adjustment n =
  let m = abs n in
  assert (m < 0x1_000_000);
  let ml = m land 0xFFF and mh = m land 0xFFF_000 in
  if n < 0
  then (
    if mh <> 0
    then
      A.ins4 SUB_immediate (DSL.sp (), DSL.sp (), DSL.imm mh, DSL.optional_none);
    if ml <> 0
    then
      A.ins4 SUB_immediate (DSL.sp (), DSL.sp (), DSL.imm ml, DSL.optional_none))
  else (
    if mh <> 0
    then
      A.ins4 ADD_immediate (DSL.sp (), DSL.sp (), DSL.imm mh, DSL.optional_none);
    if ml <> 0
    then
      A.ins4 ADD_immediate (DSL.sp (), DSL.sp (), DSL.imm ml, DSL.optional_none));
  if n <> 0 then D.cfi_adjust_cfa_offset ~bytes:(-n)

(* Output add-immediate / sub-immediate / cmp-immediate instructions *)

let rec emit_addimm rd rs n =
  if n < 0
  then emit_subimm rd rs (-n)
  else if n <= 0xFFF
  then
    A.ins4 ADD_immediate
      (DSL.reg_x rd, DSL.reg_x rs, DSL.imm n, DSL.optional_none)
  else (
    assert (n <= 0xFFF_FFF);
    let nl = n land 0xFFF and nh = n land 0xFFF_000 in
    A.ins4 ADD_immediate
      (DSL.reg_x rd, DSL.reg_x rs, DSL.imm nh, DSL.optional_none);
    if nl <> 0
    then
      A.ins4 ADD_immediate
        (DSL.reg_x rd, DSL.reg_x rd, DSL.imm nl, DSL.optional_none))

and emit_subimm rd rs n =
  if n < 0
  then emit_addimm rd rs (-n)
  else if n <= 0xFFF
  then
    A.ins4 SUB_immediate
      (DSL.reg_x rd, DSL.reg_x rs, DSL.imm n, DSL.optional_none)
  else (
    assert (n <= 0xFFF_FFF);
    let nl = n land 0xFFF and nh = n land 0xFFF_000 in
    A.ins4 SUB_immediate
      (DSL.reg_x rd, DSL.reg_x rs, DSL.imm nh, DSL.optional_none);
    if nl <> 0
    then
      A.ins4 SUB_immediate
        (DSL.reg_x rd, DSL.reg_x rd, DSL.imm nl, DSL.optional_none))

let emit_cmpimm rs n =
  if n >= 0
  then A.ins_cmp (DSL.reg_x rs) (DSL.imm n) DSL.optional_none
  else A.ins_cmn (DSL.reg_x rs) (DSL.imm (-n)) DSL.optional_none

(* Name of current function *)
let function_name = ref ""

(* Entry point for tail recursive calls *)
let tailrec_entry_point = ref None

(* Pending floating-point literals *)
let float32_literals = ref ([] : (int32 * L.t) list)

let float_literals = ref ([] : (int64 * L.t) list)

let vec128_literals = ref ([] : (Cmm.vec128_bits * L.t) list)

(* Label a floating-point literal *)
let add_literal p f =
  try List.assoc f !p
  with Not_found ->
    (* CR sspies: The [Text] section here is incorrect. We should be in the
       respective section of the literal type (i.e., 16 or 8 bytes). The code
       below uses the [Text] section, because that is the section that we are in
       when we emit literals in the function body. Only macOS currently switches
       to a dedicated section. *)
    let lbl = L.create Text in
    p := (f, lbl) :: !p;
    lbl

let float32_literal f = add_literal float32_literals f

let float_literal f = add_literal float_literals f

let vec128_literal f = add_literal vec128_literals f

(* Emit all pending literals *)
let emit_literals p align emit_literal =
  if not (Misc.Stdlib.List.is_empty !p)
  then (
    if macosx
    then
      D.switch_to_section_raw
        ~names:["__TEXT"; "__literal" ^ Int.to_string align]
        ~flags:None
        ~args:[Int.to_string align ^ "byte_literals"]
        ~is_delayed:false
    else
      D.switch_to_section_raw
        ~names:[".rodata.cst" ^ Int.to_string align]
        ~flags:(Some "aM")
        ~args:["@progbits"; Int.to_string align]
        ~is_delayed:false;
    (* CR sspies: We set the internal section ref to Text here, because section
       ref does not support named text sections yet. Fix this when cleaning up
       the section mechanism. *)
    D.unsafe_set_internal_section_ref Text;
    D.align ~fill:Nop ~bytes:align;
    List.iter emit_literal !p;
    p := [])

let emit_float32_literal (f, lbl) =
  D.define_label lbl;
  D.float32_from_bits f;
  (* padding to 8 bytes *)
  D.int32 0xDEAD_BEEFl

let emit_float_literal (f, lbl) =
  D.define_label lbl;
  D.float64_from_bits f

let emit_vec128_literal (({ word0; word1 } : Cmm.vec128_bits), lbl) =
  D.define_label lbl;
  D.float64_from_bits word0;
  D.float64_from_bits word1

let emit_literals () =
  (* Align float32 literals to [size_float]=8 bytes, not 4. *)
  emit_literals float32_literals size_float emit_float32_literal;
  emit_literals float_literals size_float emit_float_literal;
  emit_literals vec128_literals size_vec128 emit_vec128_literal

(* Emit code to load the address of a symbol *)

let emit_load_symbol_addr dst s =
  let open Arm64_ast.Ast.Symbol in
  if macosx
  then (
    A.ins2 ADRP (DSL.reg_x dst, DSL.symbol (Needs_reloc GOT_PAGE) s);
    A.ins2 LDR
      ( DSL.reg_x dst,
        DSL.emit_mem_symbol dst ~reloc:(Needs_reloc GOT_PAGE_OFF) s ))
  else if not !Clflags.dlcode
  then (
    A.ins2 ADRP (DSL.reg_x dst, DSL.symbol (Needs_reloc PAGE) s);
    A.ins4 ADD_immediate
      ( DSL.reg_x dst,
        DSL.reg_x dst,
        DSL.symbol (Needs_reloc LOWER_TWELVE) s,
        DSL.optional_none ))
  else (
    A.ins2 ADRP (DSL.reg_x dst, DSL.symbol (Needs_reloc GOT_PAGE) s);
    A.ins2 LDR
      ( DSL.reg_x dst,
        DSL.emit_mem_symbol dst ~reloc:(Needs_reloc GOT_LOWER_TWELVE) s ))

(* The following functions are used for calculating the sizes of the call GC and
   bounds check points emitted out-of-line from the function body. See
   branch_relaxation.mli. *)

let num_call_gc_points instr =
  let rec loop instr call_gc =
    match instr.desc with
    | Lend -> call_gc
    | Lop (Alloc { mode = Heap; _ }) when !fastcode_flag ->
      loop instr.next (call_gc + 1)
    | Lop Poll -> loop instr.next (call_gc + 1)
    (* The following four should never be seen, since this function is run
       before branch relaxation. *)
    | Lop (Specific (Ifar_alloc _)) | Lop (Specific Ifar_poll) -> assert false
    | Lop (Alloc { mode = Local | Heap; _ })
    | Lop
        (Specific
          ( Imuladd | Imulsub | Inegmulf | Imuladdf | Inegmuladdf | Imulsubf
          | Inegmulsubf | Isqrtf | Imove32
          | Ishiftarith (_, _)
          | Ibswap _ | Isignext _ | Isimd _ ))
    | Lop
        ( Move | Spill | Reload | Opaque | Pause | Begin_region | End_region
        | Dls_get | Tls_get | Const_int _ | Const_float32 _ | Const_float _
        | Const_symbol _ | Const_vec128 _ | Stackoffset _ | Load _
        | Store (_, _, _)
        | Intop _ | Int128op _
        | Intop_imm (_, _)
        | Intop_atomic _
        | Floatop (_, _)
        | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
        | Name_for_debugger _ )
    | Lprologue | Lepilogue_open | Lepilogue_close | Lreloadretaddr | Lreturn
    | Lentertrap | Lpoptrap _ | Lcall_op _ | Llabel _ | Lbranch _
    | Lcondbranch (_, _)
    | Lcondbranch3 (_, _, _)
    | Lswitch _ | Ladjust_stack_offset _ | Lpushtrap _ | Lraise _
    | Lstackcheck _ ->
      loop instr.next call_gc
    | Lop (Const_vec256 _ | Const_vec512 _) ->
      Misc.fatal_error "arm64: got 256/512 bit vector"
    | Lop (Specific (Illvm_intrinsic intr)) ->
      Misc.fatal_errorf
        "Emit: Unexpected llvm_intrinsic %s: not using LLVM backend" intr
  in
  loop instr 0

let max_out_of_line_code_offset ~num_call_gc =
  if num_call_gc < 1
  then 0
  else
    let size_of_call_gc = 2 in
    let size_of_last_thing = size_of_call_gc in
    let total_size = size_of_call_gc * num_call_gc in
    let max_offset = total_size - size_of_last_thing in
    assert (max_offset >= 0);
    max_offset

module BR = Branch_relaxation.Make (struct
  (* CR-someday mshinwell: B and BL have +/- 128Mb ranges; for the moment we
     assume we will never exceed this. It would seem to be most likely to occur
     for branches between functions; in this case, the linker should be able to
     insert veneers anyway. (See section 4.6.7 of the document "ELF for the ARM
     64-bit architecture (AArch64)".) *)

  type distance = int

  module Cond_branch = struct
    type t =
      | TB
      | CB
      | Bcc

    let all = [TB; CB; Bcc]

    (* AArch64 instructions are 32 bits wide, so [distance] in this module means
       units of 32-bit words. *)
    let max_displacement = function
      | TB -> 32 * 1024 / 4 (* +/- 32Kb *)
      | CB | Bcc -> 1 * 1024 * 1024 / 4 (* +/- 1Mb *)

    let classify_instr = function
      | Lop (Alloc _) | Lop Poll -> Some Bcc
      (* The various "far" variants in [specific_operation] don't need to return
         [Some] here, since their code sequences never contain any conditional
         branches that might need relaxing. *)
      | Lcondbranch (Itruetest, _) | Lcondbranch (Ifalsetest, _) -> Some CB
      | Lcondbranch (Iinttest _, _)
      | Lcondbranch (Iinttest_imm _, _)
      | Lcondbranch (Ifloattest _, _) ->
        Some Bcc
      | Lcondbranch (Ioddtest, _) | Lcondbranch (Ieventest, _) -> Some TB
      | Lcondbranch3 _ -> Some Bcc
      | Lop
          ( Specific _ | Move | Spill | Reload | Opaque | Begin_region | Pause
          | End_region | Dls_get | Tls_get | Const_int _ | Const_float32 _
          | Const_float _ | Const_symbol _ | Const_vec128 _ | Stackoffset _
          | Load _
          | Store (_, _, _)
          | Intop _ | Int128op _
          | Intop_imm (_, _)
          | Intop_atomic _
          | Floatop (_, _)
          | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
          | Name_for_debugger _ )
      | Lprologue | Lepilogue_open | Lepilogue_close | Lend | Lreloadretaddr
      | Lreturn | Lentertrap | Lpoptrap _ | Lcall_op _ | Llabel _ | Lbranch _
      | Lswitch _ | Ladjust_stack_offset _ | Lpushtrap _ | Lraise _
      | Lstackcheck _ ->
        None
      | Lop (Const_vec256 _ | Const_vec512 _) ->
        Misc.fatal_error "arm64: got 256/512 bit vector"
  end

  let offset_pc_at_branch = 0

  let prologue_size () =
    (if initial_stack_offset () > 0 then 2 else 0)
    + if !contains_calls then 1 else 0

  let epilogue_size () = if !contains_calls then 3 else 2

  let memory_access_size (memory_chunk : Cmm.memory_chunk) =
    match memory_chunk with
    | Single { reg = Float64 } -> 2
    | Single { reg = Float32 } -> 1
    | Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
    | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val | Double
    | Onetwentyeight_unaligned | Onetwentyeight_aligned ->
      1
    | Twofiftysix_aligned | Twofiftysix_unaligned | Fivetwelve_aligned
    | Fivetwelve_unaligned ->
      Misc.fatal_error "arm64: got 256/512 bit vector"

  let instr_size = function
    | Lend -> 0
    | Lprologue -> prologue_size ()
    | Lepilogue_open -> epilogue_size ()
    | Lepilogue_close -> 0
    | Lop (Move | Spill | Reload) -> 1
    | Lop (Const_int n) -> num_instructions_for_intconst n
    | Lop (Const_float32 _) -> 2
    | Lop (Const_float _) -> 2
    | Lop (Const_vec128 _) -> 2
    | Lop (Const_vec256 _ | Const_vec512 _) ->
      Misc.fatal_error "arm64: got 256/512 bit vector"
    | Lop (Const_symbol _) -> 2
    | Lop (Intop_atomic _) ->
      (* Never generated; builtins are not yet translated to atomics *)
      assert false
    | Lcall_op Lcall_ind -> 1
    | Lcall_op (Lcall_imm _) -> 1
    | Lcall_op Ltailcall_ind -> epilogue_size ()
    | Lcall_op (Ltailcall_imm { func; _ }) ->
      if String.equal func.sym_name !function_name then 1 else epilogue_size ()
    | Lcall_op
        (Lextcall
          { alloc;
            stack_ofs;
            stack_align = _;
            func = _;
            ty_res = _;
            ty_args = _;
            returns = _
          }) ->
      if Config.runtime5 && stack_ofs > 0 then 5 else if alloc then 3 else 5
    | Lop (Stackoffset _) -> 2
    | Lop (Load { memory_chunk; addressing_mode; is_atomic; mutability = _ }) ->
      let based = match addressing_mode with Iindexed _ -> 0 | Ibased _ -> 1
      and barrier = if is_atomic then 1 else 0
      and single = memory_access_size memory_chunk in
      based + barrier + single
    | Lop (Store (memory_chunk, addressing_mode, assignment)) ->
      let based = match addressing_mode with Iindexed _ -> 0 | Ibased _ -> 1
      and barrier =
        match memory_chunk, assignment with
        | (Word_int | Word_val), true -> 1
        | (Word_int | Word_val), false -> 0
        | ( ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
            | Thirtytwo_unsigned | Thirtytwo_signed | Single _ | Double
            | Onetwentyeight_unaligned | Onetwentyeight_aligned ),
            _ ) ->
          0
        | ( ( Twofiftysix_aligned | Twofiftysix_unaligned | Fivetwelve_aligned
            | Fivetwelve_unaligned ),
            _ ) ->
          Misc.fatal_error "arm64: got 256/512 bit vector"
      and single = memory_access_size memory_chunk in
      based + barrier + single
    | Lop (Alloc { mode = Local; _ }) -> 9
    | Lop (Alloc { mode = Heap; _ }) when !fastcode_flag -> 5
    | Lop (Specific (Ifar_alloc _)) when !fastcode_flag -> 6
    | Lop Poll -> 3
    | Lop Pause -> 1
    | Lop (Specific Ifar_poll) -> 4
    | Lop (Alloc { mode = Heap; bytes = num_bytes; _ })
    | Lop (Specific (Ifar_alloc { bytes = num_bytes; _ })) -> (
      match num_bytes with
      | 16 | 24 | 32 -> 1
      | _ -> 1 + num_instructions_for_intconst (Nativeint.of_int num_bytes))
    | Lop (Csel _) -> 4
    | Lop (Begin_region | End_region) -> 1
    | Lop (Intop (Icomp _)) -> 2
    | Lop (Floatop (Float64, Icompf _)) -> 2
    | Lop (Floatop (Float32, Icompf _)) -> 2
    | Lop (Intop_imm (Icomp _, _)) -> 2
    | Lop (Int128op (Iadd128 | Isub128 | Imul64 _)) -> 2
    | Lop (Intop Imod) -> 2
    | Lop (Intop (Imulh _)) -> 1
    | Lop (Intop (Iclz _)) -> 1
    | Lop (Intop (Ictz _)) -> if !Arch.feat_cssc then 1 else 2
    | Lop (Intop Ipopcnt) -> if !Arch.feat_cssc then 1 else 4
    | Lop
        (Intop
          (Iadd | Isub | Imul | Idiv | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr))
      ->
      1
    | Lop
        (Intop_imm
          ( ( Iadd | Isub | Imul | Idiv | Imod | Imulh _ | Iand | Ior | Ixor
            | Ilsl | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt ),
            _ )) ->
      1
    | Lop (Floatop (Float64, (Iabsf | Inegf))) -> 1
    | Lop (Floatop (Float32, (Iabsf | Inegf))) -> 1
    | Lop (Specific Isqrtf) -> 1
    | Lop
        (Reinterpret_cast
          (Value_of_int | Int_of_value | Float_of_int64 | Int64_of_float)) ->
      1
    | Lop
        (Reinterpret_cast
          ( Float32_of_float | Float_of_float32 | Float32_of_int32
          | Int32_of_float32 )) ->
      1
    | Lop (Reinterpret_cast (V128_of_vec Vec128)) -> 1
    | Lop
        (Reinterpret_cast
          (V128_of_vec (Vec256 | Vec512) | V256_of_vec _ | V512_of_vec _)) ->
      Misc.fatal_error "arm64: got 256/512 bit vector"
    | Lop (Static_cast (Float_of_int Float64 | Int_of_float Float64)) -> 1
    | Lop
        (Static_cast
          ( Float_of_int Float32
          | Int_of_float Float32
          | Float_of_float32 | Float32_of_float )) ->
      1
    | Lop (Static_cast (Scalar_of_v128 Float16x8 | V128_of_scalar Float16x8)) ->
      Misc.fatal_error "float16 scalar type not supported"
    | Lop (Static_cast (Scalar_of_v128 (Int8x16 | Int16x8))) -> 2
    | Lop
        (Static_cast
          (Scalar_of_v128 (Int32x4 | Int64x2 | Float32x4 | Float64x2))) ->
      1
    | Lop (Static_cast (V128_of_scalar _)) -> 1
    | Lop
        (Static_cast
          ( V256_of_scalar _ | Scalar_of_v256 _ | V512_of_scalar _
          | Scalar_of_v512 _ )) ->
      Misc.fatal_error "arm64: got 256/512 bit vector"
    | Lop (Floatop (Float64, (Iaddf | Isubf | Imulf | Idivf))) -> 1
    | Lop (Floatop (Float32, (Iaddf | Isubf | Imulf | Idivf))) -> 1
    | Lop (Specific Inegmulf) -> 1
    | Lop Opaque -> 0
    | Lop (Specific (Imuladdf | Inegmuladdf | Imulsubf | Inegmulsubf)) -> 1
    | Lop (Specific (Ishiftarith _)) -> 1
    | Lop (Specific (Imuladd | Imulsub)) -> 1
    | Lop (Specific (Ibswap { bitwidth = Sixteen })) -> 2
    | Lop (Specific (Ibswap { bitwidth = Thirtytwo | Sixtyfour })) -> 1
    | Lop (Specific Imove32) -> 1
    | Lop (Specific (Isignext _)) -> 1
    | Lop (Name_for_debugger _) -> 0
    | Lcall_op (Lprobe _) ->
      fatal_error "Optimized probes not supported on arm64."
    | Lop (Probe_is_enabled _) -> 3
    | Lop Dls_get -> 1
    | Lop Tls_get -> 1
    | Lreloadretaddr -> 0
    | Lreturn -> epilogue_size ()
    | Llabel _ -> 0
    | Lbranch _ -> 1
    | Lcondbranch (tst, _) -> (
      match tst with
      | Itruetest -> 1
      | Ifalsetest -> 1
      | Iinttest _ -> 2
      | Iinttest_imm _ -> 2
      | Ifloattest _ -> 2
      | Ioddtest -> 1
      | Ieventest -> 1)
    | Lcondbranch3 (lbl0, lbl1, lbl2) -> (
      1
      + (match lbl0 with None -> 0 | Some _ -> 1)
      + (match lbl1 with None -> 0 | Some _ -> 1)
      + match lbl2 with None -> 0 | Some _ -> 1)
    | Lswitch jumptbl -> 3 + Array.length jumptbl
    | Lentertrap -> 0
    | Ladjust_stack_offset _ -> 0
    | Lpushtrap _ -> 4
    | Lpoptrap _ -> 1
    | Lraise k -> (
      match k with
      | Lambda.Raise_regular -> 1
      | Lambda.Raise_reraise -> 1
      | Lambda.Raise_notrace -> 4)
    | Lstackcheck _ -> 5
    | Lop (Specific (Isimd simd)) -> simd_instr_size simd
    | Lop (Specific (Illvm_intrinsic intr)) ->
      Misc.fatal_errorf
        "Emit.size:Unexpected llvm_intrinsic %s: not using LLVM backend" intr

  let relax_poll () = Lop (Specific Ifar_poll)

  let relax_allocation ~num_bytes ~dbginfo =
    Lop (Specific (Ifar_alloc { bytes = num_bytes; dbginfo }))
end)

let cond_for_float_comparison : Cmm.float_comparison -> DSL.Float_cond.t =
  function
  | CFeq -> EQ
  | CFneq -> NE
  | CFlt -> CC
  | CFnlt -> CS
  | CFle -> LS
  | CFnle -> HI
  | CFgt -> GT
  | CFngt -> LE
  | CFge -> GE
  | CFnge -> LT

let cond_for_cset_for_float_comparison : Cmm.float_comparison -> DSL.Cond.t =
  function
  | CFeq -> EQ
  | CFneq -> NE
  | CFlt -> CC
  | CFnlt -> CS
  | CFle -> LS
  | CFnle -> HI
  | CFgt -> GT
  | CFngt -> LE
  | CFge -> GE
  | CFnge -> LT

(* Output the assembly code for allocation. *)

let assembly_code_for_allocation i ~local ~n ~far ~dbginfo =
  if local
  then (
    let r = i.res.(0) in
    let module DS = Domainstate in
    let domain_local_sp_offset = DS.(idx_of_field Domain_local_sp) * 8 in
    let domain_local_limit_offset = DS.(idx_of_field Domain_local_limit) * 8 in
    let domain_local_top_offset = DS.(idx_of_field Domain_local_top) * 8 in
    A.ins2 LDR
      ( DSL.reg_x reg_tmp1,
        DSL.addressing (Iindexed domain_local_limit_offset) reg_domain_state_ptr
      );
    A.ins2 LDR
      ( DSL.reg_x r,
        DSL.addressing (Iindexed domain_local_sp_offset) reg_domain_state_ptr );
    emit_subimm r r n;
    A.ins2 STR
      ( DSL.reg_x r,
        DSL.addressing (Iindexed domain_local_sp_offset) reg_domain_state_ptr );
    A.ins_cmp_reg (DSL.reg_x r) (DSL.reg_x reg_tmp1) DSL.optional_none;
    let lbl_call = L.create Text in
    A.ins1 (B_cond LT) (local_label lbl_call);
    let lbl_after_alloc = L.create Text in
    D.define_label lbl_after_alloc;
    A.ins2 LDR
      ( DSL.reg_x reg_tmp1,
        DSL.addressing (Iindexed domain_local_top_offset) reg_domain_state_ptr
      );
    A.ins4 ADD_shifted_register
      (DSL.reg_x r, DSL.reg_x r, DSL.reg_x reg_tmp1, DSL.optional_none);
    A.ins4 ADD_immediate (DSL.reg_x r, DSL.reg_x r, DSL.imm 8, DSL.optional_none);
    local_realloc_sites
      := { lr_lbl = lbl_call; lr_dbg = i.dbg; lr_return_lbl = lbl_after_alloc }
         :: !local_realloc_sites)
  else
    let lbl_frame = record_frame_label i.live (Dbg_alloc dbginfo) in
    if !fastcode_flag
    then (
      let lbl_after_alloc = L.create Text in
      let lbl_call_gc = L.create Text in
      (*= n is at most Max_young_whsize * 8, i.e. currently 0x808,
         so it is reasonable to assume n < 0x1_000.  This makes
         the generated code simpler. *)
      assert (16 <= n && n < 0x1_000 && n land 0x7 = 0);
      let offset = Domainstate.(idx_of_field Domain_young_limit) * 8 in
      A.ins2 LDR
        ( DSL.reg_x reg_tmp1,
          DSL.addressing (Iindexed offset) reg_domain_state_ptr );
      emit_subimm reg_alloc_ptr reg_alloc_ptr n;
      A.ins_cmp_reg (DSL.reg_x reg_alloc_ptr) (DSL.reg_x reg_tmp1)
        DSL.optional_none;
      (if not far
      then A.ins1 (B_cond CC) (local_label lbl_call_gc)
      else
        let lbl = L.create Text in
        A.ins1 (B_cond CS) (local_label lbl);
        A.ins1 B (local_label lbl_call_gc);
        D.define_label lbl);
      A.labeled_ins4 lbl_after_alloc ADD_immediate
        ( DSL.reg_x i.res.(0),
          DSL.reg_x reg_alloc_ptr,
          DSL.imm 8,
          DSL.optional_none );
      call_gc_sites
        := { gc_lbl = lbl_call_gc;
             gc_return_lbl = lbl_after_alloc;
             gc_frame_lbl = lbl_frame
           }
           :: !call_gc_sites)
    else (
      (match n with
      | 16 -> A.ins1 BL (runtime_function "caml_alloc1")
      | 24 -> A.ins1 BL (runtime_function "caml_alloc2")
      | 32 -> A.ins1 BL (runtime_function "caml_alloc3")
      | _ ->
        emit_intconst reg_x8 (Nativeint.of_int n);
        A.ins1 BL (runtime_function "caml_allocN"));
      A.labeled_ins4 lbl_frame ADD_immediate
        ( DSL.reg_x i.res.(0),
          DSL.reg_x reg_alloc_ptr,
          DSL.imm 8,
          DSL.optional_none ))

let assembly_code_for_poll i ~far ~return_label =
  let lbl_frame = record_frame_label i.live (Dbg_alloc []) in
  let lbl_call_gc = L.create Text in
  let lbl_after_poll =
    match return_label with None -> L.create Text | Some lbl -> lbl
  in
  let offset = Domainstate.(idx_of_field Domain_young_limit) * 8 in
  A.ins2 LDR
    (DSL.reg_x reg_tmp1, DSL.addressing (Iindexed offset) reg_domain_state_ptr);
  A.ins_cmp_reg (DSL.reg_x reg_alloc_ptr) (DSL.reg_x reg_tmp1) DSL.optional_none;
  (if not far
  then (
    match return_label with
    | None ->
      A.ins1 (B_cond LS) (local_label lbl_call_gc);
      D.define_label lbl_after_poll
    | Some return_label ->
      A.ins1 (B_cond HI) (local_label return_label);
      A.ins1 B (local_label lbl_call_gc))
  else
    match return_label with
    | None ->
      A.ins1 (B_cond HI) (local_label lbl_after_poll);
      A.ins1 B (local_label lbl_call_gc);
      D.define_label lbl_after_poll
    | Some return_label ->
      let lbl = L.create Text in
      A.ins1 (B_cond LS) (local_label lbl);
      A.ins1 B (local_label return_label);
      A.labeled_ins1 lbl B (local_label lbl_call_gc));
  call_gc_sites
    := { gc_lbl = lbl_call_gc;
         gc_return_lbl = lbl_after_poll;
         gc_frame_lbl = lbl_frame
       }
       :: !call_gc_sites

(* Output .text section directive, or named .text.caml.<name> if enabled. *)

let emit_named_text_section func_name =
  if !Clflags.function_sections
  then (
    (* CR sspies: Clean this up and add proper support for function sections in
       the new asm directives. *)
    D.switch_to_section_raw
      ~names:[".text.caml." ^ S.encode (S.create_global func_name)]
      ~flags:(Some "ax") ~args:["%progbits"] ~is_delayed:false;
    (* Warning: We set the internal section ref to Text here, because it
       currently does not supported named text sections. In the rest of this
       file, we pretend the section is called Text rather than the function
       specific text section. *)
    D.unsafe_set_internal_section_ref Text)
  else D.text ()

(* Emit code to load an emitted literal *)

let emit_load_literal dst lbl =
  let open Arm64_ast.Ast.Symbol in
  if macosx
  then (
    A.ins2 ADRP (DSL.reg_x reg_tmp1, DSL.label (Needs_reloc PAGE) lbl);
    match dst.typ with
    | Float ->
      A.ins2 LDR_simd_and_fp
        ( DSL.reg_d dst,
          DSL.emit_mem_label reg_tmp1 ~reloc:(Needs_reloc PAGE_OFF) lbl )
    | Float32 ->
      A.ins2 LDR_simd_and_fp
        ( DSL.reg_s dst,
          DSL.emit_mem_label reg_tmp1 ~reloc:(Needs_reloc PAGE_OFF) lbl )
    | Val | Int | Addr ->
      A.ins2 LDR
        ( DSL.reg_x dst,
          DSL.emit_mem_label reg_tmp1 ~reloc:(Needs_reloc PAGE_OFF) lbl )
    | Vec128 | Valx2 ->
      A.ins2 LDR_simd_and_fp
        ( DSL.reg_q_operand dst,
          DSL.emit_mem_label reg_tmp1 ~reloc:(Needs_reloc PAGE_OFF) lbl )
    | Vec256 | Vec512 ->
      Misc.fatal_errorf "emit_load_literal: unexpected vector register %a"
        Printreg.reg dst)
  else (
    A.ins2 ADRP (DSL.reg_x reg_tmp1, DSL.label (Needs_reloc PAGE) lbl);
    match dst.typ with
    | Float ->
      A.ins2 LDR_simd_and_fp
        ( DSL.reg_d dst,
          DSL.emit_mem_label reg_tmp1 ~reloc:(Needs_reloc LOWER_TWELVE) lbl )
    | Float32 ->
      A.ins2 LDR_simd_and_fp
        ( DSL.reg_s dst,
          DSL.emit_mem_label reg_tmp1 ~reloc:(Needs_reloc LOWER_TWELVE) lbl )
    | Val | Int | Addr ->
      A.ins2 LDR
        ( DSL.reg_x dst,
          DSL.emit_mem_label reg_tmp1 ~reloc:(Needs_reloc LOWER_TWELVE) lbl )
    | Vec128 | Valx2 ->
      A.ins2 LDR_simd_and_fp
        ( DSL.reg_q_operand dst,
          DSL.emit_mem_label reg_tmp1 ~reloc:(Needs_reloc LOWER_TWELVE) lbl )
    | Vec256 | Vec512 ->
      Misc.fatal_errorf "emit_load_literal: unexpected vector register %a"
        Printreg.reg dst)

let move (src : Reg.t) (dst : Reg.t) =
  let distinct = not (Reg.same_loc src dst) in
  if distinct
  then
    match src.typ, src.loc, dst.typ, dst.loc with
    | Float, Reg _, Float, Reg _ -> A.ins2 FMOV_fp (DSL.reg_d dst, DSL.reg_d src)
    | Float32, Reg _, Float32, Reg _ ->
      A.ins2 FMOV_fp (DSL.reg_s dst, DSL.reg_s src)
    | (Vec128 | Valx2), Reg _, (Vec128 | Valx2), Reg _ ->
      A.ins_mov_vector (DSL.reg_v16b_operand dst) (DSL.reg_v16b_operand src)
    | (Vec256 | Vec512), _, _, _ | _, _, (Vec256 | Vec512), _ ->
      Misc.fatal_error "arm64: got 256/512 bit vector"
    | (Int | Val | Addr), Reg _, (Int | Val | Addr), Reg _ ->
      A.ins_mov_reg (DSL.reg_x dst) (DSL.reg_x src)
    | Float, Reg _, Float, Stack _ ->
      A.ins2 STR_simd_and_fp (DSL.reg_d src, DSL.stack dst)
    | Float32, Reg _, Float32, Stack _ ->
      A.ins2 STR_simd_and_fp (DSL.reg_s src, DSL.stack dst)
    | (Vec128 | Valx2), Reg _, (Vec128 | Valx2), Stack _ ->
      A.ins2 STR_simd_and_fp (DSL.reg_q_operand src, DSL.stack dst)
    | (Int | Val | Addr), Reg _, (Int | Val | Addr), Stack _ ->
      A.ins2 STR (DSL.reg_x src, DSL.stack dst)
    | Float, Stack _, Float, Reg _ ->
      A.ins2 LDR_simd_and_fp (DSL.reg_d dst, DSL.stack src)
    | Float32, Stack _, Float32, Reg _ ->
      A.ins2 LDR_simd_and_fp (DSL.reg_s dst, DSL.stack src)
    | (Vec128 | Valx2), Stack _, (Vec128 | Valx2), Reg _ ->
      A.ins2 LDR_simd_and_fp (DSL.reg_q_operand dst, DSL.stack src)
    | (Int | Val | Addr), Stack _, (Int | Val | Addr), Reg _ ->
      A.ins2 LDR (DSL.reg_x dst, DSL.stack src)
    | _, Stack _, _, Stack _ ->
      Misc.fatal_errorf "Illegal move between stack slots (%a to %a)\n"
        Printreg.reg src Printreg.reg dst
    | _, Unknown, _, (Reg _ | Stack _ | Unknown)
    | _, (Reg _ | Stack _), _, Unknown ->
      Misc.fatal_errorf
        "Illegal move with an unknown register location (%a to %a)\n"
        Printreg.reg src Printreg.reg dst
    | ( (Float | Float32 | Vec128 | Int | Val | Addr | Valx2),
        (Reg _ | Stack _),
        _,
        _ ) ->
      Misc.fatal_errorf
        "Illegal move between registers of differing types (%a to %a)\n"
        Printreg.reg src Printreg.reg dst

let emit_reinterpret_cast (cast : Cmm.reinterpret_cast) i =
  let src = i.arg.(0) in
  let dst = i.res.(0) in
  let distinct = not (Reg.same_loc src dst) in
  match cast with
  | Int64_of_float -> A.ins2 FMOV_fp_to_gp_64 (DSL.reg_x dst, DSL.reg_d src)
  | Float_of_int64 -> A.ins2 FMOV_gp_to_fp_64 (DSL.reg_d dst, DSL.reg_x src)
  | Float32_of_int32 -> A.ins2 FMOV_gp_to_fp_32 (DSL.reg_s dst, DSL.reg_w src)
  | Int32_of_float32 -> A.ins2 FMOV_fp_to_gp_32 (DSL.reg_w dst, DSL.reg_s src)
  | Float32_of_float ->
    (* Reinterpret cast: treat both as d registers for FMOV if distinct. *)
    if distinct
    then (
      assert (Cmm.equal_machtype_component src.typ Float);
      assert (Cmm.equal_machtype_component dst.typ Float32);
      A.ins2 FMOV_fp (DSL.reg_d_of_float_reg dst, DSL.reg_d_of_float_reg src))
  | Float_of_float32 ->
    (* Reinterpret cast: treat both as d registers for FMOV if distinct. *)
    if distinct
    then (
      assert (Cmm.equal_machtype_component src.typ Float32);
      assert (Cmm.equal_machtype_component dst.typ Float);
      A.ins2 FMOV_fp (DSL.reg_d_of_float_reg dst, DSL.reg_d_of_float_reg src))
  | V128_of_vec Vec128 ->
    if distinct
    then A.ins_mov_vector (DSL.reg_v16b_operand dst) (DSL.reg_v16b_operand src)
  | V128_of_vec (Vec256 | Vec512) | V256_of_vec _ | V512_of_vec _ ->
    Misc.fatal_error "arm64: got 256/512 bit vector"
  | Int_of_value | Value_of_int -> move src dst

let emit_static_cast (cast : Cmm.static_cast) i =
  let dst = i.res.(0) in
  let src = i.arg.(0) in
  let distinct = not (Reg.same_loc src dst) in
  match cast with
  | Int_of_float Float64 -> A.ins2 FCVTZS (DSL.reg_x dst, DSL.reg_d src)
  | Int_of_float Float32 -> A.ins2 FCVTZS (DSL.reg_x dst, DSL.reg_s src)
  | Float_of_int Float64 -> A.ins2 SCVTF (DSL.reg_d dst, DSL.reg_x src)
  | Float_of_int Float32 -> A.ins2 SCVTF (DSL.reg_s dst, DSL.reg_x src)
  | Float_of_float32 -> A.ins2 FCVT (DSL.reg_d dst, DSL.reg_s src)
  | Float32_of_float -> A.ins2 FCVT (DSL.reg_s dst, DSL.reg_d src)
  | Scalar_of_v128 v -> (
    match v with
    | Int8x16 ->
      (* Note this uses [reg_s] even though the source is wider *)
      A.ins2 FMOV_fp_to_gp_32 (DSL.reg_w dst, DSL.reg_s src);
      A.ins_uxtb (DSL.reg_w dst) (DSL.reg_w dst)
    | Int16x8 ->
      A.ins2 FMOV_fp_to_gp_32 (DSL.reg_w dst, DSL.reg_s src);
      A.ins_uxth (DSL.reg_w dst) (DSL.reg_w dst)
    | Int32x4 -> A.ins2 FMOV_fp_to_gp_32 (DSL.reg_w dst, DSL.reg_s src)
    | Int64x2 -> A.ins2 FMOV_fp_to_gp_64 (DSL.reg_x dst, DSL.reg_d src)
    | Float16x8 -> Misc.fatal_error "float16 scalar type not supported"
    | Float32x4 -> if distinct then A.ins2 FMOV_fp (DSL.reg_s dst, DSL.reg_s src)
    | Float64x2 -> if distinct then A.ins2 FMOV_fp (DSL.reg_d dst, DSL.reg_d src)
    )
  | V128_of_scalar v -> (
    match v with
    | Int8x16 -> A.ins2 FMOV_gp_to_fp_32 (DSL.reg_s dst, DSL.reg_w src)
    | Int16x8 -> A.ins2 FMOV_gp_to_fp_32 (DSL.reg_s dst, DSL.reg_w src)
    | Int32x4 -> A.ins2 FMOV_gp_to_fp_32 (DSL.reg_s dst, DSL.reg_w src)
    | Int64x2 -> A.ins2 FMOV_gp_to_fp_64 (DSL.reg_d dst, DSL.reg_x src)
    | Float16x8 -> Misc.fatal_error "float16 scalar type not supported"
    | Float32x4 -> if distinct then A.ins2 FMOV_fp (DSL.reg_s dst, DSL.reg_s src)
    | Float64x2 -> if distinct then A.ins2 FMOV_fp (DSL.reg_d dst, DSL.reg_d src)
    )
  | V256_of_scalar _ | Scalar_of_v256 _ | V512_of_scalar _ | Scalar_of_v512 _ ->
    Misc.fatal_error "arm64: got 256/512 bit vector"

(* Output the assembly code for an instruction *)

let emit_instr i =
  emit_debug_info i.dbg;
  match i.desc with
  | Lend -> ()
  | Lprologue ->
    assert !prologue_required;
    let n = frame_size () in
    if n > 0 then emit_stack_adjustment (-n);
    if !contains_calls
    then (
      D.cfi_offset ~reg:30 (* return address *) ~offset:(-8);
      A.ins2 STR
        (DSL.lr (), DSL.mem_offset ~base:(DSL.Reg.sp ()) ~offset:(n - 8)))
  | Lepilogue_open ->
    let n = frame_size () in
    if !contains_calls
    then
      A.ins2 LDR
        (DSL.lr (), DSL.mem_offset ~base:(DSL.Reg.sp ()) ~offset:(n - 8));
    if n > 0 then emit_stack_adjustment n
  | Lepilogue_close ->
    let n = frame_size () in
    if n > 0 then D.cfi_adjust_cfa_offset ~bytes:n
  | Lop (Intop_atomic _) ->
    (* Never generated; builtins are not yet translated to atomics *)
    assert false
  | Lop (Reinterpret_cast cast) -> emit_reinterpret_cast cast i
  | Lop (Static_cast cast) -> emit_static_cast cast i
  | Lop (Move | Spill | Reload) -> move i.arg.(0) i.res.(0)
  | Lop (Specific Imove32) -> (
    let src = i.arg.(0) and dst = i.res.(0) in
    if not (Reg.same_loc src dst)
    then
      match src.loc, dst.loc with
      | Reg _, Reg _ -> A.ins_mov_reg_w (DSL.reg_w dst) (DSL.reg_w src)
      | Reg _, Stack _ -> A.ins2 STR (DSL.reg_w src, DSL.stack dst)
      | Stack _, Reg _ -> A.ins2 LDR (DSL.reg_w dst, DSL.stack src)
      | Stack _, Stack _ | _, Unknown | Unknown, _ -> assert false)
  | Lop (Const_int n) -> emit_intconst i.res.(0) n
  | Lop (Const_float32 f) ->
    if Int32.equal f 0l
    then A.ins2 FMOV_gp_to_fp_32 (DSL.reg_s i.res.(0), DSL.wzr ())
    else if is_immediate_float32 f
    then
      A.ins2 FMOV_scalar_immediate
        (DSL.reg_s i.res.(0), DSL.imm_float (Int32.float_of_bits f))
    else
      (* float32 constants take up 8 bytes when we emit them with
         [float_literal] (see the conversion from int32 to int64 below). Thus,
         we load the lower half. Note that this is different from Cmm 32-bit
         floats ([Csingle]), which are emitted as 4-byte constants. *)
      let lbl = float32_literal f in
      emit_load_literal i.res.(0) lbl
  | Lop (Const_float f) ->
    if Int64.equal f 0L
    then A.ins2 FMOV_gp_to_fp_64 (DSL.reg_d i.res.(0), DSL.xzr ())
    else if is_immediate_float f
    then
      A.ins2 FMOV_scalar_immediate
        (DSL.reg_d i.res.(0), DSL.imm_float (Int64.float_of_bits f))
    else
      let lbl = float_literal f in
      emit_load_literal i.res.(0) lbl
  | Lop (Const_vec256 _ | Const_vec512 _) ->
    Misc.fatal_error "arm64: got 256/512 bit vector"
  | Lop (Const_vec128 ({ word0; word1 } as l)) -> (
    match word0, word1 with
    | 0x0000_0000_0000_0000L, 0x0000_0000_0000_0000L ->
      let dst = DSL.reg_v2d_operand i.res.(0) in
      A.ins2 MOVI (dst, DSL.imm 0)
    | _ ->
      let lbl = vec128_literal l in
      emit_load_literal i.res.(0) lbl)
  | Lop (Const_symbol s) ->
    emit_load_symbol_addr i.res.(0) (symbol_of_cmm_symbol s)
  | Lcall_op Lcall_ind ->
    A.ins1 BLR (DSL.reg_x i.arg.(0));
    record_frame i.live (Dbg_other i.dbg)
  | Lcall_op (Lcall_imm { func }) ->
    A.ins1 BL (DSL.symbol (Needs_reloc CALL26) (symbol_of_cmm_symbol func));
    record_frame i.live (Dbg_other i.dbg)
  | Lcall_op Ltailcall_ind -> A.ins1 BR (DSL.reg_x i.arg.(0))
  | Lcall_op (Ltailcall_imm { func }) ->
    if String.equal func.sym_name !function_name
    then
      match !tailrec_entry_point with
      | None -> Misc.fatal_error "jump to missing tailrec entry point"
      | Some tailrec_entry_point -> A.ins1 B (local_label tailrec_entry_point)
    else A.ins1 B (DSL.symbol (Needs_reloc JUMP26) (symbol_of_cmm_symbol func))
  | Lcall_op (Lextcall { func; alloc; stack_ofs; _ }) ->
    if Config.runtime5 && stack_ofs > 0
    then (
      A.ins_mov_from_sp ~dst:(DSL.reg_x reg_stack_arg_begin);
      A.ins4 ADD_immediate
        ( DSL.reg_x reg_stack_arg_end,
          DSL.sp (),
          DSL.imm (Misc.align stack_ofs 16),
          DSL.optional_none );
      emit_load_symbol_addr reg_x8 (S.create_global func);
      A.ins1 BL (runtime_function "caml_c_call_stack_args");
      record_frame i.live (Dbg_other i.dbg))
    else if alloc
    then (
      emit_load_symbol_addr reg_x8 (S.create_global func);
      A.ins1 BL (runtime_function "caml_c_call");
      record_frame i.live (Dbg_other i.dbg))
    else (
      (*= store ocaml stack in the frame pointer register
             NB: no need to store previous x29 because OCaml frames don't
             maintain frame pointer *)
      if Config.runtime5
      then (
        A.ins_mov_from_sp ~dst:(DSL.fp ());
        D.cfi_remember_state ();
        (* CR mshinwell: name this integer *)
        D.cfi_def_cfa_register ~reg:(Int.to_string 29 (* fp *));
        let offset = Domainstate.(idx_of_field Domain_c_stack) * 8 in
        A.ins2 LDR
          ( DSL.reg_x reg_tmp1,
            DSL.addressing (Iindexed offset) reg_domain_state_ptr );
        A.ins_mov_to_sp ~src:(DSL.reg_x reg_tmp1))
      else D.cfi_remember_state ();
      A.ins1 BL (DSL.symbol (Needs_reloc CALL26) (S.create_global func));
      if Config.runtime5 then A.ins_mov_to_sp ~src:(DSL.fp ());
      D.cfi_restore_state ())
  | Lop (Stackoffset n) ->
    assert (n mod 16 = 0);
    emit_stack_adjustment (-n);
    stack_offset := !stack_offset + n
  | Lop (Load { memory_chunk; addressing_mode; is_atomic; _ }) -> (
    let open Arm64_ast.Ast.Symbol in
    assert (
      Cmm.equal_memory_chunk memory_chunk Cmm.Word_int
      || Cmm.equal_memory_chunk memory_chunk Cmm.Word_val
      || not is_atomic);
    let dst = i.res.(0) in
    let base =
      match addressing_mode with
      | Iindexed _ -> i.arg.(0)
      | Ibased (s, ofs) ->
        assert (not !Clflags.dlcode);
        (* see selection_utils.ml *)
        A.ins2 ADRP
          (DSL.reg_x reg_tmp1, DSL.symbol ~offset:ofs (Needs_reloc PAGE) s);
        reg_tmp1
    in
    let default_addressing = DSL.addressing addressing_mode base in
    match memory_chunk with
    | Byte_unsigned -> A.ins2 LDRB (DSL.reg_w dst, default_addressing)
    | Byte_signed -> A.ins2 LDRSB (DSL.reg_x dst, default_addressing)
    | Sixteen_unsigned -> A.ins2 LDRH (DSL.reg_w dst, default_addressing)
    | Sixteen_signed -> A.ins2 LDRSH (DSL.reg_x dst, default_addressing)
    | Thirtytwo_unsigned -> A.ins2 LDR (DSL.reg_w dst, default_addressing)
    | Thirtytwo_signed -> A.ins2 LDRSW (DSL.reg_x dst, default_addressing)
    | Single { reg = Float64 } ->
      A.ins2 LDR_simd_and_fp (DSL.reg_s7, default_addressing);
      A.ins2 FCVT (DSL.reg_d dst, DSL.reg_s7)
    | Word_int | Word_val ->
      if is_atomic
      then (
        assert (Arch.equal_addressing_mode addressing_mode (Iindexed 0));
        A.ins0 (DMB ISHLD);
        A.ins2 LDAR (DSL.reg_x dst, DSL.mem i.arg.(0)))
      else A.ins2 LDR (DSL.reg_x dst, default_addressing)
    | Double -> A.ins2 LDR_simd_and_fp (DSL.reg_d dst, default_addressing)
    | Single { reg = Float32 } ->
      A.ins2 LDR_simd_and_fp (DSL.reg_s dst, default_addressing)
    | Onetwentyeight_aligned ->
      A.ins2 LDR_simd_and_fp (DSL.reg_q_operand dst, default_addressing)
    | Onetwentyeight_unaligned ->
      (match addressing_mode with
      | Iindexed n ->
        A.ins4 ADD_immediate
          (DSL.reg_x reg_tmp1, DSL.reg_x i.arg.(0), DSL.imm n, DSL.optional_none)
      | Ibased (s, offset) ->
        assert (not !Clflags.dlcode);
        (* see selection_utils.ml *)
        A.ins2 ADRP (DSL.reg_x reg_tmp1, DSL.symbol ~offset (Needs_reloc PAGE) s);
        A.ins4 ADD_immediate
          ( DSL.reg_x reg_tmp1,
            DSL.reg_x reg_tmp1,
            DSL.symbol ~offset (Needs_reloc LOWER_TWELVE) s,
            DSL.optional_none ));
      A.ins2 LDR_simd_and_fp (DSL.reg_q_operand dst, DSL.mem reg_tmp1)
    | Twofiftysix_aligned | Twofiftysix_unaligned | Fivetwelve_aligned
    | Fivetwelve_unaligned ->
      Misc.fatal_error "arm64: got 256/512 bit vector")
  | Lop (Store (size, addr, assignment)) -> (
    let open Arm64_ast.Ast.Symbol in
    (* NB: assignments other than Word_int and Word_val do not follow the
       Multicore OCaml memory model and so do not emit a barrier *)
    let src = i.arg.(0) in
    let base =
      match addr with
      | Iindexed _ -> i.arg.(1)
      | Ibased (s, ofs) ->
        assert (not !Clflags.dlcode);
        A.ins2 ADRP
          (DSL.reg_x reg_tmp1, DSL.symbol ~offset:ofs (Needs_reloc PAGE) s);
        reg_tmp1
    in
    match size with
    | Byte_unsigned | Byte_signed ->
      A.ins2 STRB (DSL.reg_w src, DSL.addressing addr base)
    | Sixteen_unsigned | Sixteen_signed ->
      A.ins2 STRH (DSL.reg_w src, DSL.addressing addr base)
    | Thirtytwo_unsigned | Thirtytwo_signed ->
      A.ins2 STR (DSL.reg_w src, DSL.addressing addr base)
    | Single { reg = Float64 } ->
      A.ins2 FCVT (DSL.reg_s7, DSL.reg_d src);
      A.ins2 STR_simd_and_fp (DSL.reg_s7, DSL.addressing addr base)
    | Word_int | Word_val ->
      (* memory model barrier for non-initializing store *)
      if assignment then A.ins0 (DMB ISHLD);
      A.ins2 STR (DSL.reg_x src, DSL.addressing addr base)
    | Double -> A.ins2 STR_simd_and_fp (DSL.reg_d src, DSL.addressing addr base)
    | Single { reg = Float32 } ->
      A.ins2 STR_simd_and_fp (DSL.reg_s src, DSL.addressing addr base)
    | Onetwentyeight_aligned ->
      A.ins2 STR_simd_and_fp (DSL.reg_q_operand src, DSL.addressing addr base)
    | Onetwentyeight_unaligned -> (
      match addr with
      | Iindexed n ->
        A.ins4 ADD_immediate
          (DSL.reg_x reg_tmp1, DSL.reg_x i.arg.(1), DSL.imm n, DSL.optional_none);
        A.ins2 STR_simd_and_fp (DSL.reg_q_operand src, DSL.mem reg_tmp1)
      | Ibased (s, offset) ->
        assert (not !Clflags.dlcode);
        (* see selection_utils.ml *)
        A.ins2 ADRP (DSL.reg_x reg_tmp1, DSL.symbol ~offset (Needs_reloc PAGE) s);
        A.ins4 ADD_immediate
          ( DSL.reg_x reg_tmp1,
            DSL.reg_x reg_tmp1,
            DSL.symbol ~offset (Needs_reloc LOWER_TWELVE) s,
            DSL.optional_none );
        A.ins2 STR_simd_and_fp (DSL.reg_q_operand src, DSL.mem reg_tmp1))
    | Twofiftysix_aligned | Twofiftysix_unaligned | Fivetwelve_aligned
    | Fivetwelve_unaligned ->
      Misc.fatal_error "arm64: got 256/512 bit vector")
  | Lop (Alloc { bytes = n; dbginfo; mode = Heap }) ->
    assembly_code_for_allocation i ~n ~local:false ~far:false ~dbginfo
  | Lop (Specific (Ifar_alloc { bytes = n; dbginfo })) ->
    assembly_code_for_allocation i ~n ~local:false ~far:true ~dbginfo
  | Lop (Alloc { bytes = n; dbginfo; mode = Local }) ->
    assembly_code_for_allocation i ~n ~local:true ~far:false ~dbginfo
  | Lop Begin_region ->
    let offset = Domainstate.(idx_of_field Domain_local_sp) * 8 in
    A.ins2 LDR
      ( DSL.reg_x i.res.(0),
        DSL.addressing (Iindexed offset) reg_domain_state_ptr )
  | Lop End_region ->
    let offset = Domainstate.(idx_of_field Domain_local_sp) * 8 in
    A.ins2 STR
      ( DSL.reg_x i.arg.(0),
        DSL.addressing (Iindexed offset) reg_domain_state_ptr )
  | Lop Poll -> assembly_code_for_poll i ~far:false ~return_label:None
  | Lop Pause -> A.ins0 YIELD
  | Lop (Specific Ifar_poll) ->
    assembly_code_for_poll i ~far:true ~return_label:None
  | Lop (Intop_imm (Iadd, n)) -> emit_addimm i.res.(0) i.arg.(0) n
  | Lop (Intop_imm (Isub, n)) -> emit_subimm i.res.(0) i.arg.(0) n
  | Lop (Intop (Icomp cmp)) ->
    A.ins_cmp_reg (DSL.reg_x i.arg.(0)) (DSL.reg_x i.arg.(1)) DSL.optional_none;
    A.ins_cset (DSL.reg_x i.res.(0)) (cond_for_comparison cmp)
  | Lop (Floatop (Float64, Icompf cmp)) ->
    let comp = cond_for_cset_for_float_comparison cmp in
    A.ins2 FCMP (DSL.reg_d i.arg.(0), DSL.reg_d i.arg.(1));
    A.ins_cset (DSL.reg_x i.res.(0)) comp
  | Lop (Floatop (Float32, Icompf cmp)) ->
    let comp = cond_for_cset_for_float_comparison cmp in
    A.ins2 FCMP (DSL.reg_s i.arg.(0), DSL.reg_s i.arg.(1));
    A.ins_cset (DSL.reg_x i.res.(0)) comp
  | Lop (Intop_imm (Icomp cmp, n)) ->
    emit_cmpimm i.arg.(0) n;
    A.ins_cset (DSL.reg_x i.res.(0)) (cond_for_comparison cmp)
  | Lop (Intop Imod) ->
    A.ins3 SDIV (DSL.reg_x reg_tmp1, DSL.reg_x i.arg.(0), DSL.reg_x i.arg.(1));
    A.ins4 MSUB
      ( DSL.reg_x i.res.(0),
        DSL.reg_x reg_tmp1,
        DSL.reg_x i.arg.(1),
        DSL.reg_x i.arg.(0) )
  | Lop (Intop (Imulh { signed = true })) ->
    A.ins3 SMULH (DSL.reg_x i.res.(0), DSL.reg_x i.arg.(0), DSL.reg_x i.arg.(1))
  | Lop (Intop (Imulh { signed = false })) ->
    A.ins3 UMULH (DSL.reg_x i.res.(0), DSL.reg_x i.arg.(0), DSL.reg_x i.arg.(1))
  | Lop (Int128op _) ->
    (* CR mslater: restore after the arm DSL is merged *)
    Misc.fatal_error "arm64: got int128 op"
  | Lop (Intop Ipopcnt) ->
    if !Arch.feat_cssc
    then A.ins2 CNT (DSL.reg_x i.res.(0), DSL.reg_x i.arg.(0))
    else
      let tmp = 7 in
      let tmp_v8b = DSL.reg_op (DSL.Reg.reg_v8b tmp) in
      let tmp_b = DSL.reg_op (DSL.Reg.reg_b tmp) in
      let tmp_d = DSL.reg_op (DSL.Reg.reg_d tmp) in
      A.ins2 FMOV_gp_to_fp_64 (tmp_d, DSL.reg_x i.arg.(0));
      A.ins2 CNT_vector (tmp_v8b, tmp_v8b);
      A.ins2 ADDV (tmp_b, tmp_v8b);
      A.ins2 FMOV_fp_to_gp_64 (DSL.reg_x i.res.(0), tmp_d)
  | Lop (Intop (Ictz _)) ->
    (* [ctz Rd, Rn] is optionally supported from Armv8.7, but rbit and clz are
       supported in all ARMv8 CPUs. *)
    if !Arch.feat_cssc
    then A.ins2 CTZ (DSL.reg_x i.res.(0), DSL.reg_x i.arg.(0))
    else (
      A.ins2 RBIT (DSL.reg_x i.res.(0), DSL.reg_x i.arg.(0));
      A.ins2 CLZ (DSL.reg_x i.res.(0), DSL.reg_x i.res.(0)))
  | Lop (Intop (Iclz _)) -> A.ins2 CLZ (DSL.reg_x i.res.(0), DSL.reg_x i.arg.(0))
  | Lop (Intop ((Iand | Ior | Ixor) as op)) ->
    let instr = instr_for_bitwise_int_operation_shifted_register op in
    A.ins4 instr
      ( DSL.reg_x i.res.(0),
        DSL.reg_x i.arg.(0),
        DSL.reg_x i.arg.(1),
        DSL.optional_none )
  | Lop (Intop ((Ilsl | Ilsr | Iasr) as op)) ->
    let instr = instr_for_shift_operation op in
    A.ins3 instr (DSL.reg_x i.res.(0), DSL.reg_x i.arg.(0), DSL.reg_x i.arg.(1))
  | Lop (Intop ((Iadd | Isub) as op)) ->
    let instr = instr_for_arith_operation_shifted_register op in
    A.ins4 instr
      ( DSL.reg_x i.res.(0),
        DSL.reg_x i.arg.(0),
        DSL.reg_x i.arg.(1),
        DSL.optional_none )
  | Lop (Intop Imul) ->
    A.ins_mul (DSL.reg_x i.res.(0)) (DSL.reg_x i.arg.(0)) (DSL.reg_x i.arg.(1))
  | Lop (Intop Idiv) ->
    A.ins3 SDIV (DSL.reg_x i.res.(0), DSL.reg_x i.arg.(0), DSL.reg_x i.arg.(1))
  | Lop (Intop_imm (((Iand | Ior | Ixor) as op), n)) ->
    let instr = instr_for_bitwise_int_operation_immediate op in
    A.ins3 instr
      ( DSL.reg_x i.res.(0),
        DSL.reg_x i.arg.(0),
        DSL.bitmask (Nativeint.of_int n) )
  | Lop (Intop_imm (((Ilsl | Ilsr | Iasr) as op), shift_in_bits)) ->
    let insertion_fn =
      match[@ocaml.warning "-4"] op with
      | Ilsl -> A.ins_lsl_immediate
      | Ilsr -> A.ins_lsr_immediate
      | Iasr -> A.ins_asr_immediate
      | _ -> assert false
    in
    insertion_fn (DSL.reg_x i.res.(0)) (DSL.reg_x i.arg.(0)) ~shift_in_bits
  | Lop
      (Intop_imm
        ((Imul | Idiv | Iclz _ | Ictz _ | Ipopcnt | Imod | Imulh _), _)) ->
    Misc.fatal_errorf "emit_instr: immediate operand not supported for %a"
      Printlinear.instr i
  | Lop (Specific Isqrtf) -> (
    match DSL.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(0) with
    | S_regs (rd, rn, _) -> A.ins2 FSQRT (rd, rn)
    | D_regs (rd, rn, _) -> A.ins2 FSQRT (rd, rn))
  | Lop (Floatop ((Float32 | Float64), Iabsf)) -> (
    match DSL.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(0) with
    | S_regs (rd, rn, _) -> A.ins2 FABS (rd, rn)
    | D_regs (rd, rn, _) -> A.ins2 FABS (rd, rn))
  | Lop (Floatop ((Float32 | Float64), Inegf)) -> (
    match DSL.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(0) with
    | S_regs (rd, rn, _) -> A.ins2 FNEG (rd, rn)
    | D_regs (rd, rn, _) -> A.ins2 FNEG (rd, rn))
  | Lop (Floatop ((Float32 | Float64), Iaddf)) -> (
    match DSL.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(1) with
    | S_regs (rd, rn, rm) -> A.ins3 FADD (rd, rn, rm)
    | D_regs (rd, rn, rm) -> A.ins3 FADD (rd, rn, rm))
  | Lop (Floatop ((Float32 | Float64), Isubf)) -> (
    match DSL.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(1) with
    | S_regs (rd, rn, rm) -> A.ins3 FSUB (rd, rn, rm)
    | D_regs (rd, rn, rm) -> A.ins3 FSUB (rd, rn, rm))
  | Lop (Floatop ((Float32 | Float64), Imulf)) -> (
    match DSL.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(1) with
    | S_regs (rd, rn, rm) -> A.ins3 FMUL (rd, rn, rm)
    | D_regs (rd, rn, rm) -> A.ins3 FMUL (rd, rn, rm))
  | Lop (Floatop ((Float32 | Float64), Idivf)) -> (
    match DSL.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(1) with
    | S_regs (rd, rn, rm) -> A.ins3 FDIV (rd, rn, rm)
    | D_regs (rd, rn, rm) -> A.ins3 FDIV (rd, rn, rm))
  | Lop (Specific Inegmulf) -> (
    match DSL.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(1) with
    | S_regs (rd, rn, rm) -> A.ins3 FNMUL (rd, rn, rm)
    | D_regs (rd, rn, rm) -> A.ins3 FNMUL (rd, rn, rm))
  | Lop (Specific Imuladdf) -> (
    match DSL.reg_fp_operand_4 i.res.(0) i.arg.(1) i.arg.(2) i.arg.(0) with
    | S_regs (rd, rn, rm, ra) -> A.ins4 FMADD (rd, rn, rm, ra)
    | D_regs (rd, rn, rm, ra) -> A.ins4 FMADD (rd, rn, rm, ra))
  | Lop (Specific Inegmuladdf) -> (
    match DSL.reg_fp_operand_4 i.res.(0) i.arg.(1) i.arg.(2) i.arg.(0) with
    | S_regs (rd, rn, rm, ra) -> A.ins4 FNMADD (rd, rn, rm, ra)
    | D_regs (rd, rn, rm, ra) -> A.ins4 FNMADD (rd, rn, rm, ra))
  | Lop (Specific Imulsubf) -> (
    match DSL.reg_fp_operand_4 i.res.(0) i.arg.(1) i.arg.(2) i.arg.(0) with
    | S_regs (rd, rn, rm, ra) -> A.ins4 FMSUB (rd, rn, rm, ra)
    | D_regs (rd, rn, rm, ra) -> A.ins4 FMSUB (rd, rn, rm, ra))
  | Lop (Specific Inegmulsubf) -> (
    match DSL.reg_fp_operand_4 i.res.(0) i.arg.(1) i.arg.(2) i.arg.(0) with
    | S_regs (rd, rn, rm, ra) -> A.ins4 FNMSUB (rd, rn, rm, ra)
    | D_regs (rd, rn, rm, ra) -> A.ins4 FNMSUB (rd, rn, rm, ra))
  | Lop Opaque -> assert (Reg.equal_location i.arg.(0).loc i.res.(0).loc)
  | Lop (Specific (Ishiftarith (op, shift))) ->
    let open I in
    let open Arm64_ast.Ast.Operand.Shift.Kind in
    let emit_shift_arith instr shift_kind shift_amount =
      A.ins4 instr
        ( DSL.reg_x i.res.(0),
          DSL.reg_x i.arg.(0),
          DSL.reg_x i.arg.(1),
          DSL.optional_shift ~kind:shift_kind ~amount:shift_amount )
    in
    let instr =
      match op with
      | Ishiftadd -> ADD_shifted_register
      | Ishiftsub -> SUB_shifted_register
    in
    if shift >= 0
    then emit_shift_arith instr LSL shift
    else emit_shift_arith instr ASR (-shift)
  | Lop (Specific Imuladd) ->
    A.ins4 MADD
      ( DSL.reg_x i.res.(0),
        DSL.reg_x i.arg.(0),
        DSL.reg_x i.arg.(1),
        DSL.reg_x i.arg.(2) )
  | Lop (Specific Imulsub) ->
    A.ins4 MSUB
      ( DSL.reg_x i.res.(0),
        DSL.reg_x i.arg.(0),
        DSL.reg_x i.arg.(1),
        DSL.reg_x i.arg.(2) )
  | Lop (Specific (Ibswap { bitwidth })) -> (
    match bitwidth with
    | Sixteen ->
      A.ins2 REV16 (DSL.reg_w i.res.(0), DSL.reg_w i.arg.(0));
      A.ins4 UBFM
        (DSL.reg_w i.res.(0), DSL.reg_w i.res.(0), DSL.imm_six 0, DSL.imm_six 15)
    | Thirtytwo -> A.ins2 REV (DSL.reg_w i.res.(0), DSL.reg_w i.arg.(0))
    | Sixtyfour -> A.ins2 REV (DSL.reg_x i.res.(0), DSL.reg_x i.arg.(0)))
  | Lop (Specific (Isignext size)) ->
    A.ins4 SBFM
      ( DSL.reg_x i.res.(0),
        DSL.reg_x i.arg.(0),
        DSL.imm_six 0,
        DSL.imm_six (size - 1) )
  | Lop (Specific (Isimd simd)) -> simd_instr simd i
  | Lop (Name_for_debugger _) -> ()
  | Lcall_op (Lprobe _) ->
    fatal_error "Optimized probes not supported on arm64."
  | Lop (Probe_is_enabled { name; enabled_at_init }) ->
    let semaphore_sym =
      Probe_emission.find_or_add_semaphore name enabled_at_init i.dbg
    in
    (* Load address of the semaphore symbol *)
    emit_load_symbol_addr reg_tmp1 (S.create_global semaphore_sym);
    (* Load unsigned 2-byte integer value from offset 2 *)
    A.ins2 LDRH (DSL.reg_w i.res.(0), DSL.addressing (Iindexed 2) reg_tmp1);
    (* Compare with 0 and set result to 1 if non-zero, 0 if zero *)
    A.ins_cmp (DSL.reg_w i.res.(0)) (DSL.imm 0) DSL.optional_none;
    A.ins_cset (DSL.reg_x i.res.(0)) DSL.Cond.NE
  | Lop Dls_get ->
    if Config.runtime5
    then
      let offset = Domainstate.(idx_of_field Domain_dls_state) * 8 in
      A.ins2 LDR
        ( DSL.reg_x i.res.(0),
          DSL.addressing (Iindexed offset) reg_domain_state_ptr )
    else Misc.fatal_error "Dls is not supported in runtime4."
  | Lop Tls_get ->
    let offset = Domainstate.(idx_of_field Domain_tls_state) * 8 in
    A.ins2 LDR
      ( DSL.reg_x i.res.(0),
        DSL.addressing (Iindexed offset) reg_domain_state_ptr )
  | Lop (Csel tst) -> (
    let len = Array.length i.arg in
    let ifso = i.arg.(len - 2) in
    let ifnot = i.arg.(len - 1) in
    if Reg.same_loc ifso ifnot
    then move ifso i.res.(0)
    else
      match tst with
      | Itruetest ->
        A.ins_cmp (DSL.reg_x i.arg.(0)) (DSL.imm 0) DSL.optional_none;
        A.ins4 CSEL
          ( DSL.reg_x i.res.(0),
            DSL.reg_x i.arg.(1),
            DSL.reg_x i.arg.(2),
            DSL.cond NE )
      | Ifalsetest ->
        A.ins_cmp (DSL.reg_x i.arg.(0)) (DSL.imm 0) DSL.optional_none;
        A.ins4 CSEL
          ( DSL.reg_x i.res.(0),
            DSL.reg_x i.arg.(1),
            DSL.reg_x i.arg.(2),
            DSL.cond EQ )
      | Iinttest cmp ->
        let comp = cond_for_comparison cmp in
        A.ins_cmp_reg
          (DSL.reg_x i.arg.(0))
          (DSL.reg_x i.arg.(1))
          DSL.optional_none;
        A.ins4 CSEL
          ( DSL.reg_x i.res.(0),
            DSL.reg_x i.arg.(2),
            DSL.reg_x i.arg.(3),
            DSL.cond comp )
      | Iinttest_imm (cmp, n) ->
        let comp = cond_for_comparison cmp in
        emit_cmpimm i.arg.(0) n;
        A.ins4 CSEL
          ( DSL.reg_x i.res.(0),
            DSL.reg_x i.arg.(1),
            DSL.reg_x i.arg.(2),
            DSL.cond comp )
      | Ifloattest ((Float32 | Float64), cmp) ->
        let cond = cond_for_float_comparison cmp |> DSL.Cond.of_float_cond in
        (match DSL.reg_fp_operand_3 i.arg.(0) i.arg.(1) i.arg.(1) with
        | S_regs (rn, rm, _) -> A.ins2 FCMP (rn, rm)
        | D_regs (rn, rm, _) -> A.ins2 FCMP (rn, rm));
        A.ins4 CSEL
          ( DSL.reg_x i.res.(0),
            DSL.reg_x i.arg.(2),
            DSL.reg_x i.arg.(3),
            DSL.cond cond )
      | Ioddtest ->
        A.ins2 TST (DSL.reg_x i.arg.(0), DSL.bitmask 1n);
        A.ins4 CSEL
          ( DSL.reg_x i.res.(0),
            DSL.reg_x i.arg.(1),
            DSL.reg_x i.arg.(2),
            DSL.cond NE )
      | Ieventest ->
        A.ins2 TST (DSL.reg_x i.arg.(0), DSL.bitmask 1n);
        A.ins4 CSEL
          ( DSL.reg_x i.res.(0),
            DSL.reg_x i.arg.(1),
            DSL.reg_x i.arg.(2),
            DSL.cond EQ ))
  | Lreloadretaddr -> ()
  | Lreturn -> A.ins0 RET
  | Llabel { label = lbl; _ } ->
    let lbl = label_to_asm_label ~section:Text lbl in
    D.define_label lbl
  | Lbranch lbl ->
    let lbl = label_to_asm_label ~section:Text lbl in
    A.ins1 B (local_label lbl)
  | Lcondbranch (tst, lbl) -> (
    let lbl = label_to_asm_label ~section:Text lbl in
    match tst with
    | Itruetest -> A.ins2 CBNZ (DSL.reg_x i.arg.(0), local_label lbl)
    | Ifalsetest -> A.ins2 CBZ (DSL.reg_x i.arg.(0), local_label lbl)
    | Iinttest cmp ->
      A.ins_cmp_reg
        (DSL.reg_x i.arg.(0))
        (DSL.reg_x i.arg.(1))
        DSL.optional_none;
      let comp = cond_for_comparison cmp in
      A.ins1 (B_cond comp) (local_label lbl)
    | Iinttest_imm (cmp, n) ->
      emit_cmpimm i.arg.(0) n;
      let comp = cond_for_comparison cmp in
      A.ins1 (B_cond comp) (local_label lbl)
    | Ifloattest (Float64, cmp) ->
      let comp = cond_for_float_comparison cmp in
      A.ins2 FCMP (DSL.reg_d i.arg.(0), DSL.reg_d i.arg.(1));
      A.ins1 (B_cond_float comp) (local_label lbl)
    | Ifloattest (Float32, cmp) ->
      let comp = cond_for_float_comparison cmp in
      A.ins2 FCMP (DSL.reg_s i.arg.(0), DSL.reg_s i.arg.(1));
      A.ins1 (B_cond_float comp) (local_label lbl)
    | Ioddtest ->
      A.ins3 TBNZ (DSL.reg_x i.arg.(0), DSL.imm_six 0, local_label lbl)
    | Ieventest ->
      A.ins3 TBZ (DSL.reg_x i.arg.(0), DSL.imm_six 0, local_label lbl))
  | Lcondbranch3 (lbl0, lbl1, lbl2) -> (
    A.ins_cmp (DSL.reg_x i.arg.(0)) (DSL.imm 1) DSL.optional_none;
    (match lbl0 with
    | None -> ()
    | Some lbl ->
      let lbl = label_to_asm_label ~section:Text lbl in
      A.ins1 (B_cond LT) (local_label lbl));
    (match lbl1 with
    | None -> ()
    | Some lbl ->
      let lbl = label_to_asm_label ~section:Text lbl in
      A.ins1 (B_cond EQ) (local_label lbl));
    match lbl2 with
    | None -> ()
    | Some lbl ->
      let lbl = label_to_asm_label ~section:Text lbl in
      A.ins1 (B_cond GT) (local_label lbl))
  | Lswitch jumptbl ->
    let open Arm64_ast.Ast.Symbol in
    let lbltbl = L.create Text in
    A.ins2 ADR (DSL.reg_x reg_tmp1, DSL.label Same_section_and_unit lbltbl);
    A.ins4 ADD_shifted_register
      ( DSL.reg_x reg_tmp1,
        DSL.reg_x reg_tmp1,
        DSL.reg_x i.arg.(0),
        DSL.optional_shift ~kind:Arm64_ast.Ast.Operand.Shift.Kind.LSL ~amount:2
      );
    A.ins1 BR (DSL.reg_x reg_tmp1);
    D.define_label lbltbl;
    for j = 0 to Array.length jumptbl - 1 do
      let jumplbl = label_to_asm_label ~section:Text jumptbl.(j) in
      A.ins1 B (local_label jumplbl)
    done
  (*= Alternative:
        let lbltbl = Cmm.new_label() in
        emit_printf "	adr	%a, %a\n" femit_reg reg_tmp1 femit_label lbltbl;
        emit_printf "	ldr	%a, [%a, %a, lsl #2]\n" femit_wreg reg_tmp2
          femit_reg reg_tmp1 femit_reg i.arg.(0);
        emit_printf "	add	%a, %a, sxtb\n" femit_reg reg_tmp1 femit_wreg
          reg_tmp2;
        emit_printf "	br	%a\n" femit_reg reg_tmp1;
        emit_printf "%a:\n" femit_label lbltbl;
        for j = 0 to Array.length jumptbl - 1 do
            emit_printf "	.4byte	%a - %a\n" femit_label jumptbl.(j)
              femit_label lbltbl
        done *)
  | Lentertrap -> ()
  | Ladjust_stack_offset { delta_bytes } ->
    D.cfi_adjust_cfa_offset ~bytes:delta_bytes;
    stack_offset := !stack_offset + delta_bytes
  | Lpushtrap { lbl_handler } ->
    let open Arm64_ast.Ast.Symbol in
    let lbl_handler = label_to_asm_label ~section:Text lbl_handler in
    A.ins2 ADR (DSL.reg_x reg_tmp1, DSL.label Same_section_and_unit lbl_handler);
    stack_offset := !stack_offset + 16;
    A.ins3 STP
      ( DSL.reg_x reg_trap_ptr,
        DSL.reg_x reg_tmp1,
        DSL.mem_pre_pair ~base:(DSL.Reg.sp ()) ~offset:(-16) );
    D.cfi_adjust_cfa_offset ~bytes:16;
    A.ins_mov_from_sp ~dst:(DSL.reg_x reg_trap_ptr)
  | Lpoptrap _ ->
    A.ins2 LDR
      (DSL.reg_x reg_trap_ptr, DSL.mem_post ~base:(DSL.Reg.sp ()) ~offset:16);
    D.cfi_adjust_cfa_offset ~bytes:(-16);
    stack_offset := !stack_offset - 16
  | Lraise k -> (
    match k with
    | Lambda.Raise_regular ->
      A.ins1 BL (runtime_function "caml_raise_exn");
      record_frame Reg.Set.empty (Dbg_raise i.dbg)
    | Lambda.Raise_reraise ->
      if Config.runtime5
      then A.ins1 BL (runtime_function "caml_reraise_exn")
      else A.ins1 BL (runtime_function "caml_raise_exn");
      record_frame Reg.Set.empty (Dbg_raise i.dbg)
    | Lambda.Raise_notrace ->
      A.ins_mov_to_sp ~src:(DSL.reg_x reg_trap_ptr);
      A.ins3 LDP
        ( DSL.reg_x reg_trap_ptr,
          DSL.reg_x reg_tmp1,
          DSL.mem_post_pair ~base:(DSL.Reg.sp ()) ~offset:16 );
      A.ins1 BR (DSL.reg_x reg_tmp1))
  | Lstackcheck { max_frame_size_bytes } ->
    let overflow = L.create Text and ret = L.create Text in
    let threshold_offset =
      (Domainstate.stack_ctx_words * 8) + Stack_check.stack_threshold_size
    in
    let f = max_frame_size_bytes + threshold_offset in
    let offset = Domainstate.(idx_of_field Domain_current_stack) * 8 in
    A.ins2 LDR
      (DSL.reg_x reg_tmp1, DSL.addressing (Iindexed offset) reg_domain_state_ptr);
    emit_addimm reg_tmp1 reg_tmp1 f;
    A.ins_cmp_reg (DSL.sp ()) (DSL.reg_x reg_tmp1) DSL.optional_none;
    A.ins1 (B_cond CC) (local_label overflow);
    D.define_label ret;
    stack_realloc
      := Some
           { sc_label = overflow;
             sc_return = ret;
             sc_max_frame_size_in_bytes = max_frame_size_bytes
           }
  | Lop (Specific (Illvm_intrinsic intr)) ->
    Misc.fatal_errorf
      "Emit: Unexpected llvm_intrinsic %s: not using LLVM backend" intr

let emit_instr i =
  try emit_instr i
  with exn ->
    Format.eprintf "Exception whilst emitting instruction:@ %a\n"
      Printlinear.instr i;
    raise exn

(* Emission of an instruction sequence *)

let rec emit_all i =
  (* CR-soon xclerc for xclerc: get rid of polymorphic compare. *)
  if Stdlib.compare i.desc Lend = 0
  then ()
  else (
    emit_instr i;
    emit_all i.next)

(* Emission of a function declaration *)

let fundecl fundecl =
  let fun_end_label, fundecl =
    match Emitaux.Dwarf_helpers.record_dwarf_for_fundecl fundecl with
    | None -> None, fundecl
    | Some { fun_end_label; fundecl } -> Some fun_end_label, fundecl
  in
  function_name := fundecl.fun_name;
  fastcode_flag := fundecl.fun_fast;
  tailrec_entry_point
    := Option.map
         (label_to_asm_label ~section:Text)
         fundecl.fun_tailrec_entry_point_label;
  float_literals := [];
  stack_offset := 0;
  call_gc_sites := [];
  local_realloc_sites := [];
  clear_stack_realloc ();
  Stack_class.Tbl.copy_values ~from:fundecl.fun_num_stack_slots
    ~to_:num_stack_slots;
  prologue_required := fundecl.fun_prologue_required;
  contains_calls := fundecl.fun_contains_calls;
  emit_named_text_section !function_name;
  let fun_sym = S.create_global fundecl.fun_name in
  D.align ~fill:Nop ~bytes:8;
  D.global fun_sym;
  D.type_symbol ~ty:Function fun_sym;
  D.define_symbol_label ~section:Text fun_sym;
  emit_debug_info fundecl.fun_dbg;
  D.cfi_startproc ();
  let num_call_gc = num_call_gc_points fundecl.fun_body in
  let max_out_of_line_code_offset = max_out_of_line_code_offset ~num_call_gc in
  BR.relax fundecl.fun_body ~max_out_of_line_code_offset;
  emit_all fundecl.fun_body;
  List.iter emit_call_gc !call_gc_sites;
  List.iter emit_local_realloc !local_realloc_sites;
  emit_stack_realloc ();
  assert (List.length !call_gc_sites = num_call_gc);
  (match fun_end_label with
  | None -> ()
  | Some fun_end_label ->
    let fun_end_label = label_to_asm_label ~section:Text fun_end_label in
    D.define_label fun_end_label);
  D.cfi_endproc ();
  (* The type symbol and the size are system specific. They are not output on
     macOS. The asm directives take care of correctly handling this distinction.
     For the size, they automatically emit the size [. - symbol], meaning "this
     minus symbol definition". *)
  D.type_symbol ~ty:Function fun_sym;
  D.size fun_sym;
  emit_literals ()

(* Emission of data *)

(* CR sspies: Share the [emit_item] code with the x86 backend in emitaux. *)
let emit_item (d : Cmm.data_item) =
  match d with
  | Cdefine_symbol s ->
    let sym = symbol_of_cmm_symbol s in
    if !Clflags.dlcode || Cmm.equal_is_global s.sym_global Cmm.Global
    then
      (* GOT relocations against non-global symbols don't seem to work properly:
         GOT entries are not created for the symbols and the relocations
         evaluate to random other GOT entries. For the moment force all symbols
         to be global. *)
      D.global sym;
    D.define_symbol_label ~section:Data sym
  | Cint8 n -> D.int8 (Numbers.Int8.of_int_exn n)
  | Cint16 n -> D.int16 (Numbers.Int16.of_int_exn n)
  | Cint32 n -> D.int32 (Numbers.Int64.to_int32_exn (Int64.of_nativeint n))
  (* CR mshinwell: Add [Targetint.of_nativeint] *)
  | Cint n -> D.targetint (Targetint.of_int64 (Int64.of_nativeint n))
  | Csingle f -> D.float32 f
  | Cdouble f -> D.float64 f
  | Cvec128 { word0; word1 } ->
    D.float64_from_bits word1;
    D.float64_from_bits word0
  | Cvec256 _ | Cvec512 _ -> Misc.fatal_error "arm64: got 256/512 bit vector"
  | Csymbol_address s ->
    let sym = symbol_of_cmm_symbol s in
    D.symbol sym
  | Csymbol_offset (s, o) ->
    let sym = symbol_of_cmm_symbol s in
    D.symbol_plus_offset ~offset_in_bytes:(Targetint.of_int o) sym
  | Cstring s -> D.string s
  | Cskip n -> D.space ~bytes:n
  | Calign n -> D.align ~fill:Zero ~bytes:n

let data l =
  D.data ();
  D.align ~fill:Zero ~bytes:8;
  List.iter emit_item l

let file_emitter ~file_num ~file_name =
  D.file ~file_num:(Some file_num) ~file_name

(* Beginning / end of an assembly file *)

let begin_assembly _unix =
  reset_debug_info ();
  Probe_emission.reset ();
  Arm64_ast.Ast.DSL.Acc.set_emit_string ~emit_string:Emitaux.emit_string;
  Asm_targets.Asm_label.initialize ~new_label:(fun () ->
      Cmm.new_label () |> Label.to_int);
  (* Set up binary emitter if JIT hook is registered or save_binary_sections is
     set *)
  let use_binary_emitter =
    Option.is_some
      (Arm64_binary_emitter.Binary_emitter.For_jit.Internal_assembler.get ())
    || !Oxcaml_flags.save_binary_sections
  in
  if use_binary_emitter
  then (
    (* When saving binary sections (for verification), emit relocations for all
       symbol references to match assembler behavior *)
    if !Oxcaml_flags.save_binary_sections
    then
      Arm64_binary_emitter.Binary_emitter.emit_relocs_for_all_symbol_refs
        := true;
    let emitter = Arm64_binary_emitter.Binary_emitter.create () in
    jit_emitter := Some emitter;
    Arm64_ast.Ast.DSL.Acc.set_emit_instruction
      ~emit_instruction:
        (Arm64_binary_emitter.Binary_emitter.add_instruction emitter));
  let asm_line_buffer = Buffer.create 200 in
  D.initialize ~big_endian:Arch.big_endian
    ~emit_assembly_comments:!Oxcaml_flags.dasm_comments ~emit:(fun d ->
      (* Emit to binary emitter if in JIT mode *)
      (match !jit_emitter with
      | Some emitter ->
        Arm64_binary_emitter.Binary_emitter.add_directive emitter d
      | None -> ());
      (* Emit to text *)
      Buffer.clear asm_line_buffer;
      D.Directive.print asm_line_buffer d;
      Buffer.add_string asm_line_buffer "\n";
      Emitaux.emit_buffer asm_line_buffer);
  D.file ~file_num:None ~file_name:"";
  (* PR#7037 *)
  let data_begin = Cmm_helpers.make_symbol "data_begin" in
  let data_begin_sym = S.create_global data_begin in
  D.data ();
  D.global data_begin_sym;
  D.define_symbol_label ~section:Data data_begin_sym;
  let code_begin = Cmm_helpers.make_symbol "code_begin" in
  let code_begin_sym = S.create_global code_begin in
  emit_named_text_section code_begin;
  D.global code_begin_sym;
  D.define_symbol_label ~section:Text code_begin_sym;
  (* we need to pad here to avoid collision for the unwind test between the
     code_begin symbol and the first function. (See also #4690) Alignment is
     needed to avoid linker warnings for shared_startup__code_{begin,end} (e.g.
     tests/lib-dynlink-pr4839). *)
  if macosx
  then (
    A.ins0 NOP;
    D.align ~fill:Nop ~bytes:8);
  let code_end = Cmm_helpers.make_symbol "code_end" in
  Emitaux.Dwarf_helpers.begin_dwarf ~code_begin ~code_end ~file_emitter

let end_assembly () =
  let code_end = Cmm_helpers.make_symbol "code_end" in
  let code_end_sym = S.create_global code_end in
  emit_named_text_section code_end;
  D.global code_end_sym;
  D.define_symbol_label ~section:Text code_end_sym;
  let data_end = Cmm_helpers.make_symbol "data_end" in
  let data_end_sym = S.create_global data_end in
  D.data ();
  D.int64 0L;
  (* PR#6329 *)
  D.global data_end_sym;
  D.define_symbol_label ~section:Data data_end_sym;
  D.int64 0L;
  D.align ~fill:Zero ~bytes:8;
  (* #7887 *)
  let frametable = Cmm_helpers.make_symbol "frametable" in
  let frametable_sym = S.create_global frametable in
  D.global frametable_sym;
  D.define_symbol_label ~section:Data frametable_sym;
  (* CR sspies: Share the [emit_frames] code with the x86 backend. *)
  emit_frames
    { efa_code_label =
        (fun lbl ->
          let lbl = label_to_asm_label ~section:Text lbl in
          D.type_label ~ty:Function lbl;
          D.label lbl);
      efa_data_label =
        (fun lbl ->
          let lbl = label_to_asm_label ~section:Data lbl in
          D.type_label ~ty:Object lbl;
          D.label lbl);
      efa_i8 = (fun n -> D.int8 n);
      efa_i16 = (fun n -> D.int16 n);
      efa_i32 = (fun n -> D.int32 n);
      efa_u8 = (fun n -> D.uint8 n);
      efa_u16 = (fun n -> D.uint16 n);
      efa_u32 = (fun n -> D.uint32 n);
      efa_word = (fun n -> D.targetint (Targetint.of_int_exn n));
      efa_align = (fun n -> D.align ~fill:Zero ~bytes:n);
      efa_label_rel =
        (fun lbl ofs ->
          let lbl = label_to_asm_label ~section:Data lbl in
          D.between_this_and_label_offset_32bit_expr ~upper:lbl
            ~offset_upper:(Targetint.of_int32 ofs));
      efa_def_label =
        (fun lbl ->
          (* CR sspies: The frametable lives in the [.data] section on Arm, but
             in the [.text] section on x86. The frametable should move to the
             text section on Arm as well. *)
          let lbl = label_to_asm_label ~section:Data lbl in
          D.define_label lbl);
      efa_string = (fun s -> D.string (s ^ "\000"))
    };
  D.type_symbol ~ty:Object frametable_sym;
  D.size frametable_sym;
  if not !Oxcaml_flags.internal_assembler
  then Emitaux.Dwarf_helpers.emit_dwarf ();
  Probe_emission.emit_probe_notes ~slot_offset ~add_def_symbol:(fun _ -> ());
  D.mark_stack_non_executable ();
  (* Finalize binary emitter if enabled *)
  match !jit_emitter with
  | None -> ()
  | Some emitter -> (
    (* Clear the instruction emission callback *)
    Arm64_ast.Ast.DSL.Acc.clear_emit_instruction ();
    jit_emitter := None;
    (* Dump instructions if JIT debug is enabled *)
    (match Sys.getenv_opt "OCAML_JIT_DEBUG" with
    | Some ("true" | "1") ->
      Arm64_binary_emitter.Binary_emitter.dump_instructions emitter
    | _ -> ());
    (* Get assembled sections. for_jit is true only when there's a JIT hook
       registered (actual JIT compilation). When we're just saving binary
       sections for verification, we're generating an object file and should use
       for_jit:false to match ELF relocation format. *)
    let for_jit =
      Option.is_some
        (Arm64_binary_emitter.Binary_emitter.For_jit.Internal_assembler.get ())
    in
    let section_tbl =
      Arm64_binary_emitter.Binary_emitter.emit ~for_jit emitter
    in
    (* Convert to list of (name, section) pairs. Include both standard sections
       (by Asm_section.t) and individual sections (by name string, for function
       sections like .text.caml.<funcname>). *)
    let sections =
      let from_standard =
        Arm64_binary_emitter.All_section_states.fold section_tbl ~init:[]
          ~f:(fun section state acc ->
            let name = Asm_targets.Asm_section.to_string section in
            (name, state) :: acc)
      in
      let from_individual =
        Arm64_binary_emitter.All_section_states.fold_individual section_tbl
          ~init:[] ~f:(fun name state acc -> (name, state) :: acc)
      in
      from_standard @ from_individual
    in
    (* For JIT, we need to aggregate all .text.* sections into a single .text
       section because the JIT loader expects exactly one .text section. *)
    let sections_for_jit =
      let is_text_section sec_name =
        let len = Stdlib.String.length sec_name in
        len >= 5 && String.equal (Stdlib.String.sub sec_name 0 5) ".text"
      in
      let text_sections, other_sections =
        List.partition (fun (sec_name, _) -> is_text_section sec_name) sections
      in
      match text_sections with
      | [] -> sections
      | [(sec_name, _)] when String.equal sec_name ".text" -> sections
      | _ ->
        (* Aggregate all text sections into one .text section. Sort by name for
           deterministic ordering. *)
        let sorted_text =
          List.sort (fun (a, _) (b, _) -> String.compare a b) text_sections
        in
        let module SS = Arm64_binary_emitter.Binary_emitter.Section_state in
        let aggregated = SS.create () in
        List.iter
          (fun (_name, state) ->
            let content = SS.contents state in
            let base_offset = Buffer.length (SS.buffer aggregated) in
            Buffer.add_string (SS.buffer aggregated) content;
            (* Copy relocations with adjusted offsets *)
            List.iter
              (fun (reloc : Arm64_binary_emitter.Relocation.t) ->
                let adjusted =
                  { reloc with
                    offset_from_section_beginning =
                      reloc.offset_from_section_beginning + base_offset
                  }
                in
                SS.add_relocation aggregated adjusted)
              (SS.relocations state))
          sorted_text;
        (".text", aggregated) :: other_sections
    in
    (* Save sections to files if save_binary_sections is enabled *)
    if !Oxcaml_flags.save_binary_sections
    then (
      let dir = !Emitaux.output_prefix ^ ".binary-sections" in
      (try Sys.mkdir dir 0o755 with Sys_error _ -> ());
      List.iter
        (fun (name, state) ->
          (* Convert section name like ".text" to "section_text" *)
          let safe_name =
            if String.length name > 0 && Char.equal (String.get name 0) '.'
            then "section_" ^ String.sub name 1 (String.length name - 1)
            else "section_" ^ name
          in
          (* Save binary content *)
          let bin_filename = Filename.concat dir (safe_name ^ ".bin") in
          let oc = open_out_bin bin_filename in
          output_string oc
            (Arm64_binary_emitter.Binary_emitter.Section_state.contents state);
          close_out oc;
          (* Save relocations if any *)
          let relocs =
            Arm64_binary_emitter.Binary_emitter.Section_state.relocations state
          in
          match relocs with
          | [] -> ()
          | _ ->
            let reloc_filename = Filename.concat dir (safe_name ^ ".relocs") in
            let oc = open_out reloc_filename in
            let module R = Arm64_binary_emitter.Relocation in
            let module ED = Arm64_binary_emitter.Binary_emitter.Encode_directive
            in
            List.iter
              (fun reloc ->
                (* For paired relocations (SUBTRACTOR + UNSIGNED), write both
                   symbols as separate lines at the same offset. On RELA
                   platforms (Linux), include addends for proper verification.
                   Also convert local/file-scope symbols to section+offset. *)
                let offset = R.offset_from_section_beginning reloc in
                List.iter
                  (fun (target, addend) ->
                    (* For verification output, convert local/file-scope symbols
                       to section+offset format to match assembler behavior *)
                    let resolved_sym, resolved_addend =
                      ED.resolve_local_label_for_elf ~all_sections:section_tbl
                        ~target ~sym_offset:addend
                    in
                    if resolved_addend = 0
                    then Printf.fprintf oc "%d %s\n" offset resolved_sym
                    else
                      Printf.fprintf oc "%d %s %d\n" offset resolved_sym
                        resolved_addend)
                  (R.all_targets_with_addends reloc))
              relocs;
            close_out oc)
        sections);
    (* Call the JIT hook if registered *)
    match
      Arm64_binary_emitter.Binary_emitter.For_jit.Internal_assembler.get ()
    with
    | None -> ()
    | Some hook ->
      (* The hook expects (string * assembled_section) list and returns a file
         writer function. We ignore the file writer for JIT. Use the aggregated
         sections where all .text.* are merged into .text. *)
      let _file_writer = hook sections_for_jit in
      ())
