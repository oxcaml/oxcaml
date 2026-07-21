[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
open Arm64_ast.Ast
module DLL = Doubly_linked_list

type asm_line =
  | Ins of Instruction.t
  | Directive of Asm_targets.Asm_directives.Directive.t

type rule_result =
  | No_match
  | Matched of asm_line DLL.cell option

module Gp = struct
  type t =
    | Reg64 of int
    | Reg32 of int
    | Zr64
    | Zr32
    | Sp

  (* The zero registers and [sp] share the encoding 31, and [lr]/[fp] are stored
     with a dummy index, so normalization must go through [reg_name] rather than
     [index] or [Reg.gp_encoding]. *)
  let of_reg (type a) (reg : [`GP of a] Reg.t) : t =
    match reg.reg_name with
    | GP W -> Reg32 reg.index
    | GP X -> Reg64 reg.index
    | GP WZR -> Zr32
    | GP XZR -> Zr64
    | GP WSP | GP SP -> Sp
    | GP LR -> Reg64 30
    | GP FP -> Reg64 29

  let equal left right =
    match left, right with
    | Reg64 l, Reg64 r -> l = r
    | Reg32 l, Reg32 r -> l = r
    | Zr64, Zr64 -> true
    | Zr32, Zr32 -> true
    | Sp, Sp -> true
    | (Reg64 _ | Reg32 _ | Zr64 | Zr32 | Sp), _ -> false

  (* Whether two register names overlap the same physical register: [wN] and
     [xN] are the same register, while the zero registers are not physical
     storage and alias nothing. *)
  let same_underlying left right =
    match left, right with
    | (Reg64 l | Reg32 l), (Reg64 r | Reg32 r) -> l = r
    | Sp, Sp -> true
    | (Zr64 | Zr32), _ | _, (Zr64 | Zr32) -> false
    | Sp, (Reg64 _ | Reg32 _) | (Reg64 _ | Reg32 _), Sp -> false
end

let directive_is_barrier (d : Asm_targets.Asm_directives.Directive.t) =
  match d with
  | New_label _ | Bytes _ | Cfi_startproc | Cfi_endproc | Section _ -> true
  | Align _ | Cfi_adjust_cfa_offset _ | Cfi_def_cfa_offset _ | Cfi_offset _
  | Cfi_remember_state | Cfi_restore_state | Cfi_def_cfa_register _ | Comment _
  | Const _ | Direct_assignment _ | File _ | Global _ | Indirect_symbol _
  | Loc _ | New_line | Private_extern _ | Size _ | Sleb128 _ | Space _ | Type _
  | Uleb128 _ | Protected _ | Hidden _ | Weak _ | External _ | Reloc _ ->
    false

(* Destination registers of an instruction. [Unknown] is the conservative
   answer, used for control flow (in particular calls, which clobber all
   caller-saved registers). *)
type destinations =
  | Known of Gp.t list
  | Unknown

type effects =
  { is_barrier : bool;
    writes_flags : bool;
    destinations : destinations
  }

let gp_regs_of_operand : type o. o Operand.t -> Gp.t list =
 fun op ->
  match op with
  | Reg r -> ( match r.reg_name with GP _ -> [Gp.of_reg r] | Neon _ -> [])
  | Imm _ | Lsl_by_twelve | Shift _ | Lsl_by_multiple_of_16_bits _
  | Shift_by_element_width _ | Cond _ | Float_cond _ | Mem _ | Bitmask _
  | Optional _ | Unit ->
    []

let dests_of_first_operand : type num ops. (num, ops) many -> Gp.t list =
 fun operands ->
  match operands with
  | Singleton op -> gp_regs_of_operand op
  | Pair (op, _) -> gp_regs_of_operand op
  | Triple (op, _, _) -> gp_regs_of_operand op
  | Quad (op, _, _, _) -> gp_regs_of_operand op

(* Classification of an instruction, as needed by the rewrite rules. The match
   on the instruction name is deliberately exhaustive so that adding a new
   instruction to the AST forces a review of its classification here. All
   instructions except the ones treated specially below have their (only)
   possible destination as first operand; [dests_of_first_operand] returns the
   empty list when that operand is not a general-purpose register. *)
let effects (Instruction.I { name; operands }) : effects =
  match name with
  | B | BL | BLR | BR | B_cond _ | CBZ | CBNZ | TBZ | TBNZ | RET ->
    { is_barrier = true; writes_flags = true; destinations = Unknown }
  | DMB _ | DSB _ | YIELD ->
    { is_barrier = true; writes_flags = false; destinations = Known [] }
  | LDAR ->
    { is_barrier = true;
      writes_flags = false;
      destinations = Known (dests_of_first_operand operands)
    }
  | ADDS | SUBS_immediate | SUBS_shifted_register ->
    { is_barrier = false;
      writes_flags = true;
      destinations = Known (dests_of_first_operand operands)
    }
  | TST | FCMP ->
    { is_barrier = false; writes_flags = true; destinations = Known [] }
  | STR | STRB | STRH | STR_simd_and_fp | STP _ ->
    (* The first operand of a store is a source, not a destination. *)
    { is_barrier = false; writes_flags = false; destinations = Known [] }
  | LDP _ ->
    let destinations =
      match operands with
      | Triple (op1, op2, _) ->
        Known (gp_regs_of_operand op1 @ gp_regs_of_operand op2)
    in
    { is_barrier = false; writes_flags = false; destinations }
  | ABS_vector | ADDP_vector | ADDV | ADD_immediate | ADD_shifted_register
  | ADD_vector | ADR | ADRP | AND_immediate | AND_shifted_register | AND_vector
  | ASRV | CLZ | CM_register _ | CM_zero _ | CNT | CNT_vector | CSEL | CSINC
  | CTZ | DUP _ | EOR_immediate | EOR_shifted_register | EOR_vector | EXT | FABS
  | FADD | FADDP_vector | FADD_vector | FCM_register _ | FCM_zero _ | FCSEL
  | FCVT | FCVTL_vector | FCVTNS | FCVTNS_vector | FCVTN_vector | FCVTZS
  | FCVTZS_vector | FDIV | FDIV_vector | FMADD | FMAX | FMAX_vector | FMIN
  | FMIN_vector | FMOV_fp | FMOV_gp_to_fp_32 | FMOV_gp_to_fp_64
  | FMOV_fp_to_gp_32 | FMOV_fp_to_gp_64 | FMOV_scalar_immediate | FMSUB | FMUL
  | FMUL_vector | FNEG | FNEG_vector | FNMADD | FNMSUB | FNMUL | FRECPE_vector
  | FRINT _ | FRINT_vector _ | FRSQRTE_vector | FSQRT | FSQRT_vector | FSUB
  | FSUB_vector | INS _ | INS_V _ | LDR | LDRB | LDRH | LDRSB | LDRSH | LDRSW
  | LDR_simd_and_fp | LSLV | LSRV | MADD | MOVI | MOVK | MOVN | MOVZ | MSUB
  | MUL_vector | MVN_vector | NEG_vector | NOP | ORR_immediate
  | ORR_shifted_register | ORR_vector | RBIT | REV | REV16 | SBFM | SCVTF
  | SCVTF_vector | SDIV | SHL | SMAX_vector | SMIN_vector | SMOV _ | SMULH
  | SMULL2_vector _ | SMULL_vector _ | SQADD_vector | SQSUB_vector | SQXTN _
  | SQXTN2 _ | SSHL_vector | SSHR | SUB_immediate | SUB_shifted_register
  | SUB_vector | SXTL _ | UADDLP_vector | UBFM | UDIV | UMAX_vector
  | UMIN_vector | UMOV _ | UMULH | UMULL2_vector _ | UMULL_vector _
  | UQADD_vector | UQSUB_vector | UQXTN _ | UQXTN2 _ | USHL_vector | USHR
  | UXTL _ | XTN _ | XTN2 _ | ZIP1 | ZIP2 ->
    { is_barrier = false;
      writes_flags = false;
      destinations = Known (dests_of_first_operand operands)
    }

let is_hard_barrier = function
  | Directive d -> directive_is_barrier d
  | Ins ins -> (effects ins).is_barrier

let is_conditional_branch (Instruction.I { name; _ }) =
  match[@warning "-4"] name with
  | B_cond _ | CBZ | CBNZ | TBZ | TBNZ -> true
  | _ -> false

let writes_flags ins = (effects ins).writes_flags

let writes_any_gp_reg ins ~regs =
  match (effects ins).destinations with
  | Unknown -> true
  | Known dests ->
    List.exists
      (fun dest -> List.exists (fun reg -> Gp.same_underlying dest reg) regs)
      dests

let next_instruction cell =
  let rec loop cell_opt =
    match cell_opt with
    | None -> None
    | Some cell -> (
      match DLL.value cell with
      | Ins _ -> Some cell
      | Directive d ->
        if directive_is_barrier d then None else loop (DLL.next cell))
  in
  loop (DLL.next cell)

let decode_simple_mem (op : [`Mem of Addressing_mode.single] Operand.t) :
    (Gp.t * int) option =
  match op with
  | Mem addressing -> (
    match addressing with
    | Reg base -> Some (Gp.of_reg base, 0)
    | Offset_twelve_unsigned_scaled (base, Twelve_unsigned_scaled offset) ->
      Some (Gp.of_reg base, offset)
    | Offset_nine_signed_unscaled (base, Nine_signed_unscaled offset) ->
      Some (Gp.of_reg base, offset)
    | Offset_sym _ | Literal _ | Pre _ | Post _ -> None)

(* Delete the instruction following [cell] (as found by [next_instruction]), as
   well as any [Loc] directives sitting before it (they describe the deleted
   instruction; leaving them in place would attach them to whatever follows). *)
let delete_next_instruction_and_locs cell =
  let rec loop cell_opt =
    match cell_opt with
    | None ->
      Misc.fatal_error
        "Arm64_peephole.delete_next_instruction_and_locs: no next instruction"
    | Some cell -> (
      match DLL.value cell with
      | Ins _ -> DLL.delete_curr cell
      | Directive d ->
        if directive_is_barrier d
        then
          Misc.fatal_error
            "Arm64_peephole.delete_next_instruction_and_locs: barrier before \
             next instruction"
        else begin
          let next = DLL.next cell in
          (match[@warning "-4"] d with
          | Loc _ -> DLL.delete_curr cell
          | _ -> ());
          loop next
        end)
  in
  loop (DLL.next cell)
