(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

module Symbol = Arm64_ast.Ast.Symbol

module Kind = struct
  (** Relocation with symbol/label target and addend. On RELA platforms (Linux
      ELF), the addend is stored in the relocation entry. On REL platforms
      (macOS Mach-O), the addend is encoded in the instruction/data. *)
  type target_with_addend =
    { target : Symbol.target;
      addend : int
    }

  type t =
    | R_AARCH64_ADR_PREL_LO21 of target_with_addend
    | R_AARCH64_ADR_PREL_PG_HI21 of target_with_addend
    (* ADRP targeting the GOT entry page (for @GOTPAGE on macOS) *)
    | R_AARCH64_ADR_GOT_PAGE of target_with_addend
    | R_AARCH64_LD64_GOT_LO12_NC of target_with_addend
    | R_AARCH64_ADD_ABS_LO12_NC of target_with_addend
    | R_AARCH64_LDST64_ABS_LO12_NC of target_with_addend
    | R_AARCH64_CALL26 of target_with_addend
    | R_AARCH64_JUMP26 of target_with_addend
    (* Absolute 64-bit data reference (ARM64_RELOC_UNSIGNED on macOS) *)
    | R_AARCH64_ABS64 of target_with_addend
    (* Cross-section relative reference (SUBTRACTOR + UNSIGNED pair on macOS)
       Used for expressions like (Label - This) where Label and This are in
       different sections. The addend is stored in the data, and the linker will
       compute: addend + plus_symbol - minus_symbol *)
    | R_AARCH64_PREL32_PAIR of
        { plus_target : Symbol.target;
          minus_target : Symbol.target
        }
    (* ELF section-relative 32-bit PC-relative reference. Used on ELF for
       cross-section references when function sections are enabled. The section
       is the target section (e.g., Text or Function_text ".text.caml.func") and
       addend is the offset within that section. The linker computes:
       section_address + addend - relocation_address *)
    | R_AARCH64_PREL32 of
        { section : Asm_targets.Asm_section.t;
          addend : int
        }
end

type t =
  { offset_from_section_beginning : int;
    kind : Kind.t
  }

let offset_from_section_beginning (r : t) = r.offset_from_section_beginning

let size (r : t) : Binary_emitter_intf.data_size =
  match r.kind with
  | R_AARCH64_ABS64 _ -> Binary_emitter_intf.B64
  | R_AARCH64_ADR_PREL_LO21 _ | R_AARCH64_ADR_PREL_PG_HI21 _
  | R_AARCH64_ADR_GOT_PAGE _ | R_AARCH64_LD64_GOT_LO12_NC _
  | R_AARCH64_ADD_ABS_LO12_NC _ | R_AARCH64_LDST64_ABS_LO12_NC _
  | R_AARCH64_CALL26 _ | R_AARCH64_JUMP26 _ | R_AARCH64_PREL32_PAIR _
  | R_AARCH64_PREL32 _ ->
    Binary_emitter_intf.B32

let primary_target (r : t) : Symbol.target =
  match r.kind with
  | R_AARCH64_ADR_PREL_LO21 { target; _ }
  | R_AARCH64_ADR_PREL_PG_HI21 { target; _ }
  | R_AARCH64_ADR_GOT_PAGE { target; _ }
  | R_AARCH64_LD64_GOT_LO12_NC { target; _ }
  | R_AARCH64_ADD_ABS_LO12_NC { target; _ }
  | R_AARCH64_LDST64_ABS_LO12_NC { target; _ }
  | R_AARCH64_CALL26 { target; _ }
  | R_AARCH64_JUMP26 { target; _ }
  | R_AARCH64_ABS64 { target; _ } ->
    target
  | R_AARCH64_PREL32_PAIR { plus_target; _ } -> plus_target
  | R_AARCH64_PREL32 { section; _ } ->
    (* Section names are treated as global symbols. Use create_without_encoding
       since section names from Function_text are already encoded. *)
    let section_name = Asm_targets.Asm_section.to_string section in
    Symbol
      (Asm_targets.Asm_symbol.create_without_encoding ~visibility:Global
         section_name)

let all_targets (r : t) : Symbol.target list =
  match r.kind with
  | R_AARCH64_ADR_PREL_LO21 { target; _ }
  | R_AARCH64_ADR_PREL_PG_HI21 { target; _ }
  | R_AARCH64_ADR_GOT_PAGE { target; _ }
  | R_AARCH64_LD64_GOT_LO12_NC { target; _ }
  | R_AARCH64_ADD_ABS_LO12_NC { target; _ }
  | R_AARCH64_LDST64_ABS_LO12_NC { target; _ }
  | R_AARCH64_CALL26 { target; _ }
  | R_AARCH64_JUMP26 { target; _ }
  | R_AARCH64_ABS64 { target; _ } ->
    [target]
  | R_AARCH64_PREL32_PAIR { plus_target; minus_target } ->
    [plus_target; minus_target]
  | R_AARCH64_PREL32 { section; _ } ->
    let section_name = Asm_targets.Asm_section.to_string section in
    [ Symbol
        (Asm_targets.Asm_symbol.create_without_encoding ~visibility:Global
           section_name) ]

let all_targets_with_addends (r : t) : (Symbol.target * int) list =
  match r.kind with
  | R_AARCH64_ADR_PREL_LO21 { target; addend }
  | R_AARCH64_ADR_PREL_PG_HI21 { target; addend }
  | R_AARCH64_ADR_GOT_PAGE { target; addend }
  | R_AARCH64_LD64_GOT_LO12_NC { target; addend }
  | R_AARCH64_ADD_ABS_LO12_NC { target; addend }
  | R_AARCH64_LDST64_ABS_LO12_NC { target; addend }
  | R_AARCH64_CALL26 { target; addend }
  | R_AARCH64_JUMP26 { target; addend }
  | R_AARCH64_ABS64 { target; addend } ->
    [target, addend]
  | R_AARCH64_PREL32_PAIR { plus_target; minus_target } ->
    [plus_target, 0; minus_target, 0]
  | R_AARCH64_PREL32 { section; addend } ->
    let section_name = Asm_targets.Asm_section.to_string section in
    [ ( Symbol
          (Asm_targets.Asm_symbol.create_without_encoding ~visibility:Global
             section_name),
        addend ) ]

let is_got_reloc (r : t) =
  match r.kind with
  | R_AARCH64_ADR_GOT_PAGE _ | R_AARCH64_LD64_GOT_LO12_NC _ -> true
  | R_AARCH64_ADR_PREL_LO21 _ | R_AARCH64_ADR_PREL_PG_HI21 _
  | R_AARCH64_ADD_ABS_LO12_NC _ | R_AARCH64_LDST64_ABS_LO12_NC _
  | R_AARCH64_CALL26 _ | R_AARCH64_JUMP26 _ | R_AARCH64_ABS64 _
  | R_AARCH64_PREL32_PAIR _ | R_AARCH64_PREL32 _ ->
    false

let is_plt_reloc (r : t) =
  match r.kind with
  | R_AARCH64_CALL26 _ | R_AARCH64_JUMP26 _ -> true
  | R_AARCH64_ADR_PREL_LO21 _ | R_AARCH64_ADR_PREL_PG_HI21 _
  | R_AARCH64_ADR_GOT_PAGE _ | R_AARCH64_LD64_GOT_LO12_NC _
  | R_AARCH64_ADD_ABS_LO12_NC _ | R_AARCH64_LDST64_ABS_LO12_NC _
  | R_AARCH64_ABS64 _ | R_AARCH64_PREL32_PAIR _ | R_AARCH64_PREL32 _ ->
    false

let get_addend (kind : Kind.t) : int =
  match kind with
  | R_AARCH64_ADR_PREL_LO21 { addend; _ }
  | R_AARCH64_ADR_PREL_PG_HI21 { addend; _ }
  | R_AARCH64_ADR_GOT_PAGE { addend; _ }
  | R_AARCH64_LD64_GOT_LO12_NC { addend; _ }
  | R_AARCH64_ADD_ABS_LO12_NC { addend; _ }
  | R_AARCH64_LDST64_ABS_LO12_NC { addend; _ }
  | R_AARCH64_CALL26 { addend; _ }
  | R_AARCH64_JUMP26 { addend; _ }
  | R_AARCH64_ABS64 { addend; _ }
  | R_AARCH64_PREL32 { addend; _ } ->
    addend
  | R_AARCH64_PREL32_PAIR _ -> 0

(* Instruction patching helpers for JIT relocation.

   These functions are defined here rather than in the *_helpers.ml files (e.g.,
   adr_helpers.ml, branch_helpers.ml) to avoid a dependency cycle. The helpers
   depend on Section_state, which depends on Relocation, so Relocation cannot
   depend on the helpers. These are pure bit-manipulation functions with no
   state dependencies. *)

let int64_of_int32_unsigned (x : int32) : int64 =
  Int64.logand (Int64.of_int32 x) 0xFFFFFFFFL

let patch_adr_offset (insn : int32) (offset : int64) : int64 =
  let imm21 = Int64.to_int offset in
  let immlo = imm21 land 0b11 in
  let immhi = (imm21 lsr 2) land 0x7ffff in
  let open Int32 in
  let insn = logand insn (lognot 0x60ffffe0l) in
  let insn = logor insn (shift_left (of_int immlo) 29) in
  let insn = logor insn (shift_left (of_int immhi) 5) in
  int64_of_int32_unsigned insn

let patch_adrp_offset (insn : int32) (page_offset : int64) : int64 =
  let page_diff = Int64.shift_right page_offset 12 in
  let imm21 = Int64.to_int page_diff in
  let immlo = imm21 land 0b11 in
  let immhi = (imm21 lsr 2) land 0x7ffff in
  let open Int32 in
  let insn = logand insn (lognot 0x60ffffe0l) in
  let insn = logor insn (shift_left (of_int immlo) 29) in
  let insn = logor insn (shift_left (of_int immhi) 5) in
  int64_of_int32_unsigned insn

let patch_add_imm12 (insn : int32) (offset12 : int64) : int64 =
  let imm = Int64.to_int offset12 in
  let open Int32 in
  let insn = logand insn (lognot 0x003ffc00l) in
  let insn = logor insn (shift_left (of_int imm) 10) in
  int64_of_int32_unsigned insn

let patch_ldr_imm12 (insn : int32) (offset12 : int64) ~(scale : int) : int64 =
  let open Int32 in
  let existing_scaled = to_int (logand (shift_right_logical insn 10) 0xfffl) in
  let new_scaled = Int64.to_int offset12 / scale in
  let combined_scaled = existing_scaled + new_scaled in
  if combined_scaled > 0xfff
  then
    Misc.fatal_errorf
      "patch_ldr_imm12: combined offset 0x%x exceeds 12-bit limit (existing \
       scaled=%d, new scaled=%d)"
      combined_scaled existing_scaled new_scaled;
  let insn = logand insn (lognot 0x003ffc00l) in
  let insn = logor insn (shift_left (of_int combined_scaled) 10) in
  int64_of_int32_unsigned insn

let patch_branch26 (insn : int32) (offset : int64) : int64 =
  let imm26 = Int64.to_int offset / 4 in
  let open Int32 in
  let insn = logand insn (lognot 0x03ffffffl) in
  let insn = logor insn (of_int (imm26 land 0x03ffffff)) in
  int64_of_int32_unsigned insn

let target_to_string (target : Symbol.target) : string =
  match target with
  | Label lbl -> Asm_targets.Asm_label.encode lbl
  | Symbol sym -> Asm_targets.Asm_symbol.encode sym

let compute_value (r : t) ~target_addr ~place_address ~read_instruction
    ~lookup_target =
  match r.kind with
  | R_AARCH64_ADR_PREL_LO21 _ ->
    let offset = Int64.sub target_addr place_address in
    let insn = read_instruction () in
    Ok (patch_adr_offset insn offset)
  | R_AARCH64_ADR_PREL_PG_HI21 _ | R_AARCH64_ADR_GOT_PAGE _ ->
    let page_mask = Int64.lognot 0xFFF_L in
    let target_page = Int64.logand target_addr page_mask in
    let place_page = Int64.logand place_address page_mask in
    let page_offset = Int64.sub target_page place_page in
    let insn = read_instruction () in
    Ok (patch_adrp_offset insn page_offset)
  | R_AARCH64_ADD_ABS_LO12_NC _ ->
    let low12 = Int64.logand target_addr 0xFFF_L in
    let insn = read_instruction () in
    Ok (patch_add_imm12 insn low12)
  | R_AARCH64_LDST64_ABS_LO12_NC _ | R_AARCH64_LD64_GOT_LO12_NC _ ->
    let low12 = Int64.logand target_addr 0xFFF_L in
    let insn = read_instruction () in
    Ok (patch_ldr_imm12 insn low12 ~scale:8)
  | R_AARCH64_CALL26 _ | R_AARCH64_JUMP26 _ ->
    let offset = Int64.sub target_addr place_address in
    let insn = read_instruction () in
    Ok (patch_branch26 insn offset)
  | R_AARCH64_ABS64 _ -> Ok target_addr
  | R_AARCH64_PREL32_PAIR { plus_target = _; minus_target } -> (
    match lookup_target minus_target with
    | None ->
      Error
        (Printf.sprintf "Minus target not found: %s"
           (target_to_string minus_target))
    | Some minus_addr -> Ok (Int64.sub target_addr minus_addr))
  | R_AARCH64_PREL32 _ -> Ok (Int64.sub target_addr place_address)
