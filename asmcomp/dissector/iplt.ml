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

(* CR mshinwell: This file needs to be code reviewed *)

module String = Misc.Stdlib.String
module Rela = Compiler_owee.Owee_elf_relocation

let log_verbose = Dissector_log.log_verbose

(* Each IPLT entry has architecture-specific size:

   x86-64 (8 bytes): ff 25 XX XX XX XX - jmp [rip + disp32] (6 bytes) 90 90 -
   2-byte nop padding The 4-byte displacement at offset +2 is filled by a PC32
   relocation.

   AArch64 (16 bytes): adrp x16, #0 - load page address of IGOT entry (4 bytes)
   ldr x17, [x16, #0] - load target address from IGOT (4 bytes) br x17 - branch
   to target (4 bytes) nop - padding (4 bytes) The ADRP at offset +0 gets
   ADR_PREL_PG_HI21, the LDR at offset +4 gets LDST64_ABS_LO12_NC, both pointing
   to the IGOT symbol. *)
let entry_size =
  match Target_system.architecture () with
  | X86_64 -> 8
  | AArch64 -> 16
  | _ -> Misc.fatal_error "Dissector IPLT: unsupported architecture"

(* Delimiter for synthetic symbol names - same as IGOT *)
let delimiter = "\xf0\x9f\x90\x8d" (* Unicode snake emoji U+1F40D in UTF-8 *)

module Entry = struct
  type t =
    { index : int;
      original_symbol : string;
      iplt_symbol : string;
      igot_symbol : string
    }

  let index e = e.index

  let original_symbol e = e.original_symbol

  let iplt_symbol e = e.iplt_symbol

  let igot_symbol e = e.igot_symbol

  let offset e = e.index * entry_size
end

type t =
  { entries : Entry.t list;
    by_original_symbol : Entry.t String.Tbl.t;
    section_data : bytes
  }

let iplt_symbol_name ~prefix ~symbol =
  "iplt" ^ delimiter ^ prefix ^ delimiter ^ symbol

(* Build the x86-64 PLT entry template using X86_binary_emitter. jmp [rip + 0]
   encodes as ff 25 00 00 00 00 (6 bytes), plus two NOP instructions (90 90) for
   padding to 8 bytes. *)
let x86_64_plt_entry_template =
  let section =
    { X86_binary_emitter.sec_name = ".text.iplt";
      sec_instrs =
        [| X86_ast.Ins
             (X86_ast.JMP (X86_ast.Mem64_RIP (X86_ast.QWORD, "dummy", 0)));
           X86_ast.Ins X86_ast.NOP;
           X86_ast.Ins X86_ast.NOP
        |]
    }
  in
  let buffer = X86_binary_emitter.assemble_section X86_ast.X64 section in
  let bytes = X86_binary_emitter.contents buffer in
  assert (String.length bytes = 8);
  bytes

(* Build the AArch64 PLT entry template using the ARM64 binary emitter. Each
   entry is 16 bytes: adrp x16, dummy@PAGE (4 bytes) ldr x17, [x16,
   dummy@PAGEOFF] (4 bytes) br x17 (4 bytes) nop (4 bytes) The relocation fields
   are zeroed on RELA platforms (Linux). *)
let aarch64_plt_entry_template =
  let module Ast = Arm64_ast.Ast in
  let module O = Ast.DSL in
  let module BE = Arm64_binary_emitter.Binary_emitter in
  let dummy = Asm_targets.Asm_symbol.create_global "iplt_dummy" in
  let page_sym =
    Ast.Symbol.create_symbol (Ast.Symbol.Needs_reloc Ast.Symbol.PAGE) dummy
  in
  let pageoff_sym =
    Ast.Symbol.create_symbol (Ast.Symbol.Needs_reloc Ast.Symbol.PAGE_OFF) dummy
  in
  let emitter = BE.create () in
  BE.add_instruction emitter
    (Ast.Instruction.I
       { name = Ast.Instruction_name.ADRP;
         operands = Pair (O.reg_x 16, O.symbol page_sym)
       });
  BE.add_instruction emitter
    (Ast.Instruction.I
       { name = Ast.Instruction_name.LDR;
         operands =
           Pair
             ( O.reg_x 17,
               O.mem_symbol ~base:(Ast.Reg.reg_x 16) ~symbol:pageoff_sym )
       });
  BE.add_instruction emitter
    (Ast.Instruction.I
       { name = Ast.Instruction_name.BR; operands = Singleton (O.reg_x 17) });
  BE.add_instruction emitter
    (Ast.Instruction.I
       { name = Ast.Instruction_name.NOP; operands = Singleton O.unit_operand });
  let sections = BE.emit emitter in
  let text =
    Arm64_binary_emitter.All_section_states.get_or_create sections
      Asm_targets.Asm_section.Text
  in
  let bytes = BE.Section_state.contents text in
  assert (String.length bytes = 16);
  bytes

let plt_entry_template =
  match Target_system.architecture () with
  | X86_64 -> x86_64_plt_entry_template
  | AArch64 -> aarch64_plt_entry_template
  | _ -> Misc.fatal_error "Dissector IPLT: unsupported architecture"

let build ~prefix ~igot ~symbols =
  (* Remove duplicates while preserving order, and build lookup table *)
  let by_original_symbol = String.Tbl.create 256 in
  let unique_symbols =
    List.filter
      (fun sym ->
        if String.Tbl.mem by_original_symbol sym
        then false
        else (
          (* Placeholder - will be replaced below *)
          String.Tbl.add by_original_symbol sym
            { Entry.index = 0;
              original_symbol = sym;
              iplt_symbol = "";
              igot_symbol = ""
            };
          true))
      symbols
  in
  let entries =
    List.mapi
      (fun index original_symbol ->
        let iplt_symbol = iplt_symbol_name ~prefix ~symbol:original_symbol in
        let igot_symbol =
          Igot.igot_symbol_name ~prefix ~symbol:original_symbol
        in
        (* Verify the IGOT entry exists *)
        (match Igot.find_entry igot ~symbol:original_symbol with
        | None ->
          Misc.fatal_errorf "IPLT: no IGOT entry for symbol %s" original_symbol
        | Some _ -> ());
        log_verbose "  IPLT entry %d: %s -> %s (via %s)" index original_symbol
          iplt_symbol igot_symbol;
        let entry =
          { Entry.index; original_symbol; iplt_symbol; igot_symbol }
        in
        String.Tbl.replace by_original_symbol original_symbol entry;
        entry)
      unique_symbols
  in
  (* Build section data *)
  let num_entries = List.length entries in
  let section_data = Bytes.make (num_entries * entry_size) '\x00' in
  for i = 0 to num_entries - 1 do
    Bytes.blit_string plt_entry_template 0 section_data (i * entry_size)
      entry_size
  done;
  { entries; by_original_symbol; section_data }

let entries t = t.entries

let section_data t = t.section_data

let section_size t = Bytes.length t.section_data

let find_entry t ~symbol = String.Tbl.find_opt t.by_original_symbol symbol

module Relocation = struct
  type t =
    { offset : int;
      symbol : string;
      reloc_type : Rela.Reloc_type.t;
      addend : int64
    }

  let offset r = r.offset

  let symbol r = r.symbol

  let reloc_type r = r.reloc_type

  let addend r = r.addend
end

let relocations t =
  match Target_system.architecture () with
  | X86_64 ->
    (* x86-64: one PC32 relocation at offset +2 (the disp32 field) *)
    List.map
      (fun entry ->
        Relocation.
          { offset = Entry.offset entry + 2;
            symbol = Entry.igot_symbol entry;
            reloc_type = Rela.Reloc_type.pc32;
            addend = -4L
          })
      t.entries
  | AArch64 ->
    (* AArch64: two relocations per entry: offset +0: ADR_PREL_PG_HI21 (ADRP
       instruction) offset +4: LDST64_ABS_LO12_NC (LDR instruction) *)
    List.concat_map
      (fun entry ->
        let base = Entry.offset entry in
        let sym = Entry.igot_symbol entry in
        [ Relocation.
            { offset = base;
              symbol = sym;
              reloc_type = Rela.Reloc_type.aarch64_adr_prel_pg_hi21;
              addend = 0L
            };
          Relocation.
            { offset = base + 4;
              symbol = sym;
              reloc_type = Rela.Reloc_type.aarch64_ldst64_abs_lo12_nc;
              addend = 0L
            } ])
      t.entries
  | _ -> Misc.fatal_error "Dissector IPLT: unsupported architecture"
