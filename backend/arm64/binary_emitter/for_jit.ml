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

(* For_jit module implementing Binary_emitter_intf.S *)

module Relocation = struct
  type t = Relocation.t

  let offset_from_section_beginning (r : Relocation.t) =
    r.offset_from_section_beginning

  (* ARM64 code relocations are 32-bit patches within 32-bit instructions, but
     data relocations (ABS64) are 64-bit *)
  let size (r : t) : Binary_emitter_intf.data_size =
    match r.kind with
    | R_AARCH64_ABS64 _ -> Binary_emitter_intf.B64
    | R_AARCH64_ADR_PREL_LO21 _ | R_AARCH64_ADR_PREL_PG_HI21 _
    | R_AARCH64_ADR_GOT_PAGE _ | R_AARCH64_LD64_GOT_LO12_NC _
    | R_AARCH64_ADD_ABS_LO12_NC _ | R_AARCH64_LDST64_ABS_LO12_NC _
    | R_AARCH64_CALL26 _ | R_AARCH64_JUMP26 _ | R_AARCH64_PREL32_PAIR _ ->
      Binary_emitter_intf.B32

  let target_symbol (r : Relocation.t) : string =
    match r.kind with
    | R_AARCH64_ADR_PREL_LO21 sym
    | R_AARCH64_ADR_PREL_PG_HI21 sym
    | R_AARCH64_ADR_GOT_PAGE sym
    | R_AARCH64_LD64_GOT_LO12_NC sym
    | R_AARCH64_ADD_ABS_LO12_NC sym
    | R_AARCH64_LDST64_ABS_LO12_NC sym
    | R_AARCH64_CALL26 sym
    | R_AARCH64_JUMP26 sym
    | R_AARCH64_ABS64 sym ->
      sym
    | R_AARCH64_PREL32_PAIR { plus_symbol; _ } ->
      plus_symbol (* Return the plus symbol as the primary target *)

  (* For paired relocations (SUBTRACTOR + UNSIGNED), returns both symbols.
     Returns [plus_symbol; minus_symbol] for pairs, [symbol] otherwise. *)
  let target_symbols (r : Relocation.t) : string list =
    match r.kind with
    | R_AARCH64_ADR_PREL_LO21 sym
    | R_AARCH64_ADR_PREL_PG_HI21 sym
    | R_AARCH64_ADR_GOT_PAGE sym
    | R_AARCH64_LD64_GOT_LO12_NC sym
    | R_AARCH64_ADD_ABS_LO12_NC sym
    | R_AARCH64_LDST64_ABS_LO12_NC sym
    | R_AARCH64_CALL26 sym
    | R_AARCH64_JUMP26 sym
    | R_AARCH64_ABS64 sym ->
      [sym]
    | R_AARCH64_PREL32_PAIR { plus_symbol; minus_symbol } ->
      (* Return both: UNSIGNED uses plus_symbol, SUBTRACTOR uses minus_symbol *)
      [plus_symbol; minus_symbol]

  let is_got_reloc (r : Relocation.t) =
    match r.kind with
    | R_AARCH64_ADR_GOT_PAGE _ | R_AARCH64_LD64_GOT_LO12_NC _ -> true
    | R_AARCH64_ADR_PREL_LO21 _ | R_AARCH64_ADR_PREL_PG_HI21 _
    | R_AARCH64_ADD_ABS_LO12_NC _ | R_AARCH64_LDST64_ABS_LO12_NC _
    | R_AARCH64_CALL26 _ | R_AARCH64_JUMP26 _ | R_AARCH64_ABS64 _
    | R_AARCH64_PREL32_PAIR _ ->
      false

  let is_plt_reloc (r : t) =
    (* ARM64 CALL26/JUMP26 relocations need PLT because the target may be >128MB
       away (branch range is ±128MB). The PLT entry is placed within the JIT
       allocation and does an indirect branch. *)
    match r.kind with
    | R_AARCH64_CALL26 _ | R_AARCH64_JUMP26 _ -> true
    | R_AARCH64_ADR_PREL_LO21 _ | R_AARCH64_ADR_PREL_PG_HI21 _
    | R_AARCH64_ADR_GOT_PAGE _ | R_AARCH64_LD64_GOT_LO12_NC _
    | R_AARCH64_ADD_ABS_LO12_NC _ | R_AARCH64_LDST64_ABS_LO12_NC _
    | R_AARCH64_ABS64 _ | R_AARCH64_PREL32_PAIR _ ->
      false

  (* Helper: zero-extend int32 to int64 (don't sign-extend) *)
  let int64_of_int32_unsigned (x : int32) : int64 =
    Int64.logand (Int64.of_int32 x) 0xFFFFFFFFL

  (* Helper: encode a 21-bit page offset into ADRP instruction. ADRP format:
     immlo in bits 29:30, immhi in bits 5:23 *)
  let encode_adrp_offset (insn : int32) (page_offset : int64) : int64 =
    (* page_offset is in bytes, but ADRP uses page units (4K = 0x1000). The
       immediate is the page difference, which we already computed. page_offset
       / 4096 = number of pages difference *)
    let page_diff = Int64.shift_right page_offset 12 in
    let imm21 = Int64.to_int page_diff in
    let immlo = imm21 land 0b11 in
    let immhi = (imm21 lsr 2) land 0x7ffff in
    (* Clear old immediate bits and set new ones *)
    let open Int32 in
    let insn = logand insn (lognot 0x60ffffe0l) in
    (* Clear immlo (29:30) and immhi (5:23) *)
    let insn = logor insn (shift_left (of_int immlo) 29) in
    let insn = logor insn (shift_left (of_int immhi) 5) in
    int64_of_int32_unsigned insn

  (* Helper: encode a 12-bit offset into LDR/STR immediate instruction. The
     offset goes in bits 10:21, and is scaled by the access size. For LDR x
     (64-bit), the value in the instruction is offset/8. The new scaled offset
     is ADDED to the existing value in the instruction to support symbol+offset
     addressing where the offset is pre-encoded. *)
  let encode_ldr_imm12 (insn : int32) (offset12 : int64) ~(scale : int) : int64
      =
    let open Int32 in
    (* Read existing scaled imm12 from bits 10:21 *)
    let existing_scaled =
      to_int (logand (shift_right_logical insn 10) 0xfffl)
    in
    let new_scaled = Int64.to_int offset12 / scale in
    let combined_scaled = existing_scaled + new_scaled in
    if combined_scaled > 0xfff
    then
      Misc.fatal_errorf
        "encode_ldr_imm12: combined offset 0x%x exceeds 12-bit limit (existing \
         scaled=%d, new scaled=%d)"
        combined_scaled existing_scaled new_scaled;
    let insn = logand insn (lognot 0x003ffc00l) in
    (* Clear bits 10:21 *)
    let insn = logor insn (shift_left (of_int combined_scaled) 10) in
    int64_of_int32_unsigned insn

  (* Helper: encode a 12-bit offset into ADD immediate instruction. The offset
     goes in bits 10:21, not scaled. *)
  let encode_add_imm12 (insn : int32) (offset12 : int64) : int64 =
    let imm = Int64.to_int offset12 in
    let open Int32 in
    let insn = logand insn (lognot 0x003ffc00l) in
    (* Clear bits 10:21 *)
    let insn = logor insn (shift_left (of_int imm) 10) in
    int64_of_int32_unsigned insn

  (* Helper: encode a 26-bit offset into B/BL instruction. The offset is
     PC-relative, divided by 4, goes in bits 0:25. *)
  let encode_branch26 (insn : int32) (offset : int64) : int64 =
    let imm26 = Int64.to_int offset / 4 in
    let open Int32 in
    let insn = logand insn (lognot 0x03ffffffl) in
    (* Clear bits 0:25 *)
    let insn = logor insn (of_int (imm26 land 0x03ffffff)) in
    int64_of_int32_unsigned insn

  let compute_value (r : Relocation.t) ~place_address ~lookup_symbol
      ~read_instruction =
    let sym = target_symbol r in
    match lookup_symbol sym with
    | None -> Error (Printf.sprintf "Symbol not found: %s" sym)
    | Some target_addr -> (
      match r.kind with
      | R_AARCH64_ADR_PREL_LO21 _ ->
        (* PC-relative offset for ADR instruction, low 21 bits. ADR has the same
           bit layout as ADRP but the immediate is a byte offset, not a page
           offset. *)
        let offset = Int64.sub target_addr place_address in
        let imm21 = Int64.to_int offset in
        let immlo = imm21 land 0b11 in
        let immhi = (imm21 lsr 2) land 0x7ffff in
        let insn = read_instruction () in
        let open Int32 in
        let insn = logand insn (lognot 0x60ffffe0l) in
        let insn = logor insn (shift_left (of_int immlo) 29) in
        let insn = logor insn (shift_left (of_int immhi) 5) in
        Ok (int64_of_int32_unsigned insn)
      | R_AARCH64_ADR_PREL_PG_HI21 _ | R_AARCH64_ADR_GOT_PAGE _ ->
        (* Page-relative offset for ADRP instruction. For ADR_GOT_PAGE,
           target_addr is the GOT entry address (handled by is_got_reloc
           returning true). *)
        let page_mask = Int64.lognot 0xFFF_L in
        let target_page = Int64.logand target_addr page_mask in
        let place_page = Int64.logand place_address page_mask in
        let page_offset = Int64.sub target_page place_page in
        let insn = read_instruction () in
        let result = encode_adrp_offset insn page_offset in
        (match Sys.getenv_opt "OCAML_JIT_DEBUG" with
        | Some ("true" | "1") ->
          Printf.eprintf
            "ADRP patch: sym=%s target=%Lx place=%Lx offset=%Lx insn=%lx -> %Lx\n\
             %!"
            sym target_addr place_address page_offset insn result
        | _ -> ());
        Ok result
      | R_AARCH64_ADD_ABS_LO12_NC _ ->
        (* Lower 12 bits of absolute address for ADD instruction *)
        let low12 = Int64.logand target_addr 0xFFF_L in
        let insn = read_instruction () in
        let result = encode_add_imm12 insn low12 in
        (match Sys.getenv_opt "OCAML_JIT_DEBUG" with
        | Some ("true" | "1") ->
          Printf.eprintf
            "ADD LO12 patch: sym=%s target=%Lx low12=%Lx insn=%lx -> %Lx\n%!"
            sym target_addr low12 insn result
        | _ -> ());
        Ok result
      | R_AARCH64_LDST64_ABS_LO12_NC _ ->
        (* Lower 12 bits of absolute address for LDR/STR x instruction, scaled
           by 8 *)
        let low12 = Int64.logand target_addr 0xFFF_L in
        let insn = read_instruction () in
        let result = encode_ldr_imm12 insn low12 ~scale:8 in
        (match Sys.getenv_opt "OCAML_JIT_DEBUG" with
        | Some ("true" | "1") ->
          Printf.eprintf
            "LDST64 LO12 patch: sym=%s target=%Lx low12=%Lx insn=%lx -> %Lx\n%!"
            sym target_addr low12 insn result
        | _ -> ());
        Ok result
      | R_AARCH64_LD64_GOT_LO12_NC _ ->
        (* Lower 12 bits of GOT entry address for LDR x instruction, scaled by
           8 *)
        let low12 = Int64.logand target_addr 0xFFF_L in
        let insn = read_instruction () in
        let result = encode_ldr_imm12 insn low12 ~scale:8 in
        (match Sys.getenv_opt "OCAML_JIT_DEBUG" with
        | Some ("true" | "1") ->
          Printf.eprintf
            "LDR GOT patch: sym=%s target=%Lx low12=%Lx insn=%lx -> %Lx\n%!" sym
            target_addr low12 insn result
        | _ -> ());
        Ok result
      | R_AARCH64_CALL26 _ | R_AARCH64_JUMP26 _ ->
        (* PC-relative offset for B/BL instructions *)
        let offset = Int64.sub target_addr place_address in
        let insn = read_instruction () in
        let result = encode_branch26 insn offset in
        (match Sys.getenv_opt "OCAML_JIT_DEBUG" with
        | Some ("true" | "1") ->
          Printf.eprintf
            "BL patch: sym=%s target=%Lx place=%Lx offset=%Lx insn=%lx -> %Lx\n\
             %!"
            sym target_addr place_address offset insn result
        | _ -> ());
        Ok result
      | R_AARCH64_ABS64 _ ->
        (* Absolute 64-bit address - no instruction patching needed *)
        Ok target_addr
      | R_AARCH64_PREL32_PAIR { plus_symbol; minus_symbol } -> (
        (* Cross-section relative: addend + plus_symbol - minus_symbol *)
        match lookup_symbol minus_symbol with
        | None ->
          Error (Printf.sprintf "Minus symbol not found: %s" minus_symbol)
        | Some minus_addr ->
          let _ = plus_symbol in
          (* target_addr is plus_symbol's address *)
          Ok (Int64.sub target_addr minus_addr)))
end

module Assembled_section = struct
  type t = Section_state.t

  type relocation = Relocation.t

  let size t = Buffer.length (Section_state.buffer t)

  let contents t = Section_state.contents t

  let contents_mut t = Section_state.contents_mut t

  let relocations t = Section_state.relocations t

  let find_symbol_offset t name =
    Section_state.find_symbol_offset_in_bytes t name

  let find_label_offset t name = Section_state.find_label_offset_in_bytes t name

  let iter_symbols t ~f =
    (* For JIT, we need to export both global symbols and local labels. Local
       labels like _camlFoo__immstring51 need to be resolvable. *)
    Hashtbl.iter (fun name offset -> f ~name ~offset) (Section_state.symbols t);
    Hashtbl.iter (fun name offset -> f ~name ~offset) (Section_state.labels t)

  let add_patch t ~offset ~size:(sz : Binary_emitter_intf.data_size) ~data =
    let sz =
      match sz with
      | B8 -> Section_state.P8
      | B16 -> Section_state.P16
      | B32 -> Section_state.P32
      | B64 -> Section_state.P64
    in
    Section_state.add_patch t ~offset ~size:sz ~data
end

module Plt = struct
  (* ARM64 PLT entry: ldr x16, .+8 ; 58000050 - load address from next 8 bytes
     br x16 ; d61f0200 - branch to x16 .quad <address> ; 8 bytes of address
     Total: 16 bytes *)
  let entry_size = 16

  let write_entry buf address =
    (* ldr x16, .+8 - PC-relative load from 8 bytes ahead *)
    Buffer.add_char buf '\x50';
    Buffer.add_char buf '\x00';
    Buffer.add_char buf '\x00';
    Buffer.add_char buf '\x58';
    (* br x16 - branch to register *)
    Buffer.add_char buf '\x00';
    Buffer.add_char buf '\x02';
    Buffer.add_char buf '\x1f';
    Buffer.add_char buf '\xd6';
    (* 8-byte address (little-endian) *)
    for i = 0 to 7 do
      let byte =
        Int64.(to_int (logand (shift_right_logical address (i * 8)) 0xFFL))
      in
      Buffer.add_char buf (Char.chr byte)
    done
end

module Internal_assembler = struct
  type assembled_section = Assembled_section.t

  type hook = (string * assembled_section) list -> string -> unit

  let current_hook : hook option ref = ref None

  let register h = current_hook := Some h

  let unregister () = current_hook := None

  let get () = !current_hook
end
