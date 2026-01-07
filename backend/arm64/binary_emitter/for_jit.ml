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

module Symbol = Arm64_ast.Ast.Symbol

(* Convert from Arm64_ast.Ast.Symbol.target to Binary_emitter_intf.target *)
let convert_target (t : Symbol.target) : Binary_emitter_intf.target =
  match t with
  | Symbol.Symbol sym -> Binary_emitter_intf.Symbol sym
  | Symbol.Label lbl -> Binary_emitter_intf.Label lbl

module Relocation = struct
  type t = Relocation.t

  let offset_from_section_beginning = Relocation.offset_from_section_beginning

  let size = Relocation.size

  let target_symbol r = convert_target (Relocation.primary_target r)

  let target_symbols r = List.map convert_target (Relocation.all_targets r)

  let target_symbols_with_addends r =
    List.map
      (fun (t, addend) -> convert_target t, addend)
      (Relocation.all_targets_with_addends r)

  let is_got_reloc = Relocation.is_got_reloc

  let is_plt_reloc = Relocation.is_plt_reloc

  let compute_value (r : Relocation.t) ~place_address ~lookup_target
      ~read_instruction =
    let target = Relocation.primary_target r in
    (* Wrap lookup_target to convert from Symbol.target to Binary_emitter_intf.target *)
    let lookup_target_inner t = lookup_target (convert_target t) in
    match lookup_target_inner target with
    | None ->
      Error (Format.asprintf "Symbol not found: %a" Symbol.print_target target)
    | Some sym_addr ->
      let addend = Relocation.get_addend r.kind in
      let target_addr = Int64.add sym_addr (Int64.of_int addend) in
      Relocation.compute_value r ~target_addr ~place_address ~read_instruction
        ~lookup_target:lookup_target_inner
end

module Assembled_section = struct
  type t = Section_state.t

  type relocation = Relocation.t

  let size t = Buffer.length (Section_state.buffer t)

  let contents t = Section_state.contents t

  let contents_mut t = Section_state.contents_mut t

  let relocations t = Section_state.relocations t

  let find_symbol_offset t sym = Section_state.find_symbol_offset_in_bytes t sym

  let find_label_offset t lbl = Section_state.find_label_offset_in_bytes t lbl

  let iter_labels_and_symbols t ~f =
    (* For JIT, we need to export both global symbols and local labels. Local
       labels like _camlFoo__immstring51 need to be resolvable. *)
    Section_state.iter_symbols t ~f:(fun sym offset ->
        f (Binary_emitter_intf.Symbol sym) ~offset);
    Section_state.iter_labels t ~f:(fun lbl offset ->
        f (Binary_emitter_intf.Label lbl) ~offset)

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
