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

module Asm_label = Asm_targets.Asm_label
module Asm_section = Asm_targets.Asm_section
module Asm_symbol = Asm_targets.Asm_symbol
module Symbol = Arm64_ast.Ast.Symbol

type patch_size =
  | P8
  | P16
  | P32
  | P64

type t =
  { buffer : Buffer.t;
    mutable offset_in_bytes : int;
    symbol_offset_tbl : int Asm_symbol.Tbl.t;
    label_offset_tbl : int Asm_label.Tbl.t;
    (* Tracks symbols that are explicitly global (via Global/Weak directives).
       These get symbol table entries in ELF. File-scope symbols defined only
       via New_label are not in this set. *)
    global_symbols : Asm_symbol.Set.t ref;
    mutable relocations : Relocation.t list;
    mutable patches : (int * patch_size * int64) list
  }

let create () =
  { buffer = Buffer.create 1024;
    offset_in_bytes = 0;
    symbol_offset_tbl = Asm_symbol.Tbl.create 16;
    label_offset_tbl = Asm_label.Tbl.create 16;
    global_symbols = ref Asm_symbol.Set.empty;
    relocations = [];
    patches = []
  }

let buffer t = t.buffer

let offset_in_bytes t = t.offset_in_bytes

let set_offset_in_bytes t offset = t.offset_in_bytes <- offset

let add_relocation_at_current_offset t ~reloc_kind =
  t.relocations
    <- { Relocation.offset_from_section_beginning = t.offset_in_bytes;
         kind = reloc_kind
       }
       :: t.relocations

let add_relocation t (reloc : Relocation.t) =
  t.relocations <- reloc :: t.relocations

let define_symbol t sym =
  Asm_symbol.Tbl.replace t.symbol_offset_tbl sym t.offset_in_bytes

let define_label t lbl =
  Asm_label.Tbl.replace t.label_offset_tbl lbl t.offset_in_bytes

(* Mark a symbol as global. Called for Global and Weak directives. *)
let mark_global t sym =
  t.global_symbols := Asm_symbol.Set.add sym !(t.global_symbols)

(* Check if a symbol is explicitly global (has Global or Weak directive) *)
let is_global t sym = Asm_symbol.Set.mem sym !(t.global_symbols)

let find_symbol_offset_in_bytes t sym =
  Asm_symbol.Tbl.find_opt t.symbol_offset_tbl sym

let find_label_offset_in_bytes t lbl =
  Asm_label.Tbl.find_opt t.label_offset_tbl lbl

(* Find the nearest global symbol strictly before a given offset. Returns
   (symbol, symbol_offset) or None.

   NOTE: This is a somewhat surprising design choice. For cross-section
   relocations (e.g., frame table entries in DATA referencing labels in TEXT),
   we could create local symbols at exact positions and use addend=0. However,
   the system assembler instead uses existing global symbols (like
   _camlFoo__frametable and _camlFoo__fib_code) and computes a non-zero addend.
   We match this behavior to produce byte-identical output for testing purposes.
   Both approaches are correct for linking - the linker computes the same final
   value either way.

   We use strict < rather than <= because the assembler doesn't use symbols that
   are at the exact target offset (e.g., _code_end at a return address). *)
let find_nearest_symbol_before t offset =
  let best = ref None in
  Asm_symbol.Tbl.iter
    (fun sym sym_offset ->
      if sym_offset < offset
      then
        match !best with
        | None -> best := Some (sym, sym_offset)
        | Some (_, best_offset) when sym_offset > best_offset ->
          best := Some (sym, sym_offset)
        | Some _ -> ())
    t.symbol_offset_tbl;
  !best

(* Look up a target (symbol or label) by its typed value *)
let find_target_offset_in_bytes t (target : Symbol.target) =
  match target with
  | Symbol sym -> find_symbol_offset_in_bytes t sym
  | Label lbl -> find_label_offset_in_bytes t lbl

let relocations t = List.rev t.relocations

let iter_symbols t ~f = Asm_symbol.Tbl.iter f t.symbol_offset_tbl

let iter_labels t ~f = Asm_label.Tbl.iter f t.label_offset_tbl

let add_patch t ~offset ~size ~data =
  t.patches <- (offset, size, data) :: t.patches

let contents_mut t =
  let buf = Buffer.to_bytes t.buffer in
  let set_int8 pos v =
    Bytes.set buf pos (Char.chr (Int64.to_int v land 0xFF))
  in
  List.iter
    (fun (pos, size, v) ->
      match size with
      | P8 -> set_int8 pos v
      | P16 ->
        set_int8 pos v;
        set_int8 (pos + 1) (Int64.shift_right_logical v 8)
      | P32 ->
        set_int8 pos v;
        set_int8 (pos + 1) (Int64.shift_right_logical v 8);
        set_int8 (pos + 2) (Int64.shift_right_logical v 16);
        set_int8 (pos + 3) (Int64.shift_right_logical v 24)
      | P64 ->
        set_int8 pos v;
        set_int8 (pos + 1) (Int64.shift_right_logical v 8);
        set_int8 (pos + 2) (Int64.shift_right_logical v 16);
        set_int8 (pos + 3) (Int64.shift_right_logical v 24);
        set_int8 (pos + 4) (Int64.shift_right_logical v 32);
        set_int8 (pos + 5) (Int64.shift_right_logical v 40);
        set_int8 (pos + 6) (Int64.shift_right_logical v 48);
        set_int8 (pos + 7) (Int64.shift_right_logical v 56))
    t.patches;
  buf

let contents t = Bytes.to_string (contents_mut t)
