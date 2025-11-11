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

type patch_size =
  | P8
  | P16
  | P32
  | P64

type t =
  { buffer : Buffer.t;
    mutable offset_in_bytes : int;
    symbol_offset_tbl : (string, int) Hashtbl.t;
    label_offset_tbl : (string, int) Hashtbl.t;
    mutable relocations : Relocation.t list;
    mutable patches : (int * patch_size * int64) list
  }

let create () =
  { buffer = Buffer.create 1024;
    offset_in_bytes = 0;
    symbol_offset_tbl = Hashtbl.create 16;
    label_offset_tbl = Hashtbl.create 16;
    relocations = [];
    patches = []
  }

let buffer t = t.buffer

let offset_in_bytes t = t.offset_in_bytes

let set_offset_in_bytes t offset = t.offset_in_bytes <- offset

let add_relocation_at_current_offset t ~symbol_name:_ ~reloc_kind =
  t.relocations
    <- { Relocation.offset_from_section_beginning = t.offset_in_bytes;
         kind = reloc_kind
       }
       :: t.relocations

let define_symbol t name =
  Hashtbl.replace t.symbol_offset_tbl name t.offset_in_bytes

let define_label t name =
  Hashtbl.replace t.label_offset_tbl name t.offset_in_bytes

let find_symbol_offset_in_bytes t name =
  Hashtbl.find_opt t.symbol_offset_tbl name

let find_label_offset_in_bytes t name = Hashtbl.find_opt t.label_offset_tbl name

(* Find the nearest global symbol strictly before a given offset. Returns
   (symbol_name, symbol_offset) or None.

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
  Hashtbl.iter
    (fun name sym_offset ->
      if sym_offset < offset
      then
        match !best with
        | None -> best := Some (name, sym_offset)
        | Some (_, best_offset) when sym_offset > best_offset ->
          best := Some (name, sym_offset)
        | Some _ -> ())
    t.symbol_offset_tbl;
  !best

(* Look up both symbols and labels - used for branch targets which can be
   either *)
let find_symbol_or_label_offset_in_bytes t name =
  match Hashtbl.find_opt t.symbol_offset_tbl name with
  | Some _ as result -> result
  | None -> Hashtbl.find_opt t.label_offset_tbl name

let relocations t = List.rev t.relocations

let symbols t = t.symbol_offset_tbl

let labels t = t.label_offset_tbl

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
