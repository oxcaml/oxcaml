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

type t =
  { igot : Igot.t;
    iplt : Iplt.t;
    plt_symbols : string list;
    got_symbols : string list
  }

let igot t = t.igot

let iplt t = t.iplt

let plt_symbols t = t.plt_symbols

let got_symbols t = t.got_symbols

let build ~prefix relocations =
  (* Extract unique symbol names from PLT32 relocations *)
  let plt_syms =
    List.map Extract_relocations.Relocation_entry.symbol_name
      (Extract_relocations.convert_to_plt relocations)
  in
  (* Extract unique symbol names from GOTPCRELX relocations *)
  let got_only_symbols =
    List.map Extract_relocations.Relocation_entry.symbol_name
      (Extract_relocations.convert_to_got relocations)
  in
  (* IGOT needs entries for both PLT symbols (PLT jumps through GOT) and
     GOT-only symbols. Combine the lists - Igot.build will deduplicate. Use
     List.rev_append for tail-recursion safety with large symbol lists. *)
  let all_got_symbols = List.rev_append (List.rev plt_syms) got_only_symbols in
  (* Build IGOT first (IPLT depends on it) *)
  let igot = Igot.build ~prefix ~symbols:all_got_symbols in
  (* Build IPLT for PLT symbols only *)
  let iplt = Iplt.build ~prefix ~igot ~symbols:plt_syms in
  { igot; iplt; plt_symbols = plt_syms; got_symbols = got_only_symbols }

let igot_symbol_for_got_reloc t reloc =
  let symbol = Extract_relocations.Relocation_entry.symbol_name reloc in
  match Igot.find_entry t.igot ~symbol with
  | None -> None
  | Some entry -> Some (Igot.Entry.igot_symbol entry)

let iplt_symbol_for_plt_reloc t reloc =
  let symbol = Extract_relocations.Relocation_entry.symbol_name reloc in
  match Iplt.find_entry t.iplt ~symbol with
  | None -> None
  | Some entry -> Some (Iplt.Entry.iplt_symbol entry)
