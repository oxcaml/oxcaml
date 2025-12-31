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

let log_verbose = Dissector_log.log_verbose

(* Each IGOT entry is 8 bytes (one 64-bit address) *)
let entry_size = 8

(* Delimiter for synthetic symbol names - unlikely to appear in normal
   symbols *)
let delimiter = "\xf0\x9f\x90\x8d" (* Unicode snake emoji U+1F40D in UTF-8 *)

module Entry = struct
  type t =
    { index : int;
      original_symbol : string;
      igot_symbol : string
    }

  let index e = e.index

  let original_symbol e = e.original_symbol

  let igot_symbol e = e.igot_symbol

  let offset e = e.index * entry_size
end

type t =
  { entries : Entry.t list;
    by_original_symbol : Entry.t String.Tbl.t;
    section_data : bytes
  }

let igot_symbol_name ~prefix ~symbol =
  "igot" ^ delimiter ^ prefix ^ delimiter ^ symbol

let build ~prefix ~symbols =
  (* Remove duplicates while preserving order, and build lookup table *)
  let by_original_symbol = String.Tbl.create 256 in
  let unique_symbols =
    List.filter
      (fun sym ->
        if String.Tbl.mem by_original_symbol sym
        then false
        else (
          (* Placeholder entry - will be replaced below *)
          String.Tbl.add by_original_symbol sym
            { Entry.index = 0; original_symbol = sym; igot_symbol = "" };
          true))
      symbols
  in
  let entries =
    List.mapi
      (fun index original_symbol ->
        let igot_symbol = igot_symbol_name ~prefix ~symbol:original_symbol in
        log_verbose "  IGOT entry %d: %s -> %s" index original_symbol
          igot_symbol;
        let entry = { Entry.index; original_symbol; igot_symbol } in
        String.Tbl.replace by_original_symbol original_symbol entry;
        entry)
      unique_symbols
  in
  (* Section data is zero-initialized *)
  let section_data = Bytes.make (List.length entries * entry_size) '\x00' in
  { entries; by_original_symbol; section_data }

let entries t = t.entries

let section_data t = t.section_data

let section_size t = Bytes.length t.section_data

let find_entry t ~symbol = String.Tbl.find_opt t.by_original_symbol symbol

module Relocation = struct
  type t =
    { offset : int;
      symbol : string;
      addend : int64
    }

  let offset r = r.offset

  let symbol r = r.symbol

  let addend r = r.addend
end

let relocations t =
  List.map
    (fun entry ->
      Relocation.
        { offset = Entry.offset entry;
          symbol = Entry.original_symbol entry;
          addend = 0L
        })
    t.entries
