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

(* Collection of section states for all sections in an assembly unit. *)

module Asm_section = Asm_targets.Asm_section

type t =
  { sections : Section_state.t Asm_section.Tbl.t;
    for_jit : bool
  }

let create ~for_jit = { sections = Asm_section.Tbl.create 10; for_jit }

let for_jit t = t.for_jit

let get_or_create t section =
  match Asm_section.Tbl.find_opt t.sections section with
  | Some state -> state
  | None ->
    let state = Section_state.create () in
    Asm_section.Tbl.add t.sections section state;
    state

let find t section = Asm_section.Tbl.find_opt t.sections section

let find_exn t section = Asm_section.Tbl.find t.sections section

let iter t ~f = Asm_section.Tbl.iter f t.sections

let fold t ~init ~f = Asm_section.Tbl.fold f t.sections init

(* Search all sections for a label or symbol. Returns (offset, section) if
   found. Cross-section references are handled via relocations. *)
let find_in_any_section t name =
  let result = ref None in
  Asm_section.Tbl.iter
    (fun section state ->
      if Option.is_none !result
      then
        match Section_state.find_label_offset_in_bytes state name with
        | Some offset -> result := Some (offset, section)
        | None -> (
          match Section_state.find_symbol_offset_in_bytes state name with
          | Some offset -> result := Some (offset, section)
          | None -> ()))
    t.sections;
  !result

(* Reset all section offsets to 0 (for second pass). *)
let reset_offsets t =
  Asm_section.Tbl.iter
    (fun _section state -> Section_state.set_offset_in_bytes state 0)
    t.sections
