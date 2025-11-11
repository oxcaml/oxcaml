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

module Asm_label = Asm_targets.Asm_label
module Asm_section = Asm_targets.Asm_section
module Asm_symbol = Asm_targets.Asm_symbol
module D = Asm_targets.Asm_directives
module Symbol = Arm64_ast.Ast.Symbol

type t =
  { sections : Section_state.t Asm_section.Tbl.t;
    (* Individual sections track sections by their exact name string rather than
       the Asm_section.t enum. This is needed for function sections where each
       function gets its own .text.caml.<funcname> section. These sections are
       tracked separately so we can compare them individually against the
       assembler output during verification. *)
    individual_sections : (string, Section_state.t) Hashtbl.t;
    for_jit : bool;
    direct_assignments : (string, D.Directive.Constant.t) Hashtbl.t
  }

let create ~for_jit =
  { sections = Asm_section.Tbl.create 10;
    individual_sections = Hashtbl.create 16;
    for_jit;
    direct_assignments = Hashtbl.create 16
  }

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

(* Individual sections: for sections tracked by their exact name string, such as
   function sections (.text.caml.<funcname>). *)

let get_or_create_individual t name =
  match Hashtbl.find_opt t.individual_sections name with
  | Some state -> state
  | None ->
    let state = Section_state.create () in
    Hashtbl.add t.individual_sections name state;
    state

let find_individual t name = Hashtbl.find_opt t.individual_sections name

let iter_individual t ~f = Hashtbl.iter f t.individual_sections

let fold_individual t ~init ~f = Hashtbl.fold f t.individual_sections init

(* Search individual sections for a label or symbol. Returns (offset, section)
   if found. *)
let find_in_any_individual_section t target =
  let result = ref None in
  Hashtbl.iter
    (fun section_name state ->
      if Option.is_none !result
      then
        let section = Asm_section.Function_text section_name in
        match Section_state.find_target_offset_in_bytes state target with
        | Some offset -> result := Some (offset, section)
        | None -> ())
    t.individual_sections;
  !result

(* Search individual sections for a label or symbol. Returns (offset, section,
   state) if found. *)
let find_in_any_individual_section_with_state t target =
  let result = ref None in
  Hashtbl.iter
    (fun section_name state ->
      if Option.is_none !result
      then
        let section = Asm_section.Function_text section_name in
        match Section_state.find_target_offset_in_bytes state target with
        | Some offset -> result := Some (offset, section, state)
        | None -> ())
    t.individual_sections;
  !result

(* Search all sections for a label or symbol. Returns (offset, section,
   section_state) if found. This is needed when the caller needs to access the
   actual state where the label was found. *)
let find_in_any_section_with_state t target =
  let result = ref None in
  Asm_section.Tbl.iter
    (fun section state ->
      if Option.is_none !result
      then
        match Section_state.find_target_offset_in_bytes state target with
        | Some offset -> result := Some (offset, section, state)
        | None -> ())
    t.sections;
  (* If not found in standard sections, search individual sections. *)
  (if Option.is_none !result
  then
    match find_in_any_individual_section_with_state t target with
    | Some (offset, section, state) -> result := Some (offset, section, state)
    | None -> ());
  !result

(* Search all sections for a label or symbol. Returns (offset, section) if
   found. Cross-section references are handled via relocations. *)
let find_in_any_section t target =
  let result = ref None in
  Asm_section.Tbl.iter
    (fun section state ->
      if Option.is_none !result
      then
        match Section_state.find_target_offset_in_bytes state target with
        | Some offset -> result := Some (offset, section)
        | None -> ())
    t.sections;
  (* If not found in standard sections, search individual sections. *)
  (if Option.is_none !result
  then
    match find_in_any_individual_section t target with
    | Some (offset, section) -> result := Some (offset, section)
    | None -> ());
  !result

(* Reset all section offsets to 0 (for second pass). *)
let reset_offsets t =
  Asm_section.Tbl.iter
    (fun _section state -> Section_state.set_offset_in_bytes state 0)
    t.sections;
  Hashtbl.iter
    (fun _name state -> Section_state.set_offset_in_bytes state 0)
    t.individual_sections

(* Direct assignments (e.g., .set temp0, L100 - L101) *)
let add_direct_assignment t name expr =
  Hashtbl.replace t.direct_assignments name expr

let find_direct_assignment t name = Hashtbl.find_opt t.direct_assignments name
