(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

[@@@ocaml.warning "+a-40-41-42"]

module D = Asm_targets.Asm_directives.Directive
module L = Asm_targets.Asm_label
module S = Asm_targets.Asm_symbol
module Symbol = Ast.Symbol

module Line = struct
  type t =
    | Instruction of Ast.Instruction.t
    | Directive of D.t

  let defined_label t =
    match t with Directive d -> D.defined_label d | Instruction _ -> None
end

let hidden_pc_relative_offset = "<hidden PC-relative offset>"

let hidden_gc_jump_pad = "<hidden GC jump pad>"

let computes_address : type w. w Symbol.same_unit_or_reloc -> bool = function
  | Same_section_and_unit | Needs_reloc CALL26 | Needs_reloc JUMP26 -> false
  | Needs_reloc
      ( LOWER_TWELVE | GOT_PAGE | GOT_PAGE_OFF | GOT_LOWER_TWELVE | PAGE
      | PAGE_OFF ) ->
    true

let local_label_name n = ".L" ^ Int.to_string n

let symbol_printer ~label_numbers ~hidden_labels : Symbol.printer =
  let print : type w. Format.formatter -> w Symbol.t -> unit =
   fun ppf sym ->
    let print_as target_name = Symbol.print_gas_syntax ~target_name ppf sym in
    let print_hidden name = Format.pp_print_string ppf name in
    match sym.target with
    | Label label -> (
      match L.Tbl.find_opt label_numbers label with
      | Some n -> print_as (local_label_name n)
      | None ->
        if L.Set.mem label hidden_labels
        then print_hidden hidden_gc_jump_pad
        else print_hidden hidden_pc_relative_offset)
    | Symbol s ->
      let name = S.encode_without_prefix s in
      if computes_address sym.reloc && String.starts_with ~prefix:"camlTOP" name
      then print_hidden hidden_pc_relative_offset
      else print_as name
  in
  { print }

let format ~name ~body ~hidden_gc_jump_pads =
  let body =
    List.filter
      (fun line ->
        match Line.defined_label line with
        | Some label -> not (L.is_dwarf_only_label label)
        | None -> true)
      body
  in
  let label_numbers = L.Tbl.create 16 in
  List.iter
    (fun line ->
      Option.iter
        (fun label ->
          L.Tbl.add label_numbers label (L.Tbl.length label_numbers))
        (Line.defined_label line))
    body;
  let hidden_labels =
    L.Set.of_list (List.filter_map Line.defined_label hidden_gc_jump_pads)
  in
  let symbol = symbol_printer ~label_numbers ~hidden_labels in
  let buf = Buffer.create 1024 in
  Printf.bprintf buf "%s:\n" name;
  List.iter
    (fun (line : Line.t) ->
      match line with
      | Instruction instr ->
        Buffer.add_string buf
          (Format.asprintf "\t%a\n" (Ast.Instruction.print_with ~symbol) instr)
      | Directive _ -> (
        match Line.defined_label line with
        | Some label ->
          Printf.bprintf buf "%s:\n"
            (local_label_name (L.Tbl.find label_numbers label))
        | None -> ()))
    body;
  Buffer.contents buf
