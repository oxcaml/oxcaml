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

module Elf = Compiler_owee.Owee_elf
module Rela = Compiler_owee.Owee_elf_relocation
module Strtab = Compiler_owee.Owee_elf_string_table
module Buf = Compiler_owee.Owee_buf
module String = Misc.Stdlib.String
module FRP = Form_rewrite_plan

(* Safe conversion from int64 to int with bounds checking. Fatal error if the
   value doesn't fit in an int (would happen on 32-bit platforms for values >
   2GB, or for corrupt/unreasonable values). *)
let int64_to_int value =
  if value < Int64.of_int min_int || value > Int64.of_int max_int
  then
    Misc.fatal_errorf "Dissector: offset %Ld exceeds platform int range" value
  else Int64.to_int value

let write_symbol ~cursor ~strtab sym =
  let module SE = FRP.Symbol_entry in
  Rela.write_sym_entry ~cursor
    { st_name = Strtab.add strtab (SE.name sym);
      st_info = SE.st_info sym;
      st_other = SE.st_other sym;
      st_shndx = SE.st_shndx sym;
      st_value = SE.st_value sym;
      st_size = SE.st_size sym
    }

(* When section_index >= SHN_LORESERVE, we must use SHN_XINDEX and store the
   actual index in the SYMTAB_SHNDX section. *)
let write_synthetic_symbol ~cursor ~strtab ~name ~section_index ~offset ~size
    ~is_func =
  let st_shndx =
    if section_index >= Rela.shn_loreserve
    then Rela.shn_xindex
    else section_index
  in
  Rela.write_sym_entry ~cursor
    { st_name = Strtab.add strtab name;
      st_info =
        Rela.make_st_info ~binding:Rela.Symbol_binding.global
          ~typ:
            (if is_func then Rela.Symbol_type.func else Rela.Symbol_type.notype);
      st_other = Rela.Symbol_visibility.(to_int hidden);
      st_shndx;
      st_value = Int64.of_int offset;
      st_size = Int64.of_int size
    }

let write_rela ~cursor ~symbol_to_index ~r_offset ~symbol ~r_type ~r_addend =
  let r_sym =
    String.Tbl.find_opt symbol_to_index symbol |> Option.value ~default:0
  in
  Rela.write_rela_entry ~cursor
    { r_offset = Int64.of_int r_offset; r_sym; r_type; r_addend }

let execute_plan unix ~input_file ~output_file ~header ~sections
    ~shstrtab_section ~igot_and_iplt ~plan =
  let module Unix = (val unix : Compiler_owee.Unix_intf.S) in
  let module SL = FRP.Section_layout in
  let module L = FRP.Layout in
  let input_buf = Buf.map_binary (module Unix) input_file in
  let layout = FRP.layout plan in
  let output_buf =
    Buf.map_binary_write
      (module Unix)
      output_file
      (int64_to_int (L.total_size layout))
  in
  let original_data_end =
    Array.fold_left
      (fun acc (s : Elf.section) -> max acc (Int64.add s.sh_offset s.sh_size))
      0L sections
  in
  let original_size = int64_to_int original_data_end in
  (* Use Bigarray.Array1.blit for efficient bulk copy (memcpy internally) *)
  Bigarray.Array1.blit
    (Bigarray.Array1.sub input_buf 0 original_size)
    (Bigarray.Array1.sub output_buf 0 original_size);
  let igot_layout = L.igot layout in
  let rela_igot_layout = L.rela_igot layout in
  let iplt_layout = L.iplt layout in
  let rela_iplt_layout = L.rela_iplt layout in
  let symtab_layout = L.symtab layout in
  let strtab_layout = L.strtab layout in
  let shstrtab_layout = L.shstrtab layout in
  let igot = Build_igot_and_iplt.igot igot_and_iplt in
  let iplt = Build_igot_and_iplt.iplt igot_and_iplt in
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:(int64_to_int (SL.offset igot_layout)))
    (int64_to_int (SL.size igot_layout))
    (Igot.section_data igot);
  let cursor =
    Buf.cursor output_buf ~at:(int64_to_int (SL.offset rela_igot_layout))
  in
  List.iter
    (fun r ->
      write_rela ~cursor ~symbol_to_index:(FRP.symbol_to_index plan)
        ~r_offset:(Igot.Relocation.offset r) ~symbol:(Igot.Relocation.symbol r)
        ~r_type:Rela.Reloc_type.r64 ~r_addend:(Igot.Relocation.addend r))
    (Igot.relocations igot);
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:(int64_to_int (SL.offset iplt_layout)))
    (int64_to_int (SL.size iplt_layout))
    (Iplt.section_data iplt);
  let cursor =
    Buf.cursor output_buf ~at:(int64_to_int (SL.offset rela_iplt_layout))
  in
  List.iter
    (fun r ->
      write_rela ~cursor ~symbol_to_index:(FRP.symbol_to_index plan)
        ~r_offset:(Iplt.Relocation.offset r) ~symbol:(Iplt.Relocation.symbol r)
        ~r_type:Rela.Reloc_type.pc32 ~r_addend:(Iplt.Relocation.addend r))
    (Iplt.relocations iplt);
  let cursor =
    Buf.cursor output_buf ~at:(int64_to_int (SL.offset symtab_layout))
  in
  let plan_strtab = FRP.strtab plan in
  Array.iter
    (fun sym -> write_symbol ~cursor ~strtab:plan_strtab sym)
    (FRP.original_symbols plan);
  List.iter
    (fun entry ->
      write_synthetic_symbol ~cursor ~strtab:plan_strtab
        ~name:(Igot.Entry.igot_symbol entry)
        ~section_index:(FRP.igot_idx plan) ~offset:(Igot.Entry.offset entry)
        ~size:Igot.entry_size ~is_func:false)
    (Igot.entries igot);
  List.iter
    (fun entry ->
      write_synthetic_symbol ~cursor ~strtab:plan_strtab
        ~name:(Iplt.Entry.iplt_symbol entry)
        ~section_index:(FRP.iplt_idx plan) ~offset:(Iplt.Entry.offset entry)
        ~size:Iplt.entry_size ~is_func:true)
    (Iplt.entries iplt);
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:(int64_to_int (SL.offset strtab_layout)))
    (int64_to_int (SL.size strtab_layout))
    (Strtab.contents plan_strtab);
  (* Write extended SYMTAB_SHNDX section if needed. This section must have the
     same number of entries as the symbol table.

     For symbols with st_shndx < SHN_LORESERVE, the SYMTAB_SHNDX entry is 0. For
     symbols with st_shndx = SHN_XINDEX, the SYMTAB_SHNDX entry contains the
     actual section index. *)
  (match FRP.symtab_shndx_idx plan, L.symtab_shndx layout with
  | _, Some symtab_shndx_layout ->
    let cursor =
      Buf.cursor output_buf ~at:(int64_to_int (SL.offset symtab_shndx_layout))
    in
    (* Helper to write a 32-bit little-endian value *)
    let write_u32_le cursor value =
      Buf.Write.u8 cursor (value land 0xff);
      Buf.Write.u8 cursor ((value lsr 8) land 0xff);
      Buf.Write.u8 cursor ((value lsr 16) land 0xff);
      Buf.Write.u8 cursor ((value lsr 24) land 0xff)
    in
    (* Copy original entries if input has SYMTAB_SHNDX, else write zeros *)
    (match FRP.symtab_shndx_idx plan with
    | Some symtab_shndx_idx ->
      let original_symtab_shndx_section = sections.(symtab_shndx_idx) in
      let original_symtab_shndx_body =
        Elf.section_body input_buf original_symtab_shndx_section
      in
      let original_size = Buf.size original_symtab_shndx_body in
      for i = 0 to original_size - 1 do
        Buf.Write.u8 cursor (Bigarray.Array1.get original_symtab_shndx_body i)
      done
    | None ->
      (* Input doesn't have SYMTAB_SHNDX; write zeros for all original symbols
         (they all have st_shndx < SHN_LORESERVE) *)
      let num_original = Array.length (FRP.original_symbols plan) in
      for _ = 1 to num_original do
        write_u32_le cursor 0
      done);
    (* Write extended section indices for IGOT symbols *)
    let igot_idx = FRP.igot_idx plan in
    let igot_shndx_entry =
      if igot_idx >= Rela.shn_loreserve then igot_idx else 0
    in
    List.iter
      (fun _ -> write_u32_le cursor igot_shndx_entry)
      (Igot.entries igot);
    (* Write extended section indices for IPLT symbols *)
    let iplt_idx = FRP.iplt_idx plan in
    let iplt_shndx_entry =
      if iplt_idx >= Rela.shn_loreserve then iplt_idx else 0
    in
    List.iter
      (fun _ -> write_u32_le cursor iplt_shndx_entry)
      (Iplt.entries iplt)
  | None, None -> ()
  | Some _, None ->
    Misc.fatal_error "SYMTAB_SHNDX in input but no layout allocated");
  (* Write rewritten .rela.text* sections back to their original locations *)
  List.iter
    (fun rewritten_section ->
      let cursor =
        Buf.cursor output_buf
          ~at:
            (int64_to_int
               (FRP.Rewritten_rela_section.section_offset rewritten_section))
      in
      List.iter
        (fun e -> Rela.write_rela_entry ~cursor e)
        (FRP.Rewritten_rela_section.entries rewritten_section))
    (FRP.rewritten_rela_sections plan);
  Buf.Write.fixed_bytes
    (Buf.cursor output_buf ~at:(int64_to_int (SL.offset shstrtab_layout)))
    (int64_to_int (SL.size shstrtab_layout))
    (Strtab.contents (FRP.shstrtab plan));
  let relocate_section (s : Elf.section) layout : Elf.section =
    { s with sh_offset = SL.offset layout; sh_size = SL.size layout }
  in
  (* Update sh_name to point to the (possibly renamed) section name in shstrtab,
     and also update sh_name_str so owee writes the correct name *)
  let section_name_offsets = FRP.section_name_offsets plan in
  let rename_section (s : Elf.section) : Elf.section =
    match String.Tbl.find_opt section_name_offsets s.sh_name_str with
    | Some (new_name_offset, renamed_str) ->
      { s with sh_name = new_name_offset; sh_name_str = renamed_str }
    | None -> s
  in
  let symtab_shndx_layout_opt = L.symtab_shndx layout in
  let update_section (s : Elf.section) =
    let s = rename_section s in
    match s.sh_name_str with
    | ".symtab" -> relocate_section s symtab_layout
    | ".strtab" -> relocate_section s strtab_layout
    | ".symtab_shndx" -> (
      match symtab_shndx_layout_opt with
      | Some symtab_shndx_layout -> relocate_section s symtab_shndx_layout
      | None -> s)
    (* .rela.text* sections are rewritten in place, so no relocation needed *)
    | _ -> s
  in
  let num_sections = FRP.num_sections plan in
  let igot_idx = FRP.igot_idx plan in
  let rela_igot_idx = FRP.rela_igot_idx plan in
  let iplt_idx = FRP.iplt_idx plan in
  let rela_iplt_idx = FRP.rela_iplt_idx plan in
  let symtab_idx = FRP.symtab_idx plan in
  (* Build new section headers *)
  let igot_section =
    Elf.make_progbits_section
      ~sh_name:(FRP.igot_name_offset plan)
      ~sh_name_str:(FRP.igot_name_str plan)
      ~sh_flags:Elf.Section_flags.(to_u64 (shf_write + shf_alloc))
      ~sh_offset:(SL.offset igot_layout) ~sh_size:(SL.size igot_layout)
      ~sh_addralign:16L
  in
  let rela_igot_section =
    Elf.make_rela_section
      ~sh_name:(FRP.rela_igot_name_offset plan)
      ~sh_name_str:(FRP.rela_igot_name_str plan)
      ~sh_offset:(SL.offset rela_igot_layout)
      ~sh_size:(SL.size rela_igot_layout) ~sh_link:symtab_idx ~sh_info:igot_idx
  in
  let iplt_section =
    Elf.make_progbits_section
      ~sh_name:(FRP.iplt_name_offset plan)
      ~sh_name_str:(FRP.iplt_name_str plan)
      ~sh_flags:Elf.Section_flags.(to_u64 (shf_execinstr + shf_alloc))
      ~sh_offset:(SL.offset iplt_layout) ~sh_size:(SL.size iplt_layout)
      ~sh_addralign:16L
  in
  let rela_iplt_section =
    Elf.make_rela_section
      ~sh_name:(FRP.rela_iplt_name_offset plan)
      ~sh_name_str:(FRP.rela_iplt_name_str plan)
      ~sh_offset:(SL.offset rela_iplt_layout)
      ~sh_size:(SL.size rela_iplt_layout) ~sh_link:symtab_idx ~sh_info:iplt_idx
  in
  let new_symtab_shndx_section =
    match
      ( FRP.new_symtab_shndx_idx plan,
        FRP.symtab_shndx_name_offset plan,
        symtab_shndx_layout_opt )
    with
    | Some new_idx, Some name_offset, Some symtab_shndx_layout ->
      let section =
        Elf.make_symtab_shndx_section ~sh_name:name_offset
          ~sh_name_str:".symtab_shndx"
          ~sh_offset:(SL.offset symtab_shndx_layout)
          ~sh_size:(SL.size symtab_shndx_layout)
          ~sh_link:symtab_idx
      in
      Some (new_idx, section)
    | None, None, _ -> None
    | _ -> Misc.fatal_error "Inconsistent new SYMTAB_SHNDX state"
  in
  (* Build section array: copy original sections with updates, then add new *)
  let new_sections = Array.make num_sections sections.(0) in
  Array.iteri (fun i s -> new_sections.(i) <- update_section s) sections;
  new_sections.(igot_idx) <- igot_section;
  new_sections.(rela_igot_idx) <- rela_igot_section;
  new_sections.(iplt_idx) <- iplt_section;
  new_sections.(rela_iplt_idx) <- rela_iplt_section;
  Option.iter
    (fun (idx, section) -> new_sections.(idx) <- section)
    new_symtab_shndx_section;
  (* Update shstrtab section with new offset and size. We use the already-
     updated section (which has renamed sh_name and sh_name_str from
     update_section) and just fix up the offset and size. *)
  let shstrtab_idx = header.Elf.e_shstrndx in
  let updated_shstrtab = new_sections.(shstrtab_idx) in
  new_sections.(shstrtab_idx)
    <- ({ updated_shstrtab with
          sh_offset = SL.offset shstrtab_layout;
          sh_size = SL.size shstrtab_layout
        }
         : Elf.section);
  let new_header : Elf.header =
    { header with
      e_shoff = L.section_headers_offset layout;
      e_shnum = num_sections
    }
  in
  Elf.write_elf output_buf new_header new_sections

(* Find all sections with names starting with prefix *)
let find_sections_with_prefix sections prefix =
  Array.to_list sections
  |> List.filter (fun (section : Elf.section) ->
         String.starts_with ~prefix section.sh_name_str)

let rewrite unix ~input_file ~output_file ~partition_kind ~igot_and_iplt
    ~relocations =
  let module Unix = (val unix : Compiler_owee.Unix_intf.S) in
  let input_buf = Buf.map_binary (module Unix) input_file in
  let header, sections = Elf.read_elf input_buf in
  let symtab_section =
    match
      Array.find_opt
        (fun (s : Elf.section) ->
          Elf.Section_type.(equal (of_u32 s.sh_type) sht_symtab))
        sections
    with
    | Some s -> s
    | None -> Misc.fatal_error "rewrite_sections: no symbol table found"
  in
  let strtab_section = sections.(symtab_section.sh_link) in
  (* Find all .rela.text* sections (handles function sections) *)
  let rela_text_section_list =
    find_sections_with_prefix sections ".rela.text"
  in
  let shstrtab_section = sections.(header.e_shstrndx) in
  let symtab_body = Elf.section_body input_buf symtab_section in
  let strtab_body = Elf.section_body input_buf strtab_section in
  (* Build list of (section, body) pairs *)
  let rela_text_sections =
    List.map
      (fun section -> section, Elf.section_body input_buf section)
      rela_text_section_list
  in
  let plan =
    FRP.compute ~header ~sections ~symtab_body ~strtab_body ~rela_text_sections
      ~partition_kind ~igot_and_iplt ~relocations
  in
  execute_plan unix ~input_file ~output_file ~header ~sections ~shstrtab_section
    ~igot_and_iplt ~plan
