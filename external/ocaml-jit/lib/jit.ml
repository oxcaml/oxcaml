(* Copyright (c) 2021 Nathan Rebours <nathan.p.rebours@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Import

let outcome_global : Opttoploop.evaluation_outcome option ref = ref None

(** Assemble each section using X86_binary_emitter. Empty sections are filtered *)
let binary_section_map ~arch section_map =
  String.Map.filter_map section_map ~f:(fun name instructions ->
      let binary_section = X86_section.assemble ~arch { name; instructions } in
      if X86_binary_emitter.size binary_section = 0 then None
      else Some binary_section)

let extract_text_section binary_section_map =
  let name = Jit_text_section.name in
  match String.Map.find_opt name binary_section_map with
  | None -> failwithf "No text section in generated assembler"
  | Some binary_section ->
      let text = Jit_text_section.from_binary_section binary_section in
      let binary_section_map = String.Map.remove name binary_section_map in
      (binary_section_map, text)

let pagesize = Externals.get_page_size ()

let round_to_pages section_size =
  if section_size = 0 then 0
  else
    let pages = ((section_size - 1) / pagesize) + 1 in
    pages * pagesize

let alloc_all jit_text_section binary_section_map =
  (* Allocate all sections contiguously (modulo rounding up to page boundaries)
     to minimize the chance of relocation overflow. *)
  let text_size =
    round_to_pages (Jit_text_section.in_memory_size jit_text_section)
  in
  let total_size =
    String.Map.fold binary_section_map ~init:text_size
      ~f:(fun ~key:_ ~data:binary_section total_size ->
        let size = round_to_pages (X86_binary_emitter.size binary_section) in
        size + total_size)
  in
  match Externals.memalign total_size with
  | Error msg ->
    failwithf "posix_memalign for %d bytes failed: %s" total_size msg
  | Ok address ->
    let text = { address; value = jit_text_section } in
    let address = Address.add_int address text_size in
    let map, _address =
      String.Map.fold binary_section_map
        ~init:(String.Map.empty, address)
        ~f:(fun ~key ~data:binary_section (map, address) ->
          let data = { address; value = binary_section } in
          let map = String.Map.add map ~key ~data in
          let size = round_to_pages (X86_binary_emitter.size binary_section) in
          let address = Address.add_int address size in
          map, address)
    in
    text, map

let local_symbol_map binary_section_map =
  String.Map.fold binary_section_map ~init:Symbols.empty
    ~f:(fun ~key:_ ~data all_symbols ->
      let section_symbols = Symbols.from_binary_section data in
      Symbols.strict_union section_symbols all_symbols)

let relocate_text ~symbols text_section =
  match Jit_text_section.relocate ~symbols text_section with
  | Ok text -> text
  | Error msgs ->
      failwithf "Failed to apply relocations to section %s properly:\n - %s"
        Jit_text_section.name
        (String.concat ~sep:"\n- " msgs)

let relocate_other ~symbols addressed_sections =
  String.Map.iter addressed_sections
    ~f:(fun ~key:section_name ~data:binary_section ->
      match Relocate.all ~symbols ~section_name binary_section with
      | Ok () -> ()
      | Error msgs ->
          failwithf "Failed to apply relocations to section %s properly:\n - %s"
            section_name
            (String.concat ~sep:"\n- " msgs))

let is_ro name = String.starts_with ~prefix:".rodata" name

let set_protection ~mprotect ~name address size =
  match mprotect address size with
  | Ok () -> ()
  | Error code ->
      failwithf "mprotect failed with code %d for section %s" code name

let load_text { address; value = text_section } =
  let size = Jit_text_section.in_memory_size text_section in
  let content = Jit_text_section.content text_section in
  Externals.load_section address content size;
  set_protection ~mprotect:Externals.mprotect_rx ~name:Jit_text_section.name
    address size

let load_sections addressed_sections =
  String.Map.iter addressed_sections
    ~f:(fun ~key:name ~data:{ address; value = binary_section } ->
      let size = X86_binary_emitter.size binary_section in
      let content = X86_binary_emitter.contents binary_section in
      Externals.load_section address content size;
      if is_ro name then
        set_protection ~mprotect:Externals.mprotect_ro ~name address size)

let entry_points ~phrase_name symbols =
  let separator = (* if Config.runtime5 then "." else *) "__" in
  let symbol_name name = Printf.sprintf "caml%s%s%s" phrase_name separator name in
  let find_symbol name = Symbols.find symbols (symbol_name name) in
  let frametable = find_symbol "frametable" in
  let gc_roots = find_symbol "gc_roots" in
  let data_begin = find_symbol "data_begin" in
  let data_end = find_symbol "data_end" in
  let code_begin = find_symbol "code_begin" in
  let code_end = find_symbol "code_end" in
  let entry_name = symbol_name "entry" in
  match Symbols.find symbols entry_name with
  | Some entry ->
      let open Jit_unit.Entry_points in
      {
        frametable;
        gc_roots;
        data_begin;
        data_end;
        code_begin;
        code_end;
        entry;
      }
  | None ->
      failwithf "Toplevel phrase entry point symbol %s is not defined"
        entry_name

let jit_run entry_points =
  let open Opttoploop in
  match
    try Result (Obj.magic (Externals.run_toplevel entry_points))
    with exn -> Exception exn
  with
  | Exception _ as r -> r
  | Result r -> (
      match (Obj.magic r : Toplevel_res.t) with
      | Ok x -> Result x
      | Err s -> failwithf "Jit.run: %s" s)

let get_arch () =
  (* TODO: use target arch *)
  match Sys.word_size with
  | 32 -> X86_ast.X86
  | 64 -> X86_ast.X64
  | i -> failwithf "Unexpected word size: %d" i 16

let jit_load_x86 ~outcome_ref ~delayed:_ section_map _filename =
  let arch = get_arch () in
  let section_map =
   List.fold_left
      ~f:(fun section_map (name, instrs) ->
        String.Map.add section_map
          ~key:(X86_proc.Section_name.to_string name)
          ~data:instrs)
      section_map
      ~init:String.Map.empty
  in
  let binary_section_map = binary_section_map ~arch section_map in
  Debug.print_binary_section_map binary_section_map;
  let other_sections, text = extract_text_section binary_section_map in
  let addressed_text, addressed_sections = alloc_all text other_sections in
  let other_sections_symbols = local_symbol_map addressed_sections in
  let text_section_symbols = Jit_text_section.symbols addressed_text in
  let local_symbols =
    Symbols.strict_union other_sections_symbols text_section_symbols
  in
  let symbols =
    Symbols.aggregate ~current:!Globals.symbols ~new_symbols:local_symbols
  in
  Globals.symbols := symbols;
  let relocated_text = relocate_text ~symbols addressed_text in
  relocate_other ~symbols addressed_sections;
  Debug.save_binary_sections ~phrase_name:!Opttoploop.phrase_name
    addressed_sections;
  Debug.save_text_section ~phrase_name:!Opttoploop.phrase_name relocated_text;
  load_text relocated_text;
  load_sections addressed_sections;
  let entry_points =
    entry_points ~phrase_name:!Opttoploop.phrase_name symbols
  in
  let result = jit_run entry_points in
  outcome_ref := Some result

let set_debug () =
  match Sys.getenv_opt "OCAML_JIT_DEBUG" with
  | Some ("true" | "1") -> Globals.debug := true
  | None | Some _ -> Globals.debug := false

let with_jit_x86 f =
  let ias = !X86_proc.internal_assembler in
  X86_proc.register_internal_assembler
    (jit_load_x86 ~outcome_ref:outcome_global);
  try
    let res = f () in
    X86_proc.internal_assembler := ias;
    res
  with exn ->
    X86_proc.internal_assembler := ias;
    raise exn

let jit_load_body ppf (program : Lambda.program) =
  let open Config in
  let open Opttoploop in
  let dll =
    if !Clflags.keep_asm_file then !phrase_name ^ ext_dll
    else Filename.temp_file ("caml" ^ !phrase_name) ext_dll
  in
  let filename = Filename.chop_extension dll in
  Arch.trap_notes := false;
  (* TODO: Determine machine_width properly from target configuration *)
  let machine_width = Target_system.Machine_width.Sixty_four in
  let pipeline : Asmgen.pipeline =
    Direct_to_cmm
      (Flambda2.lambda_to_cmm ~machine_width ~keep_symbol_tables:true)
  in
    Asmgen.compile_implementation
      (module Unix : Compiler_owee.Unix_intf.S)
      ~toplevel:need_symbol ~pipeline ~sourcefile:(Some filename)
      ~prefixname:filename ~ppf_dump:ppf program;
  match !outcome_global with
  | None -> failwith "No evaluation outcome"
  | Some res ->
      outcome_global := None;
      res

let jit_load ppf program = with_jit_x86 (fun () -> jit_load_body ppf program)

let jit_lookup_symbol symbol =
  match Symbols.find !Globals.symbols symbol with
  | None -> Opttoploop.default_lookup symbol
  | Some x -> Some (Address.to_obj x)

let init_top () =
  set_debug ();
  Opttoploop.register_jit
    { Opttoploop.Jit.load = jit_load; lookup_symbol = jit_lookup_symbol }
