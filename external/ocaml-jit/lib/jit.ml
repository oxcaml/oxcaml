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

let alloc_memory binary_section =
  let size = round_to_pages (X86_binary_emitter.size binary_section) in
  match Externals.memalign size with
  | Ok address -> address
  | Error msg -> failwithf "posix_memalign failed: %s" msg

let alloc_all binary_section_map =
  String.Map.map binary_section_map ~f:(fun binary_section ->
      let address = alloc_memory binary_section in
      { address; value = binary_section })

let alloc_text jit_text_section =
  let size =
    round_to_pages (Jit_text_section.in_memory_size jit_text_section)
  in
  match Externals.memalign size with
  | Ok address -> { address; value = jit_text_section }
  | Error msg -> failwithf "posix_memalign failed: %s" msg

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
  let symbol_name name = Printf.sprintf "caml%s__%s" phrase_name name in
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

let jit_load_x86 ~outcome_ref asm_program _filename =
  Debug.print_ast asm_program;
  let section_map = X86_section.Map.from_program asm_program in
  let arch = get_arch () in
  let binary_section_map = binary_section_map ~arch section_map in
  Debug.print_binary_section_map binary_section_map;
  let other_sections, text = extract_text_section binary_section_map in
  let addressed_sections = alloc_all other_sections in
  let addressed_text = alloc_text text in
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

(* Copied from opttoploop.ml *)
module Backend = struct
  let symbol_for_global' = Compilenv.symbol_for_global'

  let closure_symbol = Compilenv.closure_symbol

  let really_import_approx = Import_approx.really_import_approx

  let import_symbol = Import_approx.import_symbol

  let size_int = Arch.size_int

  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end

let backend = (module Backend : Backend_intf.S)

let jit_load_body ppf (program : Lambda.program) =
  let open Config in
  let open Opttoploop in
  let dll =
    if !Clflags.keep_asm_file then !phrase_name ^ ext_dll
    else Filename.temp_file ("caml" ^ !phrase_name) ext_dll
  in
  let filename = Filename.chop_extension dll in
  (if Config.flambda2 then
    Asmgen.compile_implementation_flambda2 () ~toplevel:need_symbol
      ~filename ~prefixname:filename
      ~flambda2:Flambda2.lambda_to_cmm ~ppf_dump:ppf
      ~size:program.main_module_block_size
      ~module_ident:program.module_ident
      ~module_initializer:program.code
      ~required_globals:program.required_globals
  else
    let middle_end =
      if Config.flambda then Flambda_middle_end.lambda_to_clambda
      else Closure_middle_end.lambda_to_clambda
    in
    Asmgen.compile_implementation ~toplevel:need_symbol ~backend ~filename
      ~prefixname:filename ~middle_end ~ppf_dump:ppf program);
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
