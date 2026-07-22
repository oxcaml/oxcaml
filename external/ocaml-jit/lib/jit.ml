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

type evaluation_outcome = Result of Obj.t | Exception of exn

let outcome_global : evaluation_outcome option ref = ref None

external ndl_loadsym : string -> Obj.t
  = "caml_sys_exit" "caml_natdynlink_loadsym"

external ndl_existssym : string -> bool
  = "caml_sys_exit" "caml_natdynlink_existssym"
[@@noalloc]

let pagesize = Externals.get_page_size ()

let round_to_pages section_size =
  if section_size = 0 then 0
  else
    let pages = ((section_size - 1) / pagesize) + 1 in
    pages * pagesize

let is_ro name = String.starts_with ~prefix:".rodata" name

let set_protection ~mprotect ~name address size =
  match mprotect address size with
  | Ok () -> ()
  | Error code ->
      failwithf "mprotect failed with code %d for section %s" code name

(** Extract text section from a map of binary sections *)
let extract_text_section (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r) (binary_section_map : a String.Map.t) =
  let name = Jit_text_section.name in
  match String.Map.find_opt name binary_section_map with
  | None -> failwithf "No text section in generated assembler"
  | Some binary_section ->
      let text =
        Jit_text_section.from_binary_section (module E) binary_section
      in
      let binary_section_map = String.Map.remove name binary_section_map in
      (binary_section_map, text)

(** Allocate memory for all sections *)
let alloc_all (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r) jit_text_section
    (binary_section_map : a String.Map.t) =
  let text_size =
    round_to_pages (Jit_text_section.in_memory_size (module E) jit_text_section)
  in
  let total_size =
    String.Map.fold binary_section_map ~init:text_size
      ~f:(fun ~key:_ ~data:binary_section total_size ->
        let size = round_to_pages (E.Assembled_section.size binary_section) in
        size + total_size)
  in
  match Externals.memalign total_size with
  | Error msg ->
      failwithf "posix_memalign for %d bytes failed: %s" total_size msg
  | Ok base_address ->
      let text = { address = base_address; value = jit_text_section } in
      let address = Address.add_int base_address text_size in
      let map, _address =
        String.Map.fold binary_section_map ~init:(String.Map.empty, address)
          ~f:(fun ~key ~data:binary_section (map, address) ->
            let data = { address; value = binary_section } in
            let map = String.Map.add map ~key ~data in
            let size =
              round_to_pages (E.Assembled_section.size binary_section)
            in
            let address = Address.add_int address size in
            (map, address))
      in
      (text, map, base_address, total_size)

(** Build symbol map from non-text sections *)
let local_symbol_map (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r)
    (binary_section_map : a addressed String.Map.t) =
  String.Map.fold binary_section_map ~init:Symbols.empty
    ~f:(fun ~key:_ ~data all_symbols ->
      let section_symbols = Symbols.from_binary_section (module E) data in
      Symbols.strict_union section_symbols all_symbols)

(** Relocate text section *)
let relocate_text (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r) ~symbols text_section =
  match Jit_text_section.relocate (module E) ~symbols text_section with
  | Ok text -> text
  | Error msgs ->
      failwithf "Failed to apply relocations to section %s properly:\n - %s"
        Jit_text_section.name
        (String.concat ~sep:"\n- " msgs)

(** Relocate non-text sections *)
let relocate_other (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r) ~symbols addressed_sections =
  String.Map.iter addressed_sections
    ~f:(fun ~key:section_name ~data:binary_section ->
      match
        Relocate.all
          (module E)
          ~symbols ~got_lookup:None ~plt_lookup:None ~section_name
          binary_section
      with
      | Ok () -> ()
      | Error msgs ->
          failwithf "Failed to apply relocations to section %s properly:\n - %s"
            section_name
            (String.concat ~sep:"\n- " msgs))

(** Load text section into memory *)
let load_text (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r) { address; value = text_section } =
  let size = Jit_text_section.in_memory_size (module E) text_section in
  let content = Jit_text_section.content (module E) text_section in
  Externals.load_section address content size;
  set_protection ~mprotect:Externals.mprotect_rx ~name:Jit_text_section.name
    address size

(** Load non-text sections into memory *)
let load_sections (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r) addressed_sections =
  String.Map.iter addressed_sections
    ~f:(fun ~key:name ~data:{ address; value = binary_section } ->
      let size = E.Assembled_section.size binary_section in
      let content = E.Assembled_section.contents binary_section in
      Externals.load_section address content size;
      if is_ro name then
        set_protection ~mprotect:Externals.mprotect_ro ~name address size)

let symbol_prefix () =
  match Target_system.derived_system () with
  | MacOS_like -> "_"
  | Linux | Win32 | Win64 | MinGW_32 | MinGW_64 | Cygwin | FreeBSD | NetBSD
  | OpenBSD | Generic_BSD | Solaris | BeOS | GNU | Dragonfly | Unknown ->
      ""

let entry_points ~phrase_name symbols =
  let separator = "__" in
  let prefix = symbol_prefix () in
  let symbol_name name =
    Printf.sprintf "%scaml%s%s%s" prefix phrase_name separator name
  in
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
  match
    try Result (Obj.magic (Externals.run_toplevel entry_points))
    with exn -> Exception exn
  with
  | Exception _ as r -> r
  | Result r -> (
      match (Obj.magic r : Toplevel_res.t) with
      | Ok x -> Result x
      | Err s -> failwithf "Jit.run: %s" s)

(** Load and run assembled binary sections. This is the main generic JIT entry
    point that works with any architecture. *)
let jit_load (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r) ~phrase_name ~outcome_ref
    (binary_section_map : a String.Map.t) =
  Debug.print_binary_section_map (module E) binary_section_map;
  let other_sections, text =
    extract_text_section (module E) binary_section_map
  in
  let addressed_text, addressed_sections, buffer_base, buffer_size =
    alloc_all (module E) text other_sections
  in
  let other_sections_symbols = local_symbol_map (module E) addressed_sections in
  let text_section_symbols =
    Jit_text_section.symbols (module E) addressed_text
  in
  let local_symbols =
    Symbols.strict_union other_sections_symbols text_section_symbols
  in
  let symbols =
    Symbols.aggregate ~current:!Globals.symbols ~new_symbols:local_symbols
  in
  Globals.symbols := symbols;
  if !Globals.debug then (
    Printf.printf "\n=== JIT Sections ===\n";
    String.Map.iter addressed_sections
      ~f:(fun ~key:name ~data:{ address; value } ->
        Printf.printf "Section %s at %Lx (size=%d)\n" name
          (Address.to_int64 address)
          (E.Assembled_section.size value));
    Printf.printf "=== End JIT Sections ===\n\n";
    Printf.printf "=== JIT Symbols ===\n";
    Symbols.dprint local_symbols;
    Printf.printf "=== End JIT Symbols ===\n\n";
    Printf.printf "=== JIT Relocations ===\n";
    String.Map.iter addressed_sections
      ~f:(fun ~key:name ~data:{ address; value } ->
        let relocs = E.Assembled_section.relocations value in
        Printf.printf "Section %s at %Lx:\n" name (Address.to_int64 address);
        List.iter
          (fun r ->
            Printf.printf "  offset=%d sym=%s\n"
              (E.Relocation.offset_from_section_beginning r)
              (Symbols.target_to_string (E.Relocation.target_symbol r)))
          relocs);
    Printf.printf "=== End JIT Relocations ===\n\n%!");
  let relocated_text = relocate_text (module E) ~symbols addressed_text in
  relocate_other (module E) ~symbols addressed_sections;
  Debug.save_binary_sections (module E) ~phrase_name addressed_sections;
  Debug.save_text_section (module E) ~phrase_name relocated_text;
  load_text (module E) relocated_text;
  load_sections (module E) addressed_sections;
  let entry_points = entry_points ~phrase_name symbols in
  (* If the compilation unit emitted an [unloadable_code_blocks] sentinel
     symbol, register the unit with the runtime so the GC can detect
     when the unit becomes unreachable and unload its text/data buffers.
     Both the code-blocks and data-blocks sentinels are looked up by
     exact linker name (constructed from [phrase_name]) — no symbol-table
     scan by suffix. *)
  let entry_points =
    let prefix = symbol_prefix () in
    let separator = "__" in
    let sentinel_name basename =
      Printf.sprintf "%scaml%s%s%s" prefix phrase_name separator basename
    in
    let sentinel basename =
      match Symbols.find local_symbols (sentinel_name basename) with
      | Some addr -> Address.to_nativeint addr
      | None -> 0n
    in
    let code_blocks_table_addr = sentinel "unloadable_code_blocks" in
    if Nativeint.equal code_blocks_table_addr 0n
    then entry_points
    else
      let data_blocks_table_addr = sentinel "unloadable_data_blocks" in
      let code_end_addr =
        match entry_points.code_end with
        | Some addr -> Address.to_nativeint addr
        | None -> failwithf "code_end missing for unloadable unit"
      in
      let frametable_addr =
        match entry_points.frametable with
        | Some addr -> Address.to_nativeint addr
        | None -> 0n
      in
      let gc_roots_addr =
        match entry_points.gc_roots with
        | Some addr -> Address.to_nativeint addr
        | None -> 0n
      in
      (* Registration window safety: between this call and [jit_run],
         a major GC on another domain might scan via gc_roots, the
         frametable, or registered code fragments — all of which are
         now visible. The unit's static blocks are born-marked
         (white -> MARKED via [normalize_block_color]) and the
         entry's Code_block has zero dep fields, so a scan in this
         window cannot dereference a not-yet-populated field. F.2
         return-address scans cannot land in our text range because
         no thread has yet entered this unit's code. *)
      Externals.register_unloadable_unit code_blocks_table_addr
        data_blocks_table_addr code_end_addr frametable_addr gc_roots_addr
        (Address.to_nativeint buffer_base)
        buffer_size;
      (* The unloadable-unit registration owns the frame table, gc_roots
         and code-fragment registrations — drop them from [entry_points]
         so [jit_run] (the legacy non-unloadable path) does not also
         register them. A duplicate gc_roots in particular is fatal
         (caml_register_dyn_global raises [Register_dyn_global_duplicate])
         when a freed unit's buffer address gets reused by a later unit.
         [data_begin]/[data_end] go to [caml_page_table_add] under
         runtime 4 only, so leaving them is harmless on runtime 5. *)
      { entry_points with
        frametable = None;
        gc_roots = None;
        code_begin = None;
        code_end = None;
      }
  in
  let result = jit_run entry_points in
  outcome_ref := Some result

(* JIT callback that handles architecture-agnostic packed sections *)
let jit_callback ~phrase_name ~outcome_ref packed =
  let (Jit_backend.Packed { emitter; sections }) = packed in
  (* Convert String_map to our String.Map *)
  let binary_section_map =
    Jit_backend.String_map.fold
      (fun name section map -> String.Map.add map ~key:name ~data:section)
      sections String.Map.empty
  in
  jit_load emitter ~phrase_name ~outcome_ref binary_section_map

let set_debug () =
  match Sys.getenv_opt "OCAML_JIT_DEBUG" with
  | Some ("true" | "1") -> Globals.debug := true
  | None | Some _ -> Globals.debug := false

let need_symbol sym =
  match Symbols.find !Globals.symbols sym with
  | Some _ -> false
  | None -> not (ndl_existssym sym)

let jit_load_lambda ~phrase_name ppf (program : Lambda.program) =
  let open Config in
  let dll =
    if !Clflags.keep_asm_file then phrase_name ^ ext_dll
    else Filename.temp_file ("caml" ^ phrase_name) ext_dll
  in
  let filename = Filename.chop_extension dll in
  Arch.trap_notes := false;
  (* TODO: Determine machine_width properly from target configuration *)
  let machine_width = Target_system.Machine_width.Sixty_four in
  let pipeline : Asmgen.pipeline =
    Direct_to_cmm
      (Flambda2.lambda_to_cmm ~machine_width ~keep_symbol_tables:true)
  in
  (* Mark this CU as unloadable: the JIT-emitted code/data is transient and
     the runtime should be free to reclaim it once the GC determines the
     unit is unreachable. This drives:
       - white (UNMARKED) headers on static data (B.1)
       - the [is_unloadable] bit on closinfo words (A.2)
       - [FRAME_DESCRIPTOR_UNLOADABLE] on frame descriptors (A.3)
       - emission of [Code_block] static items (C)
       - per-function back-pointers in [.text] (D)
     We save and restore so any non-JIT compile after this in the same
     process is unaffected.

     Disabled when [Externals.supports_unloading] is false (Linux ASan,
     Linux TCMalloc, musl): the buffer allocator on those builds returns
     memory that cannot be passed to [free], so the unload path would
     corrupt the allocator. Instead we compile the CU as non-unloadable —
     black headers, no Code_blocks, no UNLOADABLE bits — and leak the
     buffer for the life of the process. The downside is an unbounded
     leak, but it lets [Eval.eval] keep working in those configurations.
     [unloadable_metadata] then returns [None], so [register_unloadable_
     unit] is also skipped. *)
  let saved_unit_is_unloadable = !Clflags.unit_is_unloadable in
  Clflags.unit_is_unloadable := Externals.supports_unloading ();
  let restore () =
    Clflags.unit_is_unloadable := saved_unit_is_unloadable
  in
  (match
     Asmgen.compile_implementation
       (module Unix : Compiler_owee.Unix_intf.S)
       ~toplevel:need_symbol ~pipeline ~sourcefile:(Some filename)
       ~prefixname:filename ~ppf_dump:ppf program
   with
  | () -> restore ()
  | exception exn ->
      restore ();
      raise exn);
  match !outcome_global with
  | None -> failwith "No evaluation outcome"
  | Some res ->
      outcome_global := None;
      res

let jit_load_program ~phrase_name ppf program =
  set_debug ();
  Jit_backend.register (jit_callback ~phrase_name ~outcome_ref:outcome_global);
  match jit_load_lambda ~phrase_name ppf program with
  | result ->
      Jit_backend.unregister ();
      result
  | exception exn ->
      Jit_backend.unregister ();
      raise exn

let jit_load_program_top ~phrase_name ppf program :
    Opttoploop.evaluation_outcome =
  match jit_load_program ~phrase_name ppf program with
  | Result obj -> Result obj
  | Exception exn -> Exception exn

let jit_lookup_symbol symbol =
  (* Try with symbol prefix first (e.g., "_" on macOS) *)
  let prefixed_symbol = symbol_prefix () ^ symbol in
  match Symbols.find !Globals.symbols prefixed_symbol with
  | Some x -> Some (Address.to_obj x)
  | None -> (
      (* Fall back to unprefixed symbol (for external/runtime symbols) *)
      match Symbols.find !Globals.symbols symbol with
      | Some x -> Some (Address.to_obj x)
      | None -> (
          match ndl_loadsym symbol with exception _ -> None | obj -> Some obj))

let init_top () =
  set_debug ();
  Opttoploop.register_jit
    {
      Opttoploop.Jit.load = jit_load_program_top;
      lookup_symbol = jit_lookup_symbol;
    }
