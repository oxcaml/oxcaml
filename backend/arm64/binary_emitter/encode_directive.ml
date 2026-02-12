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

(* CR mshinwell: This file has not yet been code reviewed *)

module Asm_section = Asm_targets.Asm_section
module Asm_label = Asm_targets.Asm_label
module Asm_symbol = Asm_targets.Asm_symbol
module D = Asm_targets.Asm_directives
module C = D.Directive.Constant
module SS = Section_state
module Symbol = Arm64_ast.Ast.Symbol

let rec extract_symbol_name (cst : C.t) =
  match cst with
  | Label lbl -> Some (Asm_label.encode lbl)
  | Symbol sym -> Some (Asm_symbol.encode sym)
  | Variable name -> Some name
  | Add (a, _) | Sub (a, _) -> extract_symbol_name a
  | Signed_int _ | Unsigned_int _ | This -> None

(* Extract (target, offset) from expressions like (Label - This) + offset *)
let extract_target_this_offset (cst : C.t) : (Symbol.target * int64) option =
  match[@warning "-4"] cst with
  | Add (Sub (Label lbl, This), Signed_int offset) ->
    Some (Symbol.Label lbl, offset)
  | Add (Sub (Symbol sym, This), Signed_int offset) ->
    Some (Symbol.Symbol sym, offset)
  | Sub (Label lbl, This) -> Some (Symbol.Label lbl, 0L)
  | Sub (Symbol sym, This) -> Some (Symbol.Symbol sym, 0L)
  | _ -> None

let rec eval_constant state ~all_sections const =
  (* For same-section relative expressions like (Label - This), the offset
     within the section is all that matters. Cross-section references are
     detected and handled via relocations before this function is called. *)
  let this () = Int64.of_int (SS.offset_in_bytes state) in
  let lookup_label lbl =
    (* First try current section, then fall back to global lookup. *)
    match SS.find_label_offset_in_bytes state lbl with
    | Some offset -> Some (Int64.of_int offset)
    | None -> (
      match
        All_section_states.find_in_any_section all_sections (Symbol.Label lbl)
      with
      | Some (offset, _section) -> Some (Int64.of_int offset)
      | None -> None)
  in
  let lookup_symbol sym =
    (* Note: We do NOT suppress global symbols here even in PIC mode (dlcode).
       This lookup is used for evaluating constant expressions like "symbol -
       label" which compute relative offsets at assembly time. The dlcode check
       for absolute symbol references is handled separately in
       is_absolute_symbol_reference. *)
    match SS.find_symbol_offset_in_bytes state sym with
    | Some offset -> Some (Int64.of_int offset)
    | None -> (
      match
        All_section_states.find_in_any_section all_sections (Symbol.Symbol sym)
      with
      | Some (offset, _section) -> Some (Int64.of_int offset)
      | None -> None)
  in
  let lookup_variable name =
    (* Check if this is a direct assignment (e.g., temp0 from .set) *)
    match All_section_states.find_direct_assignment all_sections name with
    | Some expr ->
      (* Recursively evaluate the assigned expression *)
      eval_constant state ~all_sections expr
    | None -> None
  in
  C.eval ~this ~lookup_label ~lookup_symbol ~lookup_variable const

(* Handle cross-section (Label - This) + offset pattern. This occurs in
   frametable entries where a DATA section location references a TEXT section
   label (return address), or when DATA references read-only data sections like
   .rodata.cst8. On ELF with function sections, we emit R_AARCH64_PREL32 with
   section name and addend. On macOS, we emit PREL32_PAIR using symbol pairs. *)
let is_cross_section_relative_reference state ~all_sections ~current_section c =
  match extract_target_this_offset c with
  | None -> None
  | Some (target, offset_upper) -> (
    (* First check if target is in current section *)
    match SS.find_target_offset_in_bytes state target with
    | Some _ -> None (* Same section, use normal eval *)
    | None -> (
      let macosx = String.equal Config.system "macosx" in
      let for_jit = All_section_states.for_jit all_sections in
      (* On ELF (not JIT) with function sections, check individual sections
         first. The assembler emits R_AARCH64_PREL32 with section symbol and
         addend. For JIT mode, we aggregate sections so can't use section
         symbols. *)
      if
        (not macosx) && (not for_jit)
        &&
        match
          All_section_states.find_in_any_individual_section_with_state
            all_sections target
        with
        | Some (target_offset, section, _target_state) ->
          if not (Asm_section.equal current_section Asm_section.Data)
          then
            Misc.fatal_errorf
              "Cross-section (Label - This) from non-DATA section %s to %s not \
               supported"
              (Asm_section.to_string current_section)
              (Asm_section.to_string section)
          else
            (* ELF with function sections: use R_AARCH64_PREL32 with section
               symbol and addend. The addend is the offset of the label within
               the section plus any user-specified offset. *)
            let addend = target_offset + Int64.to_int offset_upper in
            SS.add_relocation_at_current_offset state
              ~reloc_kind:(R_AARCH64_PREL32 { section; addend });
            true
        | None -> false
      then Some 0L (* ELF RELA: addend in relocation, emit 0 in data *)
      else
        (* Try cross-section lookup for standard sections. Use
           find_in_any_section_with_state to get the actual state where the
           target was found. *)
        match
          All_section_states.find_in_any_section_with_state all_sections target
        with
        | None -> None (* Not found at all *)
        | Some (target_offset, label_section, _target_state) -> (
          if Asm_section.equal label_section current_section
          then None (* Same section after all *)
          else if not (Asm_section.equal current_section Asm_section.Data)
          then
            Misc.fatal_errorf
              "Cross-section (Label - This) from non-DATA section %s to %s not \
               supported"
              (Asm_section.to_string current_section)
              (Asm_section.to_string label_section)
          else if (not macosx) && not for_jit
          then (
            (* ELF without function sections: use R_AARCH64_PREL32 with standard
               section symbol. The assembler uses section symbols for
               cross-section references. *)
            let addend = target_offset + Int64.to_int offset_upper in
            SS.add_relocation_at_current_offset state
              ~reloc_kind:(R_AARCH64_PREL32 { section = label_section; addend });
            Some 0L (* ELF RELA: addend in relocation, emit 0 in data *))
          else
            (* macOS or JIT: use symbol pairs (SUBTRACTOR + UNSIGNED). The
               linker computes: plus_sym - minus_sym + addend So: addend =
               (target - plus_sym) - (current - minus_sym) *)
            let current_pos = SS.offset_in_bytes state in
            (* Find nearest symbol in DATA for SUBTRACTOR *)
            match SS.find_nearest_symbol_before state current_pos with
            | None ->
              Misc.fatal_error
                "No symbol in DATA section for cross-section relocation"
            | Some (minus_symbol, minus_sym_offset) -> (
              (* Find nearest symbol in target section for UNSIGNED. Note: when
                 using find_in_any_section_with_state, we get the target offset
                 within the actual section state. *)
              let target_state_for_lookup =
                All_section_states.find_exn all_sections label_section
              in
              match
                SS.find_nearest_symbol_before target_state_for_lookup
                  target_offset
              with
              | None ->
                Misc.fatal_errorf
                  "No symbol in %s section for cross-section relocation"
                  (Asm_section.to_string label_section)
              | Some (plus_sym, plus_sym_offset) ->
                let addend =
                  Int64.add offset_upper
                    (Int64.sub
                       (Int64.of_int (target_offset - plus_sym_offset))
                       (Int64.of_int (current_pos - minus_sym_offset)))
                in
                (* find_nearest_symbol_before returns Asm_symbol.t directly *)
                let plus_target : Symbol.target = Symbol plus_sym in
                let minus_target : Symbol.target = Symbol minus_symbol in
                SS.add_relocation_at_current_offset state
                  ~reloc_kind:
                    (R_AARCH64_PREL32_PAIR { plus_target; minus_target });
                (* On macOS (Mach-O), the addend must be in the data. *)
                Some addend))))

(* Returns true if we're on a RELA platform (Linux ELF) where addends are stored
   in the relocation entry rather than in the instruction/data. On REL platforms
   (macOS Mach-O), addends are encoded in the instruction. *)
let is_rela_platform () =
  match Config.system with
  | "linux" | "linux_eabi" | "linux_eabihf" | "freebsd" | "netbsd" | "openbsd"
    ->
    true
  | "macosx" | "darwin" -> false
  | _ -> false (* Default to REL behavior for unknown systems *)

(* Encode a Symbol.target to its string representation *)
let encode_target (target : Symbol.target) : string =
  match target with
  | Label lbl -> Asm_label.encode lbl
  | Symbol sym -> Asm_symbol.encode sym

(* Check if a target is a global symbol in the given state. Only symbols
   explicitly marked as global (via Global or Weak directives) return true.
   File-scope symbols defined only via New_label return false. *)
let is_global_in_state state (target : Symbol.target) : bool =
  match target with Symbol sym -> SS.is_global state sym | Label _ -> false

(* For verification against the assembler, local symbols need to be converted:

   - On Linux ELF: local symbols (.L prefix) don't have symbol table entries,
   assembler converts them to section symbol + offset

   - On macOS: local symbols (L prefix) don't have symbol table entries,
   assembler converts them to nearest global symbol + offset

   Returns (symbol_name, addend) where symbol_name is the converted symbol name
   and addend includes any necessary offset adjustments. *)
let resolve_local_label_for_elf ~all_sections ~target ~sym_offset =
  let sym_name = encode_target target in
  if is_rela_platform ()
  then
    (* Linux ELF: check if this is a local/file-scope symbol that needs
       conversion to section + offset *)
    let is_local_label =
      String.length sym_name >= 2
      && Char.equal sym_name.[0] '.'
      && Char.equal sym_name.[1] 'L'
    in
    (* Try to find the symbol in individual sections first (function
       sections) *)
    let try_individual_sections () =
      match
        All_section_states.find_in_any_individual_section_with_state
          all_sections target
      with
      | Some (label_offset, section, target_state) ->
        (* Check if this is a global symbol in the target section *)
        let is_global_in_target = is_global_in_state target_state target in
        if is_local_label || not is_global_in_target
        then
          let section_name = Asm_section.to_string section in
          Some (section_name, label_offset + sym_offset)
        else None
      | None -> None
    in
    (* Try to find in standard sections *)
    let try_standard_sections () =
      match
        All_section_states.find_in_any_section_with_state all_sections target
      with
      | Some (label_offset, section, target_state) ->
        let section_name = Asm_section.to_string section in
        let is_global_in_target = is_global_in_state target_state target in
        if is_local_label || not is_global_in_target
        then Some (section_name, label_offset + sym_offset)
        else None
      | None -> None
    in
    match try_individual_sections () with
    | Some result -> result
    | None -> (
      match try_standard_sections () with
      | Some result -> result
      | None ->
        (* Symbol not found or is global - use original name *)
        sym_name, sym_offset)
  else
    (* macOS: check if this is a local label (L prefix) that needs conversion to
       nearest global symbol + offset *)
    let is_local_label =
      String.length sym_name >= 1 && Char.equal sym_name.[0] 'L'
    in
    if not is_local_label
    then (* Global symbol or external - use directly *)
      sym_name, sym_offset
    else
      (* Find the target's offset and section *)
      match
        All_section_states.find_in_any_section_with_state all_sections target
      with
      | None ->
        (* Target not found - use original name (will be external reference) *)
        sym_name, sym_offset
      | Some (label_offset, _section, target_state) -> (
        if
          (* Check if target is actually a global symbol *)
          is_global_in_state target_state target
        then sym_name, sym_offset
        else
          (* Find global symbol at the target offset (if co-located) or nearest
             before. This handles function entry labels which are co-located
             with global function symbols. *)
          let target_abs_offset = label_offset in
          match
            SS.find_global_symbol_at_or_before target_state target_abs_offset
          with
          | None ->
            (* No global symbol before target - shouldn't happen in well-formed
               output, fall back to original *)
            sym_name, sym_offset
          | Some (nearest_sym, nearest_sym_offset) ->
            let addend = target_abs_offset - nearest_sym_offset + sym_offset in
            Asm_symbol.encode nearest_sym, addend)

(* When true, emit relocations for ALL 8-byte symbol references (matching
   assembler behavior). When false, only emit relocations for cross-section
   references and resolve same-section refs at emit time. Set to true for
   verification against the assembler. *)
let emit_relocs_for_all_symbol_refs = ref false

(* Extract (target, addend) from absolute symbol expressions like: - Symbol sym
   -> (Symbol sym, 0) - Label lbl -> (Label lbl, 0) - Add (Symbol sym,
   Signed_int offset) -> (Symbol sym, offset) - Add (Signed_int offset, Symbol
   sym) -> (Symbol sym, offset) - Add (Label lbl, Signed_int offset) -> (Label
   lbl, offset) *)
let rec target_and_addend_of_constant (cst : C.t) :
    (Symbol.target * int64) option =
  match cst with
  | Label lbl -> Some (Label lbl, 0L)
  | Symbol sym -> Some (Symbol sym, 0L)
  (* Symbol/Label + offset (either order) *)
  | Add (Symbol sym, Signed_int offset) | Add (Signed_int offset, Symbol sym) ->
    Some (Symbol sym, offset)
  | Add (Label lbl, Signed_int offset) | Add (Signed_int offset, Label lbl) ->
    Some (Label lbl, offset)
  | Add (Symbol sym, Unsigned_int offset) | Add (Unsigned_int offset, Symbol sym)
    ->
    Some (Symbol sym, Numbers.Uint64.to_int64 offset)
  | Add (Label lbl, Unsigned_int offset) | Add (Unsigned_int offset, Label lbl)
    ->
    Some (Label lbl, Numbers.Uint64.to_int64 offset)
  (* Nested expressions with offset on right *)
  | Add (inner, Signed_int offset) -> (
    match target_and_addend_of_constant inner with
    | Some (target, inner_offset) -> Some (target, Int64.add inner_offset offset)
    | None -> None)
  | Add (inner, Unsigned_int offset) -> (
    match target_and_addend_of_constant inner with
    | Some (target, inner_offset) ->
      Some (target, Int64.add inner_offset (Numbers.Uint64.to_int64 offset))
    | None -> None)
  (* Nested expressions with offset on left *)
  | Add (Signed_int offset, inner) -> (
    match target_and_addend_of_constant inner with
    | Some (target, inner_offset) -> Some (target, Int64.add offset inner_offset)
    | None -> None)
  | Add (Unsigned_int offset, inner) -> (
    match target_and_addend_of_constant inner with
    | Some (target, inner_offset) ->
      Some (target, Int64.add (Numbers.Uint64.to_int64 offset) inner_offset)
    | None -> None)
  | Add (_, _) -> None (* Other Add patterns not supported *)
  | Signed_int _ | Unsigned_int _ | This | Variable _ | Sub _ -> None

(* Handle absolute symbol reference. For .8byte symbol references in object
   files, the assembler always emits relocations. We can either match that
   behavior (for verification) or resolve same-section refs at emit time (more
   efficient for JIT). For 4-byte references, we also handle them in
   verification mode. *)
let is_absolute_symbol_reference state ~all_sections ~current_section
    ~width_bytes (cst : C.t) =
  (* Only handle 4-byte and 8-byte symbol references *)
  if width_bytes <> 8 && width_bytes <> 4
  then None
  else
    match target_and_addend_of_constant cst with
    | None -> None
    | Some (target, addend) -> (
      let for_jit = All_section_states.for_jit all_sections in
      (* Check if symbol is in the same section *)
      let same_section_offset = SS.find_target_offset_in_bytes state target in
      let is_same_section = Option.is_some same_section_offset in
      (* Check if this is a global symbol (explicit Global/Weak directive) *)
      let is_global = is_global_in_state state target in
      (* On macOS (REL), the data value depends on target type and scope:

         - For global symbols: emit just the addend (relocation points to
         symbol)

         - For local labels/file-scope symbols: the assembler finds the nearest
         global symbol before the target and creates a relocation to that
         symbol. The data contains (target_offset - symbol_offset + addend).

         - On Linux RELA: addend in relocation, emit 0 in data *)
      if is_same_section
      then
        if for_jit || !emit_relocs_for_all_symbol_refs
        then (
          let target_offset = Option.get same_section_offset in
          if is_rela_platform ()
          then (
            (* Linux RELA: emit 0, put original target in relocation *)
            SS.add_relocation_at_current_offset state
              ~reloc_kind:(R_AARCH64_ABS64 { target; addend = 0 });
            Some 0L)
          else if is_global
          then (
            (* Global symbol on macOS: emit addend, relocation to symbol *)
            SS.add_relocation_at_current_offset state
              ~reloc_kind:(R_AARCH64_ABS64 { target; addend = 0 });
            Some addend)
          else
            (* Local label or file-scope symbol on macOS: find global symbol at
               or before target and emit offset relative to it *)
            match SS.find_global_symbol_at_or_before state target_offset with
            | Some (nearest_sym, nearest_sym_offset) ->
              let data_value =
                Int64.add addend
                  (Int64.of_int (target_offset - nearest_sym_offset))
              in
              SS.add_relocation_at_current_offset state
                ~reloc_kind:
                  (R_AARCH64_ABS64 { target = Symbol nearest_sym; addend = 0 });
              Some data_value
            | None ->
              (* No global symbol before target - fall back to absolute offset.
                 This shouldn't happen in well-formed OCaml output. *)
              SS.add_relocation_at_current_offset state
                ~reloc_kind:(R_AARCH64_ABS64 { target; addend = 0 });
              Some (Int64.add addend (Int64.of_int target_offset)))
        else None (* Resolve same-section refs at emit time via eval_constant *)
      else
        (* Cross-section reference - always needs relocation *)
        match
          All_section_states.find_in_any_section_with_state all_sections target
        with
        | None -> None (* Not found, will fall through to emit_unresolved *)
        | Some (target_offset, sym_section, target_state) ->
          if
            Asm_section.equal sym_section current_section
            && (not for_jit)
            && not !emit_relocs_for_all_symbol_refs
          then None (* Same section after all, resolve at emit time *)
          else
            (* Check if global in the target section's state *)
            let is_global_in_target = is_global_in_state target_state target in
            (* Check if target is a Label in a TEXT section. On macOS, labels in
               TEXT sections get symbol table entries and direct relocations.
               Labels in DATA/rodata sections get converted to
               section+offset. *)
            let is_label_in_text =
              match target with
              | Symbol.Label _ -> Asm_section.section_is_text sym_section
              | Symbol _ -> false
            in
            let data_value =
              if is_rela_platform ()
              then 0L
              else if is_global_in_target || is_label_in_text
              then addend
              else Int64.add addend (Int64.of_int target_offset)
            in
            (* Keep original target in relocation for JIT use. The conversion to
               section+offset for verification is done in emit.ml *)
            SS.add_relocation_at_current_offset state
              ~reloc_kind:(R_AARCH64_ABS64 { target; addend = 0 });
            Some data_value)

(* Handle unresolved symbol reference by emitting the addend (on macOS) or zeros
   (on Linux) and recording a relocation for the linker to patch. *)
let emit_unresolved_symbol_relocation state ~width_bytes c =
  let buf = SS.buffer state in
  let addend =
    match target_and_addend_of_constant c with
    | Some (target, addend) when width_bytes = 8 ->
      SS.add_relocation_at_current_offset state
        ~reloc_kind:(R_AARCH64_ABS64 { target; addend = 0 });
      addend
    | Some _ ->
      let symbol_name = Option.get (extract_symbol_name c) in
      Misc.fatal_errorf
        "Unresolved %d-byte reference to symbol %s (only 8-byte relocations \
         supported)"
        width_bytes symbol_name
    | None ->
      Misc.fatal_errorf
        "Unresolved %d-byte constant expression (only 8-byte relocations \
         supported)"
        width_bytes
  in
  (* On macOS (REL), emit addend in the data. On Linux (RELA), emit zeros. *)
  if is_rela_platform ()
  then
    for _ = 1 to width_bytes do
      Buffer.add_char buf '\x00'
    done
  else D.Directive.emit_int_le buf ~width_bytes addend

(* Emit a constant value, handling cross-section references and relocations. *)
let emit_constant state ~all_sections ~current_section constant =
  let buf = SS.buffer state in
  let module C = D.Directive.Constant_with_width in
  let c = C.constant constant in
  let width = C.width_in_bytes constant in
  let width_bytes = C.Width_in_bytes.to_int width in
  let abs_result =
    is_absolute_symbol_reference state ~all_sections ~current_section
      ~width_bytes c
  in
  let value_opt =
    match
      is_cross_section_relative_reference state ~all_sections ~current_section c
    with
    | Some addend -> Some addend
    | None -> (
      match abs_result with
      | Some v -> Some v
      | None -> eval_constant state ~all_sections c)
  in
  match value_opt with
  | Some value -> D.Directive.emit_int_le buf ~width_bytes value
  | None -> emit_unresolved_symbol_relocation state ~width_bytes c

let emit_alignment state ~bytes ~(fill : D.align_padding) =
  let buf = SS.buffer state in
  let offset = SS.offset_in_bytes state in
  let remainder = offset mod bytes in
  if remainder <> 0
  then
    let padding = bytes - remainder in
    match fill with
    | Nop ->
      (* Emit NOP instructions (4 bytes each) for code alignment *)
      let nop_count = padding / 4 in
      let zero_count = padding mod 4 in
      let nop = Int64.of_int32 (Nop_helpers.encode_nop ()) in
      for _ = 1 to nop_count do
        D.Directive.emit_int_le buf ~width_bytes:4 nop
      done;
      for _ = 1 to zero_count do
        Buffer.add_char buf '\x00'
      done
    | Zero ->
      for _ = 1 to padding do
        Buffer.add_char buf '\x00'
      done

let emit_directive state ~current_section ~all_sections
    (directive : D.Directive.t) =
  let buf = SS.buffer state in
  (* Update current section when we see a Section directive *)
  (match[@warning "-4"] directive with
  | Section (section, _) -> current_section := section
  | _ -> ());
  match directive with
  | Bytes { str; _ } -> Buffer.add_string buf str
  | Space { bytes } ->
    for _ = 1 to bytes do
      Buffer.add_char buf '\x00'
    done
  | Align { bytes; fill } -> emit_alignment state ~bytes ~fill
  | Const { constant; _ } ->
    emit_constant state ~all_sections ~current_section:!current_section constant
  | Sleb128 { constant; _ } -> (
    match eval_constant state ~all_sections constant with
    | Some value -> D.Directive.emit_sleb128 buf value
    | None -> Misc.fatal_error "Cannot emit SLEB128 for external symbol")
  | Uleb128 { constant; _ } -> (
    match eval_constant state ~all_sections constant with
    | Some value -> D.Directive.emit_uleb128 buf value
    | None -> Misc.fatal_error "Cannot emit ULEB128 for external symbol")
  (* Directives that don't emit data *)
  | Cfi_adjust_cfa_offset _ | Cfi_def_cfa_offset _ | Cfi_endproc | Cfi_offset _
  | Cfi_startproc | Cfi_remember_state | Cfi_restore_state
  | Cfi_def_cfa_register _ | Comment _ | Direct_assignment _ | File _ | Global _
  | Indirect_symbol _ | Loc _ | New_label _ | New_line | Private_extern _
  | Section _ | Size _ | Type _ | Protected _ | Hidden _ | Weak _ | External _
  | Reloc _ ->
    ()
