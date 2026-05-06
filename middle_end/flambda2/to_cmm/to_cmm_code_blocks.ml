(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Mark Shinwell, Jane Street                       *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

module C = Cmm_helpers
module R = To_cmm_result

let linkage_name code_id =
  C.code_block_symbol_name
    (Linkage_name.to_string (Code_id.linkage_name code_id))

let code_block_symbol_for code_id : Cmm.symbol =
  (* Code_block symbols always belong to the current CU (we only emit them for
     code defined here); they are exported because back-pointers in foreign CUs
     may reference them via direct calls into our text. *)
  (* REVIEW(codex): The motivation for exporting Code_block symbols is a bit unclear:
     the back-pointer word is emitted in this function's own .text and points
     at this function's Code_block, so it is not obvious why "foreign CUs" need
     to reference the Code_block symbol. If the intent is "ensure the symbol is
     discoverable by the JIT loader", consider stating that explicitly. *)
  { sym_name = linkage_name code_id; sym_global = Global }

let dep_is_unloadable all_code dep_code_id =
  match Exported_code.find all_code dep_code_id with
  | None -> false
  | Some com -> Code_metadata.is_unloadable (Code_or_metadata.code_metadata com)

let emit_code_block_for ~all_code (code : Code.t) res =
  if (not !Clflags.unit_is_unloadable) || not (Code.is_unloadable code)
  then res
  else
    let code_id = Code.code_id code in
    let free_names = Code.free_names_of_params_and_body code in
    let code_id_deps =
      Name_occurrences.code_ids free_names
      |> Code_id.Set.filter (dep_is_unloadable all_code)
      |> Code_id.Set.elements
    in
    (* REVIEW(claude): contrast with [code_id_deps], which is filtered
       by [dep_is_unloadable], [symbol_deps] is unfiltered — so cross-CU
       references (e.g. caml_int_ops, stdlib lifted constants) become
       fields of every unloadable Code_block. caml_darken on a black
       (NOT_MARKABLE) cross-CU block is a no-op so this is sound, but it
       bloats every Code_block by the symbol-reference fan-out and adds
       work to the mark scan. Same-CU [Local] symbols are also a no-op
       darken (black header) but consume a slot too. Worth filtering to
       same-CU Globals plus same-CU Locals' Global ancestors (per B.1).
       The [Local]/[Global] selection below also doesn't quite match
       what the linker expects: a Symbol whose [Symbol.is_local] field
       (if any) says Local but whose CU is the current one will still be
       emitted with [sym_global = Local], regardless of what to_cmm_static
       decided when emitting the definition. If those ever disagree
       (e.g. via lifted/inlined constants) the resulting reference is
       inconsistent. *)
    let symbol_deps =
      Name_occurrences.symbols free_names |> Symbol.Set.elements
    in
    let dep_fields =
      (* REVIEW(codex): [Symbol] occurrences here are assumed to denote static blocks
         whose addresses are valid OCaml values (so they can safely live in a
         scannable [Code_block]). If any non-value symbol can appear (e.g. code
         labels, C globals), the GC would treat the address as a heap pointer
         when scanning Code_blocks and could crash. Consider filtering based on
         symbol kind if available, or adding a debug-only assertion. *)
      List.map
        (fun cid -> Cmm.Csymbol_address (code_block_symbol_for cid))
        code_id_deps
      @ List.map
          (fun sym ->
            let sym_name = Linkage_name.to_string (Symbol.linkage_name sym) in
            let sym_global : Cmm.is_global =
              if Compilation_unit.is_current (Symbol.compilation_unit sym)
              then Local
              else Global
            in
            Cmm.Csymbol_address { sym_name; sym_global })
          symbol_deps
    in
    let n_fields = List.length dep_fields in
    let header = C.unit_block_header Runtimetags.code_block_tag n_fields in
    let block_sym = code_block_symbol_for code_id in
    (* Suppress unloadable_data_block tracking for Code_blocks: they are tracked
       separately via the runtime's [code_blocks] list (located by the JIT
       loader using the [_code_block] symbol-name suffix). *)
    let prev = !C.suppress_unloadable_data_block_tracking in
    C.suppress_unloadable_data_block_tracking := true;
    let data_items = C.emit_unit_block block_sym header dep_fields in
    C.suppress_unloadable_data_block_tracking := prev;
    R.add_archive_data_items res data_items

(* REVIEW(claude): the entry function's [Code_block] has zero
   dependency fields, even though the entry calls top-level functions
   in the unit and references the unit's static data. The intent is
   that the entry is only on-stack during initialisation (so F.2 keeps
   its Code_block alive while running), and after [Eval.eval] returns
   nothing reaches the entry's Code_block. That's correct for the
   module-initialiser case here. But it does mean: if a major GC fires
   *during* eval'd initialisation and walks the running entry, the
   entry's Code_block is darkened but the rest of the unit is not
   marked through the Code_block fields — only through whatever the
   stack/closures already point at. As long as every same-CU function
   the entry transitively calls is itself reachable via stack frames
   when the entry is on-stack, this is fine. Add a sanity-checking
   test (e.g. [unload_signal_gc] at extreme recursion depth). *)
let emit_entry_code_block ~(entry_sym : Cmm.symbol) res =
  if not !Clflags.unit_is_unloadable
  then res
  else
    let block_sym : Cmm.symbol =
      { sym_name = C.code_block_symbol_name entry_sym.sym_name;
        sym_global = Global
      }
    in
    let header = C.unit_block_header Runtimetags.code_block_tag 0 in
    (* Suppress data-block tracking: Code_blocks are tracked separately via the
       [_code_block] suffix during JIT registration (see jit.ml's
       [unloadable_metadata]). *)
    let prev = !C.suppress_unloadable_data_block_tracking in
    C.suppress_unloadable_data_block_tracking := true;
    let data_items = C.emit_unit_block block_sym header [] in
    C.suppress_unloadable_data_block_tracking := prev;
    R.add_archive_data_items res data_items
