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
    let symbol_deps =
      Name_occurrences.symbols free_names |> Symbol.Set.elements
    in
    let dep_fields =
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
