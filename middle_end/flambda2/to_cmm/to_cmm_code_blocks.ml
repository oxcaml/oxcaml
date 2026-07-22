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
     code defined here). They are exported as [Global] so the JIT loader can
     locate them by name when populating the unit's [code_blocks] table. *)
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
    let entry_linkage_name =
      Linkage_name.to_string (Code_id.linkage_name code_id)
    in
    C.register_unloadable_code_block_entry entry_linkage_name;
    let free_names = Code.free_names_of_params_and_body code in
    let code_id_deps =
      Name_occurrences.code_ids free_names
      |> Code_id.Set.filter (dep_is_unloadable all_code)
      |> Code_id.Set.elements
    in
    (* Filter [symbol_deps] to symbols defined in the current (unloadable) CU.
       Symbols carry data-block addresses (Symbol.mli: "identifies a piece of
       statically-allocated data"); cross-CU symbols (e.g. [caml_int_ops],
       stdlib lifted constants, predefined exceptions) have NOT_MARKABLE
       headers, so [caml_darken] is a no-op on them and including them only
       bloats Code_block dep_fields and the mark-scan workload. Same-CU [Local]
       symbols are also no-op darkens (black headers) but the same-CU filter
       keeps them in: any same-CU data block referenced from a function's code
       path may be marked via the Code_block dep_field chain, and B.1 emits
       same-CU unloadable data blocks with white headers. *)
    let symbol_deps =
      Name_occurrences.symbols free_names
      |> Symbol.Set.filter (fun sym ->
          Compilation_unit.is_current (Symbol.compilation_unit sym))
      |> Symbol.Set.elements
    in
    let dep_fields =
      List.map
        (fun cid -> Cmm.Csymbol_address (code_block_symbol_for cid))
        code_id_deps
      @ List.map
          (fun sym ->
            let sym_name = Linkage_name.to_string (Symbol.linkage_name sym) in
            Cmm.Csymbol_address { sym_name; sym_global = Local })
          symbol_deps
    in
    let n_fields = List.length dep_fields in
    let header = C.unit_block_header Runtimetags.code_block_tag n_fields in
    let block_sym = code_block_symbol_for code_id in
    let data_items = C.emit_unit_block block_sym header dep_fields in
    R.add_archive_data_items res data_items

(* The entry function's [Code_block] has zero dependency fields, even though the
   entry calls top-level functions in the unit and references the unit's static
   data. This is fine because the unit's static blocks are only donated to the
   major heap *after* the initialiser has finished
   ([caml_activate_unloadable_unit]): while the entry runs, the GC does not
   manage the unit's blocks at all, so nothing depends on the entry's dep
   fields. Once [Eval.eval] returns, nothing reaches the entry's Code_block and
   it is reclaimed with the rest of the unit's dead blocks. *)
let emit_entry_code_block ~(entry_sym : Cmm.symbol) res =
  if not !Clflags.unit_is_unloadable
  then res
  else (
    C.register_unloadable_code_block_entry entry_sym.sym_name;
    let block_sym : Cmm.symbol =
      { sym_name = C.code_block_symbol_name entry_sym.sym_name;
        sym_global = Global
      }
    in
    let header = C.unit_block_header Runtimetags.code_block_tag 0 in
    let data_items = C.emit_unit_block block_sym header [] in
    R.add_archive_data_items res data_items)
