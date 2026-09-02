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
          Current_unit.is_current (Symbol.compilation_unit sym))
      (* Also include static data invented during Cmm translation of this
         function's body (e.g. sets of closures lifted by To_cmm itself): such
         symbols postdate simplification and so are absent from [free_names],
         yet the function's machine code references them directly. They are
         same-CU by construction. See [To_cmm_result.add_code_dep_symbol]. *)
      |> Symbol.Set.union (R.code_dep_symbols res code_id)
      (* Drop symbols bound to the runtime's permanent atoms (zero-sized
         statics): they are not blocks of this unit, need no marking to stay
         alive, and are referenced via named relocations rather than the [Local]
         labels used for dep fields below. *)
      |> Symbol.Set.filter (fun sym ->
          not (R.symbol_is_aliased_to_atom res sym))
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
    (* A [Code_block] must always live in the unit's donated heap-extent region,
       even when it has no dependencies: it is the liveness anchor for its
       function. The closure scan (F.1) and the stack scans (F.2/F.3) darken it
       via the back-pointer at [entry - 1]; if it were replaced by a (permanent,
       NOT_MARKABLE) runtime atom, a live heap closure over the function would
       keep nothing of the unit marked and the unit would be unloaded with the
       closure's code pointer still live. Since the extent machinery cannot
       represent a freed zero-wosize block, pad a dependency-free [Code_block]
       to one field holding the value of the runtime's tag-0 atom (a valid,
       permanently-NOT_MARKABLE value that the mark scan skips). *)
    let dep_fields =
      match dep_fields with
      | [] -> [C.atom_value_data_item ~tag:0]
      | _ :: _ -> dep_fields
    in
    let n_fields = List.length dep_fields in
    let header = C.unit_block_header Runtimetags.code_block_tag n_fields in
    let block_sym = code_block_symbol_for code_id in
    let data_items = C.emit_unit_block block_sym header dep_fields in
    R.add_archive_data_items res data_items

(* The entry function's [Code_block] would have zero dependency fields, even
   though the entry calls top-level functions in the unit and references the
   unit's static data: the unit's static blocks are only donated to the major
   heap *after* the initialiser has finished ([caml_activate_unloadable_unit]),
   so while the entry runs, the GC does not manage the unit's blocks at all, and
   nothing depends on the entry's dep fields. Nor does anything anchor the unit
   through the entry's [Code_block] afterwards: the entry has no closure (it is
   called directly by the loader, exactly once, before activation), so the
   F.1/F.2 darkening paths can only reach its [Code_block] pre-activation, when
   darkening is a no-op on the blocks' NOT_MARKABLE emission headers. The
   entry's [Code_block] is therefore not emitted at all; its symbol (still
   referenced by the back-pointer at [entry - 1]) is bound to the runtime's
   permanent atom of tag [Code_block_tag] by the JIT loader.

   The entry is also not registered in the [unloadable_code_blocks] sentinel:
   the runtime must not treat the shared atom as one of the unit's
   [Code_block]s, and the per-function text ranges derived from the sentinel
   (see [jit_register_unloadable_unit_native]) need not cover the entry's code —
   a PC-based code-fragment lookup for a PC in the entry can only happen
   pre-activation (see above), where a miss or a mapping to a neighbouring
   function's fragment both result in a no-op darken. *)
let emit_entry_code_block ~(entry_sym : Cmm.symbol) res =
  if not !Clflags.unit_is_unloadable
  then res
  else (
    C.register_atom_aliased_symbol
      (C.code_block_symbol_name entry_sym.sym_name)
      ~tag:Runtimetags.code_block_tag;
    res)
