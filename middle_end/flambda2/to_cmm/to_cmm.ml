(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Env = To_cmm_env
module R = To_cmm_result

(* Notes:

   - an int64 on a 32-bit host is represented across two registers, hence most
   operations on them will actually need to call C primitive that can handle
   them.

   - int32 on 64 bits are represented as an int64 in the range of 32-bit
   integers. Thus we insert sign extensions after every operation on 32-bits
   integers that may have a result outside of the range. *)

module C = struct
  include Cmm_helpers
  include To_cmm_shared
end

(* Get constant definitions from Cmmgen_state. (The To_cmm translation uses
   functions from Cmm_helpers which populate some mutable state in
   Cmmgen_state.) *)

let flush_cmm_helpers_state res =
  let aux name cst (res, acc) =
    match (cst : Cmmgen_state.constant) with
    | Const_table (global, l) ->
      let res, sym = R.raw_symbol res ~global name in
      res, C.cdata (C.define_symbol sym @ l) :: acc
  in
  (* reset the structured constants, just in case *)
  Cmmgen_state.clear_local_structured_constants ();
  match Cmmgen_state.get_and_clear_data_items () with
  | [] ->
    let cst_map = Cmmgen_state.get_and_clear_constants () in
    Misc.Stdlib.String.Map.fold aux cst_map (res, [])
  | _ ->
    Misc.fatal_errorf
      "There shouldn't be any data items in Cmmgen_state during Flambda 2 to \
       Cmm translation"

(* Note about the root (module block) symbol: it does not need any particular
   treatment. Specifically concerning its treatment as a GC root, it's like any
   other statically allocated symbol: if it has an associated computation, then
   it will already be included in the list of GC roots; otherwise it does not
   *have* to be a root. *)

let unit0 ~offsets ~all_code ~reachable_names flambda_unit =
  (* If someone wants to add 32-bit support in the future there will be a
     (merged) PR on oxcaml/oxcaml which can be used as a guide:
     https://github.com/oxcaml/oxcaml/pull/685 *)
  if Target_system.is_32_bit ()
  then
    Misc.fatal_error
      "Flambda 2 to Cmm conversion does not support 32-bit targets";
  let dummy_k = Continuation.create () in
  (* The dummy continuation is passed here since we're going to manually arrange
     that the return continuation turns into "return unit". (Module initialisers
     return the unit value). *)
  let env =
    Env.create offsets all_code ~return_continuation:dummy_k
      ~return_continuation_arity:[] ~trans_prim:To_cmm_primitive.trans_prim
      ~exn_continuation:(Flambda_unit.exn_continuation flambda_unit)
  in
  let ret_var = Variable.create "*ret*" Flambda_kind.value in
  let ret_var_duid = Flambda_debug_uid.none in
  let _env, return_cont_params =
    (* The environment is dropped because the handler for the dummy continuation
       (which just returns unit) doesn't use any of the parameters. *)
    C.continuation_bound_parameters env
      (Bound_parameters.create
         [ Bound_parameter.create ret_var Flambda_kind.With_subkind.any_value
             ret_var_duid ])
  in
  let return_cont, env =
    Env.add_jump_cont env
      (Flambda_unit.return_continuation flambda_unit)
      ~param_types:(List.map snd return_cont_params)
  in
  (* See comment in [To_cmm_set_of_closures] about binding [my_region] *)
  let env, toplevel_region_var =
    Env.create_bound_parameter env
      (Flambda_unit.toplevel_my_region flambda_unit, Flambda_debug_uid.none)
  in
  let r =
    R.create ~reachable_names
      ~module_symbol:(Flambda_unit.module_symbol flambda_unit)
  in
  let body, body_free_vars, body_symbol_inits, res =
    To_cmm_expr.expr env r (Flambda_unit.body flambda_unit)
  in
  if not (To_cmm_env.Symbol_inits.is_empty body_symbol_inits)
  then
    Misc.fatal_errorf
      "Did not find where to place the following symbol initializations: %a"
      To_cmm_env.Symbol_inits.print body_symbol_inits;
  let free_vars =
    To_cmm_shared.remove_var_with_provenance body_free_vars toplevel_region_var
  in
  if not (Backend_var.Set.is_empty free_vars)
  then
    Misc.fatal_errorf
      "Unbound free_vars in module init code when translating to cmm: %a"
      Backend_var.Set.print free_vars;
  (* CR mshinwell: This should at least be given a source file location. *)
  let dbg = Debuginfo.none in
  let body =
    let unit_value = C.targetint ~dbg (Targetint_32_64.one Sixty_four) in
    C.create_ccatch ~rec_flag:false ~body
      ~handlers:
        [ C.handler ~dbg return_cont
            (C.remove_skipped_params return_cont_params)
            unit_value false ]
  in
  let body =
    if !Clflags.afl_instrument
    then Afl_instrument.instrument_initialiser body (fun () -> dbg)
    else body
  in
  let entry_name = Cmm_helpers.make_symbol "entry" in
  let res, entry_sym = R.raw_symbol res ~global:Global entry_name in
  let entry =
    let fun_codegen =
      let fun_codegen = [Cmm.Reduce_code_size; Cmm.Use_linscan_regalloc] in
      let fun_codegen =
        if Flambda_features.backend_cse_at_toplevel ()
        then fun_codegen
        else Cmm.No_CSE :: fun_codegen
      in
      (* The entry (module initializer) is a function in this CU just like any
         other; if the unit is unloadable, it needs the [Unloadable] codegen
         option so the back-end emits frame-descriptor UNLOADABLE bits and the
         back-pointer at [entry - 1]. The matching [Code_block] is emitted below
         by [To_cmm_code_blocks.emit_entry_code_block]. *)
      if !Clflags.unit_is_unloadable
      then Cmm.Unloadable :: fun_codegen
      else fun_codegen
    in
    C.cfunction
      (C.fundecl entry_sym [] body fun_codegen dbg Default_poll Cmm.typ_val)
  in
  (* Per-function [Code_block]s are emitted alongside fundecls in
     [To_cmm_static.add_functions]. Here we just emit the [Code_block] for the
     entry (module initializer) function, which doesn't go through that path. *)
  let res = To_cmm_code_blocks.emit_entry_code_block ~entry_sym res in
  (* Sentinel for the unit's [Code_block]s. Layout: [count; entry_1;
     code_block_1; ...; entry_count; code_block_count] The JIT loader reads this
     by exact name to discover every (function-entry, code-block) pair without
     scanning the symbol table by suffix. The symbol is always emitted (with
     count = 0 if no unloadable functions exist) so the loader can rely on its
     presence whenever the CU is unloadable. This is a raw array, not a
     heap-shaped block, so it is emitted outside the
     [unloadable_blocks_start]/[unloadable_blocks_end] bracket below. *)
  let res, unloadable_sentinel_data =
    if !Clflags.unit_is_unloadable
    then
      let entries = C.flush_unloadable_code_block_entries () in
      let seen = Hashtbl.create 8 in
      let entries =
        List.rev entries
        |> List.filter (fun name ->
            if Hashtbl.mem seen name
            then false
            else (
              Hashtbl.add seen name ();
              true))
      in
      let count = List.length entries in
      let array_name =
        Cmm_helpers.make_symbol C.unloadable_code_blocks_symbol_basename
      in
      let res, array_sym = R.raw_symbol res ~global:Global array_name in
      let pairs =
        List.concat_map
          (fun entry_name ->
            let cb_name = C.code_block_symbol_name entry_name in
            [ Cmm.Csymbol_address { sym_name = entry_name; sym_global = Global };
              Cmm.Csymbol_address { sym_name = cb_name; sym_global = Global } ])
          entries
      in
      let data_items =
        Cmm.Cdefine_symbol array_sym
        :: Cmm.Cint (Nativeint.of_int count)
        :: pairs
      in
      res, [C.cdata data_items]
    else res, []
  in
  (* In unloadable mode, bracket the unit's static data blocks between two
     symbols. The JIT loader passes the delimited region to the runtime, which
     donates it to the major heap as a heap extent once the unit's initialiser
     has run ([caml_activate_unloadable_unit]). Everything between the two
     symbols must be a well-formed block — a header word followed by its fields,
     with no padding in between — because the heap-extent machinery walks the
     region header-by-header. [data_items] (from [R.to_cmm] below) satisfies
     this: it consists solely of static blocks emitted via the
     [Cmm_helpers.emit_*] block emitters. Raw data (the gc_roots table, the
     sentinel above, and any [Cmmgen_state] jump tables) is emitted outside the
     bracket.

     The bracket ends with an anonymous one-field padding block. This guarantees
     that a zero-wosize block (an empty array, a dependency-free [Code_block],
     or an empty module block) is never the *last* block of the region: such a
     block's value points one word past its header, and that address must stay
     inside the donated region so that address-based classification of the value
     (e.g. [Is_young] in [caml_darken]) cannot misattribute it to another memory
     region. The padding block itself is never referenced; the GC frees it in
     place after the first major cycle following activation, which is fine
     (extent free blocks are simply skipped by subsequent sweeps). *)
  let res, unloadable_blocks_bracket =
    if !Clflags.unit_is_unloadable
    then
      let res, start_sym =
        R.raw_symbol res ~global:Global
          (Cmm_helpers.make_symbol C.unloadable_blocks_start_symbol_basename)
      in
      let res, end_sym =
        R.raw_symbol res ~global:Global
          (Cmm_helpers.make_symbol C.unloadable_blocks_end_symbol_basename)
      in
      let padding_block =
        [C.cint (C.unit_block_header Obj.abstract_tag 1); C.cint 0n]
      in
      ( res,
        Some
          ( C.cdata [Cmm.Cdefine_symbol start_sym],
            C.cdata (padding_block @ [Cmm.Cdefine_symbol end_sym]) ) )
    else res, None
  in
  let { R.data_items; gc_roots; functions } = R.to_cmm res in
  let _res, cmm_helpers_data = flush_cmm_helpers_state res in
  let gc_root_data = C.gc_root_table gc_roots in
  let data_items =
    match unloadable_blocks_bracket with
    | None -> data_items
    | Some (start_phrase, end_phrase) ->
      (start_phrase :: data_items) @ [end_phrase]
  in
  (gc_root_data :: unloadable_sentinel_data)
  @ data_items @ cmm_helpers_data @ functions @ [entry]

let unit ~offsets ~all_code ~reachable_names flambda_unit =
  Profile.record_call "flambda_to_cmm" (fun () ->
      unit0 ~offsets ~all_code ~reachable_names flambda_unit)
