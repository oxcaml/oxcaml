(******************************************************************************
 *                                  OxCaml                                    *
 *                           Leo Lee, Jane Street                             *
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

open! Jsoo_imports.Import

(** Blocks with the continuation potentially not yet defined.

    For efficiency reasons, [body] will be inserted head-first, so the
    instructions are in reverse. [end_block_with_last_exn] will therefore
    reverse this before archiving. *)
type partial_block =
  { params : Jsir.Var.t list;
    body : Jsir.instr list;
    addr : Jsir.Addr.t
  }

type t =
  { complete_blocks : Jsir.block Jsir.Addr.Map.t;
    current_blocks : partial_block list;
    next_addr : Jsir.Addr.t;
    reserved_addrs : Jsir.Addr.Set.t;
    invalid_switch_block : Jsir.Addr.t option;
    next_method_cache_id : int;
        (** JSOO has a similar variable which is incremented for every method
            call; we mimic this here. *)
    imported_compilation_units : Compilation_unit.Set.t;
    entry_prelude : Jsir.instr list;
        (** Instructions to put at the start of the entry block, in reverse
            order. Used for the fetching of the global data and of external
            symbols. *)
    global_data_var : Jsir.Var.t option;
    external_symbols : Jsir.Var.t Symbol.Map.t;
        (** The variables bound in [entry_prelude] to symbols from other
            compilation units. *)
    late_bound_vars : Jsir.Var.Set.t;
        (** Variables created ahead of their definition and since bound with
            [To_jsir_shared.bind_vars]. *)
    call_events : call_events option;
        (** The events to put around the calls added by [add_call_exn], set by
            [with_debuginfo_exn]. *)
    reachable_names : Name_occurrences.t
        (** The names of this unit that other compilation units may refer to
            Only these symbols need to be in the global symbol table. *)
  }

and call_events =
  { start : Parse_info.t;
    end_ : Parse_info.t option
  }

let create ~reachable_names =
  { complete_blocks = Jsir.Addr.Map.empty;
    current_blocks = [];
    next_addr = Jsir.Addr.zero;
    reserved_addrs = Jsir.Addr.Set.empty;
    invalid_switch_block = None;
    next_method_cache_id = 1;
    imported_compilation_units = Compilation_unit.Set.empty;
    entry_prelude = [];
    global_data_var = None;
    external_symbols = Symbol.Map.empty;
    late_bound_vars = Jsir.Var.Set.empty;
    call_events = None;
    reachable_names
  }

let map_top_current_block_exn t ~f =
  let top_current_block, rest_current_blocks =
    match t.current_blocks with
    | [] -> Misc.fatal_error "To_jsir_result: expected nonempty current_blocks"
    | hd :: tl -> hd, tl
  in
  let top_current_block =
    { top_current_block with body = f top_current_block.body }
  in
  { t with current_blocks = top_current_block :: rest_current_blocks }

let add_instr_exn t instr =
  map_top_current_block_exn t ~f:(fun body -> instr :: body)

(* An event supersedes an event immediately preceding it, so that there are
   never two consecutive events (which JSOO rejects). *)
let push_event parse_info (body : Jsir.instr list) : Jsir.instr list =
  match body with
  | Event _ :: body -> Event parse_info :: body
  | (Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _) :: _ | [] ->
    Event parse_info :: body

let parse_info_of_debuginfo dbg ~pos =
  if not (Flambda_features.debug ())
  then None
  else
    let loc = Debuginfo.to_location dbg in
    if Location.is_none loc
    then None
    else
      match pos with
      | `Start -> Some (Parse_info.t_of_pos loc.loc_start)
      | `End -> Some (Parse_info.t_of_pos loc.loc_end)

let add_event_exn t dbg ~pos =
  match parse_info_of_debuginfo dbg ~pos with
  | None -> t
  | Some parse_info -> map_top_current_block_exn t ~f:(push_event parse_info)

let add_call_exn t instr =
  match t.call_events with
  | None -> add_instr_exn t instr
  | Some { start; end_ } ->
    map_top_current_block_exn t ~f:(fun body ->
        let body = instr :: push_event start body in
        match end_ with None -> body | Some end_ -> push_event end_ body)

let with_debuginfo_exn t dbg ~event_after ~f =
  let call_events =
    match parse_info_of_debuginfo dbg ~pos:`Start with
    | None -> None
    | Some start ->
      let end_ =
        if event_after then parse_info_of_debuginfo dbg ~pos:`End else None
      in
      Some { start; end_ }
  in
  let outer_call_events = t.call_events in
  let x, t = f { t with call_events } in
  x, { t with call_events = outer_call_events }

let new_block t ~params =
  let new_block = { params; body = []; addr = t.next_addr } in
  ( { t with
      current_blocks = new_block :: t.current_blocks;
      next_addr = Jsir.Addr.succ t.next_addr
    },
    t.next_addr )

let reserve_address t =
  ( { t with
      next_addr = Jsir.Addr.succ t.next_addr;
      reserved_addrs = Jsir.Addr.Set.add t.next_addr t.reserved_addrs
    },
    t.next_addr )

let new_block_with_addr_exn t ~params ~addr =
  if not (Jsir.Addr.Set.mem addr t.reserved_addrs)
  then
    Misc.fatal_errorf
      "To_jsir_result.new_block_with_addr_exn: expected provided address %d to \
       be reserved"
      addr;
  let new_block = { params; body = []; addr } in
  { t with
    current_blocks = new_block :: t.current_blocks;
    reserved_addrs = Jsir.Addr.Set.remove addr t.reserved_addrs
  }

let end_block_with_last_exn t last =
  let { params; body; addr }, rest_current_blocks =
    match t.current_blocks with
    | [] ->
      Misc.fatal_error
        "To_jsir_result.end_block_with_last_exn: expected nonempty \
         current_blocks"
    | hd :: tl -> hd, tl
  in
  let new_block : Jsir.block =
    { params; body = List.rev body; branch = last }
  in
  let complete_blocks = Jsir.Addr.Map.add addr new_block t.complete_blocks in
  { t with complete_blocks; current_blocks = rest_current_blocks }

let invalid_switch_block t =
  match t.invalid_switch_block with
  | Some addr -> t, addr
  | None ->
    let t, addr = new_block t ~params:[] in
    let t =
      add_instr_exn t
        (Let
           ( Jsir.Var.fresh (),
             Prim (Extern ("caml_invalid_switch_arm", None), []) ))
    in
    let t = end_block_with_last_exn t Stop in
    { t with invalid_switch_block = Some addr }, addr

let get_public_method t ~obj ~field =
  let method_cache_id = t.next_method_cache_id in
  let f = Jsir.Var.fresh () in
  let t =
    add_instr_exn t
      (Let
         ( f,
           Prim
             ( Extern ("caml_get_public_method", None),
               [ Pv obj;
                 Pv field;
                 Pc (Int (Targetint.of_int_exn method_cache_id)) ] ) ))
  in
  { t with next_method_cache_id = method_cache_id + 1 }, f

let import_compilation_unit t compilation_unit =
  match Compilation_unit.equal compilation_unit Compilation_unit.predef_exn with
  | true ->
    (* We shouldn't add this to our linking information, because JSOO adds these
       to global data by default *)
    t
  | false ->
    { t with
      imported_compilation_units =
        Compilation_unit.Set.add compilation_unit t.imported_compilation_units
    }

let add_late_bound_var_exn t var =
  if Jsir.Var.Set.mem var t.late_bound_vars
  then
    Misc.fatal_errorf
      "JSIR variable %a is bound twice. This usually means that a function or \
       value slot appears in more than one set of closures, which the JSIR \
       translation does not support."
      Jsir.Var.print var;
  { t with late_bound_vars = Jsir.Var.Set.add var t.late_bound_vars }

let add_entry_prelude_instr t instr =
  { t with entry_prelude = instr :: t.entry_prelude }

let global_data_var t =
  match t.global_data_var with
  | Some var -> t, var
  | None ->
    let var = Jsir.Var.fresh () in
    let t =
      add_entry_prelude_instr t
        (Let (var, Prim (Extern ("caml_get_global_data", None), [])))
    in
    { t with global_data_var = Some var }, var

let symbol_to_native_strings symbol =
  ( Symbol.compilation_unit symbol
    |> Compilation_unit.name |> Compilation_unit.Name.to_string
    |> Jsir.Native_string.of_string,
    Symbol.linkage_name_as_string symbol |> Jsir.Native_string.of_string )

let symbol_is_for_compilation_unit symbol =
  let compilation_unit = Symbol.compilation_unit symbol in
  Symbol.equal (Symbol.for_compilation_unit compilation_unit) symbol

let register_symbol t symbol var =
  if symbol_is_for_compilation_unit symbol
  then
    (* We don't need to add this symbol to our symbol table, because we will
       instead fetch it from jsoo's global data table (see
       [external_symbol_lookup]) *)
    t
  else if not (Name_occurrences.mem_symbol t.reachable_names symbol)
  then
    (* No other compilation unit can refer to this symbol, as it is not exported *)
    t
  else
    let compilation_unit_name, symbol_name = symbol_to_native_strings symbol in
    add_instr_exn t
      (Let
         ( Jsir.Var.fresh (),
           Prim
             ( Extern ("caml_register_symbol", None),
               [ Pc (NativeString compilation_unit_name);
                 Pc (NativeString symbol_name);
                 Pv var ] ) ))

let global_data_lookup t ~symbol_name : t * Jsir.expr =
  let t, global_data = global_data_var t in
  let symbol_name = Jsir.Native_string.of_string symbol_name in
  ( t,
    Prim
      ( Extern ("caml_js_get", None),
        [Pv global_data; Pc (NativeString symbol_name)] ) )

let predef_exception_lookup t symbol =
  let symbol_name = Symbol.linkage_name_as_string symbol in
  (* Chop off caml_exn_ *)
  let caml_exn_ = "caml_exn_" in
  if not (String.starts_with ~prefix:caml_exn_ symbol_name)
  then
    Misc.fatal_errorf
      "Predefined exception symbol %a doesn't start with \"caml_exn_\""
      Symbol.print symbol;
  let symbol_name =
    String.sub symbol_name (String.length caml_exn_)
      (String.length symbol_name - String.length caml_exn_)
  in
  (* jsoo already registers these in global data *)
  global_data_lookup t ~symbol_name:("predef:" ^ symbol_name)

let external_symbol_lookup t symbol : t * Jsir.expr =
  match Symbol.is_predefined_exception symbol with
  | true -> predef_exception_lookup t symbol
  | false -> (
    match symbol_is_for_compilation_unit symbol with
    | true ->
      let compilation_unit = Symbol.compilation_unit symbol in
      let t = import_compilation_unit t compilation_unit in
      let compilation_unit_name =
        Compilation_unit.name_as_string compilation_unit
      in
      global_data_lookup t ~symbol_name:compilation_unit_name
    | false ->
      (* The unit defining the symbol must be linked (and initialised) before
         this one, so record it as an import too. *)
      let t = import_compilation_unit t (Symbol.compilation_unit symbol) in
      let compilation_unit_name, symbol_name =
        symbol_to_native_strings symbol
      in
      ( t,
        Prim
          ( Extern ("caml_get_symbol", None),
            [ Pc (NativeString compilation_unit_name);
              Pc (NativeString symbol_name) ] ) ))

(* Symbols from other compilation units are fetched once at the start of the
   entry block. *)
let external_symbol_var t symbol =
  match Symbol.Map.find_opt symbol t.external_symbols with
  | Some var -> t, var
  | None ->
    (* The lookup may fetch the global data, which must come first in the
       prelude. *)
    let t, expr = external_symbol_lookup t symbol in
    let var = Jsir.Var.fresh () in
    let t = add_entry_prelude_instr t (Let (var, expr)) in
    ( { t with external_symbols = Symbol.Map.add symbol var t.external_symbols },
      var )

type program =
  { program : Jsir.program;
    imported_compilation_units : Compilation_unit.Set.t
  }

let to_program_exn
    { complete_blocks;
      current_blocks;
      next_addr = _;
      reserved_addrs;
      invalid_switch_block = _;
      next_method_cache_id = _;
      imported_compilation_units;
      entry_prelude;
      global_data_var = _;
      external_symbols = _;
      late_bound_vars = _;
      call_events = _;
      reachable_names = _
    } =
  if List.length current_blocks <> 0
  then
    Misc.fatal_errorf
      "To_jsir_result.to_program_exn: expected current_blocks to be empty, \
       instead found %d"
      (List.length current_blocks);
  if not (Jsir.Addr.Set.is_empty reserved_addrs)
  then
    Misc.fatal_error
      "To_jsir_result.to_program_exn: expected all reserved addresses to be \
       used";
  let complete_blocks =
    match entry_prelude with
    | [] -> complete_blocks
    | _ :: _ ->
      let entry_block = Jsir.Addr.Map.find Jsir.Addr.zero complete_blocks in
      let body = List.rev_append entry_prelude entry_block.body in
      Jsir.Addr.Map.add Jsir.Addr.zero { entry_block with body } complete_blocks
  in
  let free_pc = (Jsir.Addr.Map.max_binding complete_blocks |> fst) + 1 in
  let program =
    { Jsir.start = Jsir.Addr.zero; blocks = complete_blocks; free_pc }
  in
  { program; imported_compilation_units }
