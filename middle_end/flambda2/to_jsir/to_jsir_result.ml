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
    global_data_var : Jsir.Var.t option;
    late_bound_vars : Jsir.Var.Set.t;
        (** Variables created ahead of their definition and since bound with
            [To_jsir_shared.bind_vars]. *)
    call_events : call_events option
        (** The events to put around the calls added by [add_call_exn], set by
            [with_debuginfo_exn]. *)
  }

and call_events =
  { start : Parse_info.t;
    end_ : Parse_info.t option
  }

let create () =
  { complete_blocks = Jsir.Addr.Map.empty;
    current_blocks = [];
    next_addr = Jsir.Addr.zero;
    reserved_addrs = Jsir.Addr.Set.empty;
    invalid_switch_block = None;
    next_method_cache_id = 1;
    imported_compilation_units = Compilation_unit.Set.empty;
    global_data_var = None;
    late_bound_vars = Jsir.Var.Set.empty;
    call_events = None
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

let global_data_var t =
  match t.global_data_var with
  | Some var -> t, var
  | None ->
    let var = Jsir.Var.fresh () in
    { t with global_data_var = Some var }, var

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
      global_data_var;
      late_bound_vars = _;
      call_events = _
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
    match global_data_var with
    | None -> complete_blocks
    | Some var ->
      let entry_block = Jsir.Addr.Map.find Jsir.Addr.zero complete_blocks in
      let body : Jsir.instr list =
        Let (var, Prim (Extern ("caml_get_global_data", None), []))
        :: entry_block.body
      in
      Jsir.Addr.Map.add Jsir.Addr.zero { entry_block with body } complete_blocks
  in
  let free_pc = (Jsir.Addr.Map.max_binding complete_blocks |> fst) + 1 in
  let program =
    { Jsir.start = Jsir.Addr.zero; blocks = complete_blocks; free_pc }
  in
  { program; imported_compilation_units }
