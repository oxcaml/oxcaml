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

    For efficiency reasons, [body] will be inserted head-first,
    so the instructions are in reverse. [end_block_with_last_exn] will
    therefore reverse this before archiving. *)
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
    invalid_switch_block : Jsir.Addr.t option
  }

let create () =
  { complete_blocks = Jsir.Addr.Map.empty;
    current_blocks = [];
    next_addr = Jsir.Addr.zero;
    reserved_addrs = Jsir.Addr.Set.empty;
    invalid_switch_block = None
  }

let add_instr_exn t instr =
  let top_current_block, rest_current_blocks =
    match t.current_blocks with
    | [] ->
      Misc.fatal_error
        "To_jsir_result.add_instr_exn: expected nonempty current_blocks"
    | hd :: tl -> hd, tl
  in
  let top_current_block =
    { top_current_block with body = instr :: top_current_block.body }
  in
  { t with current_blocks = top_current_block :: rest_current_blocks }

let add_debuginfo_exn t dbg ~pos =
  add_instr_exn t (Event (Parse_info.t_of_debuginfo dbg ~pos))

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

let to_program_exn
    { complete_blocks;
      current_blocks;
      next_addr = _;
      reserved_addrs;
      invalid_switch_block = _
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
  let free_pc = (Jsir.Addr.Map.max_binding complete_blocks |> fst) + 1 in
  { Jsir.start = Jsir.Addr.zero; blocks = complete_blocks; free_pc }

let invalid_switch_block t =
  match t.invalid_switch_block with
  | Some addr -> t, addr
  | None ->
    let t, addr = new_block t ~params:[] in
    let t =
      add_instr_exn t
        (Let (Jsir.Var.fresh (), Prim (Extern "caml_invalid_switch_arm", [])))
    in
    let t = end_block_with_last_exn t Stop in
    { t with invalid_switch_block = Some addr }, addr
