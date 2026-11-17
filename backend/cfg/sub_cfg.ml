(* MIT License

   Copyright (c) 2024 Jane Street Group LLC

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)
[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module DLL = Doubly_linked_list

(* Instruction ids. *)
let instr_id = InstructionId.make_sequence ()

let reset_instr_id () = InstructionId.reset instr_id

let next_instr_id () = InstructionId.get_and_incr instr_id

let make_instr desc arg res dbg ~phantom_available_before =
  { Cfg.desc;
    arg;
    res;
    dbg;
    fdo = Fdo_info.none;
    live = Reg.Set.empty;
    stack_offset = Cfg.invalid_stack_offset;
    id = next_instr_id ();
    available_before = Unreachable;
    available_across = Unreachable;
    phantom_available_before
  }

type t =
  { mutable entry : Cfg.basic_block;
    mutable exit : Cfg.basic_block;
    layout : Cfg.basic_block DLL.t
  }

let exit_has_never_terminator sub_cfg =
  Cfg.is_never_terminator sub_cfg.exit.terminator.desc

let make_never_block ?label () : Cfg.basic_block =
  Cfg.make_empty_block ?label
    (make_instr Cfg.Never [||] [||] Debuginfo.none
       ~phantom_available_before:None)

let make_empty () =
  let exit = make_never_block () in
  let entry =
    Cfg.make_empty_block
      (make_instr (Cfg.Always exit.start) [||] [||] Debuginfo.none
         ~phantom_available_before:None)
  in
  let layout = DLL.make_empty () in
  DLL.add_end layout entry;
  DLL.add_end layout exit;
  { entry; exit; layout }

let start_label sub_cfg = sub_cfg.entry.start

let exit_label sub_cfg = sub_cfg.exit.start

let add_block_at_start sub_cfg block =
  DLL.add_begin sub_cfg.layout block;
  sub_cfg.entry <- block

let add_empty_block_at_start sub_cfg ~label =
  Cfg.make_empty_block ~label
    (make_instr
       (Cfg.Always (start_label sub_cfg))
       [||] [||] Debuginfo.none ~phantom_available_before:None)
  |> add_block_at_start sub_cfg

let add_block sub_cfg block =
  DLL.add_end sub_cfg.layout block;
  sub_cfg.exit <- block

let add_never_block sub_cfg ~label =
  add_block sub_cfg (make_never_block ~label ())

let add_instruction_at_start sub_cfg desc arg res dbg ~phantom_available_before
    =
  (* We don't check [exit_has_never_terminator] since we're adding at the start,
     and this function is only used in very specific situations (note comment in
     the interface). *)
  DLL.add_begin sub_cfg.entry.body
    (make_instr desc arg res dbg ~phantom_available_before)

let add_instruction' sub_cfg instr =
  assert (exit_has_never_terminator sub_cfg);
  DLL.add_end sub_cfg.exit.body instr

let add_instruction sub_cfg desc arg res dbg ~phantom_available_before =
  add_instruction' sub_cfg
    (make_instr desc arg res dbg ~phantom_available_before)

let set_terminator sub_cfg desc arg res dbg ~phantom_available_before =
  assert (Cfg.is_never_terminator sub_cfg.exit.terminator.desc);
  sub_cfg.exit.terminator
    <- make_instr desc arg res dbg ~phantom_available_before

let link_if_needed ~(from : Cfg.basic_block) ~(to_ : Cfg.basic_block)
    ~phantom_available_before () =
  if Cfg.is_never_terminator from.terminator.desc
  then
    from.terminator
      <- { from.terminator with
           desc = Always to_.start;
           id = next_instr_id ();
           phantom_available_before
         }

let iter_basic_blocks sub_cfg ~f = DLL.iter sub_cfg.layout ~f

let exists_basic_blocks sub_cfg ~f = DLL.exists sub_cfg.layout ~f

let transfer ~from ~to_ = DLL.transfer ~from:from.layout ~to_:to_.layout ()

type join_branch =
  { sub_cfg : t;
    may_fall_through : bool
  }

let join ~from ~to_ ~phantom_available_before =
  List.iter
    (fun { sub_cfg = from; may_fall_through = _ } -> transfer ~from ~to_)
    from;
  let join_block = make_never_block () in
  List.iter
    (fun { sub_cfg = from; may_fall_through } ->
      (* A branch whose emission stopped because the code never returns must not
         be linked to the join block, even if its exit block still has a [Never]
         terminator: that can happen when the divergence arose inside a nested
         construct (e.g. a [Ccatch] all of whose arms diverge), whose own join
         block was left unfilled. Linking such an exit would create a reference
         to a block that may never be materialised. *)
      if may_fall_through
      then
        link_if_needed ~from:from.exit ~to_:join_block ~phantom_available_before
          ())
    from;
  add_block to_ join_block

let join_tail ~from ~to_ =
  List.iter (fun from -> transfer ~from ~to_) from;
  add_never_block to_ ~label:(Cmm.new_label ())

let update_exit_terminator sub_cfg desc ~arg ~dbg ~phantom_available_before =
  sub_cfg.exit.terminator
    <- { sub_cfg.exit.terminator with
         desc;
         id = next_instr_id ();
         arg;
         dbg;
         phantom_available_before
       }

(* The debuginfo of the instructions accepted by the two functions below is only
   used to describe code locations. That of other instructions can also end up
   in frametables (and thus in backtraces), or in the messages of checks such as
   [@zero_alloc] and [@poll error]: [set_block_entry_debuginfo] must not give
   them one.

   The first group is also restricted to what selection emits without a Cmm term
   to take a debuginfo from, together with constants, which stand for a whole
   arm such as [then 0]. Moves join the results of arms; trap instructions are
   grouped with them because they open and close the scope of a handler without
   computing anything, and do not appear in frametables. [Csel], by contrast,
   computes a value for a Cmm operation: selection gives it the debuginfo of
   that operation, as for arithmetic, so one without debuginfo is left that way.

   Among terminators, [Tailcall_self] is implemented as a jump, but is an
   application in the source: like the other calls it has the debuginfo of its
   Cmm term and must not receive that of an arm. *)

let basic_only_moves_values (desc : Cfg.basic) =
  match desc with
  | Op
      ( Move | Spill | Reload | Const_int _ | Const_float32 _ | Const_float _
      | Const_symbol _ | Const_vec128 _ | Const_vec256 _ | Const_vec512 _
      | Const_mask _ )
  | Pushtrap _ | Poptrap _ ->
    true
  | Op
      ( Stackoffset _ | Load _ | Store _ | Intop _ | Int128op _ | Intop_imm _
      | Intop_atomic _ | Floatop _ | Csel _ | Reinterpret_cast _ | Static_cast _
      | Probe_is_enabled _ | Opaque | Begin_region | End_region | Specific _
      | Name_for_debugger _ | Dls_get | Tls_get | Domain_index | Thread_self_id
      | Poll | Pause | Alloc _ | Trace_pc_guard_gate | Trace_pc_guard _ )
  | Reloadretaddr | Prologue | Epilogue | Stack_check _ ->
    false

let terminator_only_transfers_control (desc : Cfg.terminator) =
  match desc with
  | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ | Return ->
    true
  | Raise _ | Tailcall_self _ | Tailcall_func _ | Call_no_return _
  | External_call_no_return _ | Call _ | Prim _ | Invalid _ ->
    false

(* Returns [false] if an instruction having a debuginfo was found. *)
let set_leading_debuginfo (block : Cfg.basic_block) dbg =
  let has_dbg (instr : _ Cfg.instruction) = not (Debuginfo.is_none instr.dbg) in
  let rec set_body cell =
    match cell with
    | None -> true
    | Some cell ->
      let instr : Cfg.basic Cfg.instruction = DLL.value cell in
      if has_dbg instr
      then false
      else (
        if basic_only_moves_values instr.desc then instr.dbg <- dbg;
        set_body (DLL.next cell))
  in
  if set_body (DLL.hd_cell block.body) && not (has_dbg block.terminator)
  then (
    if terminator_only_transfers_control block.terminator.desc
    then block.terminator.dbg <- dbg;
    true)
  else false

let set_block_entry_debuginfo block dbg =
  if not (Debuginfo.is_none dbg)
  then ignore (set_leading_debuginfo block dbg : bool)

let set_entry_debuginfo sub_cfg dbg =
  let rec set_block cell =
    match cell with
    | None -> ()
    | Some cell -> (
      let block : Cfg.basic_block = DLL.value cell in
      let is_empty = DLL.is_empty block.body in
      if set_leading_debuginfo block dbg
      then
        (* The entry block is empty when the code starts in the next one. *)
        let next = DLL.next cell in
        let falls_through_to_next label =
          match next with
          | None -> false
          | Some next ->
            let next : Cfg.basic_block = DLL.value next in
            Label.equal label next.start
        in
        match block.terminator.desc with
        | Always label when is_empty && falls_through_to_next label ->
          set_block next
        | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
        | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
        | Tailcall_func _ | Call_no_return _ | External_call_no_return _
        | Call _ | Prim _ | Invalid _ ->
          ())
  in
  if not (Debuginfo.is_none dbg) then set_block (DLL.hd_cell sub_cfg.layout)

let mark_as_trap_handler sub_cfg = sub_cfg.entry.is_trap_handler <- true

let dump sub_cfg =
  let liveness = InstructionId.Tbl.create 32 in
  DLL.iter sub_cfg.layout ~f:(fun (block : Cfg.basic_block) ->
      Format.eprintf "Block %a@." Label.print block.start;
      Regalloc_irc_utils.log_body_and_terminator block.body block.terminator
        liveness)

(* note: `dump` is for debugging, and thus not always in use. *)
let (_ : t -> unit) = dump
