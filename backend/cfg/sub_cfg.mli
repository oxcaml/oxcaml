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

(** These IDs are also used by [make_instr] *)
val instr_id : InstructionId.sequence

val reset_instr_id : unit -> unit

(* CR mshinwell: consolidate with [Cfg.make_instruction] and tidy up ID
   interface *)
val make_instr :
  'a ->
  Reg.t array ->
  Reg.t array ->
  Debuginfo.t ->
  phantom_available_before:Backend_var.Set.t option ->
  'a Cfg.instruction

(** A "sub" CFG is the counterpart of an instruction list in the original Mach
    selection pass.

    It is essentially a collection of blocks (stored as a layout, i.e. as a
    list), with two designated blocks:

    - an entry block;

    - an exit block.

    The exit block is where more instructions are being added, which means that
    the terminator of an in-construction "sub" CFG is `Never`, and will be
    changed only when no additional instructions will be inserted to the block.
*)

type t

val exit_has_never_terminator : t -> bool

val make_empty : unit -> t

val add_empty_block_at_start : t -> label:Label.t -> unit

val add_never_block : t -> label:Label.t -> unit

(** Use [add_instruction] in preference to this function. *)
val add_instruction_at_start :
  t ->
  Cfg.basic ->
  Reg.t array ->
  Reg.t array ->
  Debuginfo.t ->
  phantom_available_before:Backend_var.Set.t option ->
  unit

(** [add_instruction] can only be called when the terminator is [Never]. *)
val add_instruction :
  t ->
  Cfg.basic ->
  Reg.t array ->
  Reg.t array ->
  Debuginfo.t ->
  phantom_available_before:Backend_var.Set.t option ->
  unit

(** [add_instruction'] can only be called when the terminator is [Never]. *)
val add_instruction' : t -> Cfg.basic Cfg.instruction -> unit

(** [set_terminator] can only be called when the terminator is [Never]. *)
val set_terminator :
  t ->
  Cfg.terminator ->
  Reg.t array ->
  Reg.t array ->
  Debuginfo.t ->
  phantom_available_before:Backend_var.Set.t option ->
  unit

val iter_basic_blocks : t -> f:(Cfg.basic_block -> unit) -> unit

val exists_basic_blocks : t -> f:(Cfg.basic_block -> bool) -> bool

type join_branch =
  { sub_cfg : t;
    may_fall_through : bool
        (** Whether emission of the branch completed normally, rather than
            stopping because the code never returns. *)
  }

(** Branches that may not fall through are not linked to the join block. *)
val join :
  from:join_branch list ->
  to_:t ->
  phantom_available_before:Backend_var.Set.t option ->
  unit

val join_tail : from:t list -> to_:t -> unit

val update_exit_terminator :
  t ->
  Cfg.terminator ->
  arg:Reg.t array ->
  dbg:Debuginfo.t ->
  phantom_available_before:Backend_var.Set.t option ->
  unit

val start_label : t -> Label.t

(* CR-someday mshinwell: the two functions below could mostly be removed if
   instructions had an accurate debuginfo from earlier on. Those they give a
   debuginfo to fall into four groups:

   - Constants and variables forming a whole arm ([then "big"], [| None -> y]).
   [Lconst] and [Lvar] have no location in Lambda, so the [Apply_cont] of the
   arm has none, and To_cmm falls back to the location of the switch. With a
   location from Lambda, To_cmm would pass it to the constant, which keeps its
   debuginfo through selection. The arm would then also be located at its body
   rather than at the condition.

   - The moves and the jump or return implementing an [Apply_cont]. They have no
   Cmm term to take a debuginfo from: [Cexit] has no debuginfo, and a return is
   an expression in tail position. With a debuginfo on [Cexit], and an explicit
   return, selection could use it directly, and the debuginfos of the arms of
   [Cifthenelse] and [Cswitch] would not be needed for code from To_cmm.

   - Symbols. To_cmm deliberately gives them no debuginfo: that of their context
   moved the first located instruction of some functions into an inlined frame,
   which changes where a breakpoint on the function stops. They need the same
   treatment as constants, once that problem is solved.

   - The moves joining arms in value position. These only arise for
   [Cifthenelse] built by [Cmm_helpers], and would still need the debuginfo of
   the arm, or a debuginfo on the join. *)

(** [set_block_entry_debuginfo block dbg] gives the debuginfo [dbg] to the
    instructions at the start of [block] (terminator included), stopping at the
    first one that already has a debuginfo. Instructions after that one are left
    alone: in the line number table they take the location of what precedes them
    in the block. Only moves, constants, trap instructions, and terminators
    transferring control within the function or returning from it are given
    [dbg]: for other instructions a debuginfo is more than a code location. Does
    nothing if [dbg] is [Debuginfo.none]. *)
val set_block_entry_debuginfo : Cfg.basic_block -> Debuginfo.t -> unit

(** Like [set_block_entry_debuginfo], on the first non-empty block of [t]. This
    is used for the arms of [Cmm.Cifthenelse] and [Cmm.Cswitch], whose debuginfo
    is the location of the entry to the arm. It must be called before [t] is
    passed to [join] or [join_tail]. *)
val set_entry_debuginfo : t -> Debuginfo.t -> unit

(** The label of the block that instructions and the terminator are being added
    to. *)
val exit_label : t -> Label.t

val mark_as_trap_handler : t -> unit

val dump : t -> unit
