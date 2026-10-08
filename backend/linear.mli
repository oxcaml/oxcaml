(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Transformation of Mach code into a list of pseudo-instructions. *)

[@@@ocaml.warning "+a-40-41-42"]

(* CR sspies: Consider using [Asm_label.t] for this label to avoid duplication
   in the assembly backends. *)
type label = Cmm.label

type phantom_defining_expr = private
  | Lphantom_const_int of Targetint.t
  | Lphantom_const_symbol of Cmm.symbol
  | Lphantom_var of Backend_var.t
  | Lphantom_offset_var of
      { var : Backend_var.t;
        offset_in_words : int
      }
  | Lphantom_read_field of
      { var : Backend_var.t;
        field : int
      }
  | Lphantom_read_symbol_field of
      { sym : Cmm.symbol;
        field : int
      }
  | Lphantom_block of
      { tag : int;
        fields : Backend_var.t list
      }
  | Lphantom_optimised_out

val lphantom_const_int : Targetint.t -> phantom_defining_expr

val lphantom_optimised_out : phantom_defining_expr

val lphantom_const_symbol : Cmm.symbol -> phantom_defining_expr

val lphantom_var : Backend_var.t -> phantom_defining_expr

val lphantom_offset_var :
  var:Backend_var.t -> offset_in_words:int -> phantom_defining_expr

val lphantom_read_field :
  var:Backend_var.t -> field:int -> phantom_defining_expr

val lphantom_read_symbol_field :
  sym:Cmm.symbol -> field:int -> phantom_defining_expr

val lphantom_block :
  tag:int -> fields:Backend_var.t list -> phantom_defining_expr

(** The pseudo-instrumentation counters (see [Fdo_counter]) of a control-flow
    edge. *)
type fdo_counters = Fdo_counter.t list

(** A jump target, with the counters of the edge the jump takes. *)
type successor =
  { target : label;
    fdo_counters : fdo_counters
  }

type instruction =
  { mutable desc : instruction_desc;
    mutable next : instruction;
    arg : Reg.t array;
    res : Reg.t array;
    dbg : Debuginfo.t;
    fdo : Fdo_info.t;
    live : Reg.Set.t;
    available_before : Reg_availability_set.t;
    available_across : Reg_availability_set.t;
    phantom_available_before : Backend_var.Set.t option
  }

and instruction_desc =
  | Lprologue
    (* [Lepilogue_open] and [Lepilogue_close] shrink the stack on exiting a
       function. They are split so that the terminator can be emitted between
       them, to maintain the correct debug information.

       [Lepilogue_open] reverts the stack pointer and adjusts the CFA offset in
       preparation for the function ending, and [Lepilogue_close] adjusts the
       CFA offset back in case the function continues.

       The two instructions should be paired together, with the terminator
       between them. Any additional instructions between them that affect the
       stack pointer and/or CFA will likely cause incorrect results. *)
  | Lepilogue_open
  | Lepilogue_close
  | Lend
  | Lop of Operation.t
  | Lcall_op of call_operation
  | Lreloadretaddr
  | Lreturn
  | Llabel_for_jump_target of label
  | Llabel_for_dwarf of label
      (** Only delimits DWARF ranges, never a jump target. *)
  | Lbranch of label
  | Lcondbranch of
      { test : Operation.test;
        taken : successor;
        fallthrough_counters : fdo_counters
            (** the counters of the edge to the next instruction *)
      }
  | Lcondbranch3 of
      { lt : successor option;
        eq : successor option;
        gt : successor option;
        fallthrough_counters : fdo_counters
            (** the counters of the edge to the next instruction, taken for the
                outcomes without a jump *)
      }
  | Lswitch of successor array
  | Lentertrap
  | Ladjust_stack_offset of { delta_bytes : int }
  | Lpushtrap of { lbl_handler : label }
  | Lpoptrap of { lbl_handler : label }
  | Lraise of Lambda.raise_kind
  | Lstackcheck of { max_frame_size_bytes : int }

(* [callsite_counter] is the pseudo-instrumentation counter of the call site,
   joined at profile decoding time with the entry counter of the function the
   call lands in. *)
and call_operation =
  | Lcall_ind of { callsite_counter : Fdo_counter.t option }
  | Lcall_imm of
      { func : Cmm.symbol;
        callsite_counter : Fdo_counter.t option
      }
  | Ltailcall_ind of { callsite_counter : Fdo_counter.t option }
  | Ltailcall_imm of
      { func : Cmm.symbol;
        callsite_counter : Fdo_counter.t option
      }
  | Lextcall of
      { func : string;
        ty_res : Cmm.machtype;
        ty_args : Cmm.exttype list;
        alloc : bool;
        returns : bool;
        stack_ofs : int;
        stack_align : Cmm.stack_align
      }
  | Lprobe of
      { name : string;
        handler_code_sym : string;
        enabled_at_init : bool
      }

val has_fallthrough : instruction_desc -> bool

val end_instr : instruction

val instr_cons :
  instruction_desc ->
  Reg.t array ->
  Reg.t array ->
  instruction ->
  available_before:Reg_availability_set.t ->
  available_across:Reg_availability_set.t ->
  phantom_available_before:Backend_var.Set.t option ->
  instruction

type fundecl =
  { fun_name : string;
    fun_args : Reg.Set.t;
    fun_body : instruction;
    fun_fast : bool;
    fun_dbg : Debuginfo.t;
    fun_fdo_entry_counters : fdo_counters;
        (** the counters of the function's entry edge: its own entry counter
            first, then those of the calls inlined at the head of its body *)
    fun_function_body_hash : Fdo_counter.Function_body_hash.t option;
        (** the body hash its interior counters were numbered with *)
    fun_tailrec_entry_point_label : label option;
    fun_contains_calls : bool;
    fun_num_stack_slots : int Stack_class.Tbl.t;
    fun_frame_required : bool;
    fun_prologue_required : bool;
    fun_phantom_lets :
      (Backend_var.Provenance.t option * phantom_defining_expr)
      Backend_var.Map.t
  }

val traps_to_bytes : int -> int
