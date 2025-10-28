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

open! Int_replace_polymorphic_compare [@@warning "-66"]

type label = Cmm.label

type phantom_defining_expr =
  | Lphantom_const_int of Targetint.t
  | Lphantom_const_symbol of string
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
      { sym : string;
        field : int
      }
  | Lphantom_block of
      { tag : int;
        fields : Backend_var.t list
      }

let lphantom_const_int i = Lphantom_const_int i

let lphantom_const_symbol s = Lphantom_const_symbol s

let lphantom_var v = Lphantom_var v

let lphantom_offset_var ~var ~offset_in_words =
  Lphantom_offset_var { var; offset_in_words }

let lphantom_read_field ~var ~field = Lphantom_read_field { var; field }

let lphantom_read_symbol_field ~sym ~field =
  Lphantom_read_symbol_field { sym; field }

let lphantom_block ~tag ~fields = Lphantom_block { tag; fields }

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
  | Lepilogue_open
  | Lepilogue_close
  | Lend
  | Lop of Operation.t
  | Lcall_op of call_operation
  | Lreloadretaddr
  | Lreturn
  | Llabel of
      { label : label;
        section_name : string option
      }
  | Lbranch of label
  | Lcondbranch of Operation.test * label
  | Lcondbranch3 of label option * label option * label option
  | Lswitch of label array
  | Lentertrap
  | Ladjust_stack_offset of { delta_bytes : int }
  | Lpushtrap of { lbl_handler : label }
  | Lpoptrap of { lbl_handler : label }
  | Lraise of Lambda.raise_kind
  | Lstackcheck of { max_frame_size_bytes : int }

and call_operation =
  | Lcall_ind
  | Lcall_imm of { func : Cmm.symbol }
  | Ltailcall_ind
  | Ltailcall_imm of { func : Cmm.symbol }
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

let has_fallthrough = function
  | Lreturn | Lbranch _ | Lswitch _ | Lraise _
  | Lcall_op Ltailcall_ind
  | Lcall_op (Ltailcall_imm _)
  | Lepilogue_close ->
    false
  | Lcall_op (Lcall_ind | Lcall_imm _ | Lextcall _ | Lprobe _)
  | Lprologue | Lepilogue_open | Lend | Lreloadretaddr | Lentertrap | Lpoptrap _
  | Lop _ | Llabel _
  | Lcondbranch (_, _)
  | Lcondbranch3 (_, _, _)
  | Ladjust_stack_offset _ | Lpushtrap _ | Lstackcheck _ ->
    true

type fundecl =
  { fun_name : string;
    fun_args : Reg.Set.t;
    fun_body : instruction;
    fun_fast : bool;
    fun_dbg : Debuginfo.t;
    fun_tailrec_entry_point_label : label option;
    fun_contains_calls : bool;
    fun_num_stack_slots : int Stack_class.Tbl.t;
    fun_frame_required : bool;
    fun_prologue_required : bool;
    fun_section_name : string option;
    fun_phantom_lets :
      (Backend_var.Provenance.t option * phantom_defining_expr)
      Backend_var.Map.t
  }

(* Invert a test *)

(* The "end" instruction *)

let rec end_instr =
  { desc = Lend;
    next = end_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    fdo = Fdo_info.none;
    live = Reg.Set.empty;
    available_before = Unreachable;
    available_across = Unreachable;
    phantom_available_before = None
  }

(* Cons an instruction (live, debug empty) *)

let instr_cons d a r n ~available_before ~available_across
    ~phantom_available_before =
  { desc = d;
    next = n;
    arg = a;
    res = r;
    dbg = Debuginfo.none;
    fdo = Fdo_info.none;
    live = Reg.Set.empty;
    available_before;
    available_across;
    phantom_available_before
  }

let traps_to_bytes traps = Proc.trap_size_in_bytes () * traps
