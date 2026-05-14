(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Int_replace_polymorphic_compare
open! Dwarf_low
open! Dwarf_high
module SLDL = Simple_location_description_lang

let reg_location_description (reg : Reg.t) ~(offset : Stack_reg_offset.t option)
    ~need_rvalue =
  let module SLD = Simple_location_description in
  (* CR-someday mshinwell: Implement support for values split across registers,
     if any such still exist. *)
  let sldl =
    match reg.loc with
    | Unknown ->
      Misc.fatal_errorf "Register without location: %a" Printreg.reg reg
    | Reg phys_reg ->
      let dwarf_reg_number = Regs.dwarf_reg_number reg.typ phys_reg in
      if not need_rvalue
      then SLDL.of_lvalue (SLDL.Lvalue.in_register ~dwarf_reg_number)
      else SLDL.of_rvalue (SLDL.Rvalue.in_register ~dwarf_reg_number)
    | Stack _ -> (
      match offset with
      | None ->
        Misc.fatal_errorf
          "Register %a assigned to stack but no offset from CFA or domainstate \
           pointer provided"
          Printreg.reg reg
      | Some offset -> (
        match offset with
        | Bytes_relative_to_cfa offset_in_bytes ->
          if offset_in_bytes mod Arch.size_addr <> 0
          then
            Misc.fatal_errorf "Misaligned stack slot at offset %d (reg %a)"
              offset_in_bytes Printreg.reg reg;
          let offset_in_words =
            Targetint.of_int_exn (offset_in_bytes / Arch.size_addr)
          in
          if not need_rvalue
          then SLDL.of_lvalue (SLDL.Lvalue.in_stack_slot ~offset_in_words)
          else SLDL.of_rvalue (SLDL.Rvalue.in_stack_slot ~offset_in_words)
        | Bytes_relative_to_domainstate_pointer offset_in_bytes ->
          if offset_in_bytes mod Arch.size_addr <> 0
          then
            Misc.fatal_errorf
              "Misaligned domainstate slot at offset %d (reg %a)"
              offset_in_bytes Printreg.reg reg;
          let offset_in_words =
            Targetint.of_int_exn (offset_in_bytes / Arch.size_addr)
          in
          let domainstate_ptr_dwarf_register_number =
            Proc.domainstate_ptr_dwarf_register_number
          in
          if not need_rvalue
          then
            SLDL.of_lvalue
              (SLDL.Lvalue.in_domainstate_slot ~offset_in_words
                 ~domainstate_ptr_dwarf_register_number)
          else
            SLDL.of_rvalue
              (SLDL.Rvalue.in_domainstate_slot ~offset_in_words
                 ~domainstate_ptr_dwarf_register_number)))
  in
  SLDL.compile sldl
