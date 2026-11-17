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

(* Pretty-printing of linearized machine code *)

[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@warning "-66"]
open Format
open Linear

let label ppf l = Format.fprintf ppf "L%a" Label.format l

(* The FDO counters are printed as in Cmm dumps, when there are any. *)
let callsite_counter ppf = function
  | None -> ()
  | Some counter ->
    fprintf ppf " callsite_counter(%s)" (Fdo_counter.to_string counter)

let fdo_annotation ~keyword ppf annotation =
  if not (Fdo_annotation.is_empty annotation)
  then fprintf ppf " %s%a" keyword Fdo_annotation.print annotation

let successor ppf { target; fdo_annotation = annotation } =
  fprintf ppf "%a%a" label target
    (fdo_annotation ~keyword:"fdo_annotation")
    annotation

let fallthrough_annotation =
  fdo_annotation ~keyword:"fallthrough_fdo_annotation"

let call_operation ?(print_reg = Printreg.reg) ppf op arg =
  let regs = Printreg.regs' ~print_reg in
  match op with
  | Lcall_ind { callsite_counter = counter } ->
    fprintf ppf "call %a%a" regs arg callsite_counter counter
  | Lcall_imm { func; callsite_counter = counter } ->
    fprintf ppf "call \"%s\" %a%a" func.sym_name regs arg callsite_counter
      counter
  | Ltailcall_ind { callsite_counter = counter } ->
    fprintf ppf "tailcall %a%a" regs arg callsite_counter counter
  | Ltailcall_imm { func; callsite_counter = counter } ->
    fprintf ppf "tailcall \"%s\" %a%a" func.sym_name regs arg callsite_counter
      counter
  | Lextcall { func; alloc; _ } ->
    fprintf ppf "extcall \"%s\" %a%s" func regs arg
      (if alloc then "" else " (noalloc)")
  | Lprobe { name; handler_code_sym; enabled_at_init } ->
    fprintf ppf "probe \"%s\" %s%s %a"
      (if enabled_at_init then "enabled_at_init " else "")
      name handler_code_sym regs arg

let instr' ?(print_reg = Printreg.reg) ppf i =
  let reg = print_reg in
  let regs = Printreg.regs' ~print_reg in
  let regsetaddr = Printreg.regsetaddr' ~print_reg in
  let test = Operation.format_test ~print_reg in
  let operation = Printoperation.operation ~print_reg in
  if !Oxcaml_flags.davail || !Dwarf_flags.debug_avail_sets
  then (
    let module RAS = Reg_availability_set in
    let ras_is_nonempty (set : RAS.t) =
      match set with
      | Ok set ->
        not
          (Reg_with_debug_info.Set_distinguishing_names_and_locations.is_empty
             set)
      | Unreachable -> true
    in
    if ras_is_nonempty i.available_before || ras_is_nonempty i.available_across
    then
      if RAS.equal i.available_before i.available_across
      then
        fprintf ppf "@[<1>AB=AA={%a}@]@," (RAS.print ~print_reg:reg)
          i.available_before
      else (
        fprintf ppf "@[<1>AB={%a}" (RAS.print ~print_reg:reg) i.available_before;
        fprintf ppf ",AA={%a}" (RAS.print ~print_reg:reg) i.available_across;
        fprintf ppf "@]@,");
    match i.phantom_available_before with
    | None -> ()
    | Some phantom_vars ->
      if not (Backend_var.Set.is_empty phantom_vars)
      then fprintf ppf "@[<1>PAB={%a}@]@," Backend_var.Set.print phantom_vars);
  (match i.desc with
  | Lend -> ()
  | Lprologue -> fprintf ppf "prologue"
  | Lepilogue_open -> fprintf ppf "epilogue_open"
  | Lepilogue_close -> fprintf ppf "epilogue_close"
  | Lop op ->
    (match[@warning "-4"] op with
    | Alloc _ | Poll -> fprintf ppf "@[<1>{%a}@]@," regsetaddr i.live
    | _ -> ());
    operation op i.arg ppf i.res
  | Lcall_op op ->
    (match op with
    | Lcall_ind _ | Lcall_imm _ | Lextcall _ | Lprobe _ ->
      fprintf ppf "@[<1>{%a}@]@," regsetaddr i.live
    | Ltailcall_imm _ | Ltailcall_ind _ -> ());
    call_operation ppf op i.arg
  | Lreloadretaddr -> fprintf ppf "reload retaddr"
  | Lreturn -> fprintf ppf "return %a" regs i.arg
  | Llabel_for_jump_target lbl -> fprintf ppf "%a:" label lbl
  | Llabel_for_dwarf lbl -> fprintf ppf "%a: (DWARF only label)" label lbl
  | Lbranch lbl -> fprintf ppf "goto %a" label lbl
  | Lcondbranch { test = tst; taken; fallthrough_annotation = annotation } ->
    fprintf ppf "if %a goto %a%a" (test tst) i.arg successor taken
      fallthrough_annotation annotation
  | Lcondbranch3 { lt; eq; gt; fallthrough_annotation = annotation } ->
    fprintf ppf "switch3 %a%a" reg i.arg.(0) fallthrough_annotation annotation;
    let case n = function
      | None -> ()
      | Some lbl -> fprintf ppf "@,case %i: goto %a" n successor lbl
    in
    case 0 lt;
    case 1 eq;
    case 2 gt;
    fprintf ppf "@,endswitch"
  | Lswitch successors ->
    fprintf ppf "switch %a" reg i.arg.(0);
    Array.iteri
      (fun i lbl -> fprintf ppf "case %i: goto %a" i successor lbl)
      successors;
    fprintf ppf "@,endswitch"
  | Lentertrap -> fprintf ppf "enter trap"
  | Ladjust_stack_offset { delta_bytes } ->
    fprintf ppf "adjust pseudo stack offset by %d bytes" delta_bytes
  | Lpushtrap { lbl_handler } -> fprintf ppf "push trap %a" label lbl_handler
  | Lpoptrap { lbl_handler } -> fprintf ppf "pop trap %a" label lbl_handler
  | Lraise k -> fprintf ppf "%s %a" (Lambda.raise_kind k) reg i.arg.(0)
  | Lstackcheck { max_frame_size_bytes } ->
    fprintf ppf "stack check (%d bytes)" max_frame_size_bytes);
  if (not (Debuginfo.is_none i.dbg)) && !Clflags.locations
  then fprintf ppf " %s" (Debuginfo.to_string i.dbg)

let instr ppf i = instr' ppf i

let rec all_instr ppf i =
  match[@warning "-4"] i.desc with
  | Lend -> ()
  | _ -> fprintf ppf "%a@,%a" instr i all_instr i.next

let fundecl ppf f =
  let dbg =
    if Debuginfo.is_none f.fun_dbg || not !Clflags.locations
    then ""
    else " " ^ Debuginfo.to_string f.fun_dbg
  in
  let entry_annotation ppf entry_annotation =
    if not (Fdo_annotation.With_body_hash.is_empty entry_annotation)
    then
      fprintf ppf " fdo_entry_annotation%a" Fdo_annotation.With_body_hash.print
        entry_annotation
  in
  fprintf ppf "@[<v 2>%s:%s%a@,%a@]" f.fun_name dbg entry_annotation
    f.fun_fdo_entry_annotation all_instr f.fun_body
