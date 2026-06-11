(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare
open! Asm_targets
open! Dwarf_low
open! Dwarf_high
module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state
module L = Linear
module RD = Reg_with_debug_info
module SLDL = Simple_location_description_lang
module V = Backend_var

(* CFA-relative offset for a register assigned to a stack slot, mirroring the
   calculation in [Available_ranges_vars.Subrange_info.create]. Returns [None]
   for registers in the domainstate area: that area is not preserved during
   calls (it is for example used to pass excess arguments), so values there
   cannot reliably be recovered by the debugger whilst the callee (or anything
   the callee calls) is executing. *)
let cfa_offset_for_stack_reg (reg : Reg.t) ~stack_offset ~fun_contains_calls
    ~fun_num_stack_slots =
  match reg.loc with
  | Reg _ | Unknown -> None
  | Stack loc -> (
    let stack_offset =
      stack_offset
      + Proc.initial_stack_offset ~contains_calls:fun_contains_calls
          ~num_stack_slots:fun_num_stack_slots
    in
    let frame_size =
      Proc.frame_size ~stack_offset ~contains_calls:fun_contains_calls
        ~num_stack_slots:fun_num_stack_slots
    in
    let slot_offset =
      Proc.slot_offset loc
        ~stack_class:(Stack_class.of_machtype reg.typ)
        ~stack_offset ~fun_contains_calls ~fun_num_stack_slots
    in
    match slot_offset with
    | Bytes_relative_to_stack_pointer i ->
      Some (Stack_reg_offset.Bytes_relative_to_cfa (frame_size - i))
    | Bytes_relative_to_domainstate_pointer _ -> None)

let single_call_value_attribute simple_location_description =
  [ DAH.create_single_call_value_location_description
      (Single_location_description.of_simple_location_description
         simple_location_description) ]

(* The [DW_AT_call_value] (GNU: [DW_AT_GNU_call_site_value]) attribute of a call
   site parameter DIE describes how to compute the value passed for the
   parameter, as an expression to be evaluated in the context of the _caller_
   (typically after the debugger has virtually unwound to it). As such only the
   following may be quoted: - constants; - spilled copies of the relevant value,
   at CFA-relative stack locations (such slots, unlike registers and the
   domainstate area, are preserved during the call). For tail calls the caller's
   frame no longer exists by the time the callee executes, so only constants may
   be quoted. *)
let call_site_value_attribute ~(arg : Reg.t)
    ~(available_before : Reg_availability_set.t) ~is_tail ~stack_offset
    ~fun_contains_calls ~fun_num_stack_slots =
  match available_before with
  | Unreachable -> []
  | Ok avail -> (
    match
      RD.Set_distinguishing_names_and_locations.find_reg_with_same_location_exn
        avail arg
    with
    | exception Not_found -> []
    | rd -> (
      match RD.debug_info rd with
      | None -> []
      | Some debug_info -> (
        match RD.Debug_info.holds_value_of debug_info with
        | Const_int i ->
          single_call_value_attribute
            (SLDL.compile
               (SLDL.of_rvalue
                  (SLDL.Rvalue.signed_int_const
                     (Targetint.of_int64 (Int64.of_nativeint i)))))
        | Const_naked_float f ->
          single_call_value_attribute
            (SLDL.compile (SLDL.of_rvalue (SLDL.Rvalue.float_const f)))
        | Const_symbol sym ->
          single_call_value_attribute
            (SLDL.compile
               (SLDL.of_rvalue
                  (SLDL.Rvalue.const_symbol (Asm_symbol.create_global sym))))
        | Var var -> (
          if is_tail
          then []
          else if RD.Debug_info.num_parts_of_value debug_info <> 1
          then [] (* Values split across registers are not handled yet. *)
          else
            (* Note that [RD.assigned_to_stack] excludes the domainstate
               area. *)
            let spilled_copy =
              RD.Set_distinguishing_names_and_locations.fold
                (fun rd' spilled_copy ->
                  match spilled_copy with
                  | Some _ -> spilled_copy
                  | None -> (
                    if not (RD.assigned_to_stack rd')
                    then None
                    else
                      match RD.debug_info rd' with
                      | None -> None
                      | Some debug_info' -> (
                        match RD.Debug_info.holds_value_of debug_info' with
                        | Var var'
                          when V.same var var'
                               && RD.Debug_info.num_parts_of_value debug_info'
                                  = 1 ->
                          Some rd'
                        | Var _ | Const_int _ | Const_naked_float _
                        | Const_symbol _ ->
                          None)))
                avail None
            in
            match spilled_copy with
            | None -> []
            | Some rd' -> (
              let reg' = RD.reg rd' in
              match
                cfa_offset_for_stack_reg reg' ~stack_offset ~fun_contains_calls
                  ~fun_num_stack_slots
              with
              | None -> []
              | Some offset ->
                single_call_value_attribute
                  (Dwarf_reg_locations.reg_location_description reg'
                     ~offset:(Some offset) ~need_rvalue:true))))))

let add_call_site_parameter ~call_site_die ~arg_index ~(arg : Reg.t)
    ~available_before ~is_tail ~stack_offset ~fun_contains_calls
    ~fun_num_stack_slots =
  match arg.loc with
  | Stack _ | Unknown ->
    (* Excess arguments are passed via the domainstate area; see above. *)
    ()
  | Reg _ -> (
    match
      call_site_value_attribute ~arg ~available_before ~is_tail ~stack_offset
        ~fun_contains_calls ~fun_num_stack_slots
    with
    | [] ->
      (* A call site parameter DIE without a means of computing the value passed
         is of no use to debuggers. *)
      ()
    | value_attribute ->
      let location_attribute =
        [ DAH.create_single_location_description
            (Single_location_description.of_simple_location_description
               (Dwarf_reg_locations.reg_location_description arg ~offset:None
                  ~need_rvalue:false)) ]
      in
      let tag : Dwarf_tag.t =
        match !Dwarf_flags.gdwarf_version with
        | Four -> Dwarf_4 GNU_call_site_parameter
        | Five -> Call_site_parameter
      in
      (* We don't give the name of the parameter since it is complicated to
         calculate (and there is currently insufficient information to perform
         the calculation if the callee is in a different compilation unit). *)
      Proto_die.create_ignore ~sort_priority:arg_index
        ~parent:(Some call_site_die) ~tag
        ~attribute_values:(location_attribute @ value_attribute)
        ())

let add_call_site state ~function_proto_die ~(insn : L.instruction)
    ~return_label ~return_label_offset_in_bytes ~is_tail ~target_attributes
    ~(args : Reg.t array) ~stack_offset ~fun_contains_calls ~fun_num_stack_slots
    =
  let position_attributes =
    (* The innermost item of the debuginfo gives the source location of the call
       itself. *)
    match List.rev (Debuginfo.to_items insn.dbg) with
    | [] -> []
    | { dinfo_file; dinfo_line; dinfo_char_start; _ } :: _ ->
      [DAH.create_call_file (DS.get_file_num state dinfo_file)]
      @ (if dinfo_line >= 0 then [DAH.create_call_line dinfo_line] else [])
      @
      if dinfo_char_start >= 0
      then [DAH.create_call_column dinfo_char_start]
      else []
  in
  let pc_attributes =
    let return_label = Asm_label.create_int Text (Label.to_int return_label) in
    match !Dwarf_flags.gdwarf_version, return_label_offset_in_bytes with
    | Four, 0 ->
      (* In DWARF-4 (GNU extension) call sites, [DW_AT_low_pc] gives the return
         address. *)
      [DAH.create_low_pc return_label]
    | Four, offset ->
      [ DAH.create_low_pc_with_offset return_label
          ~offset_in_bytes:(Targetint.of_int_exn offset) ]
    | Five, 0 -> [DAH.create_call_return_pc return_label]
    | Five, offset ->
      [ DAH.create_call_return_pc_with_offset return_label
          ~offset_in_bytes:(Targetint.of_int_exn offset) ]
  in
  let tail_call_attributes =
    (* The attribute is omitted for non-tail calls, which is interpreted as a
       value of false. *)
    if is_tail then [DAH.create_call_tail_call ~is_tail] else []
  in
  let tag : Dwarf_tag.t =
    match !Dwarf_flags.gdwarf_version with
    | Four -> Dwarf_4 GNU_call_site
    | Five -> Call_site
  in
  let call_site_die =
    Proto_die.create ~parent:(Some function_proto_die) ~tag
      ~attribute_values:
        (target_attributes @ position_attributes @ pc_attributes
       @ tail_call_attributes)
      ()
  in
  Array.iteri
    (fun arg_index arg ->
      add_call_site_parameter ~call_site_die ~arg_index ~arg
        ~available_before:insn.available_before ~is_tail ~stack_offset
        ~fun_contains_calls ~fun_num_stack_slots)
    args

let callee_attributes_from_symbol state ~fun_symbol ~demangled_name =
  let die_symbol =
    match
      Asm_symbol.Tbl.find (DS.function_abstract_instances state) fun_symbol
    with
    | _proto_die, die_symbol -> die_symbol
    | exception Not_found ->
      (* The callee may be in another compilation unit (or indeed not be an
         OCaml function at all), or merely not yet seen in this unit; in the
         latter case the DIE created here will be completed when the definition
         of the callee is encountered. *)
      let _proto_die, die_symbol =
        Dwarf_abstract_instances.add_empty state
          ~compilation_unit_proto_die:(DS.compilation_unit_proto_die state)
          ~fun_symbol ~demangled_name
      in
      die_symbol
  in
  [DAH.create_call_origin ~die_symbol]

let direct_callee_attributes state ~(callee : Cmm.symbol) =
  callee_attributes_from_symbol state
    ~fun_symbol:(Asm_symbol.create_global callee.sym_name)
    ~demangled_name:callee.sym_name

let external_callee_attributes state ~func =
  callee_attributes_from_symbol state
    ~fun_symbol:(Asm_symbol.create_global func)
    ~demangled_name:func

let indirect_callee_attributes ~(callee : Reg.t) =
  match callee.loc with
  | Reg _ ->
    (* The register holding the callee's address is clobbered by the call itself
       (hence "clobbered" in the attribute name). *)
    [ DAH.create_call_target_clobbered
        (Single_location_description.of_simple_location_description
           (Dwarf_reg_locations.reg_location_description callee ~offset:None
              ~need_rvalue:false)) ]
  | Stack _ | Unknown -> []

(* Insert a label immediately after [insn] (which the emitter will define at the
   return address of the call) and return it. *)
let insert_label_after (insn : L.instruction) =
  let label = Cmm.new_label () in
  let label_insn : L.instruction =
    { desc = L.Llabel { label; section_name = None };
      next = insn.next;
      arg = [||];
      res = [||];
      dbg = insn.dbg;
      fdo = insn.fdo;
      live = insn.live;
      available_before = insn.available_before;
      available_across = insn.available_across;
      phantom_available_before = insn.phantom_available_before
    }
  in
  insn.next <- label_insn;
  label

let dwarf state (fundecl : L.fundecl) ~function_proto_die =
  let fun_contains_calls = fundecl.fun_contains_calls in
  let fun_num_stack_slots = fundecl.fun_num_stack_slots in
  let described_a_call_site = ref false in
  let add_call_site insn ~return_label ?(return_label_offset_in_bytes = 0)
      ?(is_tail = false) ~target_attributes ~args ~stack_offset () =
    described_a_call_site := true;
    add_call_site state ~function_proto_die ~insn ~return_label
      ~return_label_offset_in_bytes ~is_tail ~target_attributes ~args
      ~stack_offset ~fun_contains_calls ~fun_num_stack_slots
  in
  let rec traverse (insn : L.instruction) ~stack_offset =
    match insn.desc with
    | Lend -> ()
    | Lcall_op call_op ->
      (match call_op with
      | Lcall_ind ->
        let callee = insn.arg.(0) in
        let args = Array.sub insn.arg 1 (Array.length insn.arg - 1) in
        let return_label = insert_label_after insn in
        add_call_site insn ~return_label
          ~target_attributes:(indirect_callee_attributes ~callee)
          ~args ~stack_offset ()
      | Lcall_imm { func } ->
        let return_label = insert_label_after insn in
        add_call_site insn ~return_label
          ~target_attributes:(direct_callee_attributes state ~callee:func)
          ~args:insn.arg ~stack_offset ()
      (* For tail calls, only constant parameter values are currently described
         (see [call_site_value_attribute] above). CR mshinwell: describe other
         parameters of tail calls using chained entry values. *)
      | Ltailcall_ind ->
        let callee = insn.arg.(0) in
        let args = Array.sub insn.arg 1 (Array.length insn.arg - 1) in
        let return_label = insert_label_after insn in
        add_call_site insn ~return_label ~is_tail:true
          ~target_attributes:(indirect_callee_attributes ~callee)
          ~args ~stack_offset ()
      | Ltailcall_imm { func } ->
        (* Call site entries are not generated for self tail calls ("tail
           recursion calls"); see DWARF-5 spec section 3.4.1, page 89, lines
           19--22. *)
        if not (String.equal func.sym_name fundecl.fun_name)
        then
          let return_label = insert_label_after insn in
          add_call_site insn ~return_label ~is_tail:true
            ~target_attributes:(direct_callee_attributes state ~callee:func)
            ~args:insn.arg ~stack_offset ()
      | Lextcall { func; alloc; stack_ofs; _ } -> (
        (* The emitter may generate instructions after the actual call
           instruction within the sequence for an [Lextcall], in which case the
           label inserted after the Linear instruction does not lie at the
           return address; compensate accordingly. *)
        match Proc.extcall_code_size_after_return_address ~alloc ~stack_ofs with
        | None -> ()
        | Some code_size_after_return_address ->
          let return_label = insert_label_after insn in
          add_call_site insn ~return_label
            ~return_label_offset_in_bytes:(-code_size_after_return_address)
            ~target_attributes:(external_callee_attributes state ~func)
            ~args:insn.arg ~stack_offset ())
      | Lprobe _ -> ());
      traverse insn.next ~stack_offset
    | Lop op ->
      let stack_offset =
        match op with
        | Stackoffset delta -> stack_offset + delta
        | _ -> stack_offset
      in
      traverse insn.next ~stack_offset
    | Lpushtrap _ ->
      traverse insn.next
        ~stack_offset:(stack_offset + Proc.trap_frame_size_in_bytes)
    | Lpoptrap _ ->
      traverse insn.next
        ~stack_offset:(stack_offset - Proc.trap_frame_size_in_bytes)
    | Ladjust_stack_offset { delta_bytes } ->
      traverse insn.next ~stack_offset:(stack_offset + delta_bytes)
    | Lprologue | Lepilogue_open | Lepilogue_close | Lreloadretaddr | Lreturn
    | Llabel _ | Lbranch _ | Lcondbranch _ | Lcondbranch3 _ | Lswitch _
    | Lentertrap | Lraise _ | Lstackcheck _ ->
      traverse insn.next ~stack_offset
  in
  traverse fundecl.fun_body ~stack_offset:0;
  (* Strictly speaking the "all calls" attribute asserts that every call in the
     function has a call site entry (DWARF-5 spec section 3.3.1.6), which is not
     currently the case here: tail calls (and probes, and on some architectures
     external calls) are not described. However LLDB will not parse the call
     site entries of a function at all unless its subprogram DIE carries this
     attribute (see [SymbolFileDWARF::CollectCallEdges]), so it is set whenever
     any call site entry exists. The deviation from the spec only risks
     confusing consumers' reconstruction of virtual tail-call frames, which
     requires call site entries for tail calls; these are never emitted at
     present. *)
  if !described_a_call_site
  then
    Proto_die.add_or_replace_attribute_value function_proto_die
      (DAH.create_call_all_calls ())
