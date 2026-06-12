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

(* If the value held in [reg] (at the relevant call point) also has a spilled
   copy, at a CFA-relative stack location -- which, unlike registers and the
   domainstate area, is preserved during the call -- then return an rvalue
   describing the contents of such location. *)
let spilled_copy_rvalue ~(reg : Reg.t) ~avail ~stack_offset ~fun_contains_calls
    ~fun_num_stack_slots : _ SLDL.Rvalue.t option =
  match
    RD.Set_distinguishing_names_and_locations.find_reg_with_same_location_exn
      avail reg
  with
  | exception Not_found -> None
  | rd -> (
    match RD.debug_info rd with
    | None -> None
    | Some debug_info -> (
      match RD.Debug_info.holds_value_of debug_info with
      | Const_int _ | Const_naked_float _ | Const_symbol _ ->
        (* CR mshinwell: is it useful to return [Some] here instead? *)
        None
      | Var var -> (
        if RD.Debug_info.num_parts_of_value debug_info <> 1
        then None (* Values split across registers are not handled yet. *)
        else
          (* Note that [RD.assigned_to_stack] excludes the domainstate area. *)
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
                             && RD.Debug_info.num_parts_of_value debug_info' = 1
                        ->
                        Some rd'
                      | Var _ | Const_int _ | Const_naked_float _
                      | Const_symbol _ ->
                        None)))
              avail None
          in
          match spilled_copy with
          | None -> None
          | Some rd' -> (
            let reg' = RD.reg rd' in
            match
              cfa_offset_for_stack_reg reg' ~stack_offset ~fun_contains_calls
                ~fun_num_stack_slots
            with
            | None -> None
            | Some (Bytes_relative_to_cfa offset_in_bytes) ->
              if offset_in_bytes mod Arch.size_addr <> 0
              then None
              else
                Some
                  (SLDL.Rvalue.in_stack_slot
                     ~offset_in_words:
                       (Targetint.of_int_exn (offset_in_bytes / Arch.size_addr)))
            | Some (Bytes_relative_to_domainstate_pointer _) -> None))))

let dwarf_version () =
  match !Dwarf_flags.gdwarf_version with
  | Four -> Dwarf_version.four
  | Five -> Dwarf_version.five

(* If the given variable is a parameter of the function being compiled that
   arrived in a register -- only parameters are available at the function's
   entry point, whose availability set is [entry_available] -- then return an
   rvalue describing the value of the variable as the DWARF "entry value" of
   that register. Variables at this level are immutable, so such a description
   is valid at any point in the function. Debuggers recover entry values by
   virtually unwinding to the caller and consulting the call site information
   there; in particular such descriptions remain valid at tail call sites,
   whose enclosing frame has been destroyed by the time the callee executes
   (recovery then chains through the tail-calling function's own incoming call
   site information, one frame further up). *)
let entry_value_rvalue ~(entry_available : Reg_availability_set.t) var :
    _ SLDL.Rvalue.t option =
  match entry_available with
  | Unreachable -> None
  | Ok entry_avail ->
    RD.Set_distinguishing_names_and_locations.fold
      (fun rd entry_value ->
        match entry_value with
        | Some _ -> entry_value
        | None -> (
          match RD.debug_info rd with
          | None -> None
          | Some debug_info -> (
            match RD.Debug_info.holds_value_of debug_info with
            | Var var'
              when V.same var var'
                   && RD.Debug_info.num_parts_of_value debug_info = 1 -> (
              let reg = RD.reg rd in
              match reg.loc with
              | Reg phys_reg ->
                let dwarf_reg_number = Regs.dwarf_reg_number reg.typ phys_reg in
                Some
                  (SLDL.Rvalue.entry_value_of_register ~dwarf_reg_number
                     (dwarf_version ()))
              | Stack _ | Unknown -> None)
            | Var _ | Const_int _ | Const_naked_float _ | Const_symbol _ ->
              None)))
      entry_avail None

(* The [DW_AT_call_value] (GNU: [DW_AT_GNU_call_site_value]) attribute of a call
   site parameter DIE describes how to compute the value passed for the
   parameter, as an expression to be evaluated in the context of the _caller_
   (typically after the debugger has virtually unwound to it). As such only the
   following may be specified:

   - constants;

   - spilled copies of the relevant value, at CFA-relative stack locations (such
   slots, unlike registers and the domainstate area, are preserved during the
   call);

   - entry values (see [entry_value_rvalue] above), when the value passed is a
   parameter of the calling function.

   For tail calls the caller's frame no longer exists by the time the callee
   executes, so only constants and entry values may be quoted. *)
let call_site_value_attribute ~(arg : Reg.t)
    ~(available_before : Reg_availability_set.t) ~entry_available ~is_tail
    ~stack_offset ~fun_contains_calls ~fun_num_stack_slots =
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
                  (Dwarf_reg_locations.address_of_cmm_symbol_rvalue sym)))
        | Var var -> (
          let rvalue =
            (* Spilled copies are preferred to entry values: the debugger can
               read them directly, without chaining through further call site
               information. They cannot however be used at tail call sites
               (the frame containing the spilled copy no longer exists when
               the callee executes). *)
            let spilled_copy =
              if is_tail
              then None
              else
                spilled_copy_rvalue ~reg:arg ~avail ~stack_offset
                  ~fun_contains_calls ~fun_num_stack_slots
            in
            match spilled_copy with
            | Some _ -> spilled_copy
            | None -> entry_value_rvalue ~entry_available var
          in
          match rvalue with
          | None -> []
          | Some rvalue ->
            single_call_value_attribute (SLDL.compile (SLDL.of_rvalue rvalue))
          ))))

let add_call_site_parameter ~call_site_die ~arg_index ~(arg : Reg.t)
    ~available_before ~entry_available ~is_tail ~stack_offset
    ~fun_contains_calls ~fun_num_stack_slots =
  match arg.loc with
  | Stack _ | Unknown ->
    (* Excess arguments are passed via the domainstate area; see above. *)
    ()
  | Reg _ -> (
    match
      call_site_value_attribute ~arg ~available_before ~entry_available
        ~is_tail ~stack_offset ~fun_contains_calls ~fun_num_stack_slots
    with
    | [] ->
      (* A call site parameter DIE without a means of computing the value passed
         is of no use to debuggers. *)
      (* CR mshinwell: does it produce an LLDB warning though? *)
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
    ~(args : Reg.t array) ~entry_available ~stack_offset ~fun_contains_calls
    ~fun_num_stack_slots =
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
        ~available_before:insn.available_before ~entry_available ~is_tail
        ~stack_offset ~fun_contains_calls ~fun_num_stack_slots)
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

let indirect_callee_attributes ~(callee : Reg.t) ~(args : Reg.t array)
    ~(available_before : Reg_availability_set.t) ~entry_available ~is_tail
    ~stack_offset ~fun_contains_calls ~fun_num_stack_slots =
  let closure_rvalue =
    (* The code pointer for an OCaml indirect call lies within the closure (more
       precisely the function slot), which is passed as the final argument. If
       the closure is statically allocated (a symbol), has a spilled copy
       surviving the call, or is a parameter of the function being compiled
       (whose value may be described as the entry value of the register in
       which it arrived), then the code pointer can be recomputed by the
       debugger in the context of the caller's frame even after the call. This
       permits the use of [DW_AT_call_target] rather than the "clobbered"
       variant: debuggers cannot identify callees, in particular when resolving
       entry values during virtual unwinding, from clobbered target expressions.
       (Spilled copies are not usable for tail calls, whose frames no longer
       exist by the time the callee executes; statically-allocated closures and
       entry values are always usable.) *)
    if Array.length args < 1
    then None
    else
      match available_before with
      | Unreachable -> None
      | Ok avail -> (
        let closure = args.(Array.length args - 1) in
        match closure.loc with
        | Stack _ | Unknown -> None
        | Reg _ -> (
          match
            RD.Set_distinguishing_names_and_locations
            .find_reg_with_same_location_exn avail closure
          with
          | exception Not_found -> None
          | rd -> (
            match RD.debug_info rd with
            | None -> None
            | Some debug_info -> (
              match RD.Debug_info.holds_value_of debug_info with
              | Const_symbol sym ->
                Some (Dwarf_reg_locations.address_of_cmm_symbol_rvalue sym)
              | Var var -> (
                let spilled_copy =
                  if is_tail
                  then None
                  else
                    spilled_copy_rvalue ~reg:closure ~avail ~stack_offset
                      ~fun_contains_calls ~fun_num_stack_slots
                in
                match spilled_copy with
                | Some _ -> spilled_copy
                | None -> entry_value_rvalue ~entry_available var)
              | Const_int _ | Const_naked_float _ -> None))))
  in
  match closure_rvalue with
  | Some closure ->
    (* The code pointer that an [Lcall_ind] branches to was loaded from a fixed
       field of the function slot, chosen when the call was compiled: field 0
       for single-argument calls (generic applications of one argument, see
       [Cmm_helpers.apply_or_call_caml_apply], and full applications of
       single-parameter functions, see [Cmm_helpers.indirect_full_call]); field
       2 for full applications passing more than one argument
       ([Cmm_helpers.indirect_full_call], and likewise the [caml_applyN] inline
       fast path, whose arity guard ensures full application). Generic
       applications of more than one argument, including overapplications, are
       direct calls to [caml_applyN] and do not arise here. The closure
       information word must not be consulted instead: at a field-0 call site
       the closure may be of any arity (for example a partial application
       entering [caml_curryN], or a tupled function, whose arity is encoded
       negatively), yet field 0 is still the code pointer that was called. CR
       mshinwell: the field selection in [Cmm_helpers.indirect_full_call] and
       the function slot layout key on the number of source parameters (see
       [args_ty] in [To_cmm_expr.translate_apply0] and
       [To_cmm_set_of_closures.get_func_decl_params_arity]), whereas machine
       arguments are counted here. These differ only for known-arity indirect
       full applications of functions whose single parameter is unarized to
       multiple machine registers (an unboxed product): field 2 is selected here
       where the call site loaded field 0. Fixing this would require propagating
       the field index from Cmm to Linear. The consequence is only that the
       debugger will fail to identify the callee for such call sites. *)
    let num_value_args = Array.length args - 1 in
    let field = if num_value_args <= 1 then 0 else 2 in
    [ DAH.create_call_target
        (Single_location_description.of_simple_location_description
           (SLDL.compile
              (SLDL.of_rvalue
                 (SLDL.Rvalue.read_field_unguarded ~block:closure
                    ~field:(Targetint.of_int_exn field))))) ]
  | None -> (
    match callee.loc with
    | Reg _ ->
      (* The register holding the callee's address is clobbered by the call
         itself (hence "clobbered" in the attribute name). *)
      [ DAH.create_call_target_clobbered
          (Single_location_description.of_simple_location_description
             (Dwarf_reg_locations.reg_location_description callee ~offset:None
                ~need_rvalue:false)) ]
    | Stack _ | Unknown -> [])

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
  (* The availability set at the function's entry point, which determines the
     registers in which the function's parameters arrived (see
     [entry_value_rvalue] above). The registers are not associated with the
     parameters until the [Name_for_debugger] operations at the start of the
     function's body have been processed, so the availability set is taken
     from just after any such leading operations (at which point no register
     has yet been modified). *)
  let entry_available =
    let rec after_leading_naming_ops (insn : L.instruction) =
      match insn.desc with
      | Lprologue | Llabel _ | Lop (Name_for_debugger _) ->
        after_leading_naming_ops insn.next
      | _ -> insn.available_before
    in
    after_leading_naming_ops fundecl.fun_body
  in
  let described_a_call_site = ref false in
  let add_call_site insn ~return_label ?(return_label_offset_in_bytes = 0)
      ?(is_tail = false) ~target_attributes ~args ~stack_offset () =
    described_a_call_site := true;
    add_call_site state ~function_proto_die ~insn ~return_label
      ~return_label_offset_in_bytes ~is_tail ~target_attributes ~args
      ~entry_available ~stack_offset ~fun_contains_calls ~fun_num_stack_slots
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
          ~target_attributes:
            (indirect_callee_attributes ~callee ~args
               ~available_before:insn.available_before ~entry_available
               ~is_tail:false ~stack_offset ~fun_contains_calls
               ~fun_num_stack_slots)
          ~args ~stack_offset ()
      | Lcall_imm { func } ->
        let return_label = insert_label_after insn in
        add_call_site insn ~return_label
          ~target_attributes:(direct_callee_attributes state ~callee:func)
          ~args:insn.arg ~stack_offset ()
      (* For tail calls, parameter values are described using constants and
         chained entry values; likewise the targets of indirect tail calls
         (see [call_site_value_attribute] and [indirect_callee_attributes]
         above). *)
      | Ltailcall_ind ->
        let callee = insn.arg.(0) in
        let args = Array.sub insn.arg 1 (Array.length insn.arg - 1) in
        let return_label = insert_label_after insn in
        add_call_site insn ~return_label ~is_tail:true
          ~target_attributes:
            (indirect_callee_attributes ~callee ~args
               ~available_before:insn.available_before ~entry_available
               ~is_tail:true ~stack_offset ~fun_contains_calls
               ~fun_num_stack_slots)
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
     currently the case here: probes, self tail calls and, on architectures
     where [Proc.extcall_code_size_after_return_address] returns [None],
     external calls are not described. However LLDB will not parse the call
     site entries of a function at all unless its subprogram DIE carries this
     attribute (see [SymbolFileDWARF::CollectCallEdges]), so it is set whenever
     any call site entry exists. *)
  if !described_a_call_site
  then
    Proto_die.add_or_replace_attribute_value function_proto_die
      (DAH.create_call_all_calls ())
