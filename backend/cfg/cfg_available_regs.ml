open! Int_replace_polymorphic_compare
module R = Reg
module RAS = Reg_availability_set
module RD = Reg_with_debug_info

module RD_quotient_set =
  Reg_with_debug_info.Set_distinguishing_names_and_locations

let dprintf fmt =
  if !Dwarf_flags.ddebug_available_regs
  then Format.eprintf fmt
  else Format.ifprintf Format.err_formatter fmt

(* If permitted to do so by the command line flags, this pass will extend live
   ranges for otherwise dead but available registers across allocations, polls
   and calls when it is safe to do so. This allows the values of more variables
   to be seen in the debugger, for example when the last use of some variable is
   just before a call, and the debugger is standing in the callee. It may
   however affect the semantics of e.g. finalizers. *)
(* CR mshinwell: We've seen a bug when enabling this, so it should be assumed
   codegen is broken when enabled. *)
let extend_live () = !Dwarf_flags.gdwarf_may_alter_codegen_experimental

(* CR mshinwell: The old "all_regs_that_might_be_named" (see git history) seems
   broken, I think a backwards dataflow pass may be necessary to compute this *)

let check_invariants : type a.
    a Cfg.instruction ->
    print_instr:(Format.formatter -> a Cfg.instruction -> unit) ->
    avail_before:RAS.t ->
    unit =
 fun instr ~print_instr ~avail_before ->
  match avail_before with
  | Unreachable -> ()
  | Ok avail_before ->
    (* Every register that is live across an instruction should also be
       available before the instruction. *)
    let live = instr.live in
    if
      not
        (R.Set.subset live
           (RD.Set_distinguishing_names_and_locations.forget_debug_info
              avail_before))
    then
      Misc.fatal_errorf
        "Named live registers not a subset of available registers: live={%a}  \
         avail_before=%a missing={%a} insn=%a"
        Printreg.regset live
        (RAS.print ~print_reg:Printreg.reg)
        (RAS.Ok avail_before) Printreg.regset
        (R.Set.diff live
           (RD.Set_distinguishing_names_and_locations.forget_debug_info
              avail_before))
        print_instr instr;
    (* Every register that is an input to an instruction should be available. *)
    let args = R.set_of_array instr.arg in
    let avail_before_fdi =
      RD.Set_distinguishing_names_and_locations.forget_debug_info avail_before
    in
    if not (R.Set.subset args avail_before_fdi)
    then
      Misc.fatal_errorf
        "Instruction has unavailable input register(s): avail_before=%a \
         avail_before_fdi={%a} inputs={%a} insn=%a"
        (RAS.print ~print_reg:Printreg.reg)
        (RAS.Ok avail_before) Printreg.regset avail_before_fdi Printreg.regset
        args print_instr instr

module Domain = struct
  type t = { avail_before : RAS.t } [@@unboxed]

  let bot = { avail_before = Unreachable }

  let ras_print = RAS.print ~print_reg:Printreg.reg

  let join ({ avail_before = left_avail } : t)
      ({ avail_before = right_avail } : t) : t =
    let res =
      { avail_before =
          RAS.inter_removing_conflicting_debug_info left_avail right_avail
      }
    in
    dprintf "JOIN: %a\n%a\n->%a\n%!" ras_print left_avail ras_print right_avail
      ras_print res.avail_before;
    res

  let less_equal x y =
    let res =
      match join y x with
      | { avail_before = joined } ->
        let { avail_before = y } = y in
        RAS.equal joined y
    in
    dprintf "LESS_EQUAL: %a\n%a\n->%b\n%!" ras_print x.avail_before ras_print
      y.avail_before res;
    res
end

(* [Transfer] calculates, given the registers "available before" an instruction
   [instr], the registers that are available both "across" and immediately after
   [instr]. This is a forwards dataflow analysis.

   "available before" can be thought of, at the assembly level, as the set of
   registers available when the program counter is equal to the address of the
   particular instruction under consideration (that is to say, immediately prior
   to the instruction being executed). Inputs to that instruction are available
   at this point even if the instruction will clobber them. Results from the
   previous instruction are also available at this point.

   "available across" is the registers available during the execution of some
   particular instruction. These are the registers "available before" minus
   registers that may be clobbered or otherwise invalidated by the instruction.
   (The notion of "available across" is only useful for [Op] instructions.
   Recall that some of these may expand into multiple machine instructions
   including clobbers, e.g. for [Alloc].)

   The [available_before] and [available_across] fields of each instruction are
   updated by the transfer functions. *)
module Transfer = struct
  type domain = Domain.t

  type context = unit

  type image =
    { normal : domain;
      exceptional : domain
    }

  let unreachable = RAS.Unreachable

  let ok set = RAS.Ok set

  let[@inline] common : type a.
      avail_before:RD_quotient_set.t ->
      destroyed_at:(a -> Reg.t array) ->
      is_interesting_constructor:(a -> bool) ->
      is_end_region:(a -> bool) ->
      a Cfg.instruction ->
      RAS.t * RAS.t =
   fun ~avail_before ~destroyed_at ~is_interesting_constructor ~is_end_region
       instr ->
    (* We split the calculation of registers that become unavailable after a
       call into two parts. First: anything that the target marks as destroyed
       by the operation, combined with any registers that will be clobbered by
       the operation writing out its results. *)
    let made_unavailable_1 =
      let regs_clobbered = Array.append (destroyed_at instr.desc) instr.res in
      RD_quotient_set.made_unavailable_by_clobber avail_before ~regs_clobbered
    in
    (* Second: the cases of (a) allocations, (b) other polling points, (c) OCaml
       to OCaml function calls and (d) end-region operations. In these cases,
       since the GC may run, registers always become unavailable unless: (a)
       they are "live across" the instruction; and/or (b) they hold immediates
       and are assigned to the stack. For the moment we assume that [Ispecific]
       instructions do not run the GC. *)
    (* CR-someday mshinwell: Consider factoring this out from here and
       [Available_ranges.Make_ranges.end_pos_offset]. *)
    let made_unavailable_2 =
      match is_interesting_constructor instr.desc with
      | true ->
        (* CR gyorsh/mshinwell: There are some tricky corner cases here. I just
           wanted to write down a comment about it, nothing to fix here.

           Machtypes associated with Reg.t may be wrong at this point due to
           aliasing with hardregs and elided moves, even for instructions that
           need a frametable entry. The only guarantee we have for these program
           points is that for every physical location that may have a value that
           is live across, there is a register in the live set that has the
           correct type, and hence the location will be updated. It's possible
           that there is another Reg.t in the live set for which the location is
           the same but the type is different, and it's not the one that we put
           in avail_before. This could lead to dropping debug info associated
           with a register. For registers that are not in the live set and whose
           type is not a pointer, they may still be containing values (that are
           not live across) and we shouldn't try to inspect them. *)
        RD_quotient_set.filter
          (fun reg ->
            let holds_immediate = RD.holds_non_pointer reg in
            let on_stack = RD.assigned_to_stack reg in
            let live_across =
              (* Note that this can't check register stamps, since a no-op move
                 might have been elided, and the stamps might not match between
                 the live set and a [Reg.t] we have which is actually in it. *)
              Reg.Set.exists
                (fun live_reg ->
                  Reg.same_loc_fatal_on_unknown
                    ~fatal_message:
                      "Found Unknown register location, but we should now be \
                       post-register allocation"
                    live_reg (RD.reg reg))
                instr.live
            in
            let remains_available =
              live_across || (holds_immediate && on_stack)
            in
            if
              remains_available
              || (not (extend_live ()))
              || is_end_region instr.desc
              || (not (RD.assigned_to_stack reg))
              || RD_quotient_set.mem reg made_unavailable_1
              || Reg.is_of_type_addr (RD.reg reg)
            then not remains_available
            else (
              instr.live <- Reg.Set.add (RD.reg reg) instr.live;
              false))
          avail_before
      | false -> RD_quotient_set.empty
    in
    let made_unavailable =
      RD_quotient_set.union made_unavailable_1 made_unavailable_2
    in
    let avail_across = RD_quotient_set.diff avail_before made_unavailable in
    let avail_after =
      let res = Reg.set_of_array instr.res in
      RD_quotient_set.union
        (RD_quotient_set.without_debug_info res)
        avail_across
    in
    dprintf "...avail_before %a\n%!" RD_quotient_set.print avail_before;
    dprintf "...avail_across %a\n%!" RD_quotient_set.print avail_across;
    dprintf "...avail_after %a\n%!" RD_quotient_set.print avail_after;
    ok avail_across, ok avail_after

  let basic ({ avail_before } : domain) (instr : Cfg.basic Cfg.instruction) () :
      domain =
    dprintf "Instruction: (id=%a) %a\n%!" InstructionId.print instr.id
      Cfg.print_basic instr;
    instr.available_before <- avail_before;
    if !Dwarf_flags.ddebug_invariants
    then check_invariants instr ~print_instr:Cfg.print_basic ~avail_before;
    (* CR gyorsh/mshinwell: There is something subtle here about the handling of
       Name_for_debugger in Cfg_availability : the field regs is part of the
       operation description (not instruction argument or result), and it starts
       with pseudo-registers. We rely on the mutability of Reg.loc to update
       these regs to physical locations. Right? If so, there is nothing that
       forces these registers to be updated throughout the backend, or preserve
       sharing of Reg.t. For example, register allocation can introduce new
       temporaries in the split phase and substitute them into instructions,
       leaving old temporaries in Name_for_debugger only. This could be another
       source of missing debug info. *)
    let avail_across, avail_after =
      match avail_before with
      | Unreachable -> unreachable, unreachable
      | Ok avail_before -> (
        match instr.desc with
        | Op (Name_for_debugger { ident; which_parameter; provenance; regs }) ->
          let avail_after =
            ref avail_before
            (* forgetting_ident *)
          in
          let num_parts_of_value = Array.length regs in
          (* Add debug info about [ident], but only for registers that are known
             to be available. ([Name_for_debugger] is not intended to be used in
             advance.) *)
          for part_of_value = 0 to num_parts_of_value - 1 do
            let reg = regs.(part_of_value) in
            (* Registers that still have unknown locations should not be used in
               the Cfg at this point (they would cause fatal errors in the
               emitter). As such we ignore them explicitly here
               ([mem_reg_by_loc] would cause a fatal error otherwise).

               Registers in [Name_for_debugger] operations that are still in
               use, but which have been substituted since the original
               construction of such operations, should have been renamed by
               [Regalloc_substitution.apply_basic_instruction_in_place]. *)
            if
              (not (Reg.is_unknown reg))
              && RD_quotient_set.mem_reg_by_loc avail_before reg
            then
              let regd =
                RD.create ~reg ~holds_value_of:ident ~part_of_value
                  ~num_parts_of_value ~which_parameter ~provenance
              in
              avail_after
                := RD_quotient_set.add regd
                     (RD_quotient_set.filter_reg_by_loc !avail_after reg)
          done;
          dprintf "...ident = %a\n%!" Ident.print_with_scope ident;
          dprintf "...regd = %a\n%!" Ident.print_with_scope ident;
          dprintf "...avail_before %a\n%!" RD_quotient_set.print avail_before;
          dprintf "...avail_across %a\n%!" RD_quotient_set.print avail_before;
          dprintf "...avail_after %a\n%!" RD_quotient_set.print !avail_after;
          ok avail_before, ok !avail_after
        | Op (Move | Reload | Spill) ->
          (* Moves are special: they enable us to propagate names. No-op moves
             need to be handled specially---in this case, we may learn that a
             given hard register holds the value of multiple pseudoregisters
             (all of which have the same value). *)
          let move_to_same_location =
            let move_to_same_location = ref true in
            for i = 0 to Array.length instr.arg - 1 do
              let arg = instr.arg.(i) in
              let res = instr.res.(i) in
              (* CR mshinwell: check this [move_to_same_location] logic is
                 correct, including the next comment (xclerc also wondered about
                 the latter iirc) *)
              (* Note that the register classes must be the same, so we don't
                 need to check that. *)
              if not (Reg.equal_location arg.loc res.loc)
              then move_to_same_location := false
            done;
            !move_to_same_location
          in
          dprintf "...move_to_same_location %b\n%!" move_to_same_location;
          let made_unavailable =
            if move_to_same_location
            then RD_quotient_set.empty
            else
              RD_quotient_set.made_unavailable_by_clobber avail_before
                ~regs_clobbered:instr.res
          in
          dprintf "...made_unavailable %a\n%!" RD_quotient_set.print
            made_unavailable;
          let results =
            Array.map2
              (fun arg_reg result_reg ->
                (* We just need to find any register in [avail_before] with the
                   same location. The debug info on the registers doesn't
                   matter. *)
                match
                  RD_quotient_set.find_reg_with_same_location_exn avail_before
                    arg_reg
                with
                | exception Not_found ->
                  (* We need this to keep availability sets correct even in the
                     absence of debug info *)
                  RD.create_without_debug_info ~reg:result_reg
                | arg_reg ->
                  if Option.is_some (RD.debug_info arg_reg)
                  then
                    (* Propagate the debug info, i.e. what the register is known
                       to hold *)
                    RD.create_copying_debug_info ~reg:result_reg
                      ~debug_info_from:arg_reg
                  else
                    (* Same comment as in the [Not_found] case above *)
                    RD.create_without_debug_info ~reg:result_reg
                (* None *))
              instr.arg instr.res
          in
          dprintf "...results (%a)\n%!"
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
               (fun ppf reg -> (RD.print ~print_reg:Printreg.reg) ppf reg))
            (Array.to_list results);
          let avail_across =
            RD_quotient_set.diff avail_before made_unavailable
          in
          dprintf "...avail_before %a\n%!" RD_quotient_set.print avail_before;
          dprintf "...avail_across %a\n%!" RD_quotient_set.print avail_across;
          let avail_after =
            Array.fold_left
              (fun avail_after reg -> RD_quotient_set.add reg avail_after)
              avail_across results
          in
          dprintf "...avail_after %a\n%!" RD_quotient_set.print avail_after;
          ok avail_across, ok avail_after
        | Op
            ( Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
            | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ | Stackoffset _
            | Load _ | Store _ | Intop _ | Int128op _ | Intop_imm _
            | Intop_atomic _ | Floatop _ | Csel _ | Reinterpret_cast _
            | Static_cast _ | Probe_is_enabled _ | Opaque | Begin_region
            | End_region | Specific _ | Dls_get | Tls_get | Poll | Alloc _
            | Pause )
        | Reloadretaddr | Pushtrap _ | Poptrap _ | Prologue | Epilogue
        | Stack_check _ ->
          let is_op_end_region = Cfg.is_end_region in
          common ~avail_before ~destroyed_at:Proc.destroyed_at_basic
            ~is_interesting_constructor:is_op_end_region
            ~is_end_region:is_op_end_region instr)
    in
    instr.available_across <- avail_across;
    { avail_before = avail_after }

  let terminator ({ avail_before } : domain)
      (term : Cfg.terminator Cfg.instruction) () : image =
    dprintf "Instruction: (id=%a) %a\n%!" InstructionId.print term.id
      Cfg.print_terminator term;
    term.available_before <- avail_before;
    if !Dwarf_flags.ddebug_invariants
    then check_invariants term ~print_instr:Cfg.print_terminator ~avail_before;
    let avail_across, avail_after =
      match avail_before with
      | Unreachable -> unreachable, unreachable
      | Ok avail_before -> (
        match term.desc with
        | Never -> assert false
        | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
        | Switch _ | Call _ | Prim _ | Return | Raise _ | Tailcall_func _
        | Call_no_return _ | Invalid _ | Tailcall_self _ ->
          common ~avail_before ~destroyed_at:Proc.destroyed_at_terminator
            ~is_interesting_constructor:
              Cfg.(
                function
                | Never -> assert false
                | Call _ | Prim { op = Probe _; label_after = _ } -> true
                | Always _ | Parity_test _ | Truth_test _ | Float_test _
                | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
                | Tailcall_func _ | Call_no_return _ | Invalid _
                | Prim { op = External _; label_after = _ } ->
                  false)
            ~is_end_region:(fun _ -> false)
            term)
    in
    term.available_across <- avail_across;
    let avail_before_exn_handler =
      match avail_after with
      | Unreachable -> unreachable
      | Ok avail_at_raise ->
        let without_exn_bucket =
          RD_quotient_set.filter_reg_by_loc avail_at_raise Proc.loc_exn_bucket
        in
        let with_anonymous_exn_bucket =
          RD_quotient_set.add
            (RD.create_without_debug_info ~reg:Proc.loc_exn_bucket)
            without_exn_bucket
        in
        ok with_anonymous_exn_bucket
    in
    { normal = { avail_before = avail_after };
      exceptional = { avail_before = avail_before_exn_handler }
    }
end

module Analysis = Cfg_dataflow.Forward (Domain) (Transfer)

let run : Cfg_with_layout.t -> Cfg_with_layout.t =
 fun cfg_with_layout ->
  if !Clflags.debug && not !Dwarf_flags.restrict_to_upstream_dwarf
  then (
    let cfg = Cfg_with_layout.cfg cfg_with_layout in
    let fun_args = R.set_of_array cfg.fun_args in
    let fun_name = Cfg.fun_name cfg in
    dprintf "Function %s\n%!" fun_name;
    let avail_before = RAS.Ok (RD_quotient_set.without_debug_info fun_args) in
    let init : Domain.t = { Domain.avail_before } in
    match Analysis.run cfg ~init ~handlers_are_entry_points:false () with
    | Error () ->
      Misc.fatal_errorf "Cfg_available_regs.run: dataflow analysis failed"
    | Ok (_ : Domain.t Label.Tbl.t) ->
      (* CR mshinwell: consider adding command-line flag to save cfg as .dot *)
      ());
  cfg_with_layout
