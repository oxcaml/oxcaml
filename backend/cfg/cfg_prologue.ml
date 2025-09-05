[@@@ocaml.warning "+a-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

(* Before this pass, the CFG should not contain any prologues/epilogues. Iterate
   over the CFG and make sure that this is the case. *)
let validate_no_prologue (cfg : Cfg.t) =
  Label.Tbl.iter
    (fun _ block ->
      let body = block.Cfg.body in
      DLL.iter body ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
          match[@ocaml.warning "-4"] instr.desc with
          | Prologue | Epilogue ->
            Misc.fatal_error
              "Cfg contains prologue/epilogue before Cfg_prologue pass"
          | _ -> ()))
    cfg.blocks

module Instruction_requirements = struct
  type t =
    (* This instruction does not use the stack, so it doesn't matter if there's
       a prologue on the stack or not*)
    | No_requirements
      (* This instruction uses the stack, either through stack slots or as a
         call, and hence requires a prologue to already be on the stack. *)
    | Requires_prologue
      (* This instruction must only occur when there's no prologue on the stack.
         This is the case for [Return] and tailcalls. Any instruction with this
         requirement must either occur on an execution path where there's no
         prologue, or occur after the epilogue.

         Only terminators can have this requirement. *)
    | Requires_no_prologue

  (* [Prologue] and [Epilogue] instructions will always be treated differently
     than other instructions (as they affect the state) and hence don't get
     requirements. *)
  type or_prologue =
    | Prologue
    | Epilogue
    | Requirements of t

  let instr_uses_stack_slots (instr : _ Cfg.instruction) =
    let regs_use_stack_slots =
      Array.exists (fun reg ->
          match reg.Reg.loc with
          | Stack (Local _) -> true
          | Stack (Incoming _ | Outgoing _ | Domainstate _) | Reg _ | Unknown ->
            false)
    in
    regs_use_stack_slots instr.Cfg.arg || regs_use_stack_slots instr.Cfg.res

  (* CR-soon cfalas: this has a lot of overlap with
     [Cfg.basic_block_contains_calls], but as soon as shrink wrapping is
     implemented and enabled, we can remove [Cfg.basic_block_contains_calls]. *)
  let terminator (instr : Cfg.terminator Cfg.instruction) fun_name =
    if instr_uses_stack_slots instr
    then Requires_prologue
    else
      match instr.desc with
      (* These will cause the function to return, and therefore the stack should
         be unwound. *)
      | Cfg.Return | Tailcall_func Indirect -> Requires_no_prologue
      | Tailcall_func (Direct func)
        when not (String.equal func.sym_name fun_name) ->
        Requires_no_prologue
      (* These are implemented by calling a function when emitted and therefore
         need a prologue. *)
      | Call _ | Call_no_return _
      | Raise (Raise_regular | Raise_reraise)
      | Prim { op = External _ | Probe _; _ } ->
        Requires_prologue
      | Tailcall_func (Direct _)
      | Tailcall_self _
      | Raise Raise_notrace
      | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
      | Int_test _ | Switch _ ->
        No_requirements

  let basic (instr : Cfg.basic Cfg.instruction) =
    if instr_uses_stack_slots instr
    then Requirements Requires_prologue
    else
      match instr.desc with
      | Prologue -> Prologue
      | Epilogue -> Epilogue
      (* [Stackoffset] instructions are only added after [Call]s, so any
         [Stackoffset] instructions should already only occur after a prologue,
         but adding a requirement for completeness. *)
      | Op (Stackoffset _) -> Requirements Requires_prologue
      (* Allocations and polls are implemented by calling a function when
         emitted, and therefore need a prologue for the function call. *)
      | Op (Alloc _ | Poll) -> Requirements Requires_prologue
      | Op
          ( Move | Spill | Reload | Const_int _ | Const_float32 _
          | Const_float _ | Const_symbol _ | Const_vec128 _ | Const_vec256 _
          | Const_vec512 _ | Load _ | Store _ | Intop _ | Intop_imm _
          | Intop_atomic _ | Floatop _ | Csel _ | Reinterpret_cast _
          | Static_cast _ | Probe_is_enabled _ | Opaque | Begin_region
          | End_region | Specific _ | Name_for_debugger _ | Dls_get | Pause )
      | Pushtrap _ | Poptrap _ | Reloadretaddr | Stack_check _ ->
        Requirements No_requirements
end

let prologue_needed_block (block : Cfg.basic_block) ~fun_name =
  (* CR-soon cfalas: Move to [Proc] so that it's arch-dependent and
     frame_pointers only affects the output for amd64. *)
  Config.with_frame_pointers || block.is_trap_handler
  || DLL.exists block.Cfg.body ~f:(fun instr ->
         match Instruction_requirements.basic instr with
         | Requirements Requires_prologue -> true
         | Prologue | Epilogue
         | Requirements (No_requirements | Requires_no_prologue) ->
           false)
  ||
  match Instruction_requirements.terminator block.terminator fun_name with
  | Requires_prologue -> true
  | No_requirements | Requires_no_prologue -> false

module Path_no_more_prologue = struct
  type state = bool

  type context = { fun_name : string }

  (* The domain represents whether the path leading to a specific block from
     backwards needs a prologue or not. *)
  module Domain : Cfg_dataflow.Domain_S with type t = state = struct
    type t = bool

    (* An empty block doesn't need a prologue. *)
    let bot = false

    (* If any of the predecessors of a block need a prologue, then we consider
       the block to also need a prologue. *)
    let join = ( || )

    let less_equal a b = b || not a
  end

  module Transfer :
    Cfg_dataflow.Backward_transfer
      with type domain = bool
       and type context = context
       and type error = unit = struct
    type domain = bool

    type error = unit

    type nonrec context = context

    let transfer : domain -> Instruction_requirements.t -> domain =
     fun domain requirements ->
      match domain, requirements with
      | _, Requires_prologue -> true
      | domain, (No_requirements | Requires_no_prologue) -> domain

    let basic :
        domain ->
        Cfg.basic Cfg.instruction ->
        Cfg.basic_block ->
        context ->
        (domain, error) result =
     fun domain instr _ _ ->
      match domain, Instruction_requirements.basic instr with
      | _, (Prologue | Epilogue) ->
        Misc.fatal_error
          "found prologue or epilogue instruction before prologue addition \
           phase"
      | domain, Requirements requirements -> Ok (transfer domain requirements)

    let terminator :
        domain ->
        exn:domain ->
        Cfg.terminator Cfg.instruction ->
        Cfg.basic_block ->
        context ->
        (domain, error) result =
     fun domain ~exn:_ instr _ { fun_name } ->
      let res =
        transfer domain (Instruction_requirements.terminator instr fun_name)
      in
      Ok res

    let exception_ : domain -> context -> (domain, error) result =
     fun _ _ -> Ok true
  end

  module T = struct
    include Cfg_dataflow.Backward (Domain) (Transfer)
  end

  include (T : module type of T with type context := context)

  type t = Cfg.t * bool InstructionId.Tbl.t

  let build (cfg : Cfg.t) : t =
    match run cfg ~init:false ~map:Instr { fun_name = cfg.fun_name } with
    | Ok res_at_exit -> cfg, res_at_exit
    | Aborted _ | Max_iterations_reached ->
      Misc.fatal_error "Cfg_prologue: unreachable code"

  let needs_prologue (t : t) (label : Label.t) =
    let cfg, tbl = t in
    let block = Cfg.get_block_exn cfg label in
    let first_instr =
      Option.value
        (Option.map (fun inst -> inst.Cfg.id) (DLL.hd block.body))
        ~default:block.terminator.id
    in
    InstructionId.Tbl.find tbl first_instr
end

(* CR-someday cfalas: This implementation can take O(n^2) memory if there are
   many blocks which need an epilogue. Ideally we should be able to re-use some
   of the epilogues stored for the children instead of storing a fresh copy for
   each block to bring this worst-case down. *)
module Reachable_epilogues = struct
  type t = Label.Set.t Label.Tbl.t

  let build (cfg : Cfg.t) (_doms : Cfg_dominators.t) : t =
    let t = Label.Tbl.map cfg.blocks (fun _ -> Label.Set.empty) in
    let visited = ref Label.Set.empty in
    let prologues_needed = Path_no_more_prologue.build cfg in
    let rec collect label =
      if not (Label.Set.mem label !visited)
      then (
        visited := Label.Set.add label !visited;
        let block = Cfg.get_block_exn cfg label in
        (match
           Instruction_requirements.terminator block.terminator cfg.fun_name
         with
        | Requires_no_prologue ->
          Label.Tbl.replace t label (Label.Set.singleton label)
        | No_requirements | Requires_prologue -> ());
        Label.Set.iter
          (fun succ_label ->
            collect succ_label;
            let succ_epilogues = Label.Tbl.find t succ_label in
            let new_epilogue =
              if Path_no_more_prologue.needs_prologue prologues_needed label
                 || block.terminator.stack_offset <> 0
              then Label.Set.union (Label.Tbl.find t label) succ_epilogues
              else Label.Set.singleton label
            in
            Label.Tbl.replace t label new_epilogue)
          (Cfg.successor_labels ~normal:true ~exn:true block))
    in
    collect cfg.entry_label;
    t

  let from_block (t : t) (label : Label.t) = Label.Tbl.find t label
end

(* CR-soon cfalas: consider moving this to [Cfg] *)
let descendants (cfg : Cfg.t) (block : Cfg.basic_block) : Label.Set.t =
  let visited = ref Label.Set.empty in
  let rec collect label =
    if not (Label.Set.mem label !visited)
    then (
      visited := Label.Set.add label !visited;
      let block = Cfg.get_block_exn cfg label in
      Label.Set.iter
        (fun succ_label -> collect succ_label)
        (Cfg.successor_labels ~normal:true ~exn:true block))
  in
  collect block.start;
  !visited

(* The dataflow analysis here is used to determine whether a block on any path
   leading to a block in the CFG needs a prologue. This then allows us to check
   whether we can stop shrink-wrapping if all descendant leaf blocks need a
   prologue *)
module Path_needs_prologue = struct
  type state = bool

  type context = { fun_name : string }

  (* The domain represents whether the path leading to a specific block needs a
     prologue or not. *)
  module Domain : Cfg_dataflow.Domain_S with type t = state = struct
    type t = bool

    (* An empty block doesn't need a prologue. *)
    let bot = false

    (* If any of the predecessors of a block need a prologue, then we consider
       the block to also need a prologue. *)
    let join = ( || )

    let less_equal a b = b || not a
  end

  module Transfer :
    Cfg_dataflow.Forward_transfer
      with type domain = bool
       and type context = context = struct
    type domain = bool

    type nonrec context = context

    type image =
      { normal : domain;
        exceptional : domain
      }

    let transfer : domain -> Instruction_requirements.t -> domain =
     fun domain requirements ->
      match domain, requirements with
      | _, Requires_prologue -> true
      | domain, (No_requirements | Requires_no_prologue) -> domain

    let basic domain instr _ _ =
      match domain, Instruction_requirements.basic instr with
      | _, (Prologue | Epilogue) ->
        Misc.fatal_error
          "found prologue or epilogue instruction before prologue addition \
           phase"
      | domain, Requirements requirements -> transfer domain requirements

    let terminator domain instr _block { fun_name } =
      let res =
        transfer domain (Instruction_requirements.terminator instr fun_name)
      in
      { normal = res; exceptional = res }
  end

  module T = struct
    include Cfg_dataflow.Forward (Domain) (Transfer)
  end

  include (T : module type of T with type context := context)

  type t = bool Label.Tbl.t

  let build (cfg : Cfg.t) : t =
    match
      run ~init:false ~handlers_are_entry_points:true cfg
        { fun_name = cfg.fun_name }
    with
    | Ok res_at_entry ->
      (* The result returned by the forward analysis is the state at the entry
         of each block, but we want the state just before the terminator, i.e.
         after the entire body of the block. *)
      (* CR-someday cfalas: to avoid having to recompute this, we can change
         [Cfg_dataflow] so that we can choose whether we want the results
         returned to be at the block input or at the block output (or before the
         terminator). This can be done for both forward and backward
         analysis. *)
      let res_at_exit = Label.Tbl.copy res_at_entry in
      let context = { fun_name = cfg.fun_name } in
      Label.Tbl.iter
        (fun label at_entry ->
          let block = Cfg.get_block_exn cfg label in
          let at_exit =
            DLL.fold_left
              ~f:(fun acc instr -> Transfer.basic acc instr block context)
              ~init:at_entry block.body
          in
          let at_exit =
            (Transfer.terminator at_exit block.terminator block context).normal
          in
          Label.Tbl.replace res_at_exit label at_exit)
        res_at_entry;
      res_at_exit
    | Error () -> Misc.fatal_error "Cfg_prologue: unreachable code"

  let needs_prologue (t : t) (label : Label.t) = Label.Tbl.find t label
end

(* Module to adjust the forward analysis results based on stack_offset
   constraints. If a block has non-zero stack_offset and needs a prologue
   backwards, it must also be marked as needing one forwards. *)
module Stack_offset_adjustment_forward = struct
  type context = { backward : Path_no_more_prologue.t }

  module Domain : Cfg_dataflow.Domain_S with type t = bool = struct
    type t = bool

    let bot = false

    let join = ( || )

    let less_equal a b = b || not a
  end

  module Transfer :
    Cfg_dataflow.Forward_transfer
      with type domain = bool
       and type context = context = struct
    type domain = bool

    type nonrec context = context

    type image =
      { normal : domain;
        exceptional : domain
      }

    let basic :
        domain ->
        Cfg.basic Cfg.instruction ->
        Cfg.basic_block ->
        context ->
        domain =
     fun domain instr block { backward; _ } ->
      (* If this instruction has non-zero stack_offset and needs prologue
         backwards, force domain to true *)
      if instr.stack_offset <> 0
         && Path_no_more_prologue.needs_prologue backward block.start
      then true
      else domain

    let terminator :
        domain ->
        Cfg.terminator Cfg.instruction ->
        Cfg.basic_block ->
        context ->
        image =
     fun domain instr block { backward; _ } ->
      (* Check if terminator's block has non-zero stack_offset *)
      let adjusted_domain =
        if instr.stack_offset <> 0
           && Path_no_more_prologue.needs_prologue backward block.start
        then true
        else domain
      in
      { normal = adjusted_domain; exceptional = adjusted_domain }
  end

  module T = struct
    include Cfg_dataflow.Forward (Domain) (Transfer)
  end

  include (T : module type of T with type context := context)

  let build (cfg : Cfg.t) (backward : Path_no_more_prologue.t)
      (initial_forward : bool Label.Tbl.t) : bool Label.Tbl.t =
    (* Run forward dataflow to propagate the adjustments *)
    match run cfg ~init:false ~handlers_are_entry_points:true { backward } with
    | Ok res_at_entry ->
      (* Combine with initial adjustments *)
      let res_at_exit = Label.Tbl.copy res_at_entry in
      let context = { backward } in
      Label.Tbl.iter
        (fun label at_entry ->
          let block = Cfg.get_block_exn cfg label in
          let at_exit =
            DLL.fold_left
              ~f:(fun acc instr -> Transfer.basic acc instr block context)
              ~init:at_entry block.body
          in
          let at_exit =
            (Transfer.terminator at_exit block.terminator block context).normal
          in
          Label.Tbl.replace res_at_exit label at_exit)
        res_at_entry;
      Label.Tbl.iter
        (fun label init_val ->
          if init_val then Label.Tbl.replace res_at_exit label true)
        initial_forward;
      res_at_exit
    | Error () ->
      Misc.fatal_error "Stack_offset_adjustment_forward: dataflow failed"
end

(* Module to adjust the backward analysis results based on stack_offset
   constraints. If a block has non-zero stack_offset and needs a prologue
   forwards, it should propagate that backwards. *)
module Stack_offset_adjustment_backward = struct
  type context = { forward : bool Label.Tbl.t }

  module Domain : Cfg_dataflow.Domain_S with type t = bool = struct
    type t = bool

    let bot = false

    let join = ( || )

    let less_equal a b = b || not a
  end

  module Transfer :
    Cfg_dataflow.Backward_transfer
      with type domain = bool
       and type context = context
       and type error = unit = struct
    type domain = bool

    type error = unit

    type nonrec context = context

    let basic :
        domain ->
        Cfg.basic Cfg.instruction ->
        Cfg.basic_block ->
        context ->
        (domain, error) result =
     fun domain instr block { forward } ->
      (* If this instruction has non-zero stack_offset and block needs prologue
         forwards, force domain to true *)
      if instr.stack_offset <> 0
         && Path_needs_prologue.needs_prologue forward block.start
      then Ok true
      else Ok domain

    let terminator :
        domain ->
        exn:domain ->
        Cfg.terminator Cfg.instruction ->
        Cfg.basic_block ->
        context ->
        (domain, error) result =
     fun domain ~exn:_ instr block { forward } ->
      let adjusted_domain =
        if instr.stack_offset <> 0
           && Path_needs_prologue.needs_prologue forward block.start
        then true
        else domain
      in
      Ok adjusted_domain

    let exception_ : domain -> context -> (domain, error) result =
     fun _ _ -> Ok true
  end

  module T = struct
    include Cfg_dataflow.Backward (Domain) (Transfer)
  end

  include (T : module type of T with type context := context)

  let build (cfg : Cfg.t) (forward : bool Label.Tbl.t)
      (initial_backward : Path_no_more_prologue.t) : Path_no_more_prologue.t =
    let _, initial_backward_tbl = initial_backward in
    (* Run backward dataflow with the adjusted forward results *)
    match run cfg ~init:false ~map:Instr { forward } with
    | Ok result ->
      (* Combine with initial backward analysis - if either says true, use
         true *)
      InstructionId.Tbl.iter
        (fun id value -> if value then InstructionId.Tbl.replace result id true)
        initial_backward_tbl;
      cfg, result
    | Aborted _ | Max_iterations_reached ->
      Misc.fatal_error "Stack_offset_adjustment_backward: dataflow failed"
end

let can_place_prologues (prologue_labels : Label.Set.t) (cfg : Cfg.t)
    (doms : Cfg_dominators.t) (loop_infos : Cfg_loop_infos.t)
    (epilogue_blocks : Label.Set.t) =
  (* Moving a prologue to a loop might cause it to execute multiple times, which
     is both inefficient as well as possibly incorrect.

     Having a non-zero stack offset means that the prologue is added after a
     [Pushtrap] or [Stackoffset] which shouldn't be allowed. This is because the
     prologue is added at the stack pointer, which would overlap with the
     handler pushed by a [Pushtrap]. *)
  if Label.Set.exists
       (fun label ->
         let block = Cfg.get_block_exn cfg label in
         Cfg_loop_infos.is_in_loop loop_infos label || block.stack_offset <> 0)
       prologue_labels
  then
    false
    (* Check that there are no prologues which might execute after another
       prologue has already executed.

       This might happen when duplicating a prologue in the following CFG:

     * Block A: Condition with branch to Block B / C
     * Block B: Contains an instruction requiring a prologue, with terminator
       that jumps to Block C
     * Block C: Return

       If we duplicate the prologue to both B and C (which are both children of
       A), the prologue will execute twice on the A->B->C path.

       This check will also prevent us from having a
       Prologue..Epilogue..Prologue..Epilogue structure. However, we probably
       shouldn't emit such structures anyway. *)
  else if Label.Set.exists
            (fun prologue ->
              let descendants =
                descendants cfg (Cfg.get_block_exn cfg prologue)
              in
              let descendant_prologues =
                Label.Set.inter prologue_labels descendants
              in
              let descendant_prologues =
                Label.Set.remove prologue descendant_prologues
              in
              not (Label.Set.is_empty descendant_prologues))
            prologue_labels
  then false
  else
    (* Check that the blocks requiring an epilogue are dominated by the prologue
       block. Consider the CFG from the example above. If we try to place the
       prologue in block B, the prologue would not dominate the epilogue in
       block C, so in some cases the epilogue would be executed without a
       prologue on the stack, which would be illegal. *)
    (* CR-soon cfalas: This condition has the correct effect, but can be
       slightly misleading in diamond cases. For example, consider a CFG with
       blocks A-D, and edges A->B, A->C, B->D, C->D. The current implementation
       will not allow us to move the prologue from A to B and C, because neither
       B nor C dominate D. In these cases, we are allowed to duplicate the
       prologue, but we still don't want to, as this only happens when *all* of
       the children of A require a prologue, in which case we can save space by
       placing the prologue at A without an impact on performance. *)
    Label.Set.for_all
      (fun epilogue_label ->
        Label.Set.exists
          (fun prologue_label ->
            Cfg_dominators.is_dominating doms prologue_label epilogue_label)
          prologue_labels)
      epilogue_blocks

let _find_prologue_and_epilogues_shrink_wrapped
    (cfg_with_infos : Cfg_with_infos.t) =
  let rec visit (tree : Cfg_dominators.dominator_tree) (cfg : Cfg.t)
      (doms : Cfg_dominators.t) (loop_infos : Cfg_loop_infos.t)
      (reachable_epilogues : Reachable_epilogues.t)
      (path_needs_prologue : Path_needs_prologue.t) : Label.Set.t * Label.Set.t
      =
    let block = Cfg.get_block_exn cfg tree.label in
    let epilogue_blocks =
      Reachable_epilogues.from_block reachable_epilogues tree.label
    in
    (* If the current block needs a prologue, we can't propagate the prologue
       downwards. If all paths eventually need a prologue (i.e. all reachable
       blocks where we would add an epilogue require a prologue at some point on
       any path to that block), we can place it here without any disadvantages.

       This also covers the case where all paths lead to an exception and/or are
       cold. *)
    (* CR-soon cfalas: this assumes that all descendants of a cold block are
       also cold.*)
    let all_need_prologue =
      Label.Set.for_all
        (fun label ->
          let block = Cfg.get_block_exn cfg label in
          block.cold
          || Path_needs_prologue.needs_prologue path_needs_prologue label)
        epilogue_blocks
      && (not (Label.Set.is_empty epilogue_blocks))
      && not (epilogue_blocks = Label.Set.singleton tree.label)
    in
    if prologue_needed_block block ~fun_name:cfg.fun_name || all_need_prologue
    then Label.Set.singleton tree.label, epilogue_blocks
    else
      let children_prologue_block =
        List.map
          (fun tree ->
            visit tree cfg doms loop_infos reachable_epilogues
              path_needs_prologue)
          tree.children
      in
      let child_prologue_blocks, child_epilogue_blocks =
        List.fold_left
          (fun (child_prologues, child_epilogues) (all_prologues, all_epilogues) ->
            ( Label.Set.union all_prologues child_prologues,
              Label.Set.union all_epilogues child_epilogues ))
          (Label.Set.empty, Label.Set.empty)
          children_prologue_block
      in
      if can_place_prologues child_prologue_blocks cfg doms loop_infos
           child_epilogue_blocks
      then child_prologue_blocks, child_epilogue_blocks
      else Label.Set.singleton tree.label, epilogue_blocks
  in
  (* [Proc.prologue_required] is cheap and should provide an over-estimate of
     when we would need a prologue (in some cases [Proc.prologue_required] will
     return [true] because it uses the value of [cfg.fun_contains_calls] which
     was computed before CFG simplification, which can remove calls if they are
     dead, making the prologue unnecessary). *)
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  if Proc.prologue_required ~fun_contains_calls:cfg.fun_contains_calls
       ~fun_num_stack_slots:cfg.fun_num_stack_slots
  then (
    let doms = Cfg_with_infos.dominators cfg_with_infos in
    (* note: the other entries in the forest are dead code *)
    let tree = Cfg_dominators.dominator_tree_for_entry_point doms in
    let loop_infos = Cfg_with_infos.loop_infos cfg_with_infos in
    let reachable_epilogues = Reachable_epilogues.build cfg doms in
    let path_needs_prologue = Path_needs_prologue.build cfg in
    let prologue_blocks, epilogue_blocks =
      visit tree cfg doms loop_infos reachable_epilogues path_needs_prologue
    in
    if not
         (can_place_prologues prologue_blocks cfg doms loop_infos
            epilogue_blocks)
    then
      Misc.fatal_errorf
        "Cfg_prologue: can't place prologues and epilogues at selected blocks";
    prologue_blocks, epilogue_blocks)
  else Label.Set.empty, Label.Set.empty

let add_instr_on_edge ~(src : Label.t) ~(dst : Label.t)
    (cfg_with_layout : Cfg_with_layout.t) desc =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let src_block = Cfg.get_block_exn cfg src in
  let dst_block = Cfg.get_block_exn cfg dst in
  let terminator_as_basic terminator =
    { terminator with Cfg.desc = Cfg.Prologue }
  in
  let next_instr =
    Option.value (DLL.hd dst_block.body)
      ~default:(terminator_as_basic dst_block.terminator)
  in
  let instr =
    Cfg.make_instruction_from_copy next_instr ~desc
      ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
      ()
  in
  if List.length (Cfg.predecessor_labels dst_block) = 1
  then
    (* We can place the prologue/epilogue at the beginning of the dst block *)
    DLL.add_begin dst_block.body instr
  else if Label.Set.cardinal
            (Cfg.successor_labels src_block ~normal:true ~exn:true)
          = 1
          && instr.desc = Cfg.Prologue
  then DLL.add_end src_block.body instr
  else
    (* Insert a block in the middle *)
    let inserted_blocks =
      Cfg_with_layout.insert_block cfg_with_layout (DLL.make_single instr)
        ~after:src_block ~before:(Some dst_block)
        ~next_instruction_id:(fun () ->
          InstructionId.get_and_incr cfg.next_instruction_id)
    in
    assert (List.length inserted_blocks = 1)

module Edge = struct
  module T = struct
    type t = Label.t * Label.t

    let compare (left_src, left_dst) (right_src, right_dst) =
      let c = Label.compare left_src right_src in
      if c <> 0 then c else Label.compare left_dst right_dst
  end

  include T
  module Set = Set.Make (T)
end

let find_prologue_and_epilogues_alt (cfg_with_infos : Cfg_with_infos.t) =
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let terminator_as_basic terminator =
    { terminator with Cfg.desc = Cfg.Prologue }
  in
  if Proc.prologue_required ~fun_contains_calls:cfg.fun_contains_calls
       ~fun_num_stack_slots:cfg.fun_num_stack_slots
  then (
    (* Find intersection of backwards and forwards analysis *)
    let backward = Path_no_more_prologue.build cfg in
    let forward = Path_needs_prologue.build cfg in
    (* Adjust the analyses to account for stack_offset constraints *)
    let adjusted_forward =
      Stack_offset_adjustment_forward.build cfg backward forward
    in
    let adjusted_backward =
      Stack_offset_adjustment_backward.build cfg adjusted_forward backward
    in
    let blocks = Label.Tbl.copy cfg.blocks in
    let forward, backward = adjusted_forward, adjusted_backward in
    let prologue_edges, epilogue_edges =
      Label.Tbl.fold
        (fun label block (prol, epil) ->
          let predecessors = Label.Set.of_list (Cfg.predecessor_labels block) in
          let successors = Cfg.successor_labels block ~normal:true ~exn:true in
          if Path_needs_prologue.needs_prologue forward label
             && Path_no_more_prologue.needs_prologue backward label
          then
            let prol =
              Label.Set.fold
                (fun p acc ->
                  if (not (Path_needs_prologue.needs_prologue forward p))
                     && Path_no_more_prologue.needs_prologue backward p
                  then (p, label) :: acc
                  else acc)
                predecessors prol
            in
            if Label.Set.is_empty successors
               && Instruction_requirements.terminator block.Cfg.terminator
                    cfg.fun_name
                  = Instruction_requirements.Requires_no_prologue
            then (
              DLL.add_end block.Cfg.body
                (Cfg.make_instruction_from_copy block.Cfg.terminator
                   ~desc:Cfg.Epilogue
                   ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
                   ());
              prol, epil)
            else
              ( prol,
                Label.Set.fold
                  (fun succ acc ->
                    if Path_needs_prologue.needs_prologue forward succ
                       && not
                            (Path_no_more_prologue.needs_prologue backward succ)
                    then (label, succ) :: acc
                    else acc)
                  successors epil )
          else prol, epil)
        blocks ([], [])
    in
    let epilogue_edges = ref (Edge.Set.of_list epilogue_edges) in
    let q = Queue.of_seq (Edge.Set.to_seq !epilogue_edges) in
    while not (Queue.is_empty q) do
      let _, dst = Queue.take q in
      (* If all predecessors of dst have an epilogue, move epilogue down *)
      let dst_block = Cfg.get_block_exn cfg dst in
      let preds = Cfg.predecessor_labels dst_block in
      let succs = Cfg.successor_labels dst_block ~normal:true ~exn:true in
      if Label.Set.cardinal succs <= List.length preds
         && List.for_all (fun p -> Edge.Set.mem (p, dst) !epilogue_edges) preds
      then (
        List.iter
          (fun p -> epilogue_edges := Edge.Set.remove (p, dst) !epilogue_edges)
          preds;
        if Label.Set.is_empty succs
        then
          let next_instr = terminator_as_basic dst_block.terminator in
          DLL.add_end dst_block.body
            (Cfg.make_instruction_from_copy next_instr ~desc:Cfg.Epilogue
               ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
               ())
        else
          Label.Set.iter
            (fun succ ->
              epilogue_edges := Edge.Set.add (dst, succ) !epilogue_edges;
              Queue.add (dst, succ) q)
            succs)
    done;
    let prologue_edges = ref (Edge.Set.of_list prologue_edges) in
    let q = Queue.of_seq (Edge.Set.to_seq !prologue_edges) in
    while not (Queue.is_empty q) do
      let src, _ = Queue.take q in
      (* If all successors of src have a prologue, move prologue up *)
      let src_block = Cfg.get_block_exn cfg src in
      let succs = Cfg.successor_labels src_block ~normal:true ~exn:true in
      let preds = Cfg.predecessor_labels src_block in
      if Label.Set.cardinal succs >= List.length preds
         && Label.Set.for_all
              (fun succ -> Edge.Set.mem (src, succ) !prologue_edges)
              succs
      then (
        Label.Set.iter
          (fun succ ->
            prologue_edges := Edge.Set.remove (src, succ) !prologue_edges)
          succs;
        if List.length preds > 0
        then
          List.iter
            (fun p ->
              prologue_edges := Edge.Set.add (p, src) !prologue_edges;
              Queue.add (p, src) q)
            preds
        else
          let next_instr =
            Option.value (DLL.hd src_block.body)
              ~default:(terminator_as_basic src_block.terminator)
          in
          DLL.add_begin src_block.body
            (Cfg.make_instruction_from_copy next_instr ~desc:Cfg.Prologue
               ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
               ()))
    done;
    Edge.Set.iter
      (fun (src, dst) ->
        add_instr_on_edge ~src ~dst cfg_with_layout Cfg.Epilogue)
      !epilogue_edges;
    Edge.Set.iter
      (fun (src, dst) ->
        add_instr_on_edge ~src ~dst cfg_with_layout Cfg.Prologue)
      !prologue_edges;
    ())

let find_prologue_and_epilogues_at_entry (cfg_with_infos : Cfg_with_infos.t) =
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  if Proc.prologue_required ~fun_contains_calls:cfg.fun_contains_calls
       ~fun_num_stack_slots:cfg.fun_num_stack_slots
  then
    let epilogue_blocks =
      Cfg.fold_blocks cfg
        ~f:(fun label block acc ->
          match
            Instruction_requirements.terminator block.terminator cfg.fun_name
          with
          | Requires_no_prologue -> Label.Set.add label acc
          | No_requirements | Requires_prologue -> acc)
        ~init:Label.Set.empty
    in
    Label.Set.singleton cfg.entry_label, epilogue_blocks
  else Label.Set.empty, Label.Set.empty

let add_prologue_if_required (cfg_with_infos : Cfg_with_infos.t) ~f =
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  let prologue_blocks, epilogue_blocks = f cfg_with_infos in
  let terminator_as_basic terminator =
    { terminator with Cfg.desc = Cfg.Prologue }
  in
  Label.Set.iter
    (fun prologue_label ->
      let prologue_block = Cfg.get_block_exn cfg prologue_label in
      let next_instr =
        Option.value
          (DLL.hd prologue_block.body)
          ~default:(terminator_as_basic prologue_block.terminator)
      in
      DLL.add_begin prologue_block.body
        (Cfg.make_instruction_from_copy next_instr ~desc:Cfg.Prologue
           ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
           ()))
    prologue_blocks;
  Label.Set.iter
    (fun label ->
      let block = Cfg.get_block_exn cfg label in
      let terminator = terminator_as_basic block.terminator in
      DLL.add_end block.body
        (Cfg.make_instruction_from_copy terminator ~desc:Cfg.Epilogue
           ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
           ()))
    epilogue_blocks

module Validator = struct
  type state =
    | No_prologue_on_stack
    | Prologue_on_stack

  (* This is necessary to make a set, but the ordering of elements is
     arbitrary. *)
  let state_compare left right =
    match left, right with
    | No_prologue_on_stack, No_prologue_on_stack -> 0
    | No_prologue_on_stack, Prologue_on_stack -> 1
    | Prologue_on_stack, No_prologue_on_stack -> -1
    | Prologue_on_stack, Prologue_on_stack -> 0

  module State_set = Set.Make (struct
    type t = state

    let compare = state_compare
  end)

  (* The validator domain represents the set of possible states at an
     instruction (i.e. a state {Prologue_on_stack, No_prologue_on_stack} means
     that depending on the execution path used to get to that block/instruction,
     we can either have a prologue on the stack or not).

     Non-singleton states are allowed in cases where there is no Prologue,
     Epilogue nor any instructions which require a prologue (this happens e.g.
     when two [raise] terminators reach the same handler, but one is before the
     prologue, and the other is after the prologue - this is allowed when the
     handler does not do any stack operations, which means it is not affected if
     there's a prologue on the stack or not, but should not be a valid state if
     the handler uses the stack). *)
  module Domain : Cfg_dataflow.Domain_S with type t = State_set.t = struct
    type t = State_set.t

    let bot = State_set.empty

    let join = State_set.union

    let less_equal = State_set.subset
  end

  type context = { fun_name : string }

  module Transfer :
    Cfg_dataflow.Forward_transfer
      with type domain = State_set.t
       and type context = context = struct
    type domain = State_set.t

    type nonrec context = context

    type image =
      { normal : domain;
        exceptional : domain
      }

    let error_with_instruction (msg : string) (instr : _ Cfg.instruction) =
      Misc.fatal_errorf "Cfg_prologue: error validating instruction %s: %s"
        (InstructionId.to_string_padded instr.id)
        msg

    let basic :
        domain ->
        Cfg.basic Cfg.instruction ->
        Cfg.basic_block ->
        context ->
        domain =
     fun domain instr _block _ ->
      State_set.map
        (fun domain ->
          match domain, Instruction_requirements.basic instr with
          | No_prologue_on_stack, Prologue when instr.stack_offset <> 0 ->
            error_with_instruction "prologue has a non-zero stack offset" instr
          | Prologue_on_stack, Epilogue when instr.stack_offset <> 0 ->
            error_with_instruction "epilogue has a non-zero stack offset" instr
          | No_prologue_on_stack, Prologue -> Prologue_on_stack
          | No_prologue_on_stack, Epilogue ->
            error_with_instruction
              "epilogue appears without a prologue on the stack" instr
          | No_prologue_on_stack, Requirements Requires_prologue ->
            error_with_instruction
              "instruction needs prologue but no prologue on the stack" instr
          | ( No_prologue_on_stack,
              Requirements (No_requirements | Requires_no_prologue) ) ->
            No_prologue_on_stack
          | Prologue_on_stack, Prologue ->
            error_with_instruction
              "prologue appears while prologue is already on the stack" instr
          | Prologue_on_stack, Epilogue -> No_prologue_on_stack
          | Prologue_on_stack, Requirements (No_requirements | Requires_prologue)
            ->
            Prologue_on_stack
          | Prologue_on_stack, Requirements Requires_no_prologue ->
            error_with_instruction
              "basic instruction requires no prologue, this should never happen"
              instr)
        domain

    let terminator :
        domain ->
        Cfg.terminator Cfg.instruction ->
        Cfg.basic_block ->
        context ->
        image =
     fun domain instr _block { fun_name } ->
      let res =
        State_set.map
          (fun domain ->
            match
              domain, Instruction_requirements.terminator instr fun_name
            with
            | No_prologue_on_stack, Requires_prologue ->
              error_with_instruction
                "instruction needs prologue but no prologue on the stack" instr
            | No_prologue_on_stack, (No_requirements | Requires_no_prologue) ->
              No_prologue_on_stack
            | Prologue_on_stack, (No_requirements | Requires_prologue) ->
              Prologue_on_stack
            | Prologue_on_stack, Requires_no_prologue ->
              error_with_instruction
                "terminator needs to appear after epilogue but prologue is on \
                 stack"
                instr)
          domain
      in
      { normal = res; exceptional = res }
  end

  module T = struct
    include Cfg_dataflow.Forward (Domain) (Transfer)
  end

  include (T : module type of T with type context := context)
end

let run : Cfg_with_infos.t -> Cfg_with_infos.t =
 fun cfg_with_infos ->
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  if !Oxcaml_flags.cfg_prologue_validate then validate_no_prologue cfg;
  (match !Oxcaml_flags.cfg_prologue_shrink_wrap with
  | true
    when Label.Tbl.length cfg.blocks
         <= !Oxcaml_flags.cfg_prologue_shrink_wrap_threshold
         && not Config.with_frame_pointers ->
    find_prologue_and_epilogues_alt cfg_with_infos
  | _ ->
    add_prologue_if_required cfg_with_infos
      ~f:find_prologue_and_epilogues_at_entry);
  cfg_with_infos

let validate : Cfg_with_infos.t -> Cfg_with_infos.t =
 fun cfg_with_infos ->
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  let fun_name = Cfg.fun_name cfg in
  match !Oxcaml_flags.cfg_prologue_validate with
  | true -> (
    match
      Validator.run cfg
        ~init:(Validator.State_set.singleton No_prologue_on_stack)
        ~handlers_are_entry_points:false { fun_name }
    with
    | Ok block_states ->
      Label.Tbl.iter
        (fun label state ->
          let block = Cfg.get_block_exn cfg label in
          if block.is_trap_handler
             && Validator.State_set.mem No_prologue_on_stack state
          then
            Misc.fatal_errorf
              "Cfg_prologue: can reach trap handler with no prologue at block \
               %s"
              (Label.to_string label))
        block_states;
      cfg_with_infos
    | Error () -> Misc.fatal_error "Cfg_prologue: dataflow analysis failed")
  | false -> cfg_with_infos
