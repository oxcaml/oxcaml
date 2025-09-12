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
      | Op (Alloc _ | Poll { enabled = true }) -> Requirements Requires_prologue
      | Op
          ( Move | Spill | Reload | Const_int _ | Const_float32 _
          | Const_float _ | Const_symbol _ | Const_vec128 _ | Const_vec256 _
          | Const_vec512 _ | Load _ | Store _ | Intop _ | Intop_imm _
          | Intop_atomic _ | Floatop _ | Csel _ | Reinterpret_cast _
          | Static_cast _ | Probe_is_enabled _ | Opaque | Begin_region
          | End_region | Specific _ | Name_for_debugger _ | Dls_get | Pause
          | Poll { enabled = false } )
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

(* CR-someday cfalas: This implementation can take O(n^2) memory if there are
   many blocks which need an epilogue. Ideally we should be able to re-use some
   of the epilogues stored for the children instead of storing a fresh copy for
   each block to bring this worst-case down. *)
module Reachable_epilogues = struct
  type t = Label.Set.t Label.Tbl.t

  let build (cfg : Cfg.t) : t =
    let t = Label.Tbl.map cfg.blocks (fun _ -> Label.Set.empty) in
    let visited = ref Label.Set.empty in
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
            Label.Tbl.replace t label
              (Label.Set.union (Label.Tbl.find t label)
                 (Label.Tbl.find t succ_label)))
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

let find_prologue_and_epilogues_shrink_wrapped
    (cfg_with_infos : Cfg_with_infos.t) =
  let rec visit (tree : Cfg_dominators.dominator_tree) (cfg : Cfg.t)
      (doms : Cfg_dominators.t) (loop_infos : Cfg_loop_infos.t)
      (reachable_epilogues : Reachable_epilogues.t) : Label.Set.t * Label.Set.t
      =
    let block = Cfg.get_block_exn cfg tree.label in
    let epilogue_blocks =
      Reachable_epilogues.from_block reachable_epilogues tree.label
    in
    (* If the current block needs a prologue, we can't propagate the prologue
       downwards. If it's cold or all paths lead to an exception, there's no
       reason to do so, as performance is not important. *)
    let all_exceptional_paths =
      Label.Set.for_all
        (fun label ->
          let block = Cfg.get_block_exn cfg label in
          match[@ocaml.warning "-4"] block.terminator.desc with
          | Raise _ -> true
          | _ -> false)
        epilogue_blocks
    in
    if prologue_needed_block block ~fun_name:cfg.fun_name
       || block.cold || all_exceptional_paths
    then Label.Set.singleton tree.label, epilogue_blocks
    else
      let children_prologue_block =
        List.map
          (fun tree -> visit tree cfg doms loop_infos reachable_epilogues)
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
    let reachable_epilogues = Reachable_epilogues.build cfg in
    let prologue_blocks, epilogue_blocks =
      visit tree cfg doms loop_infos reachable_epilogues
    in
    assert (
      can_place_prologues prologue_blocks cfg doms loop_infos epilogue_blocks);
    prologue_blocks, epilogue_blocks)
  else Label.Set.empty, Label.Set.empty

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

(* Add prologue at the start of the block, but after all the [Name_for_debugger]
   instructions that refer to function parameters. Such operations always come
   in a contiguous block (see selectgen). *)
let add_prologue (cfg : Cfg.t) prologue_label =
  let prologue_block = Cfg.get_block_exn cfg prologue_label in
  let make_prologue_instruction ~(like : _ Cfg.instruction) =
    Cfg.make_instruction_from_copy like ~desc:Cfg.Prologue
      ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
      ()
  in
  let is_param_name_for_debugger (basic : Cfg.basic) =
    match[@warning "-4"] basic with
    | Op (Name_for_debugger { which_parameter; _ }) ->
      Option.is_some which_parameter
    | _ -> false
  in
  let not_param_name_for_debugger (i : Cfg.basic Cfg.instruction) =
    not (is_param_name_for_debugger i.desc)
  in
  (* Insert [Prologue] before [next_instr]. [next_instr] is the first
     instruction in the body of the [prologue_block] after all
     [Name_for_debugger] instructions that refer to function parameters, or
     [None] if there are no instructions after [Name_for_debugger] in the body
     of this block (including the case where the body is empty, for example when
     dwarf generation is not enabled). If [next_instr] is None, then [Prologue]
     will be inserted at the end of the body. *)
  let next_instr =
    DLL.find_cell_opt ~f:not_param_name_for_debugger prologue_block.body
  in
  match next_instr with
  | None ->
    let prologue_instruction =
      make_prologue_instruction ~like:prologue_block.terminator
    in
    DLL.add_end prologue_block.body prologue_instruction
  | Some next_instr ->
    let prologue_instruction =
      make_prologue_instruction ~like:(DLL.value next_instr)
    in
    DLL.insert_before next_instr prologue_instruction

let add_prologue_if_required (cfg_with_infos : Cfg_with_infos.t) ~f =
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  let prologue_blocks, epilogue_blocks = f cfg_with_infos in
  Label.Set.iter (add_prologue cfg) prologue_blocks;
  Label.Set.iter
    (fun label ->
      let block = Cfg.get_block_exn cfg label in
      DLL.add_end block.body
        (Cfg.make_instruction_from_copy block.terminator ~desc:Cfg.Epilogue
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

    let basic : domain -> Cfg.basic Cfg.instruction -> context -> domain =
     fun domain instr _ ->
      State_set.map
        (fun domain ->
          match domain, Instruction_requirements.basic instr with
          | No_prologue_on_stack, Prologue when instr.stack_offset <> 0 ->
            error_with_instruction "prologue has a non-zero stack offset" instr
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
        domain -> Cfg.terminator Cfg.instruction -> context -> image =
     fun domain instr { fun_name } ->
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
         <= !Oxcaml_flags.cfg_prologue_shrink_wrap_threshold ->
    add_prologue_if_required cfg_with_infos
      ~f:find_prologue_and_epilogues_shrink_wrapped
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
