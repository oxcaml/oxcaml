[@@@ocaml.warning "+a-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

(* Before this pass, the CFG should not contain any prologues/epilogues. Iterate
   over the CFG and make sure that this is the case. *)
let validate_no_prologue (cfg_with_layout : Cfg_with_layout.t) =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
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

  let instr_uses_stack (instr : _ Cfg.instruction) =
    let regs_use_stack_slots =
      Array.exists (fun reg ->
          match reg.Reg.loc with
          | Stack (Local _) -> true
          | Stack (Incoming _ | Outgoing _ | Domainstate _) | Reg _ | Unknown ->
            false)
    in
    regs_use_stack_slots instr.Cfg.arg
    || regs_use_stack_slots instr.Cfg.res
    || instr.stack_offset <> 0

  let terminator (instr : Cfg.terminator Cfg.instruction) fun_name =
    if instr_uses_stack instr
    then Requires_prologue
    else
      match[@ocaml.warning "-4"] instr.desc with
      | Cfg.Return | Tailcall_func Indirect -> Requires_no_prologue
      | Tailcall_func (Direct func)
        when not (String.equal func.sym_name fun_name) ->
        Requires_no_prologue
      | desc when Cfg.is_nontail_call_terminator desc -> Requires_prologue
      | _ -> No_requirements

  let basic (instr : Cfg.basic Cfg.instruction) =
    if instr_uses_stack instr
    then Requirements Requires_prologue
    else
      match instr.desc with
      | Prologue -> Prologue
      | Epilogue -> Epilogue
      | Op (Stackoffset _) -> Requirements Requires_prologue
      | Op
          ( Move | Spill | Reload | Const_int _ | Const_float32 _
          | Const_float _ | Const_symbol _ | Const_vec128 _ | Const_vec256 _
          | Const_vec512 _ | Load _ | Store _ | Intop _ | Intop_imm _
          | Intop_atomic _ | Floatop _ | Csel _ | Reinterpret_cast _
          | Static_cast _ | Probe_is_enabled _ | Opaque | Begin_region
          | End_region | Specific _ | Name_for_debugger _ | Dls_get | Poll
          | Pause | Alloc _ )
      | Pushtrap _ | Poptrap _ | Reloadretaddr | Stack_check _ ->
        Requirements No_requirements
end

let add_prologue_if_required : Cfg_with_layout.t -> Cfg_with_layout.t =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let prologue_required =
    Proc.prologue_required ~fun_contains_calls:cfg.fun_contains_calls
      ~fun_num_stack_slots:cfg.fun_num_stack_slots
  in
  if prologue_required
  then (
    let terminator_as_basic terminator =
      { terminator with Cfg.desc = Cfg.Prologue }
    in
    let entry_block = Cfg.get_block_exn cfg cfg.entry_label in
    let next_instr =
      Option.value (DLL.hd entry_block.body)
        ~default:(terminator_as_basic entry_block.terminator)
    in
    DLL.add_begin entry_block.body
      (Cfg.make_instruction_from_copy next_instr ~desc:Cfg.Prologue
         ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
         ());
    let add_epilogue (block : Cfg.basic_block) =
      let terminator = terminator_as_basic block.terminator in
      DLL.add_end block.body
        (Cfg.make_instruction_from_copy terminator ~desc:Cfg.Epilogue
           ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
           ())
    in
    Cfg.iter_blocks cfg ~f:(fun _label block ->
        match
          Instruction_requirements.terminator block.terminator cfg.fun_name
        with
        | Requires_no_prologue -> add_epilogue block
        | No_requirements | Requires_prologue -> ()));
  cfg_with_layout

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

let run : Cfg_with_layout.t -> Cfg_with_layout.t =
 fun cfg_with_layout ->
  validate_no_prologue cfg_with_layout;
  let fun_name = Cfg.fun_name (Cfg_with_layout.cfg cfg_with_layout) in
  let cfg_with_layout = add_prologue_if_required cfg_with_layout in
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  match !Oxcaml_flags.cfg_prologue_validate with
  | true -> (
    match
      Validator.run cfg
        ~init:(Validator.State_set.singleton No_prologue_on_stack)
        ~handlers_are_entry_points:false { fun_name }
    with
    | Ok _ -> cfg_with_layout
    | Error () -> Misc.fatal_error "Cfg_prologue: dataflow analysis failed")
  | false -> cfg_with_layout
