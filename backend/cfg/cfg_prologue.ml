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

let instr_uses_stack_slots (instr : _ Cfg.instruction) =
  let regs_use_stack_slots =
    Array.exists (fun reg ->
        match reg.Reg.loc with
        | Stack (Local _) -> true
        | Stack (Incoming _ | Outgoing _ | Domainstate _) | Reg _ | Unknown ->
          false)
  in
  regs_use_stack_slots instr.Cfg.arg || regs_use_stack_slots instr.Cfg.res

let is_prologue_needed_terminator (instr : Cfg.terminator Cfg.instruction) =
  Cfg.is_nontail_call_terminator instr.desc || instr_uses_stack_slots instr

let is_prologue_needed_basic (instr : Cfg.basic Cfg.instruction) =
  instr_uses_stack_slots instr

let is_epilogue_needed_terminator (terminator : Cfg.terminator) fun_name =
  match terminator with
  | Cfg.Return | Tailcall_func Indirect -> true
  | Tailcall_func (Direct func) when not (String.equal func.sym_name fun_name)
    ->
    true
  | Tailcall_func (Direct _)
  | Tailcall_self _ | Never | Always _ | Parity_test _ | Truth_test _
  | Float_test _ | Int_test _ | Switch _ | Raise _ | Call_no_return _ | Prim _
  | Call _ ->
    false

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
        match block.terminator.desc with
        | Cfg.Return | Tailcall_func Indirect -> add_epilogue block
        | Tailcall_func (Direct func)
          when not (String.equal func.sym_name cfg.fun_name) ->
          add_epilogue block
        | Tailcall_func (Direct _)
        | Tailcall_self _ | Never | Always _ | Parity_test _ | Truth_test _
        | Float_test _ | Int_test _ | Switch _ | Raise _ | Call_no_return _
        | Prim _ | Call _ ->
          ()));
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
     prologue, and the other is after the prologue). *)
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

    let basic : domain -> Cfg.basic Cfg.instruction -> context -> domain =
     fun domain instr _ ->
      State_set.map
        (fun domain ->
          match[@ocaml.warning "-4"]
            domain, instr.desc, is_prologue_needed_basic instr
          with
          | No_prologue_on_stack, Cfg.Prologue, _ -> Prologue_on_stack
          | No_prologue_on_stack, Cfg.Epilogue, _ ->
            Misc.fatal_error "epilogue appears without a prologue on the stack"
          | No_prologue_on_stack, _, true ->
            Misc.fatal_error
              "instruction needs prologue, but no prologue on the stack"
          | No_prologue_on_stack, _, false -> No_prologue_on_stack
          | Prologue_on_stack, Cfg.Prologue, _ ->
            Misc.fatal_error
              "prologue instruction while prologue is already on the stack"
          | Prologue_on_stack, Cfg.Epilogue, _ -> No_prologue_on_stack
          | Prologue_on_stack, _, _ -> Prologue_on_stack)
        domain

    let terminator :
        domain -> Cfg.terminator Cfg.instruction -> context -> image =
     fun domain instr { fun_name } ->
      let res =
        State_set.map
          (fun domain ->
            match
              ( domain,
                is_prologue_needed_terminator instr,
                is_epilogue_needed_terminator instr.desc fun_name )
            with
            | _, true, true ->
              Misc.fatal_error
                "instruction needs to be both before and after terminator, \
                 this should never happen"
            | No_prologue_on_stack, true, false ->
              Misc.fatal_error "instruction needs prologue"
            | No_prologue_on_stack, false, _ -> No_prologue_on_stack
            | Prologue_on_stack, _, false -> Prologue_on_stack
            | Prologue_on_stack, false, true ->
              Misc.fatal_error "terminator needs epilogue")
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
    | Error () -> Misc.fatal_error "Cfg_prologue.run: dataflow analysis failed")
  | false -> cfg_with_layout
