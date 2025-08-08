[@@@ocaml.warning "+a-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

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

let run : Cfg_with_layout.t -> Cfg_with_layout.t =
 fun cfg_with_layout -> add_prologue_if_required cfg_with_layout
