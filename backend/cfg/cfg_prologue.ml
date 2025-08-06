[@@@ocaml.warning "+a-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

let make_instr_with_copy :
    Cfg.basic ->
    id:InstructionId.t ->
    copy:_ Cfg.instruction ->
    arg:Reg.t array ->
    res:Reg.t array ->
    Cfg.basic Cfg.instruction =
 fun desc ~id ~copy ~arg ~res ->
  { desc;
    arg;
    res;
    dbg = copy.dbg;
    fdo = copy.fdo;
    live = copy.live;
    (* note: recomputed anyway *)
    stack_offset = copy.stack_offset;
    id;
    irc_work_list = Unknown_list;
    ls_order = -1;
    available_before = copy.available_before;
    available_across = copy.available_across
  }

let add_prologue_if_required : Cfg_with_layout.t -> Cfg_with_layout.t =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let prologue_required =
    Proc.prologue_required ~fun_contains_calls:cfg.fun_contains_calls
      ~fun_num_stack_slots:cfg.fun_num_stack_slots
  in
  (if prologue_required
  then
    let entry_block = Cfg.get_block_exn cfg cfg.entry_label in
    let next_instr =
      Option.value (DLL.hd entry_block.body)
        ~default:{ entry_block.terminator with desc = Cfg.Prologue }
    in
    DLL.add_begin entry_block.body
      (make_instr_with_copy Cfg.Prologue
         ~id:(InstructionId.get_and_incr cfg.next_instruction_id)
         ~copy:next_instr ~arg:[||] ~res:[||]));
  cfg_with_layout

let run : Cfg_with_layout.t -> Cfg_with_layout.t =
 fun cfg_with_layout -> add_prologue_if_required cfg_with_layout
