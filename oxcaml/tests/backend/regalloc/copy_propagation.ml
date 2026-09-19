module DLL = Doubly_linked_list

let instruction_ids = InstructionId.make_sequence ()

let instruction ?(arg = [||]) ?(res = [||]) desc =
  Cfg.make_instruction ~desc ~arg ~res ~stack_offset:0
    ~id:(InstructionId.get_and_incr instruction_ids)
    ()

let move src dst =
  instruction (Cfg.Op Operation.Move) ~arg:[| src |] ~res:[| dst |]

let const n dst = instruction (Cfg.Op (Operation.Const_int n)) ~res:[| dst |]

let name reg =
  let regs = [| reg |] in
  ( instruction
      (Cfg.Op
         (Operation.Name_for_debugger
            { ident = Ident.create_local "x";
              provenance = None;
              which_parameter = None;
              regs
            })),
    regs )

let block label body terminator =
  let block = Cfg.make_empty_block ~label terminator in
  List.iter (DLL.add_end block.body) body;
  block

let cfg blocks =
  let cfg =
    Cfg.create ~fun_name:"copy_propagation_test" ~fun_args:[||]
      ~fun_codegen_options:[] ~fun_dbg:Debuginfo.none ~fun_contains_calls:false
      ~fun_num_stack_slots:(Stack_class.Tbl.make 0)
      ~fun_poll:Lambda.Default_poll ~next_instruction_id:instruction_ids
      ~fun_ret_type:[| Cmm.Int |] ~fun_phantom_lets:Backend_var.Map.empty
      ~allowed_to_be_irreducible:false
  in
  List.iter (Cfg.add_block_exn cfg) blocks;
  Cfg.register_predecessors_for_all_blocks cfg;
  let layout = DLL.make_empty () in
  List.iter
    (fun (block : Cfg.basic_block) -> DLL.add_end layout block.start)
    blocks;
  Cfg_with_infos.make (Cfg_with_layout.create cfg ~layout)

let run cfg =
  let (_ : Cfg_with_infos.t) = Cfg_copy_propagation.run cfg in
  ()

let contains_instruction (block : Cfg.basic_block)
    (instr : Cfg.basic Cfg.instruction) =
  DLL.exists block.body ~f:(fun (candidate : Cfg.basic Cfg.instruction) ->
      InstructionId.equal candidate.id instr.id)

let result_reg () = (Proc.loc_results_return [| Cmm.Int |]).(0)

let test_debug_use_in_another_block () =
  let src = Reg.create Cmm.Int in
  let dst = Reg.create Cmm.Int in
  let result = result_reg () in
  let next_label = Label.new_label () in
  let naming, named_regs = name dst in
  let copy = move src dst in
  let first =
    block Label.entry_label
      [const 1n src; copy; move dst result; move src result]
      (instruction (Cfg.Always next_label))
  in
  let next =
    block next_label
      [const 2n src; naming; move src result]
      (instruction Cfg.Return ~arg:[| result |])
  in
  run (cfg [first; next]);
  assert (Reg.same named_regs.(0) dst);
  assert (contains_instruction first copy)

(* The naming operand is a second read of [dst], so the move must be kept even
   though every other condition for the substitution holds. *)
let test_debug_use_counts_as_read () =
  let src = Reg.create Cmm.Int in
  let dst = Reg.create Cmm.Int in
  let result = result_reg () in
  let naming, named_regs = name dst in
  let copy = move src dst in
  let straight =
    block Label.entry_label
      [const 1n src; copy; naming; move dst result]
      (instruction Cfg.Return ~arg:[| result |])
  in
  run (cfg [straight]);
  assert (Reg.same named_regs.(0) dst);
  assert (contains_instruction straight copy)

(* The naming operand is the only read of [dst], but it precedes the move in a
   self-looping block and hence observes the value from the previous iteration;
   the move must be kept. *)
let test_debug_use_before_write_in_loop () =
  let src = Reg.create Cmm.Int in
  let dst = Reg.create Cmm.Int in
  let naming, named_regs = name dst in
  let copy = move src dst in
  let loop =
    block Label.entry_label
      [const 1n src; naming; copy]
      (instruction (Cfg.Always Label.entry_label))
  in
  run (cfg [loop]);
  assert (Reg.same named_regs.(0) dst);
  assert (contains_instruction loop copy)

let () =
  test_debug_use_in_another_block ();
  test_debug_use_counts_as_read ();
  test_debug_use_before_write_in_loop ()
