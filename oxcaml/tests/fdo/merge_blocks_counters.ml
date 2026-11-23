(* The counters of the blocks that are merged. [Cfg_merge_blocks] merges a block
   into an identical one: the counters of the merged block are dropped (see
   [Fdo_dropped_counters]), and the block it is merged into counts the
   executions of both. [Cfg_simplify] bypasses an empty block, or merges a block
   into its only predecessor, when it has a single successor: the counters of
   its branch, if it has one, are dropped. Each case prints the terminators
   before and after, the dropped counters with their reasons, and the counters
   of the original terminators that are on no edge or call after, dropped or
   not. *)

let () =
  Env.set_current_unit
    (Unit_info.make ~source_file:"merge_blocks_counters.ml"
       ~for_pack_prefix:Compilation_unit.Prefix.empty Impl
       "merge_blocks_counters")

let function_ =
  Fdo_counter.Function.create_original
    (Fdo_counter.Name.create ~name_str:"Test.f" ~discriminator:0)

let body_hash = Fdo_counter.Body_hash.of_int32 1l

let counter arm =
  Fdo_counter.of_position
    (Fdo_counter.Position.create_branch ~function_ ~body_hash ~index:0 ~arm)

let call_counter index =
  Fdo_counter.of_position
    (Fdo_counter.Position.create_call ~function_ ~body_hash ~index)

(* A successor to [target], with the counter of [arm]. *)
let successor target arm =
  Cfg.successor target ~fdo_annotation:(Fdo_annotation.singleton (counter arm))

let instruction_ids = InstructionId.make_sequence ()

(* A pseudo-register, so that the output names no register of the target.
   Pseudo-registers are numbered after the target's physical registers, of which
   each target has a different number, so the numbering restarts at 0. *)
let reg =
  Reg.For_testing.set_state ~stamp:0 ~relocatable_regs:[];
  Reg.create Cmm.Int

let instruction desc ~arg ~res =
  Cfg.make_instruction ~desc ~arg ~res ~stack_offset:0
    ~id:(InstructionId.get_and_incr instruction_ids)
    ()

let block ?(body = []) label (terminator : Cfg.terminator) =
  let block =
    Cfg.make_empty_block ~label
      (instruction terminator ~arg:[| reg |] ~res:[||])
  in
  List.iter (Doubly_linked_list.add_end block.body) body;
  block.stack_offset <- 0;
  block

let const_int n = instruction (Cfg.Op (Const_int n)) ~arg:[||] ~res:[| reg |]

let return label = block label Return

let cfg_with_layout blocks =
  let cfg =
    Cfg.create ~fun_name:"f" ~fun_args:[||] ~fun_codegen_options:[]
      ~fun_dbg:Debuginfo.none
      ~fun_fdo_entry_annotation:Fdo_annotation.With_body_hash.empty
      ~fun_contains_calls:true ~fun_num_stack_slots:(Stack_class.Tbl.make 0)
      ~fun_poll:Default_poll ~next_instruction_id:instruction_ids
      ~fun_ret_type:Cmm.typ_int ~fun_phantom_lets:Backend_var.Map.empty
      ~allowed_to_be_irreducible:false
  in
  List.iter (Cfg.add_block_exn cfg) blocks;
  Cfg.register_predecessors_for_all_blocks cfg;
  Cfg_with_layout.create cfg
    ~layout:
      (Doubly_linked_list.of_list
         (List.map (fun (block : Cfg.basic_block) -> block.start) blocks))

let counters (cfg : Cfg.t) =
  Cfg.fold_blocks cfg ~init:[] ~f:(fun _ block counters ->
      counters
      @ Fdo_annotation.to_list
          (Cfg.fdo_counters_of_terminator block.terminator.desc))

let print_terminators (cfg : Cfg.t) =
  Cfg.iter_blocks cfg ~f:(fun label block ->
      match block.terminator.desc with
      | Return -> ()
      | desc ->
        Format.printf "    %a: %a@." Label.format label
          (Printcfg.terminator_desc ~sep:"; ")
          desc)

let print_counters title counters =
  Format.printf "  @[<hov 2>%s:%a@]@." title
    (Format.pp_print_list
       ~pp_sep:(fun _ () -> ())
       (fun ppf counter ->
         Format.fprintf ppf "@ %s" (Fdo_counter.to_string counter)))
    counters

let mem counter counters = List.exists (Fdo_counter.equal counter) counters

let test name ~run blocks =
  let cfg_with_layout = cfg_with_layout blocks in
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let before = counters cfg in
  Format.printf "%s:@.  before:@." name;
  print_terminators cfg;
  Fdo_dropped_counters.reset ();
  let cfg = Cfg_with_layout.cfg (run cfg_with_layout) in
  let after = counters cfg in
  let dropped = Fdo_dropped_counters.registered () in
  Format.printf "  after:@.";
  print_terminators cfg;
  Format.printf "  dropped:@.";
  Fdo_dropped_counters.to_list dropped
  |> List.iter (fun (counter, reason) ->
      Format.printf "    %s: %s@."
        (Fdo_dropped_counters.Reason.to_string reason)
        (Fdo_counter.to_string counter));
  let dropped = List.map fst (Fdo_dropped_counters.to_list dropped) in
  print_counters "lost"
    (List.filter
       (fun counter -> not (mem counter after || mem counter dropped))
       before);
  Format.printf "@."

let entry = Label.entry_label

let a = Label.new_label ()

let b = Label.new_label ()

let c = Label.new_label ()

let d = Label.new_label ()

let () =
  (* [a] and [b] only differ by their counters. *)
  let merge_blocks = Cfg_merge_blocks.run_before_register_allocation in
  test "identical tests" ~run:merge_blocks
    [ block entry (Truth_test { ifso = successor a 0; ifnot = successor b 1 });
      block a (Truth_test { ifso = successor c 2; ifnot = successor d 3 });
      block b (Truth_test { ifso = successor c 4; ifnot = successor d 5 });
      return c;
      return d ];
  let call index : Cfg.terminator =
    Call
      { op =
          Direct
            { sym = { sym_name = "g"; sym_global = Global };
              callsite_counter = Some (call_counter index)
            };
        label_after = c
      }
  in
  test "identical calls" ~run:merge_blocks
    [ block entry (Truth_test { ifso = successor a 0; ifnot = successor b 1 });
      block a (call 0);
      block b (call 1);
      return c ];
  (* An empty block whose test goes to a single block is bypassed. *)
  test "empty block with a single successor" ~run:Cfg_simplify.run
    [ block entry (Truth_test { ifso = successor a 0; ifnot = successor b 1 });
      block a (Truth_test { ifso = successor c 2; ifnot = successor c 3 });
      return b;
      return c ];
  (* A block whose test goes to a single block, of which it is the only
     predecessor, is merged with it. *)
  test "block with a single successor" ~run:Cfg_simplify.run
    [ block entry (Truth_test { ifso = successor a 0; ifnot = successor b 1 });
      block a
        ~body:[const_int 1n]
        (Truth_test { ifso = successor c 2; ifnot = successor c 3 });
      return b;
      block c ~body:[const_int 2n] Return ]
