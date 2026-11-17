module DLL = Doubly_linked_list
module Range = Regalloc_ls_utils.Range
module Interval = Regalloc_ls_utils.Interval
module ClassIntervals = Regalloc_ls_utils.ClassIntervals

let range begin_ end_ : Range.t = { begin_; end_ }

let test_range_endpoints () =
  let ranges = DLL.of_list [range 2 4; range 6 6; range 8 10] in
  List.iter
    (fun (pos, count, live) ->
      Range.remove_expired ranges ~pos;
      assert (Int.equal (DLL.length ranges) count);
      assert (Bool.equal (Range.is_live ranges ~pos) live))
    [ 1, 3, false;
      4, 3, true;
      5, 2, false;
      6, 2, true;
      7, 1, false;
      10, 1, true;
      11, 0, false ]

let interval ranges ~begin_ ~end_ : Interval.t =
  { reg = Reg.create Cmm.Int; begin_; end_; ranges = DLL.of_list ranges }

let contains intervals (interval : Interval.t) =
  DLL.exists intervals ~f:(fun (candidate : Interval.t) ->
      Reg.same candidate.reg interval.reg)

let test_interval_endpoints () =
  let intervals = ClassIntervals.make () in
  (* Handler-entry clobbers are singleton fixed ranges. *)
  let fixed = interval [range 10 10; range 20 20] ~begin_:10 ~end_:20 in
  let active = interval [range 8 10; range 16 20] ~begin_:8 ~end_:20 in
  let inactive =
    interval [range 2 4; range 10 10; range 16 20] ~begin_:2 ~end_:20
  in
  DLL.add_end intervals.fixed_dll fixed;
  DLL.add_end intervals.active_dll active;
  DLL.add_end intervals.inactive_dll inactive;
  ClassIntervals.release_expired_intervals intervals ~pos:10;
  let starting_here = interval [range 10 11] ~begin_:10 ~end_:11 in
  assert (Interval.overlap fixed starting_here);
  assert (contains intervals.active_dll active);
  assert (contains intervals.active_dll inactive);
  assert (DLL.is_empty intervals.inactive_dll);
  ClassIntervals.release_expired_intervals intervals ~pos:11;
  assert (not (Interval.overlap fixed starting_here));
  assert (DLL.is_empty intervals.active_dll);
  assert (contains intervals.inactive_dll active);
  assert (contains intervals.inactive_dll inactive);
  ClassIntervals.release_expired_intervals intervals ~pos:16;
  assert (contains intervals.active_dll active);
  assert (contains intervals.active_dll inactive);
  assert (DLL.is_empty intervals.inactive_dll);
  ClassIntervals.release_expired_intervals intervals ~pos:20;
  assert (Interval.is_live fixed ~pos:20);
  assert (contains intervals.active_dll active);
  assert (contains intervals.active_dll inactive);
  ClassIntervals.release_expired_intervals intervals ~pos:21;
  assert (DLL.is_empty intervals.fixed_dll);
  assert (DLL.is_empty intervals.active_dll);
  assert (DLL.is_empty intervals.inactive_dll)

let test_reuses_argument_register () =
  let ids = InstructionId.make_sequence () in
  let instruction ?(arg = [||]) ?(res = [||]) desc =
    Cfg.make_instruction ~desc ~arg ~res ~stack_offset:0
      ~id:(InstructionId.get_and_incr ids)
      ()
  in
  let src = Reg.create Cmm.Int in
  let dst = Reg.create Cmm.Int in
  let result = (Proc.loc_results_return [| Cmm.Int |]).(0) in
  let entry =
    Cfg.make_empty_block ~label:Label.entry_label
      (instruction Cfg.Return ~arg:[| result |])
  in
  List.iter (DLL.add_end entry.body)
    [ instruction (Cfg.Op (Operation.Const_int 1n)) ~res:[| src |];
      instruction (Cfg.Op Operation.Move) ~arg:[| src |] ~res:[| dst |];
      instruction (Cfg.Op Operation.Move) ~arg:[| dst |] ~res:[| result |] ];
  let cfg =
    Cfg.create ~fun_name:"reuse_argument_register" ~fun_args:[||]
      ~fun_codegen_options:[] ~fun_dbg:Debuginfo.none ~fun_contains_calls:false
      ~fun_num_stack_slots:(Stack_class.Tbl.make 0)
      ~fun_poll:Lambda.Default_poll ~next_instruction_id:ids
      ~fun_ret_type:[| Cmm.Int |] ~fun_phantom_lets:Backend_var.Map.empty
      ~allowed_to_be_irreducible:false
  in
  Cfg.add_block_exn cfg entry;
  let cfg_with_infos =
    Cfg_with_layout.create cfg ~layout:(DLL.make_single Label.entry_label)
    |> Cfg_with_infos.make
  in
  let (_ : Cfg_with_infos.t) = Regalloc_ls.run cfg_with_infos in
  (* The move reads [src] at [on] and writes [dst] at [on + 1]. *)
  assert (match src.loc with Reg _ -> true | Unknown | Stack _ -> false);
  assert (Reg.same_loc src dst)

let () =
  test_range_endpoints ();
  test_interval_endpoints ();
  test_reuses_argument_register ()
