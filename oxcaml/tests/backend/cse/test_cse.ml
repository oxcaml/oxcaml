open Cfg_intf.S
module DLL = Doubly_linked_list
module Cse = Cfg_cse.Cse_generic (CSE)

let instr desc arg res =
  Sub_cfg.make_instr desc arg res Debuginfo.none ~phantom_available_before:None

let op desc arg res = instr (Op desc) arg res

let move src dst = op Move [| src |] [| dst |]

let store chunk value addr =
  op (Store (chunk, Arch.identity_addressing, true)) [| value; addr |] [||]

let block ?(handler = false) start body terminator : Cfg.basic_block =
  { start;
    body = DLL.of_list body;
    terminator;
    predecessors = Label.Set.empty;
    stack_offset = Cfg.invalid_stack_offset;
    exn = None;
    can_raise = Cfg.can_raise_terminator terminator.desc;
    is_trap_handler = handler;
    cold = false
  }

(* Check the public pass on pre-regalloc CFGs, including cases with explicit
   physical registers. All input graphs must satisfy the CFG invariants. *)
let make_cfg name args result body =
  let parameters = Proc.loc_parameters (Reg.typv args) in
  let prefix = Array.to_list (Array.map2 move parameters args) in
  (* Valx2 is internal to vectorization, not a function-return machtype. *)
  let ret_type, return_op =
    match result.Reg.typ with
    | Valx2 -> Cmm.typ_int, Operation.Static_cast (Scalar_of_v128 Int64x2)
    | _ -> [| result.Reg.typ |], Operation.Move
  in
  let ret_regs = Proc.loc_results_return ret_type in
  let entry =
    block Label.entry_label
      (prefix @ body @ [op return_op [| result |] ret_regs])
      (instr Return ret_regs [||])
  in
  let handlers =
    List.filter_map
      (fun (i : Cfg.basic Cfg.instruction) ->
        match i.desc with
        | Pushtrap { lbl_handler } ->
          Some (block ~handler:true lbl_handler [] (instr Return ret_regs [||]))
        | _ -> None)
      body
  in
  let blocks = entry :: handlers in
  let cfg =
    Cfg.create ~fun_name:name ~fun_args:parameters ~fun_codegen_options:[]
      ~fun_dbg:Debuginfo.none ~fun_contains_calls:true
      ~fun_num_stack_slots:(Stack_class.Tbl.make 0)
      ~fun_poll:Lambda.Default_poll ~next_instruction_id:Sub_cfg.instr_id
      ~fun_ret_type:ret_type ~fun_phantom_lets:Backend_var.Map.empty
      ~allowed_to_be_irreducible:false
  in
  List.iter (Cfg.add_block_exn cfg) blocks;
  Select_utils.Stack_offset_and_exn.update_cfg cfg;
  Cfg.register_predecessors_for_all_blocks cfg;
  let cfg =
    Cfg_with_layout.create cfg
      ~layout:(DLL.of_list (List.map (fun b -> b.Cfg.start) blocks))
  in
  if Cfg_invariants.run Format.err_formatter cfg
  then failwith (name ^ ": invalid input CFG");
  cfg

let run name args result body =
  let cfg = Cse.cfg_with_layout (make_cfg name args result body) in
  if Cfg_invariants.run Format.err_formatter cfg
  then failwith (name ^ ": invalid output CFG");
  let graph = Cfg_with_layout.cfg cfg in
  let entry = Cfg.get_block_exn graph graph.entry_label in
  match
    DLL.find_opt entry.body ~f:(fun (i : Cfg.basic Cfg.instruction) ->
        Array.exists (Reg.same result) i.res)
  with
  | Some i -> i
  | None -> failwith (name ^ ": missing result definition")

let expect name expected (i : Cfg.basic Cfg.instruction) =
  match i.desc with
  | Op actual when Operation.equal actual expected -> ()
  | _ ->
    Misc.fatal_errorf "%s: expected %a, got %a" name Operation.dump expected
      Printcfg.basic_desc i.desc

let symbol =
  Operation.Const_symbol
    { Cmm.sym_name = "cse_test_symbol"; sym_global = Global }

let test_clobber name make_barrier =
  let barrier = make_barrier () in
  Array.iter
    (fun clobbered ->
      let out = Reg.create Cmm.Int in
      let result = Reg.create Cmm.Int in
      let barrier = make_barrier () in
      run name [| out |] result
        ([op symbol [||] [| clobbered |]; store Word_int clobbered out]
        @ barrier
        @ [op symbol [||] [| result |]])
      |> expect name symbol)
    (Proc.destroyed_at_basic (List.hd barrier).desc)

let () =
  test_clobber "poll clobbers" (fun () -> [op Poll [||] [||]]);
  test_clobber "allocation clobbers" (fun () ->
      [ op
          (Alloc { bytes = 40; dbginfo = []; mode = Heap })
          [||]
          [| Reg.create Cmm.Val |] ]);
  test_clobber "pushtrap clobbers" (fun () ->
      let lbl_handler = Label.new_label () in
      [ instr (Pushtrap { lbl_handler }) [||] [||];
        instr (Poptrap { lbl_handler }) [||] [||] ]);
  let cached = Reg.create Cmm.Int and result = Reg.create Cmm.Int in
  run "virtual constant survives poll" [||] result
    [op symbol [||] [| cached |]; op Poll [||] [||]; op symbol [||] [| result |]]
  |> expect "virtual constant survives poll" Move

let test_alias_write name ~prepare ~overwrite =
  let source = Reg.create Cmm.Float32 in
  let out1 = Reg.create Cmm.Int and out2 = Reg.create Cmm.Int in
  let result = Reg.create Cmm.Float in
  let physical = (Proc.loc_results_return Cmm.typ_float).(0) in
  let alias = Reg.create_alias physical ~typ:Cmm.Float32 in
  let one = Operation.Const_float 0x3ff0000000000000L in
  run name [| out1; out2; source |] result
    (prepare source
    @ [op one [||] [| physical |]; store Double physical out1]
    @ [overwrite source alias; store (Single { reg = Float32 }) alias out2]
    @ [op one [||] [| result |]])
  |> expect name one

let () =
  let zero = Operation.Const_float32 0l in
  test_alias_write "fresh result invalidates physical aliases"
    ~prepare:(fun _ -> [])
    ~overwrite:(fun _ dst -> op zero [||] [| dst |]);
  test_alias_write "move invalidates physical aliases"
    ~prepare:(fun _ -> [])
    ~overwrite:move;
  test_alias_write "CSE result invalidates physical aliases"
    ~prepare:(fun src -> [op zero [||] [| src |]])
    ~overwrite:(fun _ dst -> op zero [||] [| dst |])

let load ?(mutability = Operation.Mutable) chunk addr dst =
  op
    (Load
       { memory_chunk = chunk;
         addressing_mode = Arch.identity_addressing;
         mutability;
         is_atomic = false
       })
    [| addr |] [| dst |]

let expect_move name source i =
  expect name Move i;
  if not (Reg.same i.arg.(0) source)
  then
    Misc.fatal_errorf "%s: expected source %a, got %a" name Printreg.reg source
      Printreg.reg i.arg.(0)

let test_snapshot_copy name chunk root_typ raw_typ =
  let owner = Reg.create Cmm.Val and out = Reg.create Cmm.Int in
  let root = Reg.create root_typ and raw = Reg.create raw_typ in
  if Proc.types_are_compatible root raw
  then
    let result = Reg.create root_typ in
    let load dst = load ~mutability:Immutable chunk owner dst in
    run name [| owner; out |] result
      [ load root;
        move root raw;
        store chunk raw out;
        op Poll [||] [||];
        load result ]
    |> expect_move name root

let () =
  test_snapshot_copy "Val/Int copies stay distinct" Word_val Val Int;
  test_snapshot_copy "Valx2/Vec128 copies stay distinct"
    Onetwentyeight_unaligned Valx2 Vec128;
  let raw = Reg.create Cmm.Int and out = Reg.create Cmm.Int in
  let root = Reg.create Cmm.Val and result = Reg.create Cmm.Int in
  run "Int/Val copies stay distinct" [| raw; out |] result
    [ move raw root;
      op Poll [||] [||];
      store Word_int raw out;
      load Word_int out result ]
  |> expect_move "Int/Val copies stay distinct" raw

(* CSE can extend the lifetime of an integer snapshot independently of the
   number assigned to the mixed-type copy. IRC must not coalesce the copy once
   both representations remain live across GC. *)
let () =
  let name = "CSE-extended snapshot does not coalesce with root" in
  let bits = Reg.create Cmm.Float and out = Reg.create Cmm.Int in
  let raw = Reg.create Cmm.Int and root = Reg.create Cmm.Val in
  let result = Reg.create Cmm.Int in
  let snapshot dst =
    op (Reinterpret_cast Int64_of_float) [| bits |] [| dst |]
  in
  let cfg =
    make_cfg name [| bits; out |] result
      [ snapshot raw;
        move raw root;
        op Poll [||] [||];
        store Word_val root out;
        snapshot result ]
    |> Cse.cfg_with_layout
  in
  let infos = Cfg_with_infos.make cfg in
  (match Regalloc_irc.run infos with
  | Some _ -> ()
  | None -> failwith (name ^ ": unexpected IRC fallback"));
  if Reg.equal_location raw.loc root.loc
  then failwith (name ^ ": both values occupy the same location")
