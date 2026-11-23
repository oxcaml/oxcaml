(* The counters on the edges of the terminators that [Simplify_terminator]
   rewrites. Each edge of a terminator replaced by a jump has its counters
   dropped (see [Fdo_dropped_counters]) if the jump takes it, and loses them if
   the jump makes it untakable. The edges of the integer test replacing a switch
   have the counters of the arms they cover. Each case prints the terminators
   before and after, and the counters of the original terminators that are on no
   edge after, dropped or not. *)

let () =
  Env.set_current_unit
    (Unit_info.make ~source_file:"simplify_terminator_counters.ml"
       ~for_pack_prefix:Compilation_unit.Prefix.empty Impl
       "simplify_terminator_counters")

let function_ =
  Fdo_counter.Function.create_original
    (Fdo_counter.Name.create ~name_str:"Test.f" ~discriminator:0)

let counter arm =
  Fdo_counter.of_position
    (Fdo_counter.Position.create_branch ~function_
       ~body_hash:(Fdo_counter.Body_hash.of_int32 1l)
       ~index:0 ~arm)

(* A successor to [target], with the counter of [arm]. *)
let successor target arm =
  Cfg.successor target ~fdo_annotation:(Fdo_annotation.singleton (counter arm))

let instruction_ids = InstructionId.make_sequence ()

let instruction ?(arg = [||]) ?(res = [||]) desc =
  Cfg.make_instruction ~desc ~arg ~res ~stack_offset:0
    ~id:(InstructionId.get_and_incr instruction_ids)
    ()

(* The register of the first integer argument, which has a location, as after
   register allocation. *)
let reg = (Proc.loc_parameters Cmm.typ_int).(0)

let block ?(body = []) label (terminator : Cfg.terminator) =
  let block =
    Cfg.make_empty_block ~label (instruction terminator ~arg:[| reg |])
  in
  List.iter (Doubly_linked_list.add_end block.body) body;
  block

let return label = block label Return

let const_int n = instruction (Cfg.Op (Const_int n)) ~res:[| reg |]

(* [after_regalloc] lets [Simplify_terminator] evaluate the tests whose argument
   is known, and copy tests into predecessors. *)
let cfg ~after_regalloc blocks =
  let cfg =
    Cfg.create ~fun_name:"f" ~fun_args:[||] ~fun_codegen_options:[]
      ~fun_dbg:Debuginfo.none
      ~fun_fdo_entry_annotation:Fdo_annotation.With_body_hash.empty
      ~fun_contains_calls:false ~fun_num_stack_slots:(Stack_class.Tbl.make 0)
      ~fun_poll:Default_poll ~next_instruction_id:instruction_ids
      ~fun_ret_type:Cmm.typ_int ~fun_phantom_lets:Backend_var.Map.empty
      ~allowed_to_be_irreducible:after_regalloc
  in
  cfg.register_locations_are_set <- after_regalloc;
  List.iter (Cfg.add_block_exn cfg) blocks;
  Cfg.register_predecessors_for_all_blocks cfg;
  cfg

let edge_counters (cfg : Cfg.t) =
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

let test name ~after_regalloc blocks =
  let cfg = cfg ~after_regalloc blocks in
  let before = edge_counters cfg in
  Format.printf "%s:@.  before:@." name;
  print_terminators cfg;
  Fdo_dropped_counters.reset ();
  Simplify_terminator.run cfg;
  let after = edge_counters cfg in
  let dropped =
    Fdo_dropped_counters.registered ()
    |> Fdo_dropped_counters.to_list |> List.map fst
  in
  let removed =
    List.filter
      (fun counter -> not (List.exists (Fdo_counter.equal counter) after))
      before
  in
  Format.printf "  after:@.";
  print_terminators cfg;
  print_counters "dropped" dropped;
  print_counters "removed, not dropped"
    (List.filter
       (fun counter -> not (List.exists (Fdo_counter.equal counter) dropped))
       removed);
  Format.printf "@."

let entry = Label.entry_label

let a = Label.new_label ()

let b = Label.new_label ()

let c = Label.new_label ()

let d = Label.new_label ()

let switch arms =
  Cfg.Switch
    (Array.of_list (List.mapi (fun arm target -> successor target arm) arms))

let test_switch name arms =
  test name ~after_regalloc:false
    [block entry (switch arms); return a; return b; return c; return d]

let () =
  (* A switch whose arms all go to the same block becomes a jump. *)
  test_switch "switch to a single target" [a; a; a];
  (* A switch whose arms make at most three runs, of the shapes below, becomes
     an integer test. Each edge has the counters of the arms it covers. *)
  test_switch "switch with two runs" [a; a; b; b; b];
  test_switch "switch with a run in the middle" [a; a; b; a];
  test_switch "switch with two single arms first" [a; b; c; c];
  (* Other switches are left alone. *)
  test_switch "switch with three runs" [a; b; c; a];
  test_switch "switch with four runs" [a; b; a; b];
  (* A test whose successors are all the same block becomes a jump. *)
  test "truth test to a single target" ~after_regalloc:false
    [ block entry (Truth_test { ifso = successor a 0; ifnot = successor a 1 });
      return a ];
  test "int test to a single target" ~after_regalloc:false
    [ block entry
        (Int_test
           { lt = successor a 0;
             eq = successor a 1;
             gt = successor a 2;
             is_signed = Signed;
             imm = Some 0
           });
      return a ];
  test "float test to a single target" ~after_regalloc:false
    [ block entry
        (Float_test
           { width = Float64;
             lt = successor a 0;
             eq = successor a 1;
             gt = successor a 2;
             uo = successor a 3
           });
      return a ];
  Oxcaml_flags.cfg_value_propagation := true;
  (* A test whose argument is known becomes a jump. The edge it takes has its
     counters dropped; the other loses them. *)
  test "truth test with a known argument" ~after_regalloc:true
    [ block entry
        ~body:[const_int 1n]
        (Truth_test { ifso = successor a 0; ifnot = successor b 1 });
      return a;
      return b ];
  test "switch with a known argument" ~after_regalloc:true
    [ block entry ~body:[const_int 1n] (switch [a; b; c]);
      return a;
      return b;
      return c ];
  (* A jump to an empty block whose test is known jumps to the test's target
     instead. The test keeps its counters, which miss the executions of the
     jump. *)
  test "jump to a known test" ~after_regalloc:true
    [ block entry ~body:[const_int 0n] (Always c);
      block c (Truth_test { ifso = successor a 0; ifnot = successor b 1 });
      return a;
      return b ];
  (* A jump to an empty block whose test is not known takes a copy of the test,
     with its counters: both copies count their own executions. *)
  test "jump to an unknown test" ~after_regalloc:true
    [ block entry (Always d);
      block d (Always c);
      block c (Truth_test { ifso = successor a 0; ifnot = successor b 1 });
      return a;
      return b ]
