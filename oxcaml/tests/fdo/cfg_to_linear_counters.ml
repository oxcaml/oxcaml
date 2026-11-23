(* The counters on the machine edges of the terminators that [Cfg_to_linear]
   linearizes, for each block that can follow them in the layout. A conditional
   branch has the counters of the successors it jumps to, and its fallthrough
   those of the successors it continues to, unless it continues to another
   conditional branch, which it only passes through. A terminator that becomes a
   jump or a fallthrough drops its counters (see [Fdo_dropped_counters]). Each
   case prints the Linear code and checks that every counter of the Cfg is on a
   machine edge or a call, or dropped. *)

let () =
  Env.set_current_unit
    (Unit_info.make ~source_file:"cfg_to_linear_counters.ml"
       ~for_pack_prefix:Compilation_unit.Prefix.empty Impl
       "cfg_to_linear_counters")

let function_ =
  Fdo_counter.Function.create_original
    (Fdo_counter.Name.create ~name_str:"Test.f" ~discriminator:0)

let body_hash = Fdo_counter.Body_hash.of_int32 1l

let counter arm =
  Fdo_counter.of_position
    (Fdo_counter.Position.create_branch ~function_ ~body_hash ~index:0 ~arm)

let call_counter =
  Fdo_counter.of_position
    (Fdo_counter.Position.create_call ~function_ ~body_hash ~index:1)

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

let block label (terminator : Cfg.terminator) =
  let block =
    Cfg.make_empty_block ~label
      (Cfg.make_instruction ~desc:terminator ~arg:[| reg; reg |] ~stack_offset:0
         ~id:(InstructionId.get_and_incr instruction_ids)
         ())
  in
  block.stack_offset <- 0;
  block

let entry = Label.entry_label

let a = Label.new_label ()

let b = Label.new_label ()

let c = Label.new_label ()

let d = Label.new_label ()

let names = [entry, "entry"; a, "A"; b, "B"; c, "C"; d, "D"]

let label_name label = List.assoc label names

let cfg_with_layout terminator ~next =
  let cfg =
    Cfg.create ~fun_name:"f" ~fun_args:[||] ~fun_codegen_options:[]
      ~fun_dbg:Debuginfo.none
      ~fun_fdo_entry_annotation:Fdo_annotation.With_body_hash.empty
      ~fun_contains_calls:true ~fun_num_stack_slots:(Stack_class.Tbl.make 0)
      ~fun_poll:Default_poll ~next_instruction_id:instruction_ids
      ~fun_ret_type:Cmm.typ_int ~fun_phantom_lets:Backend_var.Map.empty
      ~allowed_to_be_irreducible:false
  in
  Cfg.add_block_exn cfg (block entry terminator);
  List.iter
    (fun label -> Cfg.add_block_exn cfg (block label Return))
    [a; b; c; d];
  Cfg.register_predecessors_for_all_blocks cfg;
  let layout =
    entry :: next
    :: List.filter (fun label -> not (Label.equal label next)) [a; b; c; d]
  in
  Cfg_with_layout.create cfg ~layout:(Doubly_linked_list.of_list layout)

(* The counters on the edges and calls of the Linear code. *)
let linear_counters (fundecl : Linear.fundecl) =
  let of_successor (successor : Linear.successor) =
    Fdo_annotation.to_list successor.fdo_annotation
  in
  let of_call (op : Linear.call_operation) =
    match op with
    | Lcall_ind { callsite_counter }
    | Lcall_imm { callsite_counter; _ }
    | Ltailcall_ind { callsite_counter }
    | Ltailcall_imm { callsite_counter; _ } ->
      Option.to_list callsite_counter
    | Lextcall _ | Lprobe _ -> []
  in
  let rec counters (instr : Linear.instruction) =
    match[@warning "-fragile-match"] instr.desc with
    | Lend -> []
    | Lcondbranch { taken; fallthrough_annotation; test = _ } ->
      of_successor taken
      @ Fdo_annotation.to_list fallthrough_annotation
      @ counters instr.next
    | Lcondbranch3 { lt; eq; gt; fallthrough_annotation } ->
      List.concat_map
        (fun successor -> Option.fold ~none:[] ~some:of_successor successor)
        [lt; eq; gt]
      @ Fdo_annotation.to_list fallthrough_annotation
      @ counters instr.next
    | Lswitch successors ->
      List.concat_map of_successor (Array.to_list successors)
      @ counters instr.next
    | Lcall_op op -> of_call op @ counters instr.next
    | _ -> counters instr.next
  in
  counters fundecl.fun_body

(* The instructions of the entry block, which has the terminator: those before
   the code of the next block. *)
let print_entry_block (fundecl : Linear.fundecl) =
  let rec print (instr : Linear.instruction) =
    match[@warning "-fragile-match"] instr.desc with
    | Lend | Lreturn | Llabel_for_jump_target _ -> ()
    | _ ->
      Format.printf "    @[<v 0>%a@]@." Printlinear.instr instr;
      print instr.next
  in
  print fundecl.fun_body

let print_counters title counters =
  Format.printf "  @[<hov 2>%s:%a@]@." title
    (Format.pp_print_list
       ~pp_sep:(fun _ () -> ())
       (fun ppf counter ->
         Format.fprintf ppf "@ %s" (Fdo_counter.to_string counter)))
    counters

let mem counter counters = List.exists (Fdo_counter.equal counter) counters

let test name (terminator : Cfg.terminator) ~next =
  Fdo_dropped_counters.reset ();
  let in_cfg =
    Fdo_annotation.to_list (Cfg.fdo_counters_of_terminator terminator)
  in
  let fundecl = Cfg_to_linear.run (cfg_with_layout terminator ~next) in
  let in_linear = linear_counters fundecl in
  let dropped =
    Fdo_dropped_counters.registered ()
    |> Fdo_dropped_counters.to_list |> List.map fst
  in
  Format.printf "%s, followed by %s:@." name (label_name next);
  print_entry_block fundecl;
  print_counters "dropped" dropped;
  print_counters "lost"
    (List.filter
       (fun counter -> not (mem counter in_linear || mem counter dropped))
       in_cfg);
  Format.printf "@."

let test_layouts name terminator =
  List.iter (fun next -> test name terminator ~next) [a; b; c; d]

let int_test ?(is_signed = Scalar.Signedness.Signed) ?(imm = Some 0) lt eq gt :
    Cfg.terminator =
  Int_test
    { lt = successor lt 0;
      eq = successor eq 1;
      gt = successor gt 2;
      is_signed;
      imm
    }

let () =
  (* One instruction per line. *)
  Format.set_margin 200;
  Format.set_max_indent 190;
  Format.printf "Labels: entry = %a, A = %a, B = %a, C = %a, D = %a@.@."
    Label.format entry Label.format a Label.format b Label.format c Label.format
    d;
  test_layouts "truth test"
    (Truth_test { ifso = successor a 1; ifnot = successor b 2 });
  test_layouts "parity test"
    (Parity_test { ifso = successor a 1; ifnot = successor b 2 });
  (* Both successors are the same block: a jump or a fallthrough. *)
  test_layouts "truth test to a single target"
    (Truth_test { ifso = successor a 1; ifnot = successor a 2 });
  (* Two targets: one conditional branch. *)
  test_layouts "int test with two targets" (int_test a b a);
  (* Three targets: a cascade of conditional branches... *)
  test_layouts "int test with three targets" (int_test a b c);
  (* ...or, compared with 1 unsigned, a single comparison. *)
  test_layouts "int test with three targets, against 1 unsigned"
    (int_test ~is_signed:Unsigned ~imm:(Some 1) a b c);
  test_layouts "float test with two targets"
    (Float_test
       { width = Float64;
         lt = successor a 0;
         eq = successor b 1;
         gt = successor a 2;
         uo = successor a 3
       });
  test_layouts "float test with three targets"
    (Float_test
       { width = Float64;
         lt = successor a 0;
         eq = successor b 1;
         gt = successor c 2;
         uo = successor c 3
       });
  test "switch" ~next:a
    (Switch [| successor a 0; successor b 1; successor a 2; successor c 3 |]);
  test "call" ~next:a
    (Call
       { op =
           Direct
             { sym = { sym_name = "g"; sym_global = Global };
               callsite_counter = Some call_counter
             };
         label_after = a
       })
