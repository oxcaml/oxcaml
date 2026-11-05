open Cfg_intf.S
open Utils

(*= CR xclerc for xclerc: that test relies on the use of the polymorphic
        comparison over CFG values, but that can no longer be used since instruction
        lists now contain circular values.
   let () =
     let made_cfg =
       ({ fun_args = [| Proc.phys_reg 0 |];
          blocks =
            [ { start = entry_label;
                body = [];
                exn = None;
                terminator =
                  { id = 1;
                    desc = Return;
                    arg = [| Proc.phys_reg 0 |];
                    res = [||]
                  }
              } ];
          fun_contains_calls = false
        }
         : Cfg_desc.t)
       |> Cfg_desc.make_post_regalloc
     in
     let cfg =
       Cfg.create ~fun_name:"foo"
         ~fun_args:[| Proc.phys_reg 0 |]
         ~fun_dbg:Debuginfo.none ~fun_fast:false ~fun_contains_calls:false
         ~fun_num_stack_slots:(Array.make Proc.num_stack_slot_classes 0)
     in
     Label.Tbl.add cfg.Cfg.blocks (Cfg.entry_label cfg)
       { start = Cfg.entry_label cfg;
         body = Cfg.BasicInstructionList.make_empty ();
         exn = None;
         can_raise = false;
         is_trap_handler = false;
         predecessors = Label.Set.empty;
         stack_offset = 0;
         cold = false;
         terminator =
           { desc = Return;
             arg = [| Proc.phys_reg 0 |];
             res = [||];
             dbg = Debuginfo.none;
             fdo = None;
             stack_offset = 0;
             id = 1;
             live = Reg.Set.empty;
             irc_work_list = Unknown_list
           }
       };
     let cfg =
       cfg
       |> Cfg_with_layout.create ~layout:[]
     in
     assert (made_cfg = cfg);
     ()
*)

exception Break_test

let val_ = Array.init 8 (fun _ -> Reg.create Val)

let _addr = Array.init 8 (fun _ -> Reg.create Addr)

let float = Array.init 8 (fun _ -> Reg.create Float)

let base_templ () : Cfg_desc.t * (unit -> InstructionId.t) =
  let make_id =
    let seq = InstructionId.make_sequence () in
    let _zero : InstructionId.t = InstructionId.get_and_incr seq in
    let _one : InstructionId.t = InstructionId.get_and_incr seq in
    let _two : InstructionId.t = InstructionId.get_and_incr seq in
    fun () -> InstructionId.get_and_incr seq
  in
  let make_locs regs f =
    let locs = f (Array.map (fun (r : Reg.t) -> r.typ) regs) in
    let regs =
      Array.map2
        (fun (reg : Reg.t) (loc : Reg.t) ->
          Reg.For_testing.with_loc reg loc.loc)
        regs locs
    in
    regs, locs
  in
  (* This is one possible representation of code:

     [fun f x y a -> let y = f x y a in let x = x + y in x] *)
  let int_arg1 = int.(0) in
  let int_arg2 = int.(1) in
  let stack_loc = Reg.For_testing.with_loc int_arg1 (Stack (Local 0)) in
  let args, arg_locs =
    make_locs [| val_.(0); int_arg1; int_arg2; float.(0) |] Proc.loc_parameters
  in
  let int_arg1 = args.(1) in
  let int_arg2 = args.(2) in
  let tmp_results, tmp_result_locs =
    make_locs [| int.(2) |] Proc.loc_results_return
  in
  let results, result_locs = make_locs [| int.(3) |] Proc.loc_results_return in
  let make_moves src dst =
    Array.map2
      (fun src dst : Basic.t ->
        { id = make_id (); desc = Op Move; arg = [| src |]; res = [| dst |] })
      src dst
    |> Array.to_list
  in
  ( { fun_args = Array.copy arg_locs;
      blocks =
        [ { start = entry_label;
            body = [{ id = make_id (); desc = Prologue; arg = [||]; res = [||] }];
            exn = None;
            terminator =
              { id = make_id ();
                desc = Always move_param_label;
                arg = [||];
                res = [||]
              }
          };
          { start = move_param_label;
            body =
              make_moves arg_locs args
              @ [ { Instruction.id = make_id ();
                    desc = Op Spill;
                    arg = [| int_arg1 |];
                    res = [| stack_loc |]
                  } ]
              @ make_moves args arg_locs;
            exn = None;
            terminator =
              { id = make_id ();
                desc = Always call_label;
                arg = [||];
                res = [||]
              }
          };
          { start = call_label;
            body = [];
            exn = None;
            terminator =
              { id = make_id ();
                desc =
                  Call { op = Indirect None; label_after = move_tmp_res_label };
                arg = arg_locs;
                res = tmp_result_locs
              }
          };
          { start = move_tmp_res_label;
            body =
              make_moves tmp_result_locs tmp_results
              @ make_moves tmp_results [| int_arg2 |]
              @ [ { Instruction.id = make_id ();
                    desc = Op Reload;
                    arg = [| stack_loc |];
                    res = [| int_arg1 |]
                  } ];
            exn = None;
            terminator =
              { id = make_id ();
                desc = Always add_label;
                arg = [||];
                res = [||]
              }
          };
          { start = add_label;
            body =
              [ { Instruction.id = make_id ();
                  desc = Op (Intop Operation.Iadd);
                  arg = [| int_arg1; int_arg2 |];
                  res = [| int_arg1 |]
                } ];
            exn = None;
            terminator =
              { id = make_id ();
                desc = Always return_label;
                arg = [||];
                res = [||]
              }
          };
          { start = return_label;
            body =
              make_moves [| int_arg1 |] results
              @ make_moves results result_locs
              @ [ { id = make_id ();
                    desc = Reloadretaddr;
                    arg = [||];
                    res = [||]
                  } ];
            exn = None;
            terminator =
              { id = make_id (); desc = Return; arg = result_locs; res = [||] }
          } ];
      fun_contains_calls = true;
      fun_ret_type = Cmm.typ_val
    },
    make_id )

let validate ~rematerializations (before, after) =
  try
    let before, after =
      ( Cfg_with_infos.cfg_with_layout before,
        Cfg_with_infos.cfg_with_layout after )
    in
    let desc =
      try
        Misc.protect_refs
          [R (Oxcaml_flags.regalloc_validate, true)]
          (fun () -> Regalloc_validate.Description.create before)
      with Misc.Fatal_error ->
        Format.printf "fatal exception raised when creating description";
        raise Break_test
    in
    let desc =
      match desc with
      | None ->
        Format.printf "description was not generated";
        raise Break_test
      | Some desc -> desc
    in
    let res =
      try Regalloc_validate.test desc ~rematerializations after
      with Misc.Fatal_error ->
        Format.printf "fatal exception raised when validating description";
        raise Break_test
    in
    match res with
    | Ok cfg ->
      (* Physical equality: [Regalloc_validate.test] returns the very CFG it was
         given, and structural equality cannot be used because instruction lists
         contain cycles. *)
      if cfg == after then () else Format.printf "Validation changed cfg"
    | Error error ->
      Format.printf "Validation failed: %a" Regalloc_validate.Error.print error
  with Break_test -> ()

let save (before, after) =
  (* CR azewierzejew for azewierzejew: Fix how the files are saved. *)
  Cfg_with_layout.save_as_dot ~filename:"/tmp/before.dot"
    (Cfg_with_infos.cfg_with_layout before)
    "test-cfg-before";
  Cfg_with_layout.save_as_dot ~filename:"/tmp/after.dot"
    (Cfg_with_infos.cfg_with_layout after)
    "test-cfg-after";
  Format.printf "The failing cfgs were put in /tmp/[before|after].dot\n"

(* [rematerializations] is the record of the rematerialized instructions the
   allocator claims to have added, which the validator checks (cf.
   [Regalloc_rematerializations]); empty unless the test says otherwise. *)
let check ?rematerializations name f ~exp_std ~exp_err =
  let rematerializations =
    match rematerializations with
    | Some rematerializations -> rematerializations
    | None -> Regalloc_rematerializations.create ()
  in
  check name f ~validate:(validate ~rematerializations) ~save ~exp_std ~exp_err

let ( .&() ) (cfg : Cfg_desc.t) (label : Label.t) : Block.t =
  List.find (fun (block : Block.t) -> block.start = label) cfg.blocks

let ( .!() ) (block : Block.t) (index : int) : Basic.t =
  List.nth block.body index

(* let () = check "IRC works on base templ" (fun templ _ -> let cfg =
   Cfg_desc.make templ in cfg, Regalloc_irc.run cfg) ~exp_std:"" ~exp_err:"" *)

let () =
  check "Duplicate instruction found when creating description"
    (fun () ->
      let templ, _ = base_templ () in
      let block = templ.&(add_label) in
      (* Duplicate the instruction. *)
      block.body <- List.hd block.body :: block.body;
      let cfg = Cfg_desc.make_pre_regalloc templ in
      cfg, cfg)
    ~exp_std:"fatal exception raised when creating description"
    ~exp_err:
      ">> Fatal error: Duplicate instruction no. 8 while adding a basic \
       instruction to the description"

let () =
  check "Duplicate terminator found when creating description"
    (fun () ->
      let templ, _ = base_templ () in
      (* Change id of one terminator to the id of the other one. *)
      templ.&(add_label).terminator.id <- templ.&(call_label).terminator.id;
      let cfg = Cfg_desc.make_pre_regalloc templ in
      cfg, cfg)
    ~exp_std:"fatal exception raised when creating description"
    ~exp_err:
      ">> Fatal error: Duplicate instruction no. 13 while adding a terminator \
       instruction to the description"

let () =
  check "Regalloc specific instructions are checked when creating description"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg = Cfg_desc.make ~remove_regalloc:false ~remove_locs:true templ in
      cfg, cfg)
    ~exp_std:"fatal exception raised when creating description"
    ~exp_err:">> Fatal error: instruction 19 is a spill"

let () =
  check "Terminator result count"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      templ.&(call_label).terminator.res <- [||];
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: In instruction's no 13 results: register array length \
       has changed. Before: 1. Now: 0."

let () =
  check "Instruction result count"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      templ.&(add_label).!(0).res <- [||];
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: In instruction's no 8 results: register array length \
       has changed. Before: 1. Now: 0."

let () =
  check "Terminator argument count"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      templ.&(return_label).terminator.arg <- [||];
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: In instruction's no 3 arguments: register array length \
       has changed. Before: 1. Now: 0."

let () =
  check "Function argument isn't preassigned"
    (fun () ->
      let templ, _make_id = base_templ () in
      templ.fun_args.(0) <- Reg.dummy;
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when creating description"
    ~exp_err:
      ">> Fatal error: Register in function arguments that isn't preassigned: \
       anon:I/0"

let () =
  check "Function argument count changed"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      templ.fun_args <- Array.sub templ.fun_args 0 1;
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: In function arguments: register array length has \
       changed. Before: 4. Now: 1."

let () =
  check "Function argument precoloring changed"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      templ.fun_args.(0) <- templ.fun_args.(1);
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      (Printf.sprintf
         ">> Fatal error: In function arguments: changed preassigned \
          register's location from %s to %s"
         (Regs.register_name Cmm.Int (P RAX))
         (Regs.register_name Cmm.Int (P RBX)))

let () =
  check "Location can't be unknown after allocation"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg = Cfg_desc.make_pre_regalloc templ in
      cfg, cfg)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      (if String.equal Config.architecture "amd64"
       then
         ">> Fatal error: instruction 20 has a register (anon:V/44) with an \
          unknown location"
       else
         ">> Fatal error: instruction 20 has a register (anon:V/68) with an \
          unknown location")

let () =
  check "Precoloring can't change"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      templ.&(move_param_label).!(7).res <- templ.&(move_param_label).!(6).res;
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      (Printf.sprintf
         ">> Fatal error: In instruction's no 17 results: changed preassigned \
          register's location from %s to %s"
         (Regs.register_name Cmm.Int (P RDI))
         (Regs.register_name Cmm.Int (P RBX)))

let () =
  check "Duplicate instruction found when validating description"
    (fun () ->
      let templ, _ = base_templ () in
      (* The spill has the same id as reload instruction. *)
      templ.&(move_param_label).!(4).id <- templ.&(move_tmp_res_label).!(2).id;
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Duplicate instruction no. 10 while checking a basic \
       instruction in the new CFG"

let () =
  check "Regalloc changed existing instruction into regalloc instruction"
    (fun () ->
      let templ, _ = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      templ.&(move_param_label).!(3).desc <- Op Spill;
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Register allocation changed existing instruction no. 23 \
       into a register allocation specific instruction"

let () =
  (* An added [Move] is accepted (split inserts phi moves), and so is an added
     rematerializable instruction (cf.
     [Regalloc_split_utils.is_rematerializable_shape]); an [Intop] is
     neither. *)
  check "Regalloc added non-regalloc specific instr"
    (fun () ->
      let templ, make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      let block = templ.&(add_label) in
      let r = (List.hd block.body).res in
      block.body
        <- { desc = Op (Intop Operation.Iadd);
             id = make_id ();
             arg = [| r.(0); r.(0) |];
             res = r
           }
           :: block.body;
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Register allocation added non-regalloc specific \
       instruction no. 26"

let () =
  check "Regalloc added a 'goto' and a block"
    (fun () ->
      let templ, make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      let tmp_label = new_label 1 in
      let templ =
        { templ with
          blocks =
            { start = tmp_label;
              exn = None;
              body = [];
              terminator =
                { desc = Always return_label;
                  res = [||];
                  arg = [||];
                  id = make_id ()
                }
            }
            :: templ.blocks
        }
      in
      templ.&(add_label).terminator.desc <- Always tmp_label;
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"" ~exp_err:""

let () =
  check "Regalloc added a fallthrough block that goes to the wrong label"
    (fun () ->
      let templ, make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      let tmp_label = new_label 1 in
      let templ =
        { templ with
          blocks =
            { start = tmp_label;
              exn = None;
              body = [];
              terminator =
                { desc = Always call_label;
                  res = [||];
                  arg = [||];
                  id = make_id ()
                }
            }
            :: templ.blocks
        }
      in
      templ.&(add_label).terminator.desc <- Always tmp_label;
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: When checking equivalence of labels before and after \
       allocation got different successor id's. Successor (label, instr id) \
       before: (6, 6). Successor (label, instr id) after: (8, 13)."

let () =
  check "Regalloc added a not allowed terminator and a block"
    (fun () ->
      let templ, make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      let tmp_label = new_label 1 in
      let templ =
        { templ with
          blocks =
            { start = tmp_label;
              exn = None;
              body = [];
              terminator =
                { desc = Return; res = [||]; arg = [||]; id = make_id () }
            }
            :: templ.blocks
        }
      in
      templ.&(add_label).terminator.desc <- Always tmp_label;
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Register allocation added a terminator no. 26 but \
       that's not allowed for this type of terminator: return"

let () =
  check "Regalloc reordered instructions between blocks"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      let add_body = templ.&(add_label).body in
      templ.&(add_label).body <- [];
      templ.&(return_label).body <- add_body @ templ.&(return_label).body;
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: The instruction's no. 8 successor id has changed. \
       Before allocation: 7. After allocation (ignoring instructions added by \
       allocation): 6."

let () =
  check "Regalloc reordered instructions within a block"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      let block = templ.&(move_tmp_res_label) in
      block.body
        <- ( block.body |> List.rev |> function
             | i1 :: i2 :: t -> i2 :: i1 :: t
             | l -> l |> List.rev );
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: The instruction's no. 11 successor id has changed. \
       Before allocation: 9. After allocation (ignoring instructions added by \
       allocation): 12."

let () =
  check "Regalloc added a loop"
    (fun () ->
      let templ, make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      let tmp_label = new_label 1 in
      let templ =
        { templ with
          blocks =
            { start = tmp_label;
              exn = None;
              body = [];
              terminator =
                { desc = Always tmp_label;
                  res = [||];
                  arg = [||];
                  id = make_id ()
                }
            }
            :: templ.blocks
        }
      in
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Visiting the same block 8 without knowing the successor \
       instruction's id. That means there's a loop consisting of only \
       instructions added by the register allocator."

let () =
  check "Regalloc changed instruction desc"
    (fun () ->
      let templ, _ = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      templ.&(add_label).!(0).desc <- Op (Intop Isub);
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:">> Fatal error: The desc of instruction with id 8 changed"

let () =
  check "Regalloc changed terminator desc"
    (fun () ->
      let templ, _ = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      templ.&(return_label).terminator.desc <- Raise Raise_regular;
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: The desc of terminator with id 3 changed, before: \
       return, after: raise."

let () =
  (* Deleting a non-pure instruction (the [Prologue]) is always rejected. *)
  check "Deleted instruction"
    (fun () ->
      let templ, _ = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      templ.&(entry_label).body <- List.tl templ.&(entry_label).body;
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Instruction no. 25 was deleted by register allocator"

let () =
  (* Deleting a pure instruction whose result is live (the [Iadd] feeds the
     return) is rejected at its effective successor, the terminator of
     [add_label]. The equations alone would not notice: the result register is
     also an argument at the same location, so the equation passes through. *)
  check "Deleted pure instruction with live result"
    (fun () ->
      let templ, _ = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      templ.&(add_label).body <- List.tl templ.&(add_label).body;
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:
      (Printf.sprintf
         "Validation failed: CFG REGALLOC Check failed in instr 7:\n\
          Instruction's description before allocation: goto 6\n\
          Instruction's description after allocation: goto 6\n\
          Message: Instruction no. 8 (intop + ) was deleted by register \
          allocator but its result anon:I/36 is live\n\
          Live equations for the normal successor: [anon:I/36=%s]"
         (Regs.register_name Cmm.Int (P RBX)))
    ~exp_err:""

let () =
  (* Deleting a pure instruction whose result is dead is accepted, as
     [Cfg_deadcode] does at the end of the split pass. *)
  check "Deleted pure instruction with dead result"
    (fun () ->
      let templ, make_id = base_templ () in
      let block = templ.&(add_label) in
      let add = block.!(0) in
      (* [int.(4)] is not used anywhere else. *)
      let dead : Basic.t =
        { id = make_id ();
          desc = Op (Intop Operation.Iadd);
          arg = add.arg;
          res = [| int.(4) |]
        }
      in
      block.body <- dead :: block.body;
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      block.body <- List.tl block.body;
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"" ~exp_err:""

(* Rematerialization: instead of spilling a register before a call and reloading
   it after, the split pass may re-execute after the call the pure instruction
   that defined it, and records the inserted copy (cf.
   [Regalloc_split.make_rematerialization]) for the validator, which checks the
   record against the pre-allocation description. The tests below extend
   [base_templ] with definitions appended to [move_param_label] (before the
   call) and a use, [int_arg1 := int_arg1 + use], appended to [add_label] (after
   the call). *)

(* Physical registers holding the rematerialized values after the call; not
   otherwise used by [base_templ] after the call (the parameters and the result
   are in rax, rbx and rdi). *)
let remat_loc = Reg.Reg (Regs.P Regs.RSI)

let remat_loc' = Reg.Reg (Regs.P Regs.RDX)

let remat_templ ~(use : Reg.t) =
  let templ, make_id = base_templ () in
  let add = templ.&(add_label) in
  let int_arg1 = add.!(0).res.(0) in
  let use_instr : Basic.t =
    { id = make_id ();
      desc = Op (Intop Operation.Iadd);
      arg = [| int_arg1; use |];
      res = [| int_arg1 |]
    }
  in
  add.body <- add.body @ [use_instr];
  templ, make_id, use_instr

let append_before_call (templ : Cfg_desc.t) (instrs : Basic.t list) =
  let block = templ.&(move_param_label) in
  block.body <- block.body @ instrs

let prepend_after_call (templ : Cfg_desc.t) (instrs : Basic.t list) =
  let block = templ.&(move_tmp_res_label) in
  block.body <- instrs @ block.body

let append_after_call (templ : Cfg_desc.t) (instrs : Basic.t list) =
  let block = templ.&(move_tmp_res_label) in
  block.body <- block.body @ instrs

let const_int ~make_id (res : Reg.t) : Basic.t =
  { id = make_id (); desc = Op (Const_int 42n); arg = [||]; res = [| res |] }

let add_imm ~make_id (arg : Reg.t) (res : Reg.t) : Basic.t =
  { id = make_id ();
    desc = Op (Intop_imm (Operation.Iadd, 1));
    arg = [| arg |];
    res = [| res |]
  }

let move ~make_id (arg : Reg.t) (res : Reg.t) : Basic.t =
  { id = make_id (); desc = Op Move; arg = [| arg |]; res = [| res |] }

let () =
  let rematerializations = Regalloc_rematerializations.create () in
  check ~rematerializations "Rematerialized constant"
    (fun () ->
      let c = Reg.For_testing.with_loc int.(4) remat_loc in
      let templ, make_id, use = remat_templ ~use:c in
      let const = const_int ~make_id c in
      append_before_call templ [const];
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      (* After allocation: [c] is not spilled across the call but recomputed
         after it, into a fresh register the use then reads. *)
      let c_after = Reg.For_testing.with_loc int.(5) remat_loc in
      let copy = const_int ~make_id c_after in
      prepend_after_call templ [copy];
      use.arg <- [| use.arg.(0); c_after |];
      Regalloc_rematerializations.record rematerializations ~id:copy.id
        { Regalloc_rematerializations.source = const.id; stamp = c.stamp };
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"" ~exp_err:""

let () =
  let rematerializations = Regalloc_rematerializations.create () in
  check ~rematerializations "Rematerialized constant with a later reload"
    (fun () ->
      let c = Reg.For_testing.with_loc int.(4) remat_loc in
      let templ, make_id, use = remat_templ ~use:c in
      let const = const_int ~make_id c in
      append_before_call templ [const];
      let add = templ.&(add_label) in
      let first_uses = add.body in
      let later_use : Basic.t = { use with id = make_id () } in
      add.body <- first_uses @ [later_use];
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      let c_slot = Reg.For_testing.with_loc int.(4) (Stack (Local 1)) in
      let c_after = Reg.For_testing.with_loc int.(5) remat_loc in
      let c_reloaded = Reg.For_testing.with_loc int.(6) remat_loc' in
      let spill : Basic.t =
        { id = make_id (); desc = Op Spill; arg = [| c |]; res = [| c_slot |] }
      in
      append_before_call templ [spill];
      let copy = const_int ~make_id c_after in
      prepend_after_call templ [copy];
      use.arg <- [| use.arg.(0); c_after |];
      let reload : Basic.t =
        { id = make_id ();
          desc = Op Reload;
          arg = [| c_slot |];
          res = [| c_reloaded |]
        }
      in
      later_use.arg <- [| later_use.arg.(0); c_reloaded |];
      add.body <- first_uses @ [reload; later_use];
      (* At [copy], both [c = remat_loc] and [c = slot 1] must hold: the
         rematerialization supplies the first use, not the later reload. *)
      Regalloc_rematerializations.record rematerializations ~id:copy.id
        { Regalloc_rematerializations.source = const.id; stamp = c.stamp };
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"" ~exp_err:""

let () =
  let rematerializations = Regalloc_rematerializations.create () in
  check ~rematerializations "Rematerialization clobbers another live value"
    (fun () ->
      let c = Reg.For_testing.with_loc int.(4) remat_loc in
      let templ, make_id, use = remat_templ ~use:c in
      let const = const_int ~make_id c in
      append_before_call templ [const];
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      let live = templ.&(add_label).!(0).arg.(1) in
      let c_after = Reg.For_testing.with_loc int.(5) live.loc in
      let copy = const_int ~make_id c_after in
      (* Overwrite the call result after its move to [live.loc], before the
         addition reads it. The same-location check must remain enforced. *)
      append_after_call templ [copy];
      use.arg <- [| use.arg.(0); c_after |];
      Regalloc_rematerializations.record rematerializations ~id:copy.id
        { Regalloc_rematerializations.source = const.id; stamp = c.stamp };
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:
      (let rdi = Regs.register_name Cmm.Int (P RDI) in
       let rbx = Regs.register_name Cmm.Int (P RBX) in
       Printf.sprintf
         "Validation failed: CFG REGALLOC Check failed in instr 28:\n\
          Instruction's description before allocation: anon:I/40 := 42\n\
          Instruction's description after allocation: %s := 42\n\
          Message: Unsatisfiable equations when removing result equations.\n\
          Existing equation has to agree on 0 or 2 sides (cannot be exactly 1) \
          with the removed equation.\n\
          Existing equation anon:I/37=%s.\n\
          Removed equation: anon:I/40=%s.\n\
          Live equations for the normal successor: [anon:I/36=%s anon:I/37=%s \
          anon:I/40=%s]"
         rdi rdi rdi rbx rdi rdi)
    ~exp_err:""

let () =
  let rematerializations = Regalloc_rematerializations.create () in
  check ~rematerializations "Rematerialized constant through a move"
    (fun () ->
      let c = Reg.For_testing.with_loc int.(4) remat_loc in
      let d = Reg.For_testing.with_loc int.(5) remat_loc' in
      let templ, make_id, use = remat_templ ~use:d in
      let const = const_int ~make_id c in
      append_before_call templ [const; move ~make_id c d];
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      (* [d] is recomputed directly from the constant, bypassing the move. *)
      let d_after = Reg.For_testing.with_loc int.(6) remat_loc' in
      let copy = const_int ~make_id d_after in
      prepend_after_call templ [copy];
      use.arg <- [| use.arg.(0); d_after |];
      Regalloc_rematerializations.record rematerializations ~id:copy.id
        { Regalloc_rematerializations.source = const.id; stamp = d.stamp };
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"" ~exp_err:""

let () =
  let rematerializations = Regalloc_rematerializations.create () in
  check ~rematerializations "Rematerialized Intop_imm with a reloaded argument"
    (fun () ->
      let c = Reg.For_testing.with_loc int.(4) remat_loc in
      let d = Reg.For_testing.with_loc int.(5) remat_loc' in
      let templ, make_id, use = remat_templ ~use:d in
      let const = const_int ~make_id c in
      let add = add_imm ~make_id c d in
      append_before_call templ [const; add];
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      (* [c] is spilled across the call and reloaded before the copy of [add]
         that recomputes [d]. *)
      let c_slot = Reg.For_testing.with_loc int.(4) (Stack (Local 1)) in
      let c_after = Reg.For_testing.with_loc int.(6) remat_loc in
      let d_after = Reg.For_testing.with_loc int.(7) remat_loc' in
      let spill : Basic.t =
        { id = make_id (); desc = Op Spill; arg = [| c |]; res = [| c_slot |] }
      in
      append_before_call templ [spill];
      let copy = add_imm ~make_id c_after d_after in
      prepend_after_call templ
        [ { id = make_id ();
            desc = Op Reload;
            arg = [| c_slot |];
            res = [| c_after |]
          };
          copy ];
      use.arg <- [| use.arg.(0); d_after |];
      Regalloc_rematerializations.record rematerializations ~id:copy.id
        { Regalloc_rematerializations.source = add.id; stamp = d.stamp };
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"" ~exp_err:""

let () =
  check "Rematerialized instruction without a record"
    (fun () ->
      let c = Reg.For_testing.with_loc int.(4) remat_loc in
      let templ, make_id, use = remat_templ ~use:c in
      append_before_call templ [const_int ~make_id c];
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      let c_after = Reg.For_testing.with_loc int.(5) remat_loc in
      prepend_after_call templ [const_int ~make_id c_after];
      use.arg <- [| use.arg.(0); c_after |];
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Rematerialized instruction no. 28: no recorded \
       rematerialization"

let () =
  let rematerializations = Regalloc_rematerializations.create () in
  check ~rematerializations "Rematerialized instruction differs from its source"
    (fun () ->
      let c = Reg.For_testing.with_loc int.(4) remat_loc in
      let templ, make_id, use = remat_templ ~use:c in
      append_before_call templ [const_int ~make_id c];
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      let c_after = Reg.For_testing.with_loc int.(5) remat_loc in
      let copy = const_int ~make_id c_after in
      prepend_after_call templ [copy];
      use.arg <- [| use.arg.(0); c_after |];
      (* The recorded source is the addition of [add_label], not the
         constant. *)
      Regalloc_rematerializations.record rematerializations ~id:copy.id
        { Regalloc_rematerializations.source = templ.&(add_label).!(0).id;
          stamp = c.stamp
        };
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Rematerialized instruction no. 28: const_int 42 differs \
       from its source instruction no. 8 (intop + )"

let () =
  let rematerializations = Regalloc_rematerializations.create () in
  check ~rematerializations "Rematerialized register with several definitions"
    (fun () ->
      let c = Reg.For_testing.with_loc int.(4) remat_loc in
      let templ, make_id, use = remat_templ ~use:c in
      let const = const_int ~make_id c in
      append_before_call templ [const];
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      let c_after = Reg.For_testing.with_loc int.(5) remat_loc in
      let copy = const_int ~make_id c_after in
      prepend_after_call templ [copy];
      use.arg <- [| use.arg.(0); c_after |];
      (* The copy is claimed to recompute [int_arg1], which is defined by the
         parameter move and by the two additions of [add_label]. *)
      Regalloc_rematerializations.record rematerializations ~id:copy.id
        { Regalloc_rematerializations.source = const.id;
          stamp = use.res.(0).stamp
        };
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Rematerialized instruction no. 28: register anon:I/36 \
       has 3 definitions"

let () =
  let rematerializations = Regalloc_rematerializations.create () in
  check ~rematerializations "Rematerialized register defined by a non-move"
    (fun () ->
      let c = Reg.For_testing.with_loc int.(4) remat_loc in
      let d = Reg.For_testing.with_loc int.(5) remat_loc' in
      let templ, make_id, use = remat_templ ~use:d in
      let const = const_int ~make_id c in
      append_before_call templ [const; add_imm ~make_id c d];
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      (* The copy of the constant is claimed to recompute [d], which is actually
         [c + 1]. *)
      let d_after = Reg.For_testing.with_loc int.(6) remat_loc' in
      let copy = const_int ~make_id d_after in
      prepend_after_call templ [copy];
      use.arg <- [| use.arg.(0); d_after |];
      Regalloc_rematerializations.record rematerializations ~id:copy.id
        { Regalloc_rematerializations.source = const.id; stamp = d.stamp };
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Rematerialized instruction no. 29: register anon:I/41 \
       is defined by instruction no. 28, which is neither the source \
       instruction no. 27 nor a move"

let () =
  let rematerializations = Regalloc_rematerializations.create () in
  check ~rematerializations
    "Rematerialization source argument with several definitions"
    (fun () ->
      let d = Reg.For_testing.with_loc int.(5) remat_loc' in
      let templ, make_id, use = remat_templ ~use:d in
      let int_arg1 = use.res.(0) in
      let add = add_imm ~make_id int_arg1 d in
      append_before_call templ [add];
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      (* [int_arg1] is defined by the parameter move and by the two additions of
         [add_label]: [d] may differ from [int_arg1 + 1] after the call. *)
      let d_after = Reg.For_testing.with_loc int.(6) remat_loc' in
      let copy = add_imm ~make_id int_arg1 d_after in
      append_after_call templ [copy];
      use.arg <- [| use.arg.(0); d_after |];
      Regalloc_rematerializations.record rematerializations ~id:copy.id
        { Regalloc_rematerializations.source = add.id; stamp = d.stamp };
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Rematerialized instruction no. 28: source argument \
       anon:I/36 has 3 definitions"

let () =
  let rematerializations = Regalloc_rematerializations.create () in
  check ~rematerializations "Rematerialization source in a cycle"
    (fun () ->
      let c = Reg.For_testing.with_loc int.(4) remat_loc in
      let templ, make_id, use = remat_templ ~use:c in
      let const = const_int ~make_id c in
      append_before_call templ [const];
      (* Loop from [add_label] back to [move_param_label]: the constant is then
         part of a cycle (although still defined only once). *)
      let add = templ.&(add_label) in
      add.terminator.desc
        <- Parity_test { ifso = move_param_label; ifnot = return_label };
      add.terminator.arg <- [| use.res.(0) |];
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      let c_after = Reg.For_testing.with_loc int.(5) remat_loc in
      let copy = const_int ~make_id c_after in
      prepend_after_call templ [copy];
      use.arg <- [| use.arg.(0); c_after |];
      Regalloc_rematerializations.record rematerializations ~id:copy.id
        { Regalloc_rematerializations.source = const.id; stamp = c.stamp };
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Rematerialized instruction no. 28: source result \
       anon:I/40 is defined by instruction no. 27 in block 2, which is part of \
       a cycle"

(* A nullable address is only dereferenced on the non-null branch. The source
   load is in [move_param_label]; [return_label] uses its result. *)
let guarded_load_templ () =
  let sequence = InstructionId.make_sequence () in
  let make_id () = InstructionId.get_and_incr sequence in
  let arg = Proc.phys_reg Cmm.Int (Regs.P Regs.RAX) in
  let p = Reg.For_testing.with_loc int.(4) arg.loc in
  let q = Reg.For_testing.with_loc int.(5) remat_loc in
  let source : Basic.t =
    { id = make_id ();
      desc =
        Op
          (Load
             { memory_chunk = Word_int;
               addressing_mode = Arch.Iindexed 0;
               mutability = Immutable;
               is_atomic = false
             });
      arg = [| p |];
      res = [| q |]
    }
  in
  let templ : Cfg_desc.t =
    { fun_args = [| arg |];
      fun_contains_calls = false;
      fun_ret_type = Cmm.typ_int;
      blocks =
        [ { start = entry_label;
            exn = None;
            body = [move ~make_id arg p];
            terminator =
              { id = make_id ();
                desc =
                  Truth_test { ifso = move_param_label; ifnot = call_label };
                arg = [| p |];
                res = [||]
              }
          };
          { start = move_param_label;
            exn = None;
            body = [source];
            terminator =
              { id = make_id ();
                desc = Always return_label;
                arg = [||];
                res = [||]
              }
          };
          { start = return_label;
            exn = None;
            body = [move ~make_id q arg];
            terminator =
              { id = make_id (); desc = Return; arg = [| arg |]; res = [||] }
          };
          { start = call_label;
            exn = None;
            body =
              [ { id = make_id ();
                  desc = Op (Const_int 0n);
                  arg = [||];
                  res = [| arg |]
                } ];
            terminator =
              { id = make_id (); desc = Return; arg = [| arg |]; res = [||] }
          } ]
    }
  in
  let copy : Basic.t =
    { source with
      id = make_id ();
      res = [| Reg.For_testing.with_loc int.(6) remat_loc |]
    }
  in
  let rematerializations = Regalloc_rematerializations.create () in
  Regalloc_rematerializations.record rematerializations ~id:copy.id
    { source = source.id; stamp = q.stamp };
  templ, make_id, source, copy, rematerializations

let () =
  let templ, _, source, copy, rematerializations = guarded_load_templ () in
  check ~rematerializations "Rematerialized load before its guard"
    (fun () ->
      let before = Cfg_desc.make_pre_regalloc templ in
      (* The address is defined, but may be null. Even though [copy]'s result is
         unused, executing the added load is not safe. *)
      let entry = templ.&(entry_label) in
      entry.body <- entry.body @ [copy];
      before, Cfg_desc.make_post_regalloc templ)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      (Format.asprintf
         ">> Fatal error: Rematerialized instruction no. %a: source load no. \
          %a in block %a does not strictly dominate block %a"
         InstructionId.format copy.id InstructionId.format source.id
         Label.format move_param_label Label.format entry_label)

let () =
  let templ, _, _, copy, rematerializations = guarded_load_templ () in
  check ~rematerializations "Rematerialized load after its guarded source"
    (fun () ->
      let before = Cfg_desc.make_pre_regalloc templ in
      let return = templ.&(return_label) in
      return.!(0).arg <- copy.res;
      return.body <- copy :: return.body;
      before, Cfg_desc.make_post_regalloc templ)
    ~exp_std:"" ~exp_err:""

let () =
  let templ, make_id, _, copy, rematerializations = guarded_load_templ () in
  check ~rematerializations "Rematerialized deleted load on a split edge"
    (fun () ->
      let before = Cfg_desc.make_pre_regalloc templ in
      let source_block = templ.&(move_param_label) in
      source_block.body <- [];
      let copy_label = new_label 0 in
      source_block.terminator.desc <- Always copy_label;
      templ.&(return_label).!(0).arg <- copy.res;
      let copy_block : Block.t =
        { start = copy_label;
          exn = None;
          body = [copy];
          terminator =
            { id = make_id ();
              desc = Always return_label;
              arg = [||];
              res = [||]
            }
        }
      in
      let after =
        Cfg_desc.make_post_regalloc
          { templ with blocks = copy_block :: templ.blocks }
      in
      before, after)
    ~exp_std:"" ~exp_err:""

let make_loop ~loop_loc_first n =
  let make_id =
    let seq = InstructionId.make_sequence () in
    let _zero : InstructionId.t = InstructionId.get_and_incr seq in
    let _one : InstructionId.t = InstructionId.get_and_incr seq in
    let _two : InstructionId.t = InstructionId.get_and_incr seq in
    fun () -> InstructionId.get_and_incr seq
  in
  let make_locs regs f =
    let locs = f (Array.map (fun (r : Reg.t) -> r.typ) regs) in
    let regs =
      Array.map2
        (fun (reg : Reg.t) (loc : Reg.t) ->
          Reg.For_testing.with_loc reg loc.loc)
        regs locs
    in
    regs, locs
  in
  let loop_loc_label, loop_reg_label =
    let l1 = new_label 1 in
    let l2 = new_label 2 in
    if loop_loc_first then l1, l2 else l2, l1
  in
  let stack_loc =
    let locs =
      Array.init (n + 1) (fun i -> Reg.create_at_location Int (Stack (Local i)))
    in
    fun i -> locs.(i)
  in
  let int_arg1 = int.(0) in
  let int_arg2 = int.(1) in
  let int_arg3 = int.(2) in
  let args, arg_locs =
    make_locs [| int_arg1; int_arg2; int_arg3 |] Proc.loc_parameters
  in
  let int_arg1 = args.(0) in
  let int_arg2 = args.(1) in
  let int_arg3 = args.(2) in
  let extra_regs =
    Array.init n (fun _ -> Reg.create_at_location Int int_arg3.loc)
  in
  let results, result_locs = make_locs [| int_arg1 |] Proc.loc_results_return in
  let make_moves src dst =
    Array.map2
      (fun src dst : Basic.t ->
        { id = make_id (); desc = Op Move; arg = [| src |]; res = [| dst |] })
      src dst
    |> Array.to_list
  in
  let templ : Cfg_desc.t =
    { fun_args = arg_locs;
      blocks =
        [ { start = entry_label;
            body = [{ id = make_id (); desc = Prologue; arg = [||]; res = [||] }];
            exn = None;
            terminator =
              { id = make_id ();
                desc = Always move_param_label;
                arg = [||];
                res = [||]
              }
          };
          { start = move_param_label;
            body =
              make_moves arg_locs args
              (* Move [arg3] to all [extra_regs]. *)
              @ List.init n (fun n ->
                  { Instruction.id = make_id ();
                    desc = Op Move;
                    arg = [| int_arg3 |];
                    res = [| extra_regs.(n) |]
                  })
              (* Spill [arg3] to locations [0;n-1] *)
              @ List.init n (fun n ->
                  { Instruction.id = make_id ();
                    desc = Op Spill;
                    arg = [| int_arg3 |];
                    res = [| stack_loc n |]
                  })
              (* Spill [arg2] to location n. If we spilled [arg3] the code would
                 be correct. *)
              @ [ { Instruction.id = make_id ();
                    desc = Op Spill;
                    arg = [| int_arg2 |];
                    res = [| stack_loc n |]
                  } ];
            exn = None;
            terminator =
              { id = make_id ();
                desc =
                  Int_test
                    { lt = loop_loc_label;
                      eq = loop_reg_label;
                      gt = move_tmp_res_label;
                      is_signed = Signed;
                      imm = Some 0
                    };
                arg = [| int_arg1 |];
                res = [||]
              }
          };
          { start = loop_loc_label;
            body =
              (* Rotate all locations by one index. *)
              List.init n (fun n ->
                  (* Move loc i+1 to i. *)
                  [ { Instruction.id = make_id ();
                      desc = Op Reload;
                      arg = [| stack_loc (n + 1) |];
                      res = [| int_arg3 |]
                    };
                    { Instruction.id = make_id ();
                      desc = Op Spill;
                      arg = [| int_arg3 |];
                      res = [| stack_loc n |]
                    } ])
              |> List.concat;
            exn = None;
            terminator =
              { id = make_id ();
                desc =
                  Int_test
                    { lt = loop_loc_label;
                      eq = loop_reg_label;
                      gt = move_tmp_res_label;
                      is_signed = Signed;
                      imm = Some 1
                    };
                arg = [| int_arg1 |];
                res = [||]
              }
          };
          { start = loop_reg_label;
            body =
              (* Rotate all regs by one index. *)
              List.init (n - 1) (fun n ->
                  (* Move reg i+1 to i. *)
                  { Instruction.id = make_id ();
                    desc = Op Move;
                    arg = [| extra_regs.(n + 1) |];
                    res = [| extra_regs.(n) |]
                  });
            exn = None;
            terminator =
              { id = make_id ();
                desc =
                  Int_test
                    { lt = loop_reg_label;
                      eq = move_tmp_res_label;
                      gt = move_tmp_res_label;
                      is_signed = Signed;
                      imm = Some 2
                    };
                arg = [| int_arg1 |];
                res = [||]
              }
          };
          { start = move_tmp_res_label;
            body =
              (* Require that extra reg 0 is in location 0. This will break
                 after loop is run at least [n] times because then the spilled
                 [arg2] in location n will rotate over to location 0. For that
                 reason the fix-point algorithm will also have to run n
                 times. *)
              [ { Instruction.id = make_id ();
                  desc = Op (Const_int (Nativeint.of_int 1));
                  arg = [||];
                  res = [| int_arg1 |]
                };
                (* Load extra reg 0 from location 0.*)
                { Instruction.id = make_id ();
                  desc = Op Reload;
                  arg = [| stack_loc 0 |];
                  res = [| extra_regs.(0) |]
                };
                (* Add the extra reg 0 to accumalated result. *)
                { Instruction.id = make_id ();
                  desc = Op (Intop Iadd);
                  arg = [| int_arg1; extra_regs.(0) |];
                  res = [| int_arg1 |]
                } ];
            exn = None;
            terminator =
              { id = make_id ();
                desc = Always return_label;
                arg = [||];
                res = [||]
              }
          };
          { start = return_label;
            body =
              make_moves [| int_arg1 |] results
              @ make_moves results result_locs
              @ [ { id = make_id ();
                    desc = Reloadretaddr;
                    arg = [||];
                    res = [||]
                  } ];
            exn = None;
            terminator =
              { id = make_id (); desc = Return; arg = result_locs; res = [||] }
          } ];
      fun_contains_calls = true;
      fun_ret_type = Cmm.typ_val
    }
  in
  Cfg_desc.make_pre_regalloc templ, Cfg_desc.make_post_regalloc templ

let test_loop ~loop_loc_first n =
  assert (n >= 2);
  let start_time = Sys.time () in
  check
    (Printf.sprintf "Check loop with %d locations" n)
    (fun () -> make_loop ~loop_loc_first n)
    ~exp_std:
      (Printf.sprintf
         "Validation failed: Bad equations at entry point, reason: \
          Unsatisfiable equations when removing result equations.\n\
          Existing equation has to agree on 0 or 2 sides (cannot be exactly 1) \
          with the removed equation.\n\
          Existing equation R[%s]=%s.\n\
          Removed equation: R[%s]=%s.\n\
          Equations: R[%s]=%s R[%s]=%s R[%s]=%s\n\
          Function argument descriptions: R[%s], R[%s], R[%s]\n\
          Function argument locations: %s, %s, %s"
         (Regs.register_name Cmm.Int (P RDI))
         (Regs.register_name Cmm.Int (P RBX))
         (Regs.register_name Cmm.Int (P RBX))
         (Regs.register_name Cmm.Int (P RBX))
         (Regs.register_name Cmm.Int (P RAX))
         (Regs.register_name Cmm.Int (P RAX))
         (Regs.register_name Cmm.Int (P RDI))
         (Regs.register_name Cmm.Int (P RBX))
         (Regs.register_name Cmm.Int (P RDI))
         (Regs.register_name Cmm.Int (P RDI))
         (Regs.register_name Cmm.Int (P RAX))
         (Regs.register_name Cmm.Int (P RBX))
         (Regs.register_name Cmm.Int (P RDI))
         (Regs.register_name Cmm.Int (P RAX))
         (Regs.register_name Cmm.Int (P RBX))
         (Regs.register_name Cmm.Int (P RDI)))
    ~exp_err:"";
  let end_time = Sys.time () in
  Format.printf "  Time of loop test: %fs\n" (end_time -. start_time);
  ()

let () = test_loop ~loop_loc_first:true 2

let () = test_loop ~loop_loc_first:true 5

let () = test_loop ~loop_loc_first:true 10

let () = test_loop ~loop_loc_first:true 20

let () = test_loop ~loop_loc_first:false 2

let () = test_loop ~loop_loc_first:false 5

let () = test_loop ~loop_loc_first:false 10

let () = test_loop ~loop_loc_first:false 20

let () =
  (* Between the calls only [x] and [z] occur initially. Rematerializing them
     introduces uses of [a] and [p], retaining [p]'s reload and allowing [a] to
     be rematerialized before [x] in the same split pass. *)
  let ids = InstructionId.make_sequence () in
  let instruction ?(arg = [||]) ?(res = [||]) desc =
    Cfg.make_instruction ~desc ~arg ~res ~stack_offset:0
      ~id:(InstructionId.get_and_incr ids)
      ()
  in
  let copy src dst = instruction (Op Move) ~arg:[| src |] ~res:[| dst |] in
  let load src dst offset =
    instruction
      (Op
         (Load
            { memory_chunk = Word_val;
              addressing_mode = Arch.Iindexed offset;
              mutability = Immutable;
              is_atomic = false
            }))
      ~arg:[| src |] ~res:[| dst |]
  in
  let block label body terminator =
    let block =
      { (Cfg.make_empty_block ~label terminator) with
        can_raise = Cfg.can_raise_terminator terminator.desc;
        stack_offset = 0
      }
    in
    List.iter (DLL.add_end block.body) body;
    block
  in
  let p = Reg.create Val in
  let a = Reg.create Val in
  let x = Reg.create Val in
  let z = Reg.create Val in
  let y = Reg.create Val in
  let other = Reg.create Val in
  let result = Reg.create Int in
  let arg0 = Proc.phys_reg Val (P RAX) in
  let arg1 = Proc.phys_reg Val (P RBX) in
  let return_reg = Proc.phys_reg Int (P RAX) in
  let middle_label = Label.new_label () in
  let last_label = Label.new_label () in
  let call label_after =
    instruction
      (Call
         { op = Direct { sym_name = "consume"; sym_global = Global };
           label_after
         })
      ~arg:[| arg0; arg1 |] ~res:[| arg0 |]
  in
  let a_load = load p a 0 in
  let x_load = load a x 0 in
  let z_load = load p z 8 in
  let first =
    block Label.entry_label
      [copy arg0 p; a_load; x_load; z_load; copy x arg0; copy z arg1]
      (call middle_label)
  in
  let middle =
    block middle_label [copy x arg0; copy z arg1] (call last_label)
  in
  let last =
    block last_label
      [ load a y 8;
        load p other 16;
        instruction (Op (Intop Iadd)) ~arg:[| y; other |] ~res:[| result |];
        copy result return_reg ]
      (instruction Return ~arg:[| return_reg |])
  in
  let cfg =
    Cfg.create ~fun_name:"cascading_rematerialization" ~fun_args:[| arg0 |]
      ~fun_codegen_options:
        [ Use_regalloc_param
            [ "SPLIT_REMATERIALIZE:on";
              "IRC_INTERF_THRESHOLD:0";
              "STACK_SLOTS_THRESHOLD:1" ] ]
      ~fun_dbg:Debuginfo.none ~fun_contains_calls:true
      ~fun_num_stack_slots:(Stack_class.Tbl.make 0)
      ~fun_poll:Lambda.Default_poll ~next_instruction_id:ids
      ~fun_ret_type:Cmm.typ_int ~fun_phantom_lets:Backend_var.Map.empty
      ~allowed_to_be_irreducible:false
  in
  List.iter (Cfg.add_block_exn cfg) [first; middle; last];
  Cfg.register_predecessors_for_all_blocks cfg;
  let layout =
    Cfg_with_layout.create cfg
      ~layout:(DLL.of_list [first.start; middle.start; last.start])
  in
  let description =
    Misc.protect_refs
      [R (Oxcaml_flags.regalloc_validate, true)]
      (fun () -> Regalloc_validate.Description.create layout)
    |> Option.get
  in
  let prepared =
    match Regalloc_irc.run (Cfg_with_infos.make layout) with
    | Ok _ -> failwith "IRC did not fall back"
    | Error prepared -> prepared
  in
  let copies =
    DLL.to_list middle.body
    |> List.mapi (fun index instr -> index, instr)
    |> List.filter_map (fun (index, instr) ->
        match
          Regalloc_rematerializations.find prepared.rematerializations
            instr.Cfg.id
        with
        | None -> None
        | Some record -> Some (index, instr, record))
  in
  let find_copy (source : Cfg.basic Cfg.instruction) =
    List.find
      (fun (_, _, record) ->
        InstructionId.equal record.Regalloc_rematerializations.source source.id)
      copies
  in
  assert (List.length copies = 3);
  let a_index, a_copy, _ = find_copy a_load in
  let x_index, x_copy, _ = find_copy x_load in
  let _ = find_copy z_load in
  assert (a_index < x_index);
  assert (Reg.same a_copy.res.(0) x_copy.arg.(0));
  let live = Cfg_with_infos.liveness prepared.cfg_with_infos in
  let stack_regs =
    Reg.Set.filter Reg.is_stack
      (InstructionId.Tbl.find live first.terminator.id).across
  in
  assert (not (Reg.Set.is_empty stack_regs));
  let next_id = InstructionId.get ids in
  let allocated, rematerializations = Regalloc_ls.run_after_prelude prepared in
  (* This low-pressure CFG needs no allocation rewrites. In particular, fallback
     must not reset the existing stack twins and split them again. *)
  assert (InstructionId.equal next_id (InstructionId.get ids));
  assert (Reg.Set.for_all Reg.is_stack stack_regs);
  assert (Regalloc_stack_slots.total_number_of_slots prepared.stack_slots = 1);
  assert (rematerializations == prepared.rematerializations);
  (match
     Regalloc_validate.test description ~rematerializations
       (Cfg_with_infos.cfg_with_layout allocated)
   with
  | Ok _ -> ()
  | Error error -> Misc.fatal_errorf "%a" Regalloc_validate.Error.print error);
  Format.printf "Cascading rematerialization and IRC fallback: OK\n%!"
