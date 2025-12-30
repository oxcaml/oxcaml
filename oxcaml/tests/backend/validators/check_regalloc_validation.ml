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

let check =
  check
    ~validate:(fun (before, after) ->
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
          try Regalloc_validate.test desc after
          with Misc.Fatal_error ->
            Format.printf "fatal exception raised when validating description";
            raise Break_test
        in
        match res with
        | Ok cfg ->
          if cfg = after then () else Format.printf "Validation changed cfg"
        | Error error ->
          Format.printf "Validation failed: %a" Regalloc_validate.Error.print
            error
      with Break_test -> ())
    ~save:(fun (before, after) ->
      (* CR azewierzejew for azewierzejew: Fix how the files are saved. *)
      Cfg_with_layout.save_as_dot ~filename:"/tmp/before.dot"
        (Cfg_with_infos.cfg_with_layout before)
        "test-cfg-before";
      Cfg_with_layout.save_as_dot ~filename:"/tmp/after.dot"
        (Cfg_with_infos.cfg_with_layout after)
        "test-cfg-after";
      Format.printf "The failing cfgs were put in /tmp/[before|after].dot\n")

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
         (Reg_class.register_name Cmm.Int 0)
         (Reg_class.register_name Cmm.Int 1))

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
        ">> Fatal error: instruction 20 has a register (anon:V/37) with an \
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
         (Reg_class.register_name Cmm.Int 2)
         (Reg_class.register_name Cmm.Int 1))

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
  (*= CR xclerc for xclerc: same as above (polymorphic commpare on values
      with cycles).
      let () =
     check "Regalloc added non-regalloc specific instr"
       (fun () ->
         let templ, make_id = base_templ () in
         let cfg1 = Cfg_desc.make_pre_regalloc templ in
         let block = templ.&(add_label) in
         let r = (List.hd block.body).res in
         block.body
           <- { desc = Op Move; id = make_id (); arg = r; res = r } :: block.body;
         let cfg2 = Cfg_desc.make_post_regalloc templ in
         cfg1, cfg2)
       ~exp_std:"fatal exception raised when validating description"
       ~exp_err:
         ">> Fatal error: Register allocation added non-regalloc specific \
          instruction no. 26"
  *)
  (*= CR xclerc for xclerc: same as above (polymorphic commpare on values
     with cycles).
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
  *)
  [@@ocamlformat "wrap-comments=false"]

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
       that's not allowed for this type of terminator: Return"

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
        <- (block.body |> List.rev |> function
            | i1 :: i2 :: t -> i2 :: i1 :: t
            | l -> l |> List.rev);
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: The instruction's no. 12 successor id has changed. \
       Before allocation: 11. After allocation (ignoring instructions added by \
       allocation): 9."

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
       Return, after: Raise."

let () =
  check "Deleted instruction"
    (fun () ->
      let templ, _ = base_templ () in
      let cfg1 = Cfg_desc.make_pre_regalloc templ in
      templ.&(add_label).body <- List.tl templ.&(add_label).body;
      let cfg2 = Cfg_desc.make_post_regalloc templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Instruction no. 8 was deleted by register allocator"

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
         (Reg_class.register_name Cmm.Int 2)
         (Reg_class.register_name Cmm.Int 1)
         (Reg_class.register_name Cmm.Int 1)
         (Reg_class.register_name Cmm.Int 1)
         (Reg_class.register_name Cmm.Int 0)
         (Reg_class.register_name Cmm.Int 0)
         (Reg_class.register_name Cmm.Int 2)
         (Reg_class.register_name Cmm.Int 1)
         (Reg_class.register_name Cmm.Int 2)
         (Reg_class.register_name Cmm.Int 2)
         (Reg_class.register_name Cmm.Int 0)
         (Reg_class.register_name Cmm.Int 1)
         (Reg_class.register_name Cmm.Int 2)
         (Reg_class.register_name Cmm.Int 0)
         (Reg_class.register_name Cmm.Int 1)
         (Reg_class.register_name Cmm.Int 2))
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
