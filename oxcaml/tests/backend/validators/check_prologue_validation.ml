open Cfg_intf.S
open Utils

let check =
  check
    ~validate:(fun cfg ->
      try
        let _ =
          Misc.protect_refs
            [R (Oxcaml_flags.cfg_prologue_validate, true)]
            (fun () -> Cfg_prologue.validate cfg)
        in
        ()
      with
      | Misc.Fatal_error -> ()
      | exn ->
        Format.printf "Unexpected exception: %s" (Printexc.to_string exn);
        ())
    ~save:(fun cfg ->
      (* CR cfalas: Fix how the files are saved. *)
      let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg in
      Cfg_with_layout.save_as_dot ~filename:"/tmp/test.dot" cfg_with_layout
        "test-cfg";
      Format.printf "The failing cfg was put in /tmp/test.dot\n")

let seq = ref (InstructionId.make_sequence ())

let make_id () = InstructionId.get_and_incr !seq

let () =
  check "Single-block with necessary prologue"
    (fun () ->
      seq := InstructionId.make_sequence ();
      let cfg : Cfg_desc.t =
        let stack_loc = [| Reg.create_at_location Int (Stack (Local 0)) |] in
        { fun_args = [||];
          blocks =
            [ { start = entry_label;
                body =
                  [ { id = make_id (); desc = Prologue; arg = [||]; res = [||] };
                    { id = make_id ();
                      desc = Op Spill;
                      arg = [| int.(0) |];
                      res = stack_loc
                    };
                    { id = make_id ();
                      desc = Op Reload;
                      arg = stack_loc;
                      res = int
                    };
                    { id = make_id (); desc = Epilogue; arg = [||]; res = [||] }
                  ];
                exn = None;
                terminator =
                  { id = make_id (); desc = Return; arg = [||]; res = [||] }
              } ];
          fun_contains_calls = false;
          fun_ret_type = Cmm.typ_void
        }
      in
      Cfg_desc.make_post_regalloc cfg)
    ~exp_std:"" ~exp_err:""

(* CR-soon cfalas: this is currently allowed, but should be disallowed to make
   sure we don't emit useless prologues. *)
let () =
  check "Single-block with unnecessary prologue"
    (fun () ->
      seq := InstructionId.make_sequence ();
      let cfg : Cfg_desc.t =
        { fun_args = [||];
          blocks =
            [ { start = entry_label;
                body =
                  [ { id = make_id (); desc = Prologue; arg = [||]; res = [||] };
                    { id = make_id (); desc = Epilogue; arg = [||]; res = [||] }
                  ];
                exn = None;
                terminator =
                  { id = make_id (); desc = Return; arg = [||]; res = [||] }
              } ];
          fun_contains_calls = false;
          fun_ret_type = Cmm.typ_void
        }
      in
      Cfg_desc.make_post_regalloc cfg)
    ~exp_std:"" ~exp_err:""

let () =
  check "Missing prologue when using stack slots"
    (fun () ->
      seq := InstructionId.make_sequence ();
      let stack_loc = [| Reg.create_at_location Int (Stack (Local 0)) |] in
      let cfg : Cfg_desc.t =
        { fun_args = [||];
          blocks =
            [ { start = entry_label;
                body =
                  [ { id = make_id ();
                      desc = Op Spill;
                      arg = [| int.(0) |];
                      res = stack_loc
                    } ];
                exn = None;
                terminator =
                  { id = make_id (); desc = Return; arg = [||]; res = [||] }
              } ];
          fun_contains_calls = false;
          fun_ret_type = Cmm.typ_void
        }
      in
      Cfg_desc.make_post_regalloc cfg)
    ~exp_std:""
    ~exp_err:
      ">> Fatal error: Cfg_prologue: error validating instruction #0001: \
       instruction needs prologue but no prologue on the stack"

let () =
  check "Epilogue without prologue"
    (fun () ->
      seq := InstructionId.make_sequence ();
      let cfg : Cfg_desc.t =
        { fun_args = [||];
          blocks =
            [ { start = entry_label;
                body =
                  [{ id = make_id (); desc = Epilogue; arg = [||]; res = [||] }];
                exn = None;
                terminator =
                  { id = make_id (); desc = Return; arg = [||]; res = [||] }
              } ];
          fun_contains_calls = false;
          fun_ret_type = Cmm.typ_void
        }
      in
      Cfg_desc.make_post_regalloc cfg)
    ~exp_std:""
    ~exp_err:
      ">> Fatal error: Cfg_prologue: error validating instruction #0001: \
       epilogue appears without a prologue on the stack"

let () =
  check "Nested prologues"
    (fun () ->
      seq := InstructionId.make_sequence ();
      let cfg : Cfg_desc.t =
        { fun_args = [||];
          blocks =
            [ { start = entry_label;
                body =
                  [ { id = make_id (); desc = Prologue; arg = [||]; res = [||] };
                    { id = make_id (); desc = Prologue; arg = [||]; res = [||] }
                  ];
                exn = None;
                terminator =
                  { id = make_id (); desc = Return; arg = [||]; res = [||] }
              } ];
          fun_contains_calls = false;
          fun_ret_type = Cmm.typ_void
        }
      in
      Cfg_desc.make_post_regalloc cfg)
    ~exp_std:""
    ~exp_err:
      ">> Fatal error: Cfg_prologue: error validating instruction #0001: \
       prologue appears while prologue is already on the stack"

let () =
  check "Prologue in loop"
    (fun () ->
      seq := InstructionId.make_sequence ();
      let loop_head = new_label 1 in
      let loop_body = new_label 2 in
      let loop_exit = new_label 3 in
      let cfg : Cfg_desc.t =
        { fun_args = [||];
          blocks =
            [ { start = entry_label;
                body = [];
                exn = None;
                terminator =
                  { id = make_id ();
                    desc = Always loop_head;
                    arg = [||];
                    res = [||]
                  }
              };
              { start = loop_head;
                body = [];
                exn = None;
                terminator =
                  { id = make_id ();
                    desc = Truth_test { ifso = loop_body; ifnot = loop_exit };
                    arg = [| int.(0) |];
                    res = [||]
                  }
              };
              { start = loop_body;
                body =
                  [{ id = make_id (); desc = Prologue; arg = [||]; res = [||] }];
                exn = None;
                terminator =
                  { id = make_id ();
                    desc = Always loop_head;
                    arg = [||];
                    res = [||]
                  }
              };
              { start = loop_exit;
                body =
                  [{ id = make_id (); desc = Epilogue; arg = [||]; res = [||] }];
                exn = None;
                terminator =
                  { id = make_id (); desc = Return; arg = [||]; res = [||] }
              } ];
          fun_contains_calls = false;
          fun_ret_type = Cmm.typ_void
        }
      in
      Cfg_desc.make_post_regalloc cfg)
    ~exp_std:""
    ~exp_err:
      ">> Fatal error: Cfg_prologue: error validating instruction #0003: \
       prologue appears while prologue is already on the stack"

let () =
  check "Return with prologue on stack"
    (fun () ->
      seq := InstructionId.make_sequence ();
      let cfg : Cfg_desc.t =
        { fun_args = [||];
          blocks =
            [ { start = entry_label;
                body =
                  [{ id = make_id (); desc = Prologue; arg = [||]; res = [||] }];
                exn = None;
                terminator =
                  { id = make_id (); desc = Return; arg = [||]; res = [||] }
              } ];
          fun_contains_calls = false;
          fun_ret_type = Cmm.typ_void
        }
      in
      Cfg_desc.make_post_regalloc cfg)
    ~exp_std:""
    ~exp_err:
      ">> Fatal error: Cfg_prologue: error validating instruction #0000: \
       terminator needs to appear after epilogue but prologue is on stack"

let () =
  check "Prologue after Pushtrap"
    (fun () ->
      seq := InstructionId.make_sequence ();
      let lbl_handler = new_label 1 in
      let cfg : Cfg_desc.t =
        { fun_args = [||];
          blocks =
            [ { start = entry_label;
                body =
                  [ { id = make_id ();
                      desc = Pushtrap { lbl_handler };
                      arg = [||];
                      res = [||]
                    };
                    { id = make_id (); desc = Prologue; arg = [||]; res = [||] }
                  ];
                exn = None;
                terminator =
                  { id = make_id ();
                    desc =
                      Call { op = Indirect None; label_after = new_label 1 };
                    arg = [||];
                    res = [||]
                  }
              };
              { start = lbl_handler;
                body =
                  [{ id = make_id (); desc = Epilogue; arg = [||]; res = [||] }];
                exn = None;
                terminator =
                  { id = make_id (); desc = Return; arg = [||]; res = [||] }
              } ];
          fun_contains_calls = true;
          fun_ret_type = Cmm.typ_void
        }
      in
      let cfg_with_infos = Cfg_desc.make_post_regalloc cfg in
      let cfg = Cfg_with_infos.cfg cfg_with_infos in
      (* Manually correct the stack offset after the pushtrap *)
      let block = Cfg.get_block_exn cfg entry_label in
      let _ =
        DLL.fold_left block.body
          ~f:(fun acc (instr : Cfg.basic Cfg.instruction) ->
            instr.stack_offset <- acc;
            (* The actual value of the stack offset is not important for the
               validator, just that it's non-zero. *)
            match instr.desc with
            | Pushtrap _ -> acc + 1
            | _ -> acc)
          ~init:0
      in
      cfg_with_infos)
    ~exp_std:""
    ~exp_err:
      ">> Fatal error: Cfg_prologue: error validating instruction #0003: \
       prologue has a non-zero stack offset"

let () =
  check "Raise without prologue"
    (fun () ->
      seq := InstructionId.make_sequence ();
      let cfg : Cfg_desc.t =
        { fun_args = [||];
          blocks =
            [ { start = entry_label;
                body = [];
                exn = None;
                terminator =
                  { id = make_id ();
                    desc = Raise Raise_regular;
                    arg = [||];
                    res = [||]
                  }
              } ];
          fun_contains_calls = true;
          fun_ret_type = Cmm.typ_void
        }
      in
      Cfg_desc.make_post_regalloc cfg)
    ~exp_std:""
    ~exp_err:
      ">> Fatal error: Cfg_prologue: error validating instruction #0000: \
       instruction needs prologue but no prologue on the stack"

(* CR-soon cfalas: consider whether we want this to be allowed or not. *)
let () =
  check "Multiple prologue-epilogue pairs in sequence"
    (fun () ->
      seq := InstructionId.make_sequence ();
      let second_label = new_label 1 in
      let stack_loc = [| Reg.create_at_location Int (Stack (Local 0)) |] in
      let cfg : Cfg_desc.t =
        { fun_args = [||];
          blocks =
            [ { start = entry_label;
                body =
                  [ { id = make_id (); desc = Prologue; arg = [||]; res = [||] };
                    { id = make_id ();
                      desc = Op Spill;
                      arg = [| int.(0) |];
                      res = stack_loc
                    };
                    { id = make_id (); desc = Epilogue; arg = [||]; res = [||] }
                  ];
                exn = None;
                terminator =
                  { id = make_id ();
                    desc = Always second_label;
                    arg = [||];
                    res = [||]
                  }
              };
              { start = second_label;
                body =
                  [ { id = make_id (); desc = Prologue; arg = [||]; res = [||] };
                    { id = make_id ();
                      desc = Op Spill;
                      arg = [| int.(0) |];
                      res = stack_loc
                    };
                    { id = make_id (); desc = Epilogue; arg = [||]; res = [||] }
                  ];
                exn = None;
                terminator =
                  { id = make_id (); desc = Return; arg = [||]; res = [||] }
              } ];
          fun_contains_calls = false;
          fun_ret_type = Cmm.typ_void
        }
      in
      Cfg_desc.make_post_regalloc cfg)
    ~exp_std:"" ~exp_err:""
