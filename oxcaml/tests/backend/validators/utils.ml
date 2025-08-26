open Cfg_intf.S
module DLL = Oxcaml_utils.Doubly_linked_list

module Instruction = struct
  type 'a t =
    { mutable desc : 'a;
      mutable arg : Reg.t array;
      mutable res : Reg.t array;
      mutable id : InstructionId.t
    }

  let make ~remove_locs ({ desc; arg; res; id } : 'a t) : 'a instruction =
    let map_regs arr =
      Array.map
        (fun (r : Reg.t) ->
          { r with
            loc =
              (if remove_locs && not (Reg.is_preassigned r)
              then Unknown
              else r.loc)
          })
        arr
    in
    { desc;
      arg = map_regs arg;
      res = map_regs res;
      id;
      dbg = Debuginfo.none;
      fdo = None;
      irc_work_list = Unknown_list;
      live = Reg.Set.empty;
      stack_offset = 0;
      ls_order = -1;
      available_before = None;
      available_across = None
    }
end

module Basic = struct
  type t = basic Instruction.t

  let make ~remove_locs (t : t) : basic instruction =
    Instruction.make ~remove_locs t
end

module Terminator = struct
  type t = terminator Instruction.t

  let make ~remove_locs (t : t) : terminator instruction =
    Instruction.make ~remove_locs t
end

module Block = struct
  type t =
    { start : Label.t;
      mutable body : Basic.t list;
      terminator : Terminator.t;
      exn : Label.t option
    }

  let make ~remove_regalloc ~remove_locs ({ start; body; terminator; exn } : t)
      : Cfg.basic_block =
    let body =
      List.map (Basic.make ~remove_locs) body
      |> List.filter (function
           | { desc = Op (Spill | Reload); _ } -> not remove_regalloc
           | _ -> true)
    in
    let terminator = Terminator.make ~remove_locs terminator in
    let can_raise = Cfg.can_raise_terminator terminator.desc in
    { start;
      body = DLL.of_list body;
      terminator;
      predecessors = Label.Set.empty;
      stack_offset = 0;
      exn;
      can_raise;
      is_trap_handler = false;
      cold = false
    }
end

module Cfg_desc = struct
  type t =
    { mutable fun_args : Reg.t array;
      blocks : Block.t list;
      fun_contains_calls : bool;
      fun_ret_type : Cmm.machtype
    }

  let make ~remove_regalloc ~remove_locs
      ({ fun_args; blocks; fun_contains_calls; fun_ret_type } : t) :
      Cfg_with_layout.t =
    let cfg =
      Cfg.create ~fun_name:"foo" ~fun_args:(Array.copy fun_args)
        ~fun_dbg:Debuginfo.none ~fun_codegen_options:[] ~fun_contains_calls
        ~fun_num_stack_slots:(Stack_class.Tbl.make 0)
        ~fun_poll:Lambda.Default_poll
        ~next_instruction_id:(InstructionId.make_sequence ())
        ~fun_ret_type
    in
    List.iter
      (fun (block : Block.t) ->
        assert (not (Label.Tbl.mem cfg.blocks block.start));
        Label.Tbl.replace cfg.blocks block.start
          (Block.make ~remove_regalloc ~remove_locs block))
      blocks;
    Label.Tbl.iter
      (fun _ (block : Cfg.basic_block) ->
        Cfg.successor_labels ~normal:true ~exn:false block
        |> Label.Set.iter (fun suc ->
               let suc = Label.Tbl.find cfg.blocks suc in
               suc.predecessors <- Label.Set.add block.start suc.predecessors);
        Cfg.successor_labels ~normal:false ~exn:true block
        |> Label.Set.iter (fun suc ->
               let suc = Label.Tbl.find cfg.blocks suc in
               suc.predecessors <- Label.Set.add block.start suc.predecessors;
               suc.is_trap_handler <- true))
      cfg.blocks;
    let cfg_layout = Cfg_with_layout.create ~layout:(DLL.make_empty ()) cfg in
    (if not remove_locs
    then
      (* If we leave in the locations we want to have the actual stack slot
         count. *)
      let update_stack_slots i =
        let update_slot (r : Reg.t) =
          match r.loc, Stack_class.of_machtype r.typ with
          | Stack (Local idx), stack_class ->
            Stack_class.Tbl.update cfg.fun_num_stack_slots stack_class
              ~f:(fun curr -> max curr (idx + 1))
          | _ -> ()
        in
        Array.iter update_slot i.arg;
        Array.iter update_slot i.res
      in
      Cfg_with_layout.iter_instructions ~instruction:update_stack_slots
        ~terminator:update_stack_slots cfg_layout);
    cfg_layout

  let make_pre_regalloc t = make ~remove_regalloc:true ~remove_locs:true t

  let make_post_regalloc t = make ~remove_regalloc:false ~remove_locs:false t
end

let entry_label =
  Cfg_desc.make_post_regalloc
    { fun_args = [||];
      blocks = [];
      fun_contains_calls = false;
      fun_ret_type = Cmm.typ_void
    }
  |> Cfg_with_layout.cfg |> Cfg.entry_label

let label_add lbl k = Label.of_int_unsafe (Label.to_int lbl + k)

let move_param_label = label_add entry_label 1

let call_label = label_add entry_label 2

let move_tmp_res_label = label_add entry_label 3

let add_label = label_add entry_label 4

let return_label = label_add entry_label 5

let new_label i = label_add entry_label (6 + i)

let int = Array.init 8 (fun _ -> Reg.create Int)

let check name f ~validate ~save ~exp_std ~exp_err =
  let cfg = f () in
  let with_wrap_ppf ppf f =
    Format.pp_print_flush ppf ();
    let buf = Buffer.create 0 in
    let ppf_buf = Format.formatter_of_buffer buf in
    let old_out_func = Format.pp_get_formatter_out_functions ppf () in
    Format.pp_set_formatter_out_functions ppf
      (Format.pp_get_formatter_out_functions ppf_buf ());
    let res = f () in
    Format.pp_print_flush ppf ();
    Format.pp_set_formatter_out_functions ppf old_out_func;
    res, buf |> Buffer.to_bytes |> Bytes.to_string |> String.trim
  in
  let ((), err_out), std_out =
    with_wrap_ppf Format.std_formatter (fun () ->
        with_wrap_ppf Format.err_formatter (fun () -> validate cfg))
  in
  if exp_std = std_out && exp_err = err_out
  then Format.printf "%s: OK\n%!" name
  else
    let print_as_text msg text =
      Format.printf "@?@[<h 2>%s:" msg;
      if String.length text > 0 then Format.force_newline ();
      Format.pp_print_text Format.std_formatter text;
      Format.printf "@]\n";
      ()
    in
    Format.printf "%s: FAILED\n" name;
    print_as_text "Expected std" exp_std;
    print_as_text "Got std" std_out;
    print_as_text "Expected err" exp_err;
    print_as_text "Got err" err_out;
    Format.printf "Std as string literal:\n%S\n" std_out;
    Format.printf "Err as string literal:\n%S\n" err_out;
    Format.print_flush ();
    save cfg;
    Format.print_flush ();
    exit 1
