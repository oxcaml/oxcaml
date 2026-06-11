(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                 et al.                                 *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2024--2026 Jane Street Group LLC.                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

(** SSA → CFG lowering.

    Walks a finished {!Ssa.graph} and produces a [Cfg_with_layout.t] suitable
    for the rest of the backend pipeline. The conversion is mostly mechanical:
    each SSA block becomes a CFG block at a fresh label, instructions are
    translated 1-1, and SSA references (op results, block params) become Reg
    arrays.

    Every block parameter gets one virtual register, every [Op] gets a virtual
    reg vector sized by its [typ], and the conversion routes values between them
    with explicit Move instructions.

    Block edges:
    - [Continue] args are realised as parallel moves into the destination
      block's param regs. When src and dst overlap (cyclic or otherwise), we
      route through a fresh temp vector to avoid clobbering live values;
      otherwise the moves go directly. {!Cfg_selectgen} always uses a temp
      vector.
    - [Call] results are moved from [Proc.loc_results_call] to the
      continuation's block param regs at the start of the continuation.

    Trap stack: [finish_graph] populates each block's [block_end_trap_stack]; we
    use it to (a) decide which blocks have an exception successor (their [Raise]
    / may-raise [Call] branches to the topmost handler), and (b) emit [Poptrap]
    instructions before [Return] terminators so the runtime trap stack is empty
    on function exit.

    Comparisons feeding [Branch]: [collect_fused_comparisons] records, per
    comparison [Op], how many [Branch] terminators will fuse it directly into
    the CFG test. An Op is emitted into the body only if it has more uses than
    fused-branch consumers — otherwise [fuse_comparison] splices it directly
    into the terminator and we drop the body Op. *)

module DLL = Oxcaml_utils.Doubly_linked_list
open Ssa.Export

type block = finished Block.t

type instruction = finished Instruction.t

type value = finished Value.t

type env =
  { ssa_graph : finished Ssa.graph;
    op_regs : (finished, Reg.t array) Instruction.Id.Tbl.t;
    block_params_regs : Reg.t array Block.Tbl.t;
    block_labels : Label.t Block.Tbl.t;
    call_result_locs : Reg.t array Block.Tbl.t;
    call_stack_ofs : int Block.Tbl.t;
    fused_comparison_ops : (finished, int) Instruction.Id.Tbl.t
        (* Comparison ops that will be fused into the block terminator and
           should therefore not be emitted into the block body. The map counts
           how often a comparison has been fused. *)
  }

let create_env ssa_graph =
  { ssa_graph;
    op_regs = Instruction.Id.Tbl.create 256;
    block_params_regs = Block.Tbl.create 64;
    block_labels = Block.Tbl.create 64;
    call_result_locs = Block.Tbl.create 16;
    call_stack_ofs = Block.Tbl.create 16;
    fused_comparison_ops = Instruction.Id.Tbl.create 16
  }

let label_of env (blk : block) =
  try Block.Tbl.find env.block_labels blk
  with Not_found ->
    Misc.fatal_errorf "Cfg_of_ssa.label_of: block %a not in block_labels"
      Block.print_id blk

let get_reg env (value : value) : Reg.t =
  match value with
  | Res ({ id; _ }, output_index) ->
    let regs =
      try Instruction.Id.Tbl.find env.op_regs id
      with Not_found ->
        Misc.fatal_errorf "Cfg_of_ssa.get_reg: no regs for Op %a" Value.print
          value
    in
    regs.(output_index)
  | Block_param (block, param_index) -> (
    try
      let regs = Block.Tbl.find env.block_params_regs block in
      regs.(param_index)
    with Not_found ->
      Misc.fatal_errorf "Cfg_of_ssa: no regs for Block_param %a" Value.print
        value)
  | Undefined -> Misc.fatal_error "Cfg_of_ssa.get_reg: Undefined value"

let get_block_params_regs env (block : block) =
  try Block.Tbl.find env.block_params_regs block
  with Not_found ->
    Misc.fatal_errorf "Cfg_of_ssa: no regs for block params of %a"
      Block.print_id block

let make_cfg_instr desc arg res dbg =
  { Cfg.desc;
    id = InstructionId.get_and_incr Sub_cfg.instr_id;
    arg;
    res;
    dbg;
    fdo = Fdo_info.none;
    live = Reg.Set.empty;
    stack_offset = Cfg.invalid_stack_offset;
    available_before = Reg_availability_set.Unreachable;
    available_across = Reg_availability_set.Unreachable
  }

let emit_moves body ~src ~dst =
  Array.iter2
    (fun s d ->
      if not (Reg.same s d)
      then
        DLL.add_end body
          (make_cfg_instr (Cfg.Op Move) [| s |] [| d |] Debuginfo.none))
    src dst

(** A Branch condition that can be folded into the CFG test terminator,
    consuming the comparison's args directly. *)
let fuse_comparison (cond : value) ~true_label ~false_label :
    (Cfg.terminator * value array) option =
  match[@warning "-fragile-match"] cond with
  | Res ({ op = Intop_imm (Icomp Cne, 0); args; _ }, 0) ->
    Some (Cfg.Truth_test { ifso = true_label; ifnot = false_label }, args)
  | Res ({ op = Intop (Icomp cmp); args; _ }, 0) ->
    Some
      ( Select_utils.terminator_of_test (Iinttest cmp) ~label_true:true_label
          ~label_false:false_label,
        args )
  | Res ({ op = Intop_imm (Icomp cmp, n); args; _ }, 0) ->
    Some
      ( Select_utils.terminator_of_test
          (Iinttest_imm (cmp, n))
          ~label_true:true_label ~label_false:false_label,
        args )
  | Res ({ op = Floatop (w, Icompf cmp); args; _ }, 0) ->
    Some
      ( Select_utils.terminator_of_test
          (Ifloattest (w, cmp))
          ~label_true:true_label ~label_false:false_label,
        args )
  | Res ({ op = Intop_imm (Iand, 1); args; _ }, 0) ->
    Some (Cfg.Parity_test { ifso = false_label; ifnot = true_label }, args)
  | _ -> None

let truncate arr n = if Array.length arr > n then Array.sub arr 0 n else arr

let emit_op body op dbg rs rd : Cfg.basic Cfg.instruction =
  match Cfg_selection.pseudoregs_for_operation op rs rd with
  | Constrained (rsrc, rdst) ->
    emit_moves body ~src:rs ~dst:(truncate rsrc (Array.length rs));
    let main = make_cfg_instr (Cfg.Op op) rsrc rdst dbg in
    DLL.add_end body main;
    emit_moves body ~src:(truncate rdst (Array.length rd)) ~dst:rd;
    main
  | Use_default_regs ->
    let main = make_cfg_instr (Cfg.Op op) rs rd dbg in
    DLL.add_end body main;
    main

(** For a call op, return the args to actually pass on the stack/regs (which
    skips the function-pointer arg for [Indirect]), the matching hard-reg
    locations, and the stack offset. *)
let call_arg_locations (call_op : Cfg_intf.S.func_call_operation) virt_args =
  let rarg =
    match call_op with
    | Indirect _ -> Array.sub virt_args 1 (Array.length virt_args - 1)
    | Direct _ -> virt_args
  in
  let loc_arg, stack_ofs = Proc.loc_arguments (Reg.typv rarg) in
  rarg, loc_arg, stack_ofs

let call_arg_for_terminator (call_op : Cfg_intf.S.func_call_operation) virt_args
    loc_arg =
  match call_op with
  | Indirect _ -> Array.append [| virt_args.(0) |] loc_arg
  | Direct _ -> loc_arg

(** Move [virt_args] into the locations dictated by
    [Proc.loc_external_arguments ty_args], extending the stack as needed and
    using the per-arg move sequence produced by
    [Cfg_selection.insert_move_extcall_arg]. Returns the flattened locations,
    the stack offset, and the stack alignment. *)
let move_to_extcall_arg_locs body (ty_args : Cmm.exttype list) virt_args dbg :
    Reg.t array * int * Cmm.stack_align =
  let ty_args =
    if List.is_empty ty_args
    then List.init (Array.length virt_args) (fun _ -> Cmm.XInt)
    else ty_args
  in
  let locs, stack_ofs, stack_align = Proc.loc_external_arguments ty_args in
  let ty_args_arr = Array.of_list ty_args in
  if stack_ofs <> 0
  then
    DLL.add_end body
      (make_cfg_instr (Cfg.Op (Stackoffset stack_ofs)) [||] [||] Debuginfo.none);
  Array.iteri
    (fun i arg ->
      let src = [| arg |] in
      let dst = locs.(i) in
      match Cfg_selection.insert_move_extcall_arg ty_args_arr.(i) src dst with
      | Rewritten (basic, src, dst) ->
        DLL.add_end body (make_cfg_instr basic src dst dbg)
      | Use_default -> emit_moves body ~src ~dst)
    virt_args;
  Array.concat (Array.to_list locs), stack_ofs, stack_align

let is_exn_predecessor (pred : block) (target : block) =
  match Block.exn_successor pred with
  | Some h -> Block.equal h target
  | None -> false

let is_call_predecessor (pred : block) (target : block) =
  match Block.terminator pred with
  | Invalid { continuation = Some continuation; _ } ->
    Block.equal continuation target
  | Call { continuation = Goto continuation; _ } ->
    Block.equal continuation target
  | Call { continuation = Return | Raise _ | Unreachable; _ }
  | Switch _ | Continue _
  | Invalid { continuation = None; _ } ->
    false

(** The incoming ABI for trap handlers is different. Thus, we require ALL
    predecessors to be exception predecessors. We could relax this in the future
    by inserting a merge block to join the non-exception predecessors. *)
let block_is_trap_handler (block : block) =
  let any_exn_pred =
    List.exists (fun p -> is_exn_predecessor p block) (Block.predecessors block)
  in
  let all_exn_pred () =
    List.for_all
      (fun p -> is_exn_predecessor p block)
      (Block.predecessors block)
  in
  if any_exn_pred && not (all_exn_pred ())
  then
    Misc.fatal_errorf
      "Cfg_of_ssa: block %a mixes exception and non-exception predecessors."
      Block.print_id block;
  any_exn_pred

(** A call continuation must be reached via a single call/prim predecessor: the
    runtime puts the call's return values in fixed locations. We could relax
    this in the future by automatically splitting the edges. *)
let is_call_continuation (block : block) =
  let result =
    List.exists
      (fun p -> is_call_predecessor p block)
      (Block.predecessors block)
  in
  if result && List.length (Block.predecessors block) <> 1
  then
    Misc.fatal_errorf
      "Cfg_of_ssa: block %a has a call predecessor alongside other \
       predecessors."
      Block.print_id block;
  result

let convert_block (env : env) (block : block) : Cfg.basic_block =
  let body = DLL.make_empty () in
  if is_call_continuation block
  then begin
    let loc_res = Block.Tbl.find env.call_result_locs block in
    let virt_res = get_block_params_regs env block in
    emit_moves body ~src:loc_res ~dst:virt_res;
    let stack_ofs =
      Block.Tbl.find_opt env.call_stack_ofs block |> Option.value ~default:0
    in
    if stack_ofs <> 0
    then
      DLL.add_end body
        (make_cfg_instr (Cfg.Op (Stackoffset (-stack_ofs))) [||] [||]
           Debuginfo.none)
  end;
  let is_trap_handler = block_is_trap_handler block in
  if is_trap_handler
  then begin
    let virt_res = get_block_params_regs env block in
    let exn_bucket = [| Proc.loc_exn_bucket |] in
    let first_param = [| virt_res.(0) |] in
    emit_moves body ~src:exn_bucket ~dst:first_param
  end;
  Array.iter
    (fun (instr : instruction) ->
      match[@warning "-fragile-match"] instr with
      | Op
          { op = Name_for_debugger { ident; provenance; which_parameter; _ };
            args;
            _
          } ->
        (* The values feeding a [Name_for_debugger] become the operation's
           [regs] field rather than the CFG instruction's argument array. *)
        let regs = Array.map (get_reg env) args in
        DLL.add_end body
          (make_cfg_instr
             (Cfg.Op
                (Name_for_debugger { ident; provenance; which_parameter; regs }))
             [||] [||] Debuginfo.none)
      | Op ({ id; op; args; dbg; typ; usage_count = _; name } as operation) ->
        let regs =
          match name with
          | None -> Reg.createv typ
          | Some name -> Reg.createv_with_id ~id:(Ident.create_local name) typ
        in
        Instruction.Id.Tbl.replace env.op_regs id regs;
        if
          Instruction.Id.Tbl.find_opt env.fused_comparison_ops id
          |> Option.value ~default:(-1)
          < Instruction.usage_count operation
        then
          let arg = Array.map (get_reg env) args in
          let (_ : Cfg.basic Cfg.instruction) = emit_op body op dbg arg regs in
          ()
      | Push_trap { handler } ->
        DLL.add_end body
          (make_cfg_instr
             (Cfg.Pushtrap { lbl_handler = label_of env handler })
             [||] [||] Debuginfo.none)
      | Pop_trap { handler } ->
        DLL.add_end body
          (make_cfg_instr
             (Cfg.Poptrap { lbl_handler = label_of env handler })
             [||] [||] Debuginfo.none))
    (Block.body block);
  let dbg = Block.terminator_dbg block in
  let cfg_terminator =
    match Block.terminator block with
    | Continue { continuation = Goto start_block; args }
      when Block.is_function_start start_block ->
      (* A [Goto] back-edge to the entry block is a self-recursive tail call. We
         lower it to [Tailcall_self], passing the args through the ABI parameter
         locations: the entry block's prologue moves them back into the
         parameter registers, so the loop reuses the prologue. *)
      let virt_args = Array.map (get_reg env) args in
      let loc_arg = Proc.loc_parameters (Reg.typv virt_args) in
      emit_moves body ~src:virt_args ~dst:loc_arg;
      make_cfg_instr
        (Cfg.Tailcall_self { destination = label_of env start_block })
        loc_arg [||] dbg
    | Continue { continuation = Goto goto; args } ->
      (* Args going to dead target params are [Undefined] and need no moves. *)
      let is_defined (arg : value) =
        match arg with Undefined -> false | Res _ | Block_param _ -> true
      in
      let dst_regs =
        get_block_params_regs env goto
        |> Misc.Stdlib.Array.filteri (fun i _ -> is_defined args.(i))
      in
      let src_regs =
        args
        |> Misc.Stdlib.Array.filteri (fun _ arg -> is_defined arg)
        |> Array.map (get_reg env)
      in
      (* CR ttebbi: We should use a non-quadratic algorithm for long register
         lists. *)
      let has_overlap =
        Array.exists
          (fun s -> Array.exists (fun d -> Reg.same s d) dst_regs)
          src_regs
      in
      if has_overlap
      then (
        let tmp_regs = Reg.createv_with_typs src_regs in
        emit_moves body ~src:src_regs ~dst:tmp_regs;
        emit_moves body ~src:tmp_regs ~dst:dst_regs)
      else emit_moves body ~src:src_regs ~dst:dst_regs;
      make_cfg_instr (Cfg.Always (label_of env goto)) [||] [||] dbg
    | Continue { continuation = Return; args } ->
      (* Emit a [Poptrap] for each handler still on the trap stack at block
         exit, so the runtime trap stack is empty when the function returns. *)
      List.iter
        (fun (h : block) ->
          DLL.add_end body
            (make_cfg_instr
               (Cfg.Poptrap { lbl_handler = label_of env h })
               [||] [||] Debuginfo.none))
        (Block.block_end_trap_stack block);
      let arg = Array.map (get_reg env) args in
      let loc_res = Proc.loc_results_return (Reg.typv arg) in
      emit_moves body ~src:arg ~dst:loc_res;
      DLL.add_end body
        (make_cfg_instr Cfg.Reloadretaddr [||] [||] Debuginfo.none);
      make_cfg_instr Cfg.Return loc_res [||] dbg
    | Continue { continuation = Raise raise_kind; args } ->
      let exn_val = Array.map (get_reg env) args in
      let exn_bucket = [| Proc.loc_exn_bucket |] in
      let extra_dst =
        match Block.exn_successor block with
        | Some hb ->
          let handler_regs = get_block_params_regs env hb in
          if Array.length handler_regs > 1
          then Array.sub handler_regs 1 (Array.length handler_regs - 1)
          else [||]
        | None ->
          (* Function-level or toplevel exception continuations never have extra
             args. *)
          [||]
      in
      let dst = Array.append exn_bucket extra_dst in
      let src =
        if Array.length exn_val > Array.length dst
        then Array.sub exn_val 0 (Array.length dst)
        else (
          assert (Array.length exn_val = Array.length dst);
          exn_val)
      in
      emit_moves body ~src ~dst;
      make_cfg_instr (Cfg.Raise raise_kind) exn_bucket [||] dbg
    | Continue { continuation = Unreachable; _ } ->
      Misc.fatal_error
        "Cfg_of_ssa: a Continue continuation cannot be Unreachable"
    | Switch { index; targets } ->
      if
        (* A two-target switch is the boolean branch ([targets.(0)] false,
           [targets.(1)] true), which we fuse with its comparison when
           possible. *)
        Array.length targets = 2
      then
        let false_label = label_of env targets.(0) in
        let true_label = label_of env targets.(1) in
        match fuse_comparison index ~true_label ~false_label with
        | Some (term, args) ->
          make_cfg_instr term (Array.map (get_reg env) args) [||] dbg
        | None ->
          make_cfg_instr
            (Cfg.Truth_test { ifso = true_label; ifnot = false_label })
            [| get_reg env index |]
            [||] dbg
      else
        let index_reg = get_reg env index in
        let labels = Array.map (label_of env) targets in
        make_cfg_instr (Cfg.Switch labels) [| index_reg |] [||] dbg
    | Call { op; args; continuation = Return; nontail; may_raise = _ } ->
      assert (not nontail);
      let call_op : Cfg.func_call_operation =
        match op with
        | Direct sym -> Direct sym
        | Indirect candidates -> Indirect candidates
        | External _ | Probe _ ->
          Misc.fatal_error "Cfg_of_ssa: a primitive cannot be tail called"
      in
      let virt_args = Array.map (get_reg env) args in
      let rarg, loc_arg, _stack_ofs = call_arg_locations call_op virt_args in
      emit_moves body ~src:rarg ~dst:loc_arg;
      make_cfg_instr (Cfg.Tailcall_func call_op)
        (call_arg_for_terminator call_op virt_args loc_arg)
        [||] dbg
    | Call { continuation = Raise _; _ } ->
      Misc.fatal_error "Cfg_of_ssa: a call continuation cannot be Raise"
    | Call { op; args; continuation = Unreachable; _ } -> (
      let virt_args = Array.map (get_reg env) args in
      match op with
      | External ({ ty_args; ty_res; _ } as ext) ->
        let loc_arg, stack_ofs, stack_align =
          move_to_extcall_arg_locs body ty_args virt_args dbg
        in
        make_cfg_instr
          (Cfg.Call_no_return { ext with stack_ofs; stack_align })
          loc_arg
          (Proc.loc_external_results ty_res)
          dbg
      | Direct _ | Indirect _ | Probe _ ->
        Misc.fatal_error
          "Cfg_of_ssa: only an external call can have continuation Unreachable")
    | Call { op; args; continuation = Goto continuation; _ } -> (
      let virt_args = Array.map (get_reg env) args in
      let virt_res = get_block_params_regs env continuation in
      let ocaml_call (call_op : Cfg_intf.S.func_call_operation) =
        let rarg, loc_arg, stack_ofs_args =
          call_arg_locations call_op virt_args
        in
        let loc_res, stack_ofs_res =
          Proc.loc_results_call (Reg.typv virt_res)
        in
        let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
        if stack_ofs <> 0
        then
          DLL.add_end body
            (make_cfg_instr (Cfg.Op (Stackoffset stack_ofs)) [||] [||]
               Debuginfo.none);
        emit_moves body ~src:rarg ~dst:loc_arg;
        Block.Tbl.replace env.call_result_locs continuation loc_res;
        Block.Tbl.replace env.call_stack_ofs continuation stack_ofs;
        make_cfg_instr
          (Cfg.Call { op = call_op; label_after = label_of env continuation })
          (call_arg_for_terminator call_op virt_args loc_arg)
          loc_res dbg
      in
      match op with
      | Direct sym -> ocaml_call (Direct sym)
      | Indirect candidates -> ocaml_call (Indirect candidates)
      | External ({ ty_args; _ } as ext) ->
        let loc_arg, stack_ofs, stack_align =
          move_to_extcall_arg_locs body ty_args virt_args dbg
        in
        let loc_res = Proc.loc_external_results (Reg.typv virt_res) in
        Block.Tbl.replace env.call_result_locs continuation loc_res;
        Block.Tbl.replace env.call_stack_ofs continuation stack_ofs;
        make_cfg_instr
          (Cfg.Prim
             { op = External { ext with stack_ofs; stack_align };
               label_after = label_of env continuation
             })
          loc_arg loc_res dbg
      | Probe { name; handler_code_sym; enabled_at_init } ->
        (* Probes don't use the regular calling convention: their args stay in
           virtual registers and there are no results ([Cprobe] has type
           [typ_void]), so the continuation needs no result moves. *)
        assert (Array.length virt_res = 0);
        Block.Tbl.replace env.call_result_locs continuation [||];
        make_cfg_instr
          (Cfg.Prim
             { op = Probe { name; handler_code_sym; enabled_at_init };
               label_after = label_of env continuation
             })
          virt_args [||] dbg)
    | Invalid { message; args; continuation } ->
      (* Invalid with continuation behaves just like an external call. *)
      let virt_args = Array.map (get_reg env) args in
      let loc_arg, stack_ofs, stack_align =
        move_to_extcall_arg_locs body [Cmm.XInt] virt_args dbg
      in
      let label_after, loc_res =
        match continuation with
        | Some cont_block ->
          let virt_res = get_block_params_regs env cont_block in
          let loc_res = Proc.loc_external_results (Reg.typv virt_res) in
          Block.Tbl.replace env.call_result_locs cont_block loc_res;
          Block.Tbl.replace env.call_stack_ofs cont_block stack_ofs;
          Some (label_of env cont_block), loc_res
        | None -> None, [||]
      in
      make_cfg_instr
        (Cfg.Invalid { message; stack_ofs; stack_align; label_after })
        loc_arg loc_res dbg
  in
  let can_raise = Cfg.can_raise_terminator cfg_terminator.desc in
  { Cfg.start = label_of env block;
    body;
    terminator = cfg_terminator;
    predecessors = Label.Set.empty;
    stack_offset = Cfg.invalid_stack_offset;
    exn = None;
    can_raise;
    is_trap_handler;
    cold = false
  }

(** Remember comparisons that will be folded into branch terminators. The branch
    terminator consumes the comparison's args directly, so the comparison itself
    is no longer needed (provided no other consumer references it). *)
let collect_fused_comparisons env =
  List.iter
    (fun (block : block) ->
      match Block.terminator block with
      | Switch { index; targets } when Array.length targets = 2 -> (
        if
          Option.is_some
            (fuse_comparison index ~true_label:Label.none
               ~false_label:Label.none)
        then
          match index with
          | Res ({ id; _ }, _) ->
            Instruction.Id.Tbl.replace env.fused_comparison_ops id
              (1
              + (Instruction.Id.Tbl.find_opt env.fused_comparison_ops id
                |> Option.value ~default:0))
          | Block_param _ | Undefined -> assert false)
      | Switch _ | Continue _ | Call _ | Invalid _ -> ())
    (Ssa.blocks env.ssa_graph)

let allocate_block_labels_and_param_regs env =
  List.iter
    (fun (block : block) ->
      Block.Tbl.replace env.block_labels block (Label.new_label ());
      Block.Tbl.replace env.block_params_regs block
        (Reg.createv (Block.params_machtype block)))
    (Ssa.blocks env.ssa_graph)

let param_naming_instrs env ~fun_arg_locs =
  let idx = ref 0 in
  let param_index = ref (-1) in
  (Ssa.function_info env.ssa_graph).parameters
  |> List.filter_map (fun (var, ty) ->
      param_index := !param_index + 1;
      let provenance = Backend_var.With_provenance.provenance var in
      let ident = Backend_var.With_provenance.var var in
      let n = Array.length ty in
      let regs = Array.init n (fun i -> fun_arg_locs.(!idx + i)) in
      idx := !idx + n;
      if Option.is_some provenance
      then
        Some
          (make_cfg_instr
             (Cfg.Op
                (Name_for_debugger
                   { ident;
                     provenance;
                     which_parameter = Some !param_index;
                     regs
                   }))
             [||] [||] Debuginfo.none)
      else None)

(** Build the CFG-level entry block at [Cfg.entry_label cfg]. Note that this is
    a separate block inserted before the one corresponding to [entry]. *)
let make_entry_block env cfg ~ssa_entry_label ~fun_arg_locs : Cfg.basic_block =
  let body = DLL.make_empty () in
  param_naming_instrs env ~fun_arg_locs |> List.iter (DLL.add_end body);
  { Cfg.start = Cfg.entry_label cfg;
    body;
    terminator =
      make_cfg_instr (Cfg.Always ssa_entry_label) [||] [||] Debuginfo.none;
    predecessors = Label.Set.empty;
    stack_offset = Cfg.invalid_stack_offset;
    exn = None;
    can_raise = false;
    is_trap_handler = false;
    cold = false
  }

(* Prepend the function-entry prologue to [entry]'s CFG block: param
   [Name_for_debugger]s, moves from ABI param locations into the entry's virtual
   param regs, then an optimistic [Poll] (whose id is returned so
   [drop_optimistic_prologue_poll] can later remove it if unneeded). *)
let prepend_entry_prologue env (cfg_block : Cfg.basic_block) ~fun_arg_locs
    ~fun_arg_regs : InstructionId.t =
  let poll_instr = make_cfg_instr (Cfg.Op Poll) [||] [||] Debuginfo.none in
  DLL.add_begin cfg_block.body poll_instr;
  List.iter2
    (fun loc reg ->
      if not (Reg.same loc reg)
      then
        DLL.add_begin cfg_block.body
          (make_cfg_instr (Cfg.Op Move) [| loc |] [| reg |] Debuginfo.none))
    (fun_arg_locs |> Array.to_list |> List.rev)
    (fun_arg_regs |> Array.to_list |> List.rev);
  List.rev (param_naming_instrs env ~fun_arg_locs)
  |> List.iter (DLL.add_begin cfg_block.body);
  poll_instr.id

(* Convert each SSA block to a CFG block and append it to [cfg] and [layout]. *)
let convert_and_emit_blocks env cfg layout ~fun_arg_locs ~fun_arg_regs :
    InstructionId.t =
  let entry = Ssa.entry env.ssa_graph in
  let prologue_poll_instr_id = ref None in
  List.iter
    (fun (ssa_block : block) ->
      let cfg_block = convert_block env ssa_block in
      if Block.equal ssa_block entry
      then
        prologue_poll_instr_id
          := Some
               (prepend_entry_prologue env cfg_block ~fun_arg_locs ~fun_arg_regs);
      Cfg.add_block_exn cfg cfg_block;
      DLL.add_end layout cfg_block.start)
    (Ssa.blocks env.ssa_graph);
  match !prologue_poll_instr_id with
  | Some id -> id
  | None ->
    Misc.fatal_error
      "Cfg_of_ssa: function-entry block was not visited by \
       convert_and_emit_blocks"

(** Remove the optimistic prologue [Poll] inserted by [prepend_entry_prologue]
    unless [Cfg_polling] determines it is required for safe-point coverage. *)
let drop_optimistic_prologue_poll env ~future_funcnames cfg
    ~prologue_poll_instr_id =
  if
    not
      (Cfg_polling.requires_prologue_poll ~future_funcnames
         ~fun_name:(Ssa.function_info env.ssa_graph).sym_name
         ~optimistic_prologue_poll_instr_id:prologue_poll_instr_id cfg)
  then
    let entry =
      Cfg.get_block_exn cfg (label_of env (Ssa.entry env.ssa_graph))
    in
    DLL.filter_left entry.body ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
        not (InstructionId.equal instr.id prologue_poll_instr_id))

let convert ~future_funcnames (ssa_graph : finished Ssa.graph) :
    Cfg_with_layout.t =
  let entry = Ssa.entry ssa_graph in
  let function_info = Ssa.function_info ssa_graph in
  (* Start instruction ids at 0, matching [Cfg_selectgen.emit_fundecl]. *)
  Sub_cfg.reset_instr_id ();
  let env = create_env ssa_graph in
  collect_fused_comparisons env;
  allocate_block_labels_and_param_regs env;
  let fun_arg_regs = get_block_params_regs env entry in
  let fun_arg_locs = Proc.loc_parameters (Reg.typv fun_arg_regs) in
  let cfg =
    Cfg.create ~fun_name:function_info.sym_name ~fun_args:fun_arg_locs
      ~fun_codegen_options:
        (Cfg.of_cmm_codegen_option function_info.codegen_options)
      ~fun_dbg:function_info.dbg ~fun_contains_calls:true
      ~fun_num_stack_slots:(Stack_class.Tbl.make 0) ~fun_poll:function_info.poll
      ~next_instruction_id:Sub_cfg.instr_id ~fun_ret_type:function_info.ret_type
      ~allowed_to_be_irreducible:false
  in
  let layout = DLL.make_empty () in
  let entry_block =
    make_entry_block env cfg ~ssa_entry_label:(label_of env entry) ~fun_arg_locs
  in
  Cfg.add_block_exn cfg entry_block;
  DLL.add_end layout entry_block.start;
  let prologue_poll_instr_id =
    convert_and_emit_blocks env cfg layout ~fun_arg_locs ~fun_arg_regs
  in
  Select_utils.Stack_offset_and_exn.update_cfg cfg;
  Cfg.register_predecessors_for_all_blocks cfg;
  drop_optimistic_prologue_poll env ~future_funcnames cfg
    ~prologue_poll_instr_id;
  let fun_contains_calls =
    Label.Tbl.to_seq_values cfg.blocks
    |> Seq.exists Cfg.basic_block_contains_calls
  in
  let cfg = { cfg with fun_contains_calls } in
  let cfg_with_layout = Cfg_with_layout.create cfg ~layout in
  Cfg_simplify.run cfg_with_layout
