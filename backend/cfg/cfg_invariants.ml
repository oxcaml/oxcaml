(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2021 Jane Street Group LLC                                       *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

module CL = Cfg_with_layout
module DLL = Oxcaml_utils.Doubly_linked_list

type t =
  { mutable result : bool;
    ppf : Format.formatter;
    fun_name : string;
    cfg : Cfg.t;
    mutable tailrec_entry_label : Label.t option
  }

let report t =
  t.result <- true;
  Format.fprintf t.ppf "Cfg invariant failed in %s: " t.fun_name;
  Format.fprintf t.ppf

let print_layout ppf layout =
  Format.(
    pp_print_list ~pp_sep:pp_print_space Label.print ppf (layout |> DLL.to_list))

let check_layout t layout =
  (* [layout] is not empty and entry of cfg is the first node in the layout. *)
  (match DLL.hd layout with
  | None -> report t "Empty layout"
  | Some hd ->
    if not (Label.equal hd t.cfg.entry_label)
    then
      report t "Cfg entry node %a is not the first node in the layout @.%a@."
        Label.print t.cfg.entry_label print_layout layout);
  (* No duplicates in layout *)
  let labels =
    DLL.fold_left
      ~f:(fun acc l ->
        if Label.Set.mem l acc
        then report t "Duplicate label %a in layout@." Label.print l;
        Label.Set.add l acc)
      ~init:Label.Set.empty layout
  in
  let num_labels = Label.Set.cardinal labels in
  if DLL.length layout > num_labels
  then report t "Layout contains duplicates:@.%a@." print_layout layout;
  (* The set of nodes in the layout is the same as the set of nodes in the
     cfg. *)
  let num_nodes = Label.Tbl.length t.cfg.blocks in
  if num_nodes > num_labels
  then (
    report t "Not all cfg nodes are in the layout:@.%a" print_layout layout;
    Label.Tbl.iter
      (fun label _ ->
        if not (Label.Set.mem label labels)
        then
          report t "Cfg node label %a is missing from layout" Label.print label)
      t.cfg.blocks);
  if num_nodes < num_labels
  then
    report t "Not all labels in the layout have cfg nodes:@.%a" print_layout
      layout;
  ();
  DLL.iter
    ~f:(fun label ->
      match Cfg.get_block t.cfg label with
      | Some b ->
        if not (Label.equal b.start label)
        then
          report t "Block labelled %a is associated with label %a" Label.print
            b.start Label.print label
      | None ->
        report t "Node not found for label %a in layout" Label.print label)
    layout

let check_arity t label ctor_print ctor kind expected actual_array =
  let actual = Array.length actual_array in
  let report expected =
    report t "%a: %a with %d %s(s), expected %s@." Label.print label ctor_print
      ctor actual kind expected
  in
  match expected with
  | [] -> ()
  | [expected] ->
    if not (Int.equal expected actual) then report (Int.to_string expected)
  | expected ->
    if not (List.exists (Int.equal actual) expected)
    then report (expected |> List.map Int.to_string |> String.concat ", ")

let count_regs_in_locs locs =
  Array.fold_left (fun acc r -> acc + Array.length r) 0 locs

let check_tailrec_position t =
  (* tailrec entry point is either the entry block or the only successor of the
     entry block. *)
  match t.tailrec_entry_label with
  | None -> ()
  | Some tailrec_label ->
    if not (Label.equal tailrec_label t.cfg.entry_label)
    then
      let entry_block = Cfg.get_block_exn t.cfg t.cfg.entry_label in
      let successors =
        Cfg.successor_labels ~normal:true ~exn:false entry_block
      in
      if
        not
          (Label.Set.cardinal successors = 1
          && Label.equal tailrec_label (Label.Set.min_elt successors))
      then
        report t
          "Expected tailrec block %a to be the entry block or the only \
           successor of the entry block but entry block the \
           followingsuccessors:@.%a@."
          Label.print tailrec_label Label.Set.print successors

let check_terminator_arity t label block =
  let term = block.Cfg.terminator in
  let args = term.arg in
  let res = term.res in
  let check ~expected_args ~expected_res =
    let print_desc = Cfg.dump_terminator ~sep:"; " in
    check_arity t label print_desc term.desc "argument" expected_args args;
    check_arity t label print_desc term.desc "result" expected_res res
  in
  (* For calls where we have precise type information (e.g. external calls), we
     can recompute the calling convention. For OCaml function calls we lack the
     argument types here, so we only check properties that are type-independent
     (such as having no results). *)
  let check_external ~ty_args ~ty_res =
    (* Cmm may omit [ty_args] (empty list) for external calls; selection then
       fills in a default based on the number of actual arguments (all XInt).
       Mirror that here to avoid underestimating the arity. *)
    let ty_args =
      match ty_args with
      | [] when Array.length args > 0 ->
        List.init (Array.length args) (fun _ -> Cmm.XInt)
      | _ -> ty_args
    in
    let locs, _, _ = Proc.loc_external_arguments ty_args in
    let expected_args = [count_regs_in_locs locs] in
    let expected_res = [Array.length (Proc.loc_external_results ty_res)] in
    check ~expected_args ~expected_res
  in
  match term.desc with
  | Never -> check ~expected_args:[0] ~expected_res:[0]
  | Always _ -> check ~expected_args:[0] ~expected_res:[0]
  | Parity_test _ -> check ~expected_args:[1] ~expected_res:[0]
  | Truth_test _ -> check ~expected_args:[1] ~expected_res:[0]
  | Float_test _ ->
    (* Float comparisons are always between two operands. *)
    check ~expected_args:[2] ~expected_res:[0]
  | Int_test { imm; _ } ->
    (* With an immediate, only one register is passed; otherwise two. *)
    let expected_args = if Option.is_some imm then [1] else [2] in
    check ~expected_args ~expected_res:[0]
  | Switch _ -> check ~expected_args:[1] ~expected_res:[0]
  | Return ->
    (* Return carries the registers holding the function result in calling
       convention order. Some generated helpers (e.g. curry wrappers) may have a
       declared return type with more components than they actually return;
       those are identified by the [caml_curry] prefix and are exempted. *)
    let expected_args =
      if String.starts_with t.cfg.fun_name ~prefix:"caml_curry"
      then []
      else [Proc.loc_results_return t.cfg.fun_ret_type |> Array.length]
    in
    check ~expected_args ~expected_res:[0]
  | Raise _ -> check ~expected_args:[1] ~expected_res:[0]
  | Tailcall_self _ ->
    (* Self tailcalls reuse the current function's parameter passing convention,
       hence the arity must match [fun_args]. *)
    check ~expected_args:[Array.length t.cfg.fun_args] ~expected_res:[0]
  | Tailcall_func (Indirect _) ->
    (* The first argument is the callee; we cannot check the rest without the
       callee's type, but the array must be non-empty and there are no results
       for tailcalls. *)
    if Array.length args = 0
    then report t "%a: Indirect tailcall with no arguments" Label.print label;
    check ~expected_args:[] ~expected_res:[0]
  | Tailcall_func (Direct _) ->
    (* No result registers are produced by a tailcall. *)
    check ~expected_args:[] ~expected_res:[0]
  | Call_no_return { ty_args; ty_res; _ } ->
    (* External calls that do not return still follow the external calling
       convention for both arguments and (nonexistent) results. *)
    check_external ~ty_args ~ty_res
  | Prim { op = External { ty_args; ty_res; _ }; _ } ->
    (* External primitives carry precise type info, so we can recompute both arg
       and result arities. We must mirror the selection-time defaulting of empty
       [ty_args] to XInt per argument. *)
    check_external ~ty_args ~ty_res
  | Prim { op = Probe _; _ } ->
    (* Probes are typed as [typ_void]; they should therefore not produce result
       registers. We cannot reliably check their arguments here. *)
    check ~expected_args:[] ~expected_res:[0]
  | Call _ ->
    (* We lack the callee's signature here. In particular, [caml_program] (the
       startup thunk) is called for side effects and returns no registers, so we
       cannot assert any result arity. *)
    ()
  | Invalid { label_after; _ } ->
    (* [Invalid] is emitted as a call to [caml_flambda2_invalid]. It always
       passes the error message symbol as one OCaml-ABI argument; when zero
       alloc checking is enabled it returns an [int] (so there is a successor
       and one result register), otherwise it never returns. *)
    let ty_res =
      match label_after with None -> Cmm.typ_void | Some _ -> Cmm.typ_int
    in
    check_external ~ty_args:[Cmm.XInt] ~ty_res

let check_basic_arity t label (instr : Cfg.basic Cfg.instruction) =
  let args = instr.arg in
  let res = instr.res in
  let desc = Format.asprintf "%a" Cfg.dump_basic instr.desc in
  let check ~expected_args ~expected_res =
    check_arity t label Cfg.dump_basic instr.desc "argument" expected_args args;
    check_arity t label Cfg.dump_basic instr.desc "result" expected_res res
  in
  match instr.desc with
  | Reloadretaddr -> check ~expected_args:[0] ~expected_res:[0]
  | Pushtrap _ | Poptrap _ | Prologue | Epilogue | Stack_check _ ->
    check ~expected_args:[0] ~expected_res:[0]
  | Op op -> (
    match op with
    | Move | Spill | Reload ->
      check ~expected_args:[1] ~expected_res:[1];
      if
        Array.length args = 1
        && Array.length res = 1
        && not (Proc.types_are_compatible args.(0) res.(0))
      then
        report t "%a (instr %a): %s uses incompatible registers %a -> %a"
          Label.print label InstructionId.print instr.id desc Printreg.reg
          args.(0) Printreg.reg res.(0)
    | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
    | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ ->
      check ~expected_args:[0] ~expected_res:[1]
    | Stackoffset _ -> check ~expected_args:[0] ~expected_res:[0]
    | Load { addressing_mode; _ } ->
      check
        ~expected_args:[Arch.num_args_addressing addressing_mode]
        ~expected_res:[1]
    | Store (_, addr, _) ->
      check ~expected_args:[1 + Arch.num_args_addressing addr] ~expected_res:[0]
    | Intop op ->
      let expected_args =
        if Operation.is_unary_integer_operation op then [1] else [2]
      in
      check ~expected_args ~expected_res:[1]
    | Int128op op ->
      let expected_args, expected_res =
        match op with Iadd128 | Isub128 -> [4], [2] | Imul64 _ -> [2], [2]
      in
      check ~expected_args ~expected_res
    | Intop_imm _ -> check ~expected_args:[1] ~expected_res:[1]
    | Intop_atomic { op; addr; _ } ->
      let addr_args = Arch.num_args_addressing addr in
      let data_args =
        match op with
        | Fetch_and_add | Add | Sub | Land | Lor | Lxor | Exchange -> 1
        | Compare_set | Compare_exchange -> 2
      in
      let expected_res =
        match op with
        | Add | Sub | Land | Lor | Lxor -> [0]
        | Fetch_and_add | Exchange | Compare_set | Compare_exchange -> [1]
      in
      check ~expected_args:[addr_args + data_args] ~expected_res
    | Floatop (_, (Inegf | Iabsf)) -> check ~expected_args:[1] ~expected_res:[1]
    | Floatop (_, (Iaddf | Isubf | Imulf | Idivf)) ->
      check ~expected_args:[2] ~expected_res:[1]
    | Floatop (_, Icompf _) -> check ~expected_args:[2] ~expected_res:[1; 2]
    | Csel _ ->
      let len_args = Array.length args in
      if len_args < 3
      then
        report t "%a (instr %a): %s with %d argument(s), expected at least 3"
          Label.print label InstructionId.print instr.id desc len_args;
      check ~expected_args:[] ~expected_res:[1];
      if Array.length res = 1
      then
        let ifso = args.(len_args - 2) in
        let ifnot = args.(len_args - 1) in
        let res0 = res.(0) in
        if
          (not (Proc.types_are_compatible res0 ifso))
          || not (Proc.types_are_compatible res0 ifnot)
        then
          report t
            "%a (instr %a): %s result type mismatches selected values %a/%a"
            Label.print label InstructionId.print instr.id desc Printreg.reg
            ifso Printreg.reg ifnot
    | Reinterpret_cast _ | Static_cast _ ->
      check ~expected_args:[1] ~expected_res:[1]
    | Probe_is_enabled _ -> check ~expected_args:[0] ~expected_res:[1]
    | Opaque ->
      let len_args = Array.length args in
      let len_res = Array.length res in
      if len_args = 0 || len_res = 0
      then
        report t "%a (instr %a): %s expected non-empty args/results" Label.print
          label InstructionId.print instr.id desc
      else if len_args <> len_res
      then
        report t "%a (instr %a): %s with %d args but %d results" Label.print
          label InstructionId.print instr.id desc len_args len_res
      else
        for i = 0 to len_args - 1 do
          if not (Proc.types_are_compatible args.(i) res.(i))
          then
            report t "%a (instr %a): %s uses incompatible opaque pair %a -> %a"
              Label.print label InstructionId.print instr.id desc Printreg.reg
              args.(i) Printreg.reg res.(i)
        done
    | Begin_region -> check ~expected_args:[0] ~expected_res:[1]
    | End_region -> check ~expected_args:[1] ~expected_res:[0]
    | Specific _ -> ()
    | Name_for_debugger { regs; _ } ->
      check ~expected_args:[0; Array.length regs] ~expected_res:[0]
    | Dls_get | Tls_get | Domain_index ->
      check ~expected_args:[0] ~expected_res:[1]
    | Poll | Pause -> check ~expected_args:[0] ~expected_res:[0]
    | Alloc _ -> check ~expected_args:[0] ~expected_res:[1])

let check_tailrec t _label block =
  (* check all Tailrec Self agree on the successor label *)
  match block.Cfg.terminator.desc with
  | Tailcall_self { destination } -> (
    match t.tailrec_entry_label with
    | None -> t.tailrec_entry_label <- Some destination
    | Some l ->
      if not (Label.equal l destination)
      then
        report t "Two self-tailcall terminators with different labels: %a %a"
          Label.print l Label.print destination)
  | Call _ | Prim _ | Tailcall_func _ | Never | Always _ | Parity_test _
  | Truth_test _ | Float_test _ | Int_test _ | Switch _ | Return | Raise _
  | Call_no_return _ | Invalid _ ->
    ()

let check_can_raise t label (block : Cfg.basic_block) =
  (* Check that block's [can_raise] field agrees with terminator instruction.
     Only the terminator can raise, other instructions in the block (basic
     instructions) do not raise. *)
  let terminator_can_raise = Cfg.can_raise_terminator block.terminator.desc in
  if not (Bool.equal block.can_raise terminator_can_raise)
  then
    report t
      "Block %s: block.can_raise is %B which does not match can_raise of its \
       terminator"
      (Label.to_string label) block.can_raise;
  (* a block can have an exn successor only if the block can raise *)
  if Option.is_some block.exn && not terminator_can_raise
  then
    report t
      "Block %s has an exceptional successor but the terminator cannot raise."
      (Label.to_string label)

let check_stack_offset t label (block : Cfg.basic_block) =
  (* [stack_offset] cannot be propagated on exceptional edges because of stack
     allocated arguments. If a terminator raises, all stack allocated arguments
     are freed up to the top of trap stack, and the number of freed slots may be
     different for different predecessors of a given trap handler block. *)
  (if not block.is_trap_handler
   then
     let stack_offset = block.stack_offset in
     List.iter
       (fun predecessor ->
         let pred_block = Cfg.get_block_exn t.cfg predecessor in
         let pred_terminator_stack_offset =
           pred_block.terminator.stack_offset
         in
         if not (Int.equal stack_offset pred_terminator_stack_offset)
         then
           report t
             "Wrong stack offset: block %s in predecessors of block %s, stack \
              offset of the terminator of %s is %d, stack offset of block %s \
              is %d, expected stack offset of terminator is %d \
              (block.is_trap_handler=%b).\n"
             (Label.to_string predecessor)
             (Label.to_string label)
             (Label.to_string predecessor)
             pred_terminator_stack_offset (Label.to_string label)
             block.stack_offset stack_offset block.is_trap_handler)
       (Cfg.predecessor_labels block));
  let terminator_stack_offset = block.terminator.stack_offset in
  Label.Set.iter
    (fun successor ->
      let succ_block = Cfg.get_block_exn t.cfg successor in
      if not (Int.equal terminator_stack_offset succ_block.stack_offset)
      then
        report t
          "Wrong stack offset: block %s in normal successors of block %s, \
           stack offset of the terminator of %s is %d, stack offset of block \
           %s is %d.\n"
          (Label.to_string successor)
          (Label.to_string label) (Label.to_string label)
          terminator_stack_offset
          (Label.to_string successor)
          succ_block.stack_offset)
    (Cfg.successor_labels ~normal:true ~exn:false block);
  let stack_offset_after_body =
    DLL.fold_left block.body ~init:block.stack_offset
      ~f:(fun cur_stack_offset (basic : Cfg.basic Cfg.instruction) ->
        if not (Int.equal cur_stack_offset basic.stack_offset)
        then
          report t
            "Wrong stack offset in block %s: the offset of [(id:%a) %a] \
             instruction is %d, but expected %d.\n"
            (Label.to_string label) InstructionId.print basic.id Cfg.dump_basic
            basic.desc basic.stack_offset cur_stack_offset;
        match basic.desc with
        | Pushtrap { lbl_handler } ->
          let handler_block = Cfg.get_block_exn t.cfg lbl_handler in
          if not (Int.equal cur_stack_offset handler_block.stack_offset)
          then
            report t
              "Wrong stack offset in block %s: the offset of [(id:%a) %a] \
               instruction is %d, the offset of block %s is %d.\n"
              (Label.to_string label) InstructionId.print basic.id
              Cfg.dump_basic basic.desc cur_stack_offset
              (Label.to_string lbl_handler)
              handler_block.stack_offset;
          cur_stack_offset + Proc.trap_size_in_bytes ()
        | Poptrap { lbl_handler = _ } ->
          let new_stack_offset =
            cur_stack_offset - Proc.trap_size_in_bytes ()
          in
          if Int.compare new_stack_offset 0 < 0
          then
            report t
              "Negative stack offset in block %s: the offset after [(id:%a) \
               %a] instruction is %d\n"
              (Label.to_string label) InstructionId.print basic.id
              Cfg.dump_basic basic.desc new_stack_offset;
          new_stack_offset
        | Op (Stackoffset n) ->
          let new_stack_offset = cur_stack_offset + n in
          if Int.compare new_stack_offset 0 < 0
          then
            report t
              "Negative stack offset in block %s: the offset after [(id:%a) \
               %a] instruction is %d\n"
              (Label.to_string label) InstructionId.print basic.id
              Cfg.dump_basic basic.desc new_stack_offset;
          new_stack_offset
        | Op
            ( Move | Spill | Reload | Const_int _ | Const_float _
            | Const_float32 _ | Const_symbol _ | Const_vec128 _ | Const_vec256 _
            | Const_vec512 _ | Load _ | Store _ | Intop _ | Int128op _
            | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _ | Static_cast _
            | Reinterpret_cast _ | Probe_is_enabled _ | Opaque | Begin_region
            | End_region | Specific _ | Name_for_debugger _ | Dls_get | Tls_get
            | Domain_index | Poll | Pause | Alloc _ )
        | Reloadretaddr | Prologue | Epilogue | Stack_check _ ->
          cur_stack_offset)
  in
  if not (Int.equal stack_offset_after_body terminator_stack_offset)
  then
    report t
      "Wrong stack offset in block %s: stack offset after body is %d, stack \
       offset of the terminator is %d.\n"
      (Label.to_string label) stack_offset_after_body terminator_stack_offset;
  ()

let check_block t label (block : Cfg.basic_block) =
  DLL.iter block.body ~f:(check_basic_arity t label);
  check_terminator_arity t label block;
  check_tailrec t label block;
  check_can_raise t label block;
  (* exn and normal successors are disjoint *)
  let exn = Cfg.successor_labels ~normal:false ~exn:true block in
  let normal = Cfg.successor_labels ~normal:true ~exn:false block in
  if not (Label.Set.disjoint exn normal)
  then
    report t "exn and normal successors of %s are not disjoint"
      (Label.to_string label);
  let successors = Label.Set.union exn normal in
  (* successors and predecessors agree *)
  Label.Set.iter
    (fun successor ->
      let succ_block = Cfg.get_block_exn t.cfg successor in
      if
        not
          (List.exists (Label.equal label) (Cfg.predecessor_labels succ_block))
      then
        report t "%s in successors(%s) but %s is not in predecessors(%s)"
          (Label.to_string successor)
          (Label.to_string label) (Label.to_string label)
          (Label.to_string successor))
    successors;
  List.iter
    (fun predecessor ->
      let pred_block = Cfg.get_block_exn t.cfg predecessor in
      (* trap handler block is reachable through exceptional edges only. *)
      let exn = Cfg.successor_labels ~normal:false ~exn:true pred_block in
      let normal = Cfg.successor_labels ~normal:true ~exn:false pred_block in
      let check_edge ~must ~must_not =
        if Label.Set.mem label must_not
        then
          report t "Unexpected edge from %s to block %s"
            (Label.to_string predecessor)
            (Label.to_string label);
        if not (Label.Set.mem label must)
        then
          report t "%s in predecessors of %s but %s not in successors of %s"
            (Label.to_string predecessor)
            (Label.to_string label) (Label.to_string label)
            (Label.to_string predecessor)
      in
      if block.is_trap_handler
      then check_edge ~must:exn ~must_not:normal
      else check_edge ~must:normal ~must_not:exn)
    (Cfg.predecessor_labels block);
  (* [stack_offset] consistent across edges and calculated correctly within
     blocks *)
  check_stack_offset t label block;
  ()

let check_reducibility t cfg_with_infos =
  match t.cfg.allowed_to_be_irreducible with
  | true -> ()
  | false -> (
    match Cfg_reducibility.is_cfg_with_infos_reducible cfg_with_infos with
    | true -> ()
    | false -> report t "CFG is not reducible")

let check_liveness t cfg_with_infos =
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  let entry_block = Cfg.get_block_exn cfg cfg.entry_label in
  let first_instruction_id = Cfg.first_instruction_id entry_block in
  match
    Cfg_with_infos.liveness_find_opt cfg_with_infos first_instruction_id
  with
  | None -> report t "Unable to get liveness for first instruction"
  | Some { Cfg_liveness.before; across = _ } ->
    Reg.Set.iter
      (fun reg ->
        match reg.loc with
        | Reg _ | Stack (Incoming _ | Local _ | Domainstate _) -> ()
        | Unknown | Stack (Outgoing _) ->
          report t "Reg %a is unexpectedly live (%a) at function entry"
            Printreg.reg reg Reg.format_location reg.loc)
      before

let run ppf cfg_with_layout =
  let cfg = CL.cfg cfg_with_layout in
  let layout = CL.layout cfg_with_layout in
  let t =
    { result = false;
      ppf;
      fun_name = cfg.fun_name;
      cfg;
      tailrec_entry_label = None
    }
  in
  check_layout t layout;
  Cfg.iter_blocks ~f:(check_block t) cfg;
  check_tailrec_position t;
  let cfg_with_infos = Cfg_with_infos.make cfg_with_layout in
  check_reducibility t cfg_with_infos;
  check_liveness t cfg_with_infos;
  t.result
