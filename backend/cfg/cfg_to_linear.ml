(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
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
[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module CL = Cfg_with_layout
module L = Linear
module DLL = Doubly_linked_list

let phantom_defining_expr_to_linear (expr : Cfg.phantom_defining_expr) =
  match expr with
  | Cphantom_const_int i -> L.lphantom_const_int i
  | Cphantom_const_symbol s -> L.lphantom_const_symbol s
  | Cphantom_var v -> L.lphantom_var v
  | Cphantom_offset_var { var; offset_in_words } ->
    L.lphantom_offset_var ~var ~offset_in_words
  | Cphantom_read_field { var; field } -> L.lphantom_read_field ~var ~field
  | Cphantom_read_symbol_field { sym; field } ->
    L.lphantom_read_symbol_field ~sym ~field
  | Cphantom_block { tag; fields } -> L.lphantom_block ~tag ~fields
  | Cphantom_optimised_out -> L.lphantom_optimised_out

let to_linear_instr ?(like : _ Cfg.instruction option) desc ~next :
    L.instruction =
  let ( arg,
        res,
        dbg,
        live,
        fdo,
        available_before,
        available_across,
        phantom_available_before ) =
    match like with
    | None ->
      ( [||],
        [||],
        Debuginfo.none,
        Reg.Set.empty,
        Fdo_info.none,
        Reg_availability_set.Unreachable,
        Reg_availability_set.Unreachable,
        None )
    | Some
        { arg;
          res;
          dbg;
          live;
          fdo;
          available_before;
          available_across;
          phantom_available_before;
          desc = _;
          id = _;
          stack_offset = _
        } ->
      ( arg,
        res,
        dbg,
        live,
        fdo,
        available_before,
        available_across,
        phantom_available_before )
  in
  { desc;
    next;
    arg;
    res;
    dbg;
    live;
    fdo;
    available_before;
    available_across;
    phantom_available_before
  }

let basic_to_linear (i : _ Cfg.instruction) ~next =
  let desc = Cfg_to_linear_desc.from_basic i.desc in
  to_linear_instr ~like:i desc ~next

(* Certain "unordered" outcomes of float comparisons are not expressible as a
   single Cmm.float_comparison operator, or a disjunction of disjoint
   Cmm.float_comparison operators. For example, for float_test { lt = L0; eq =
   L0; gt = L0; uo = L1 } there is no Mach comparison for the branch to L1.

   We haven't seen a program that leads to it yet, but it is possible that
   future transformations will. So, for now these cases are fatal error. If we
   need to handle them, if needed, we can emit an unconditional jump that
   appears last, after all other conditional jumps. *)
type float_cond =
  | Must_be_last
  | Any of Cmm.float_comparison

let mk_float_cond ~lt ~eq ~gt ~uo =
  match eq, lt, gt, uo with
  | true, false, false, false -> Any CFeq
  | false, true, false, false -> Any CFlt
  | false, false, true, false -> Any CFgt
  | true, true, false, false -> Any CFle
  | true, false, true, false -> Any CFge
  | false, true, true, true -> Any CFneq
  | true, false, true, true -> Any CFnlt
  | true, true, false, true -> Any CFngt
  | false, false, true, true -> Any CFnle
  | false, true, false, true -> Any CFnge
  | true, true, true, true -> assert false (* unconditional jump *)
  | false, false, false, false -> assert false (* no successors *)
  | true, true, true, false ->
    Misc.fatal_error "Encountered disjunction of conditions: [CFle; CFgt]"
  (* Any [CFle; CFgt] *)
  (* CR-someday gyorsh: if this case is reachable, how to choose between
     equivalent representations: [CFle;CFgt] [CFlt;CFge] [CFlt;CFeq;CFgt] *)
  | false, true, true, false ->
    Misc.fatal_error "Encountered disjunction of conditions [CFlt; CFgt]"
  (* Any [CFlt; CFgt] *)
  | false, false, false, true -> Must_be_last
  | true, false, false, true -> Must_be_last

(* Resolve for each emitted conditional branch the counters of its two machine
   edges. The taken edge collects the counters of the successor positions that
   jump to the branch's target. The machine fallthrough of a branch commits to a
   successor only when the next control-flow instruction is not another
   conditional branch: then it carries the counters of the positions matching
   the fallthrough destination (an explicit trailing jump, or the next block in
   the layout); in the middle of a branch cascade it carries none.
   [Lcondbranch3] and [Lswitch] were given their counters when created. *)
let resolve_edge_counters (terminator : Cfg.terminator Cfg.instruction)
    (desc_list : L.instruction_desc list) ~(fallthrough_label : Label.t) :
    L.instruction_desc list =
  let successors = Cfg.branch_successors terminator.desc in
  let counters_of target =
    List.fold_left
      (fun counters (successor : Cfg.successor) ->
        if Label.equal successor.target target
        then Fdo_counter.add_all counters successor.fdo_counters
        else counters)
      [] successors
  in
  let is_control (desc : L.instruction_desc) =
    match[@ocaml.warning "-4"] desc with
    | L.Lcondbranch _ | L.Lcondbranch3 _ | L.Lbranch _ -> true
    | _ -> false
  in
  let rec map = function
    | [] -> []
    | desc :: rest ->
      let desc : L.instruction_desc =
        match[@ocaml.warning "-4"] desc with
        | L.Lcondbranch { test; taken; fallthrough_counters = _ } ->
          let fallthrough_counters =
            match List.find_opt is_control rest with
            | Some (L.Lbranch label) -> counters_of label
            | Some _ -> []
            | None -> counters_of fallthrough_label
          in
          L.Lcondbranch
            { test;
              taken = { taken with fdo_counters = counters_of taken.target };
              fallthrough_counters
            }
        | desc -> desc
      in
      desc :: map rest
  in
  map desc_list

(* A conditional branch to [target]; its counters are filled in by
   [resolve_edge_counters]. *)
let condbranch test target : L.instruction_desc =
  L.Lcondbranch
    { test; taken = { target; fdo_counters = [] }; fallthrough_counters = [] }

let linear_successor ({ target; fdo_counters } : Cfg.successor) : L.successor =
  { target; fdo_counters }

let linearize_terminator (func : string)
    (terminator : Cfg.terminator Cfg.instruction)
    ~(next : Linear_utils.labelled_insn) ~has_epilogue :
    L.instruction * Label.t option =
  (* CR-someday gyorsh: refactor, a lot of redundant code for different cases *)
  (* CR-someday gyorsh: for successor labels that are not fallthrough, order of
     branch instructions should depend on perf data and possibly the relative
     position of the target labels and the current block: whether the jumps are
     forward or back. This information can be obtained from the layout. For now,
     we are making an arbitrary choice. *)
  (* If one of the successors is a fallthrough label, do not emit a jump for it.
     Otherwise, the last jump is unconditional. *)
  let branch_or_fallthrough d lbl =
    if not (Label.equal next.label lbl) then d @ [L.Lbranch lbl] else d
  in
  let single d = [d], None in
  let emit_bool (c1, l1) (c2, l2) =
    (* c1 must be the inverse of c2 *)
    match Label.equal l1 next.label, Label.equal l2 next.label with
    | true, true -> []
    | false, true -> [condbranch c1 l1]
    | true, false -> [condbranch c2 l2]
    | false, false ->
      if Label.equal l1 l2
      then [L.Lbranch l1]
      else [condbranch c1 l1; L.Lbranch l2]
  in
  let desc_list, tailrec_label =
    match terminator.desc with
    | Return -> [L.Lreturn], None
    | Raise kind -> [L.Lraise kind], None
    | Tailcall_func (Indirect { callees = _; callsite_counter }) ->
      [L.Lcall_op (Ltailcall_ind { callsite_counter })], None
    | Tailcall_func (Direct { sym = func_symbol; callsite_counter }) ->
      ( [L.Lcall_op (Ltailcall_imm { func = func_symbol; callsite_counter })],
        None )
    | Tailcall_self { destination } ->
      ( [ L.Lcall_op
            (Ltailcall_imm
               { func = { sym_name = func; sym_global = Local };
                 callsite_counter = None
               }) ],
        Some destination )
    | Call_no_return
        { func_symbol;
          alloc;
          ty_args;
          ty_res;
          stack_ofs;
          stack_align;
          effects = _
        } ->
      single
        (L.Lcall_op
           (Lextcall
              { func = func_symbol;
                alloc;
                ty_args;
                ty_res;
                returns = false;
                stack_ofs;
                stack_align
              }))
    | Invalid { message = _; stack_ofs; stack_align; label_after = None; _ } ->
      single
        (L.Lcall_op
           (Lextcall
              { func = Cmm.caml_flambda2_invalid;
                alloc = false;
                ty_args = (* Arg is a statically allocated symbol. *) [XInt];
                ty_res = Cmm.typ_void;
                returns = false;
                stack_ofs;
                stack_align
              }))
    | Invalid { label_after = Some _; _ } ->
      Misc.fatal_error "Cannot linearize terminator: Invalid with a successor"
    | Call { op; label_after } ->
      let op : Linear.call_operation =
        match op with
        | Indirect { callees = _; callsite_counter } ->
          Lcall_ind { callsite_counter }
        | Direct { sym = func_symbol; callsite_counter } ->
          Lcall_imm { func = func_symbol; callsite_counter }
      in
      branch_or_fallthrough [L.Lcall_op op] label_after, None
    | Prim { op; label_after } ->
      let op : Linear.call_operation =
        match op with
        | External
            { func_symbol;
              alloc;
              ty_args;
              ty_res;
              stack_ofs;
              stack_align;
              effects = _
            } ->
          Lextcall
            { func = func_symbol;
              alloc;
              ty_args;
              ty_res;
              returns = true;
              stack_ofs;
              stack_align
            }
        | Probe { name; handler_code_sym; enabled_at_init } ->
          Lprobe { name; handler_code_sym; enabled_at_init }
      in
      branch_or_fallthrough [L.Lcall_op op] label_after, None
    | Switch successors ->
      single (L.Lswitch (Array.map linear_successor successors))
    | Never -> Misc.fatal_error "Cannot linearize terminator: Never"
    | Always label -> branch_or_fallthrough [] label, None
    | Parity_test { ifso; ifnot } ->
      emit_bool (Ieventest, ifso.target) (Ioddtest, ifnot.target), None
    | Truth_test { ifso; ifnot } ->
      emit_bool (Itruetest, ifso.target) (Ifalsetest, ifnot.target), None
    | Float_test { width; lt = lt_successor; eq; gt; uo } -> (
      let lt = lt_successor.target
      and eq = eq.target
      and gt = gt.target
      and uo = uo.target in
      let successor_labels =
        Label.Set.singleton lt |> Label.Set.add gt |> Label.Set.add eq
        |> Label.Set.add uo
      in
      match Label.Set.cardinal successor_labels with
      | 0 -> assert false
      | 1 -> branch_or_fallthrough [] (Label.Set.min_elt successor_labels), None
      | 2 | 3 | 4 ->
        let must_be_last, any =
          Label.Set.fold
            (fun lbl (must_be_last, any) ->
              let cond =
                mk_float_cond ~lt:(Label.equal lt lbl) ~eq:(Label.equal eq lbl)
                  ~gt:(Label.equal gt lbl) ~uo:(Label.equal uo lbl)
              in
              match cond with
              | Any c -> must_be_last, (c, lbl) :: any
              | Must_be_last -> lbl :: must_be_last, any)
            successor_labels ([], [])
        in
        let last =
          match must_be_last with
          | [] ->
            if Label.Set.mem next.label successor_labels
            then next.label
            else
              (* arbitrary choice (also see CR above) *)
              Label.Set.min_elt successor_labels
          | [lbl] ->
            Printf.eprintf "One success label must be last: %s\n"
              (Label.to_string lbl);
            (* CR-someday gyorsh: fail for safety, until we see a case that
               exhibits this behavior.. This behavior should not be possible
               with the current cfg construction. *)
            Misc.fatal_errorf
              "Illegal branch: one successor label must be last %a" Label.format
              lbl ()
          | _ ->
            Misc.fatal_error
              "Illegal branch: more than one successor label that must be last"
        in
        let branches =
          List.filter_map
            (fun (c, lbl) ->
              if Label.equal lbl last
              then None
              else Some (condbranch (Ifloattest (width, c)) lbl))
            any
        in
        branches @ branch_or_fallthrough [] last, None
      | _ -> assert false)
    | Int_test
        { lt = lt_successor;
          eq = eq_successor;
          gt = gt_successor;
          imm;
          is_signed
        } -> (
      let lt = lt_successor.target
      and eq = eq_successor.target
      and gt = gt_successor.target in
      let successor_labels =
        Label.Set.singleton lt |> Label.Set.add gt |> Label.Set.add eq
      in
      match Label.Set.cardinal successor_labels with
      | 0 -> assert false
      | 1 -> branch_or_fallthrough [] (Label.Set.min_elt successor_labels), None
      | 2 | 3 ->
        (* If fallthrough label is a successor, do not emit a jump for it.
           Otherwise, the last jump could be unconditional. *)
        let last =
          if Label.Set.mem next.label successor_labels
          then next.label
          else
            (* arbitrary choice (see also CR above) *)
            Label.Set.min_elt successor_labels
        in
        let cond_successor_labels = Label.Set.remove last successor_labels in
        (* Lcondbranch3 is emitted as an unsigned comparison, see ocaml PR
           #8677 *)
        let can_emit_Lcondbranch3 =
          match is_signed, imm with
          | Unsigned, Some 1 -> true
          | Unsigned, Some _ | Unsigned, None | Signed, _ -> false
        in
        if Label.Set.cardinal cond_successor_labels = 2 && can_emit_Lcondbranch3
        then
          (* generates one cmp instruction for all conditional jumps here *)
          let find (successor : Cfg.successor) =
            if Label.equal next.label successor.target
            then None
            else Some (linear_successor successor)
          in
          let lt = find lt_successor
          and eq = find eq_successor
          and gt = find gt_successor in
          (* The outcomes without a jump fall through. *)
          let fallthrough_counters =
            List.fold_left
              (fun counters ((successor : Cfg.successor), jump) ->
                match (jump : L.successor option) with
                | Some _ -> counters
                | None -> Fdo_counter.add_all counters successor.fdo_counters)
              []
              [lt_successor, lt; eq_successor, eq; gt_successor, gt]
          in
          [L.Lcondbranch3 { lt; eq; gt; fallthrough_counters }], None
        else
          let init = branch_or_fallthrough [] last in
          ( Label.Set.fold
              (fun lbl acc ->
                match
                  Scalar.Integer_comparison.create is_signed
                    ~lt:(Label.equal lt lbl) ~eq:(Label.equal eq lbl)
                    ~gt:(Label.equal gt lbl)
                with
                | Error result ->
                  Misc.fatal_errorf
                    "Cannot linearize terminator: meaningless specification of \
                     comparison, always has result %b:@ %a"
                    result Printcfg.terminator terminator
                | Ok comp ->
                  let test =
                    match imm with
                    | None -> Operation.Iinttest comp
                    | Some n -> Operation.Iinttest_imm (comp, n)
                  in
                  condbranch test lbl :: acc)
              cond_successor_labels init,
            None )
      | _ -> assert false)
  in
  let desc_list =
    match has_epilogue with
    | true ->
      (* The corresponding [Lepilogue_open] was already added when converting
         the body of the block, replacing a [Cfg.Epilogue] instruction. The
         [Lepilogue_open] should be the last instruction in the block body,
         immediately preceding the terminator. *)
      desc_list @ [L.Lepilogue_close]
    | false -> desc_list
  in
  let instr =
    List.fold_left
      (fun next desc ->
        let instr = to_linear_instr desc ~next ~like:terminator in
        match has_epilogue with
        (* In order to match the debug info generated when the epilogue was not
           a linear instruction, we need to explicitly remove debug info, as
           they were already added to Lepilogue_open. *)
        | true -> { instr with L.dbg = Debuginfo.none }
        | false -> instr)
      next.insn
      (List.rev
         (resolve_edge_counters terminator desc_list
            ~fallthrough_label:next.label))
  in
  instr, tailrec_label

let need_starting_label (block : Cfg.basic_block)
    ~(prev_block : Cfg.basic_block) =
  if block.is_trap_handler
  then true
  else
    match Label.Set.elements block.predecessors with
    | [] | _ :: _ :: _ -> true
    | [pred] when not (Label.equal pred prev_block.start) -> true
    | [_] -> (
      (* This block has a single predecessor which appears in the layout
         immediately prior to this block. *)
      (* No need for the label, unless the predecessor's terminator is [Switch]
         when the label is needed for the jump table, or [Tailcall_self] when
         the label is the target of the tail call and the predecessor cannot
         fall through. *)
      let fatal_follows_non_returning () =
        Misc.fatal_errorf
          "Cfg_to_linear.need_starting_label: block follows non-returning \
           terminator %a"
          Printcfg.terminator prev_block.terminator
      in
      match prev_block.terminator.desc with
      | Switch _ -> true
      | Tailcall_self _ ->
        (* The label is the target of the tail call, so it must be emitted. Out
           of an abundance of caution, this is restricted to [-cfg-block-layout]
           (the only pass creating such layouts), so that the behaviour with the
           flag disabled is exactly the historical one. *)
        if !Oxcaml_flags.cfg_block_layout
        then true
        else fatal_follows_non_returning ()
      | Never -> Misc.fatal_error "Cannot linearize terminator: Never"
      | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
      | Call _ | Prim _ | Invalid _ ->
        false
      | Return | Raise _ | Tailcall_func _ | Call_no_return _ ->
        (* Unreachable for a consistent CFG: these terminators have no normal
           successors, and their exceptional successors are trap handlers,
           handled above. *)
        fatal_follows_non_returning ())

let adjust_stack_offset body (block : Cfg.basic_block)
    ~(prev_block : Cfg.basic_block) =
  let block_stack_offset = block.stack_offset in
  let prev_stack_offset = prev_block.terminator.stack_offset in
  if block_stack_offset = Cfg.invalid_stack_offset
  then
    Misc.fatal_errorf "Cfg_to_linear: block %a has an invalid stack offset"
      Label.format block.start;
  if prev_stack_offset = Cfg.invalid_stack_offset
  then
    Misc.fatal_errorf
      "Cfg_to_linear: the terminator of block %a has an invalid stack offset"
      Label.format prev_block.start;
  if block_stack_offset = prev_stack_offset
  then body
  else
    let delta_bytes = block_stack_offset - prev_stack_offset in
    to_linear_instr (Ladjust_stack_offset { delta_bytes }) ~next:body

(* CR-someday gyorsh: handle duplicate labels in new layout: print the same
   block more than once. *)
let run cfg_with_layout =
  let cfg = CL.cfg cfg_with_layout in
  let layout = CL.layout cfg_with_layout in
  let next = ref Linear_utils.labelled_insn_end in
  let tailrec_label = ref None in
  DLL.iter_right_cell layout ~f:(fun cell ->
      let label = DLL.value cell in
      if not (Label.Tbl.mem cfg.blocks label)
      then Misc.fatal_errorf "Unknown block labelled %a\n" Label.format label;
      let block = Label.Tbl.find cfg.blocks label in
      assert (Label.equal label block.start);
      let body =
        let has_epilogue =
          DLL.exists block.body ~f:(fun instr ->
              match[@ocaml.warning "-4"] instr.Cfg.desc with
              | Cfg.Epilogue -> true
              | _ -> false)
        in
        let terminator, terminator_tailrec_label =
          linearize_terminator cfg.fun_name block.terminator ~next:!next
            ~has_epilogue
        in
        (match !tailrec_label, terminator_tailrec_label with
        | (Some _ | None), None -> ()
        | None, Some _ -> tailrec_label := terminator_tailrec_label
        | Some old_trl, Some new_trl -> assert (Label.equal old_trl new_trl));
        DLL.fold_right
          ~f:(fun i next -> basic_to_linear i ~next)
          ~init:terminator block.body
      in
      let insn =
        match DLL.prev cell with
        | None -> body (* Entry block of the function. Don't add label. *)
        | Some prev_cell ->
          let body =
            if block.is_trap_handler
            then to_linear_instr Lentertrap ~next:body
            else body
          in
          let prev = DLL.value prev_cell in
          let prev_block = Label.Tbl.find cfg.blocks prev in
          let body =
            if need_starting_label block ~prev_block
            then
              let instr =
                to_linear_instr (Linear.Llabel_for_jump_target block.start)
                  ~next:body
              in
              { instr with
                available_before = body.available_before;
                available_across = body.available_across
              }
            else body
          in
          adjust_stack_offset body block ~prev_block
      in
      next := { Linear_utils.label; insn });
  { Linear.fun_name = cfg.fun_name;
    fun_args = Reg.set_of_array cfg.fun_args;
    fun_body = !next.insn;
    fun_tailrec_entry_point_label = !tailrec_label;
    fun_fast = not (List.mem Cfg.Reduce_code_size cfg.fun_codegen_options);
    fun_dbg = cfg.fun_dbg;
    fun_fdo_entry_counters = cfg.fun_fdo_entry_counters;
    fun_function_body_hash = cfg.fun_function_body_hash;
    fun_contains_calls = cfg.fun_contains_calls;
    fun_num_stack_slots = cfg.fun_num_stack_slots;
    fun_frame_required = cfg.fun_frame_required;
    fun_prologue_required = cfg.fun_prologue_required;
    fun_phantom_lets =
      Backend_var.Map.map
        (fun (provenance, defining_expr) ->
          provenance, phantom_defining_expr_to_linear defining_expr)
        (Cfg.fun_phantom_lets cfg)
  }
