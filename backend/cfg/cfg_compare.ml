(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-4-9-40-41-42"]

(** Structural and register-equivalence check between two CFGs for the same
    function. Used to validate the SSA pipeline against the baseline
    [Cfg_selectgen] output.

    The comparison runs in two phases:

    - [collect_matching_blocks]: Walks from the entry blocks of both CFGs in
      lockstep, building a bijection between old and new labels via [map_label].
      Calls to [map_label] come from terminator successor fields (Goto target,
      Branch arms, Switch cases, Tailcall_self destination, Call/Prim
      continuation, Invalid continuation), from [Pushtrap]/[Poptrap] body
      instructions, and from block-level [exn] pointers. As blocks are paired up
      the framework checks that their bodies match instruction-by-instruction
      (modulo skipping moves), that their terminators have the same shape, and
      that block-level metadata (is_trap_handler, can_raise, cold, stack_offset)
      agrees. Dead trap handlers (and their [Pushtrap]/[Poptrap]s) are removed
      by [Cfg_simplify] in both pipelines when SSA is enabled, so they need no
      special handling here.

    - [verify_register_equivalence]: Backward fixpoint over matched block pairs:
      starting from all block ends with the empty equation set, propagate
      equality obligations between old and new registers up through each block's
      instructions until reaching a fixpoint. The [Equations] module tracks
      these (old_reg, new_reg) pairs , which are proof obligations rather than
      assumptions. Moves are handled as substitutions on the equations set. Any
      obligation that survives back to a block's entry is added to the
      predecessors final equation set. The two possible failure cases are
      equations propagated to the function start and equations about an
      instruction's result that remain after removing the equations for the
      matching results of two matched instructions. In addition, at GC points
      (calls, allocations, polls) the machtypes of all equated register pairs
      live across the instruction must agree, since a [Val]/[Int] discrepancy
      there changes GC behavior. *)

module DLL = Oxcaml_utils.Doubly_linked_list

(* A set of (old_reg, new_reg) pairs asserting that these registers hold equal
   values at a given program point. Represented as two [Reg.Set.t Reg.Map.t]
   maps for bidirectional lookup. Not necessarily injective. Since we are doing
   a backwards analysis, these are proof obligations rather than assumptions. *)
module Equations : sig
  type t

  val empty : t

  val equal : t -> t -> bool

  val add_pair : t -> old_reg:Reg.t -> new_reg:Reg.t -> t

  val remove_old : t -> Reg.t -> t

  val remove_new : t -> Reg.t -> t

  val remove_pair : t -> old_reg:Reg.t -> new_reg:Reg.t -> t

  val subst_old_move : t -> src:Reg.t -> dst:Reg.t -> t

  val subst_new_move : t -> src:Reg.t -> dst:Reg.t -> t

  val union : t -> t -> t

  val find_partners_of_old : t -> Reg.t -> Reg.Set.t option

  val find_partners_of_new : t -> Reg.t -> Reg.Set.t option

  val iter_old_to_new : t -> f:(Reg.t -> Reg.t -> unit) -> unit
end = struct
  type t =
    { fwd : Reg.Set.t Reg.Map.t;
      bwd : Reg.Set.t Reg.Map.t
    }

  let empty = { fwd = Reg.Map.empty; bwd = Reg.Map.empty }

  let equal a b = Reg.Map.equal Reg.Set.equal a.fwd b.fwd

  let mirror { fwd; bwd } = { fwd = bwd; bwd = fwd }

  let add_to_map key value map =
    Reg.Map.update key
      (function
        | None -> Some (Reg.Set.singleton value)
        | Some s -> Some (Reg.Set.add value s))
      map

  let add_pair { fwd; bwd } ~old_reg ~new_reg =
    { fwd = add_to_map old_reg new_reg fwd;
      bwd = add_to_map new_reg old_reg bwd
    }

  let remove_from_map key value map =
    Reg.Map.update key
      (function
        | None -> None
        | Some s ->
          let s = Reg.Set.remove value s in
          if Reg.Set.is_empty s then None else Some s)
      map

  (* Remove all equations involving [old_reg] on the old/left side. *)
  let remove_old eqs old_reg =
    match Reg.Map.find_opt old_reg eqs.fwd with
    | None -> eqs
    | Some partners ->
      { fwd = Reg.Map.remove old_reg eqs.fwd;
        bwd =
          Reg.Set.fold
            (fun partner -> remove_from_map partner old_reg)
            partners eqs.bwd
      }

  let remove_new eqs reg = mirror (remove_old (mirror eqs) reg)

  let remove_pair { fwd; bwd } ~old_reg ~new_reg =
    { fwd = remove_from_map old_reg new_reg fwd;
      bwd = remove_from_map new_reg old_reg bwd
    }

  (* Move: dst <- src. Replace dst with src on the old/left side. Note that we
     replace the destination with the source because we are processing
     backwards. *)
  let subst_old_move eqs ~src ~dst =
    match Reg.Map.find_opt dst eqs.fwd with
    | None -> eqs
    | Some partners ->
      let eqs = remove_old eqs dst in
      Reg.Set.fold
        (fun new_reg eqs -> add_pair eqs ~old_reg:src ~new_reg)
        partners eqs

  let subst_new_move eqs ~src ~dst =
    mirror (subst_old_move (mirror eqs) ~src ~dst)

  let union a b =
    let merge _ s1 s2 = Some (Reg.Set.union s1 s2) in
    { fwd = Reg.Map.union merge a.fwd b.fwd;
      bwd = Reg.Map.union merge a.bwd b.bwd
    }

  let find_partners_of_old eqs reg = Reg.Map.find_opt reg eqs.fwd

  let find_partners_of_new eqs reg = Reg.Map.find_opt reg eqs.bwd

  let iter_old_to_new eqs ~f =
    Reg.Map.iter
      (fun old_reg new_regs ->
        Reg.Set.iter (fun new_reg -> f old_reg new_reg) new_regs)
      eqs.fwd
end

(* === Helpers === *)

let is_move (instr : Cfg.basic Cfg.instruction) =
  match instr.desc with
  (* Opaque should not be treated as a move, really. However, there is a bug in
     the current Cfg_selectgen, where let a = opaque x in let b = opaque x in
     ... is translated as let a = opaque x in let b = opaque a in ... *)
  (* CR ttebbi: Fix Cfg_selectgen and remove Opaque here. *)
  | Op (Move | Opaque) ->
    Array.length instr.arg = 1
    && Array.length instr.res = 1
    && Reg.is_unknown instr.arg.(0)
    && Reg.is_unknown instr.res.(0)
  | _ -> false

(* === Phase 1: structural matching === *)

let basic_desc_match ~map_label (old_desc : Cfg.basic) (new_desc : Cfg.basic) =
  match old_desc, new_desc with
  | ( Pushtrap { lbl_handler = old_handler },
      Pushtrap { lbl_handler = new_handler } )
  | Poptrap { lbl_handler = old_handler }, Poptrap { lbl_handler = new_handler }
    ->
    map_label old_handler new_handler;
    true
  | _ -> Cfg.equal_basic old_desc new_desc

(** Compare the non-register, non-desc fields of a [Cfg.instruction] pair.
    [desc] is compared by the caller (its type and equality depend on whether it
    is a basic or terminator instruction); [arg]/[res] are compared in phase 2
    ([process_block_backward]); [id] is allowed to differ. *)
let compare_instruction_fields ~ppf_m ~kind ~old_label ~new_label
    (old_instr : _ Cfg.instruction) (new_instr : _ Cfg.instruction) =
  let[@warning "+9"] { Cfg.desc = _;
                       id = old_id;
                       arg = _;
                       res = _;
                       dbg = old_dbg;
                       fdo = old_fdo;
                       live = old_live;
                       stack_offset = old_stack_offset;
                       available_before = old_avail_before;
                       available_across = old_avail_across
                     } =
    old_instr
  in
  let[@warning "+9"] { Cfg.desc = _;
                       id = new_id;
                       arg = _;
                       res = _;
                       dbg = new_dbg;
                       fdo = new_fdo;
                       live = new_live;
                       stack_offset = new_stack_offset;
                       available_before = new_avail_before;
                       available_across = new_avail_across
                     } =
    new_instr
  in
  let report_with field pp_value old_value new_value =
    Format.fprintf ppf_m
      "%s %s mismatch at old=%a(id:%a) new=%a(id:%a): %a vs %a@." kind field
      Label.format old_label InstructionId.print old_id Label.format new_label
      InstructionId.print new_id pp_value old_value pp_value new_value
  in
  let report field =
    Format.fprintf ppf_m "%s %s mismatch at old=%a(id:%a) new=%a(id:%a)@." kind
      field Label.format old_label InstructionId.print old_id Label.format
      new_label InstructionId.print new_id
  in
  if Debuginfo.compare old_dbg new_dbg <> 0
  then report_with "dbg" Debuginfo.print_compact old_dbg new_dbg;
  if not (Int.equal old_stack_offset new_stack_offset)
  then
    report_with "stack_offset" Format.pp_print_int old_stack_offset
      new_stack_offset;
  if not (Fdo_info.equal old_fdo new_fdo) then report "fdo";
  (* [live] and the availability sets are not populated at this pipeline stage:
     [live] is empty and the availability sets are [Unreachable]. Comparing old
     against new would be misleading, since a populated value is unexpected
     regardless of whether the two sides happen to agree; flag it instead. *)
  let report_nonempty field pp_value old_value new_value =
    Format.fprintf ppf_m
      "%s %s expected to be empty at old=%a(id:%a) new=%a(id:%a): %a vs %a@."
      kind field Label.format old_label InstructionId.print old_id Label.format
      new_label InstructionId.print new_id pp_value old_value pp_value new_value
  in
  if not (Reg.Set.is_empty old_live && Reg.Set.is_empty new_live)
  then report_nonempty "live" Printreg.regset old_live new_live;
  let check_available_field field old_avail new_avail =
    let is_unreachable : Reg_availability_set.t -> bool = function
      | Unreachable -> true
      | Ok _ -> false
    in
    if not (is_unreachable old_avail && is_unreachable new_avail)
    then
      report_nonempty field
        (Reg_availability_set.print ~print_reg:Printreg.reg)
        old_avail new_avail
  in
  check_available_field "available_before" old_avail_before new_avail_before;
  check_available_field "available_across" old_avail_across new_avail_across

let compare_body ~ppf_m ~map_label ~old_label ~new_label old_body new_body =
  let rec skip_moves cell =
    match cell with
    | Some c -> if is_move (DLL.value c) then skip_moves (DLL.next c) else cell
    | None -> cell
  in
  let rec loop old_cell new_cell =
    match skip_moves old_cell, skip_moves new_cell with
    | None, None -> ()
    | Some _, None | None, Some _ ->
      Format.fprintf ppf_m "Body length mismatch at old=%a new=%a@."
        Label.format old_label Label.format new_label
    | Some old_cell, Some new_cell ->
      let (old_instr : Cfg.basic Cfg.instruction) = DLL.value old_cell in
      let (new_instr : Cfg.basic Cfg.instruction) = DLL.value new_cell in
      if not (basic_desc_match ~map_label old_instr.desc new_instr.desc)
      then
        Format.fprintf ppf_m
          "Instruction mismatch at old=%a(id:%a) new=%a(id:%a): %a vs %a@."
          Label.format old_label InstructionId.print old_instr.id Label.format
          new_label InstructionId.print new_instr.id Printcfg.basic_desc
          old_instr.desc Printcfg.basic_desc new_instr.desc
      else
        compare_instruction_fields ~ppf_m ~kind:"Instruction" ~old_label
          ~new_label old_instr new_instr;
      loop (DLL.next old_cell) (DLL.next new_cell)
  in
  loop (DLL.hd_cell old_body) (DLL.hd_cell new_body)

let terminator_structure_match ~map_label (old_term : Cfg.terminator)
    (new_term : Cfg.terminator) =
  match old_term, new_term with
  | Never, _ | _, Never ->
    Misc.fatal_error "Never is unexpected in finished graphs"
  | Always old_label, Always new_label ->
    map_label old_label new_label;
    true
  | ( Parity_test { ifso = old_ifso; ifnot = old_ifnot },
      Parity_test { ifso = new_ifso; ifnot = new_ifnot } )
  | ( Truth_test { ifso = old_ifso; ifnot = old_ifnot },
      Truth_test { ifso = new_ifso; ifnot = new_ifnot } ) ->
    map_label old_ifso new_ifso;
    map_label old_ifnot new_ifnot;
    true
  (* Truth_test ≈ Int_test against [0] when [lt = gt] (both miss-the-equality
     arms go to the same label, so only the equality bit matters). [is_signed]
     is irrelevant when comparing to [0]. *)
  | ( Int_test { lt; eq; gt; imm = Some 0; is_signed = _ },
      Truth_test { ifso; ifnot } )
    when Label.equal lt gt ->
    map_label lt ifso;
    map_label eq ifnot;
    true
  | ( Truth_test { ifso; ifnot },
      Int_test { lt; eq; gt; imm = Some 0; is_signed = _ } )
    when Label.equal lt gt ->
    map_label ifso lt;
    map_label ifnot eq;
    true
  | ( Int_test
        { lt = old_lt;
          eq = old_eq;
          gt = old_gt;
          is_signed = old_is_signed;
          imm = old_imm
        },
      Int_test
        { lt = new_lt;
          eq = new_eq;
          gt = new_gt;
          is_signed = new_is_signed;
          imm = new_imm
        } ) ->
    map_label old_lt new_lt;
    map_label old_eq new_eq;
    map_label old_gt new_gt;
    Scalar.Signedness.equal old_is_signed new_is_signed
    && Option.equal Int.equal old_imm new_imm
  | ( Float_test
        { width = old_width;
          lt = old_lt;
          eq = old_eq;
          gt = old_gt;
          uo = old_uo
        },
      Float_test
        { width = new_width;
          lt = new_lt;
          eq = new_eq;
          gt = new_gt;
          uo = new_uo
        } ) ->
    map_label old_lt new_lt;
    map_label old_eq new_eq;
    map_label old_gt new_gt;
    map_label old_uo new_uo;
    Cmm.equal_float_width old_width new_width
  | Switch old_labels, Switch new_labels ->
    if Array.length old_labels = Array.length new_labels
    then (
      Array.iter2 map_label old_labels new_labels;
      true)
    else false
  | Return, Return -> true
  | Raise old_raise_kind, Raise new_raise_kind ->
    Lambda.equal_raise_kind old_raise_kind new_raise_kind
  | ( Tailcall_self { destination = old_destination },
      Tailcall_self { destination = new_destination } ) ->
    map_label old_destination new_destination;
    true
  | Tailcall_func old_op, Tailcall_func new_op ->
    Cfg.equal_func_call_operation old_op new_op
  | Call_no_return old_op, Call_no_return new_op ->
    Cfg.equal_external_call_operation old_op new_op
  | ( Call { op = old_op; label_after = old_label_after },
      Call { op = new_op; label_after = new_label_after } ) ->
    map_label old_label_after new_label_after;
    Cfg.equal_func_call_operation old_op new_op
  | ( Prim { op = old_op; label_after = old_label_after },
      Prim { op = new_op; label_after = new_label_after } ) ->
    map_label old_label_after new_label_after;
    Cfg.equal_prim_call_operation old_op new_op
  | ( Invalid
        { message = old_message;
          stack_ofs = old_stack_ofs;
          stack_align = old_stack_align;
          label_after = old_label_after
        },
      Invalid
        { message = new_message;
          stack_ofs = new_stack_ofs;
          stack_align = new_stack_align;
          label_after = new_label_after
        } ) ->
    let labels_match =
      match old_label_after, new_label_after with
      | None, None -> true
      | Some old_label, Some new_label ->
        map_label old_label new_label;
        true
      | Some _, None | None, Some _ -> false
    in
    labels_match
    && String.equal old_message new_message
    && Int.equal old_stack_ofs new_stack_ofs
    && Cmm.equal_stack_align old_stack_align new_stack_align
  | ( ( Always _ | Parity_test _ | Truth_test _ | Int_test _ | Float_test _
      | Switch _ | Return | Raise _ | Tailcall_self _ | Tailcall_func _
      | Call_no_return _ | Call _ | Prim _ | Invalid _ ),
      _ ) ->
    false

let compare_block_flags ~ppf_m ~(old_block : Cfg.basic_block)
    ~(new_block : Cfg.basic_block) =
  let flags_equal
      ({ is_trap_handler;
         can_raise;
         stack_offset;
         cold;
         start = _;
         body = _;
         terminator = _;
         predecessors = _;
         exn = _
       } :
        Cfg.basic_block) (other : Cfg.basic_block) =
    Bool.equal is_trap_handler other.is_trap_handler
    && Bool.equal can_raise other.can_raise
    && Bool.equal cold other.cold
    && Int.equal stack_offset other.stack_offset
  in
  let format_block_flags ppf (block : Cfg.basic_block) =
    Format.fprintf ppf "is_trap_handler=%b can_raise=%b cold=%b stack_offset=%d"
      block.is_trap_handler block.can_raise block.cold block.stack_offset
  in
  if not (flags_equal old_block new_block)
  then
    Format.fprintf ppf_m "Block flags mismatch at old=%a(%a) new=%a(%a)@."
      Label.format old_block.start format_block_flags old_block Label.format
      new_block.start format_block_flags new_block

let compare_block_exn ~ppf_m ~map_label ~(old_block : Cfg.basic_block)
    ~(new_block : Cfg.basic_block) =
  match old_block.exn, new_block.exn with
  | Some old_exn, Some new_exn -> map_label old_exn new_exn
  | None, None -> ()
  | _ ->
    Format.fprintf ppf_m "Exn presence mismatch at old=%a new=%a@." Label.format
      old_block.start Label.format new_block.start

let compare_terminator ~ppf_m ~map_label ~(old_block : Cfg.basic_block)
    ~(new_block : Cfg.basic_block) =
  if
    not
      (terminator_structure_match ~map_label old_block.terminator.desc
         new_block.terminator.desc)
  then
    Format.fprintf ppf_m
      "Terminator mismatch at old=%a(id:%a) new=%a(id:%a): %a vs %a@."
      Label.format old_block.start InstructionId.print old_block.terminator.id
      Label.format new_block.start InstructionId.print new_block.terminator.id
      (Printcfg.terminator_desc ~sep:"")
      old_block.terminator.desc
      (Printcfg.terminator_desc ~sep:"")
      new_block.terminator.desc
  else
    compare_instruction_fields ~ppf_m ~kind:"Terminator"
      ~old_label:old_block.start ~new_label:new_block.start old_block.terminator
      new_block.terminator

let compare_matched_blocks ~ppf_m ~map_label ~(old_block : Cfg.basic_block)
    ~(new_block : Cfg.basic_block) =
  compare_block_flags ~ppf_m ~old_block ~new_block;
  compare_block_exn ~ppf_m ~map_label ~old_block ~new_block;
  compare_body ~ppf_m ~map_label ~old_label:old_block.start
    ~new_label:new_block.start old_block.body new_block.body;
  compare_terminator ~ppf_m ~map_label ~old_block ~new_block

let collect_matching_blocks ~ppf_m ~old_cfg ~new_cfg =
  (* new_to_old: the primary label mapping (new_label -> old_label) *)
  let new_to_old = Label.Tbl.create 64 in
  (* old_to_new: inverse mapping, used only to assert bijectivity *)
  let old_to_new = Label.Tbl.create 64 in
  let queue = Queue.create () in
  let map_label old_label new_label =
    (match Label.Tbl.find_opt new_to_old new_label with
    | Some mapped ->
      if not (Label.equal mapped old_label)
      then
        Format.fprintf ppf_m
          "Label mapping conflict: new=%a already maps to old=%a, not old=%a@."
          Label.format new_label Label.format mapped Label.format old_label
    | None -> Label.Tbl.replace new_to_old new_label old_label);
    (match Label.Tbl.find_opt old_to_new old_label with
    | Some mapped ->
      if not (Label.equal mapped new_label)
      then
        Format.fprintf ppf_m
          "Label mapping conflict: old=%a already maps to new=%a, not new=%a@."
          Label.format old_label Label.format mapped Label.format new_label
    | None -> Label.Tbl.replace old_to_new old_label new_label);
    Queue.add (old_label, new_label) queue
  in
  map_label (Cfg.entry_label old_cfg) (Cfg.entry_label new_cfg);
  let visited = ref Label.Set.empty in
  while not (Queue.is_empty queue) do
    let old_label, new_label = Queue.pop queue in
    if not (Label.Set.mem old_label !visited)
    then begin
      visited := Label.Set.add old_label !visited;
      let old_block = Cfg.get_block old_cfg old_label in
      let new_block = Cfg.get_block new_cfg new_label in
      match old_block, new_block with
      | None, _ | _, None ->
        Format.fprintf ppf_m "Missing block: old=%a(%s) new=%a(%s)@."
          Label.format old_label
          (if Option.is_none old_block then "missing" else "ok")
          Label.format new_label
          (if Option.is_none new_block then "missing" else "ok")
      | Some old_block, Some new_block ->
        compare_matched_blocks ~ppf_m ~map_label ~old_block ~new_block
    end
  done;
  (* Validate predecessors match under the label mapping *)
  new_to_old
  |> Label.Tbl.iter (fun new_label old_label ->
      match
        Cfg.get_block old_cfg old_label, Cfg.get_block new_cfg new_label
      with
      | None, _ | _, None ->
        (* Already reported as "Missing block" during the matching walk; don't
           die here, so that the buffered report still reaches the user. *)
        ()
      | Some old_block, Some new_block ->
        (* We ignore unvisited predecessors, which could be eliminated handler
           blocks and their successors.*)
        let mapped_old_preds =
          Label.Set.filter_map
            (fun old_pred -> Label.Tbl.find_opt old_to_new old_pred)
            old_block.predecessors
        in
        let visited_new_preds =
          Label.Set.filter (Label.Tbl.mem new_to_old) new_block.predecessors
        in
        if not (Label.Set.equal mapped_old_preds visited_new_preds)
        then
          Format.fprintf ppf_m "Predecessor mismatch at old=%a new=%a@."
            Label.format old_label Label.format new_label);
  new_to_old

(* === Phase 2: register equivalence ===

   Backward fixpoint over matched block pairs. Processes each block in reverse
   to propagate register equations. Only handles register tracking — all
   structural checks (desc, debuginfo) are done in phase 1. *)

let effective_arg (instr : Cfg.basic Cfg.instruction) =
  match instr.desc with
  | Op (Name_for_debugger { regs; _ }) ->
    assert (Array.length instr.arg = 0);
    regs
  | _ -> instr.arg

let process_block_backward ~ppf_m eqs ~(old_block : Cfg.basic_block)
    ~(new_block : Cfg.basic_block) =
  let old_label = old_block.start in
  let new_label = new_block.start in
  let process_reg_pairs eqs ~old_regs ~new_regs ~(old_instr : _ Cfg.instruction)
      ~(new_instr : _ Cfg.instruction) f =
    let eqs = ref eqs in
    for i = 0 to Array.length old_regs - 1 do
      let old_reg = old_regs.(i) in
      let new_reg = new_regs.(i) in
      if not (Proc.types_are_compatible old_reg new_reg)
      then
        Format.fprintf ppf_m
          "Reg type mismatch at old=%a(id:%a) new=%a(id:%a): %a vs %a@."
          Label.format old_label InstructionId.print old_instr.id Label.format
          new_label InstructionId.print new_instr.id Printreg.reg old_reg
          Printreg.reg new_reg;
      if Reg.is_unknown old_reg && Reg.is_unknown new_reg
      then eqs := f !eqs ~old_reg ~new_reg
      else if not (Reg.same_loc old_reg new_reg)
      then
        Format.fprintf ppf_m
          "Reg mismatch at old=%a(id:%a) new=%a(id:%a): %a vs %a@." Label.format
          old_label InstructionId.print old_instr.id Label.format new_label
          InstructionId.print new_instr.id Printreg.reg old_reg Printreg.reg
          new_reg
    done;
    !eqs
  in
  (* Process a matched instruction pair: remove output equations, then add input
     equations. *)
  let process_instruction eqs ~(old_instr : _ Cfg.instruction)
      ~(new_instr : _ Cfg.instruction) ~old_arg ~new_arg ~is_gc_point =
    (* Check physical result registers match, then remove matched output
       pairs *)
    let eqs =
      let old_res_count = Array.length old_instr.res in
      if old_res_count <> Array.length new_instr.res
      then (
        Format.fprintf ppf_m
          "Result register count mismatch at old=%a(id:%a) new=%a(id:%a): old \
           has %d res, new has %d res@."
          Label.format old_label InstructionId.print old_instr.id Label.format
          new_label InstructionId.print new_instr.id old_res_count
          (Array.length new_instr.res);
        eqs)
      else
        process_reg_pairs eqs ~old_regs:old_instr.res ~new_regs:new_instr.res
          ~old_instr ~new_instr (fun eqs ~old_reg ~new_reg ->
            Equations.remove_pair eqs ~old_reg ~new_reg)
    in
    (* Check for residual output equations *)
    let check_side res ~find =
      Array.iter
        (fun reg ->
          match find eqs reg with
          | None -> ()
          | Some partners ->
            Reg.Set.iter
              (fun partner ->
                Format.fprintf ppf_m
                  "Residual output equation at old=%a(id:%a) new=%a(id:%a): %a \
                   still paired with %a@."
                  Label.format old_label InstructionId.print old_instr.id
                  Label.format new_label InstructionId.print new_instr.id
                  Printreg.reg reg Printreg.reg partner)
              partners)
        res
    in
    check_side old_instr.res ~find:Equations.find_partners_of_old;
    check_side new_instr.res ~find:Equations.find_partners_of_new;
    (* Remove all remaining output equations *)
    let eqs = Array.fold_left Equations.remove_old eqs old_instr.res in
    let eqs = Array.fold_left Equations.remove_new eqs new_instr.res in
    (* At GC points (calls, allocations, polls), registers live across the
       instruction are GC roots, so a [Val]/[Int] machtype discrepancy between
       equated registers changes GC behavior. Elsewhere, machtypes may
       legitimately differ. With outputs removed and inputs not yet added, the
       current equations are exactly the pairs live across this instruction. *)
    if is_gc_point
    then
      Equations.iter_old_to_new eqs ~f:(fun old_reg new_reg ->
          if not (Cmm.equal_machtype_component old_reg.Reg.typ new_reg.Reg.typ)
          then
            Format.fprintf ppf_m
              "Register machtype mismatch at GC point old=%a(id:%a) \
               new=%a(id:%a): %a[%a] vs %a[%a]@."
              Label.format old_label InstructionId.print old_instr.id
              Label.format new_label InstructionId.print new_instr.id
              Printreg.reg old_reg Printcmm.machtype_component old_reg.Reg.typ
              Printreg.reg new_reg Printcmm.machtype_component new_reg.Reg.typ);
    (* Add input equations *)
    if Array.length old_arg <> Array.length new_arg
    then (
      Format.fprintf ppf_m
        "Arg length mismatch at old=%a(id:%a) new=%a(id:%a): %d vs %d@."
        Label.format old_label InstructionId.print old_instr.id Label.format
        new_label InstructionId.print new_instr.id (Array.length old_arg)
        (Array.length new_arg);
      eqs)
    else
      process_reg_pairs eqs ~old_regs:old_arg ~new_regs:new_arg ~old_instr
        ~new_instr (fun eqs ~old_reg ~new_reg ->
          Equations.add_pair eqs ~old_reg ~new_reg)
  in
  (* Process terminator *)
  let old_terminator = old_block.terminator in
  let new_terminator = new_block.terminator in
  let terminator_is_gc_point =
    match old_terminator.desc with
    | Call _ | Call_no_return _ | Prim _ | Invalid _ -> true
    | Never | Always _ | Parity_test _ | Truth_test _ | Int_test _
    | Float_test _ | Switch _ | Return | Raise _ | Tailcall_self _
    | Tailcall_func _ ->
      false
  in
  let eqs =
    process_instruction eqs ~old_instr:old_terminator ~new_instr:new_terminator
      ~old_arg:old_terminator.arg ~new_arg:new_terminator.arg
      ~is_gc_point:terminator_is_gc_point
  in
  (* Process body backwards (iterating the cells back to front), skipping
     moves *)
  let rec loop eqs old_cell new_cell =
    match old_cell, new_cell with
    | None, None -> eqs
    | Some cell, _ when is_move (DLL.value cell) ->
      let (old_instr : Cfg.basic Cfg.instruction) = DLL.value cell in
      loop
        (Equations.subst_old_move eqs ~src:old_instr.arg.(0)
           ~dst:old_instr.res.(0))
        (DLL.prev cell) new_cell
    | _, Some cell when is_move (DLL.value cell) ->
      let (new_instr : Cfg.basic Cfg.instruction) = DLL.value cell in
      loop
        (Equations.subst_new_move eqs ~src:new_instr.arg.(0)
           ~dst:new_instr.res.(0))
        old_cell (DLL.prev cell)
    | Some old_cell, Some new_cell ->
      let old_instr = DLL.value old_cell in
      let new_instr = DLL.value new_cell in
      let eqs =
        process_instruction eqs ~old_instr ~new_instr
          ~old_arg:(effective_arg old_instr) ~new_arg:(effective_arg new_instr)
          ~is_gc_point:(Cfg.is_alloc old_instr || Cfg.is_poll old_instr)
      in
      loop eqs (DLL.prev old_cell) (DLL.prev new_cell)
    | Some _, None | None, Some _ ->
      Format.fprintf ppf_m "Body length mismatch at old=%a new=%a@."
        Label.format old_label Label.format new_label;
      eqs
  in
  loop eqs (DLL.last_cell old_block.body) (DLL.last_cell new_block.body)

let verify_register_equivalence ~ppf_m ~old_cfg ~new_cfg ~new_to_old =
  (* block_eqs and worklist are keyed by new labels. new_to_old is used to find
     the corresponding old block. Predecessors come from the new CFG's
     block.predecessors. *)
  let block_eqs = Label.Tbl.create 64 in
  let worklist = Queue.create () in
  Label.Tbl.iter
    (fun new_label _old_label ->
      Label.Tbl.replace block_eqs new_label Equations.empty;
      Queue.add new_label worklist)
    new_to_old;
  while not (Queue.is_empty worklist) do
    let new_label = Queue.pop worklist in
    let old_label = Label.Tbl.find new_to_old new_label in
    match Cfg.get_block old_cfg old_label, Cfg.get_block new_cfg new_label with
    | None, _ | _, None ->
      (* Already reported as "Missing block" during the matching walk; don't die
         here, so that the buffered report still reaches the user. *)
      ()
    | Some old_block, Some new_block ->
      let eqs = Label.Tbl.find block_eqs new_label in
      let start_eqs = process_block_backward ~ppf_m eqs ~old_block ~new_block in
      new_block.predecessors
      |> Label.Set.iter (fun new_pred_label ->
          match Label.Tbl.find_opt block_eqs new_pred_label with
          | None -> ()
          | Some prev ->
            let merged = Equations.union prev start_eqs in
            if not (Equations.equal prev merged)
            then (
              Label.Tbl.replace block_eqs new_pred_label merged;
              Queue.add new_pred_label worklist));
      (* Check entry equations are empty *)
      if
        Label.Set.is_empty new_block.predecessors
        && not (Equations.equal start_eqs Equations.empty)
      then (
        Format.fprintf ppf_m "Undischarged equations at entry:@.";
        Equations.iter_old_to_new start_eqs ~f:(fun old_reg new_reg ->
            Format.fprintf ppf_m "  %a -> %a@." Printreg.reg old_reg
              Printreg.reg new_reg))
  done

(* === Entry point === *)

let compare ~old_cfg ~new_cfg ppf =
  let mismatches = Buffer.create 256 in
  let ppf_m = Format.formatter_of_buffer mismatches in
  begin
    (* We did run [Cfg_simplify] already in both pipelines, but it does not
       always reach a fixed-point. *)
    let old_cfg = Cfg_with_layout.cfg (Cfg_simplify.run old_cfg) in
    let new_cfg = Cfg_with_layout.cfg (Cfg_simplify.run new_cfg) in
    (* Phase 1: structural matching *)
    let new_to_old = collect_matching_blocks ~ppf_m ~old_cfg ~new_cfg in
    (* Phase 2: register equivalence *)
    verify_register_equivalence ~ppf_m ~old_cfg ~new_cfg ~new_to_old;
    (* Check CFG metadata. The stored [fun_contains_calls] flags are computed
       before the final [Cfg_simplify] in both pipelines, so they can
       legitimately differ for blocks that were subsequently removed. Instead of
       exact equality, check that the new flag is correct (set whenever the new
       CFG does contain calls) and at least as precise as the old one. *)
    if
      (not new_cfg.fun_contains_calls)
      && Label.Tbl.to_seq_values new_cfg.blocks
         |> Seq.exists Cfg.basic_block_contains_calls
    then
      Format.fprintf ppf_m
        "fun_contains_calls is unset on the new CFG even though it contains \
         calls@.";
    if new_cfg.fun_contains_calls && not old_cfg.fun_contains_calls
    then
      Format.fprintf ppf_m
        "fun_contains_calls mismatch: old=%b new=%b (new is less precise)@."
        old_cfg.fun_contains_calls new_cfg.fun_contains_calls;
    if not (String.equal old_cfg.fun_name new_cfg.fun_name)
    then
      Format.fprintf ppf_m "fun_name mismatch: old=%s new=%s@." old_cfg.fun_name
        new_cfg.fun_name;
    if Stdlib.( <> ) old_cfg.fun_codegen_options new_cfg.fun_codegen_options
    then Format.fprintf ppf_m "fun_codegen_options mismatch@.";
    if Debuginfo.compare old_cfg.fun_dbg new_cfg.fun_dbg <> 0
    then
      Format.fprintf ppf_m "fun_dbg mismatch: old=%a new=%a@."
        Debuginfo.print_compact old_cfg.fun_dbg Debuginfo.print_compact
        new_cfg.fun_dbg;
    if Stdlib.( <> ) old_cfg.fun_poll new_cfg.fun_poll
    then Format.fprintf ppf_m "fun_poll mismatch@.";
    if Stdlib.( <> ) old_cfg.fun_ret_type new_cfg.fun_ret_type
    then Format.fprintf ppf_m "fun_ret_type mismatch@."
  end;
  (* Report *)
  Format.pp_print_flush ppf_m ();
  let msg = Buffer.contents mismatches in
  if String.length msg > 0
  then (
    let fun_name = (Cfg_with_layout.cfg old_cfg).fun_name in
    Format.fprintf ppf "*** CFG comparison MISMATCH for %s:@.%s@." fun_name msg;
    Format.fprintf ppf "*** Old CFG:@.%a@." Printcfg.cfg_with_layout old_cfg;
    Format.fprintf ppf "*** New CFG (from SSA):@.%a@." Printcfg.cfg_with_layout
      new_cfg;
    Format.pp_print_flush ppf ();
    Format.print_flush ();
    Misc.fatal_errorf "CFG comparison MISMATCH for %s" fun_name)
