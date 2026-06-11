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
      (modulo skipping moves and [Pushtrap]/[Poptrap] with unreachable
      handlers), that their terminators have the same shape, and that
      block-level metadata (is_trap_handler, can_raise, cold) agrees.
      [Pushtrap]/[Poptrap] with unreachable trap handlers are skipped because
      the handlers are unreachable anyway and the trap stack is not being
      observed in this case. This is necessary because the SSA pipeline eagerly
      removes unreachable blocks including trap handlers.

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
      matching results of two matched instructions. *)

module DLL = Oxcaml_utils.Doubly_linked_list

(* A set of (old_reg, new_reg) pairs asserting that these registers hold equal
   values at a given program point. Represented as two [Reg.Set.t Reg.Map.t]
   maps for bidirectional lookup. Not necessarily injective. Since we are doing
   a backwards analysis, these are proof obligations rather than assumptions. *)
module Equations : sig
  type t

  val empty : t

  val equal : t -> t -> bool

  val add_pair : t -> old_r:Reg.t -> new_r:Reg.t -> t

  val remove_old : t -> Reg.t -> t

  val remove_new : t -> Reg.t -> t

  val remove_pair : t -> old_r:Reg.t -> new_r:Reg.t -> t

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

  let add_pair { fwd; bwd } ~old_r ~new_r =
    { fwd = add_to_map old_r new_r fwd; bwd = add_to_map new_r old_r bwd }

  let remove_from_map key value map =
    Reg.Map.update key
      (function
        | None -> None
        | Some s ->
          let s = Reg.Set.remove value s in
          if Reg.Set.is_empty s then None else Some s)
      map

  (* Remove all equations involving [old_r] on the old/left side. *)
  let remove_old eqs old_r =
    match Reg.Map.find_opt old_r eqs.fwd with
    | None -> eqs
    | Some partners ->
      { fwd = Reg.Map.remove old_r eqs.fwd;
        bwd =
          Reg.Set.fold
            (fun partner -> remove_from_map partner old_r)
            partners eqs.bwd
      }

  let remove_new eqs r = mirror (remove_old (mirror eqs) r)

  let remove_pair { fwd; bwd } ~old_r ~new_r =
    { fwd = remove_from_map old_r new_r fwd;
      bwd = remove_from_map new_r old_r bwd
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
        (fun nr eqs -> add_pair eqs ~old_r:src ~new_r:nr)
        partners eqs

  let subst_new_move eqs ~src ~dst =
    mirror (subst_old_move (mirror eqs) ~src ~dst)

  let union a b =
    let merge _ s1 s2 = Some (Reg.Set.union s1 s2) in
    { fwd = Reg.Map.union merge a.fwd b.fwd;
      bwd = Reg.Map.union merge a.bwd b.bwd
    }

  let find_partners_of_old eqs r = Reg.Map.find_opt r eqs.fwd

  let find_partners_of_new eqs r = Reg.Map.find_opt r eqs.bwd

  let iter_old_to_new eqs ~f =
    Reg.Map.iter
      (fun old_r new_rs -> Reg.Set.iter (fun new_r -> f old_r new_r) new_rs)
      eqs.fwd
end

(* === Helpers === *)

let is_move (i : Cfg.basic Cfg.instruction) =
  match i.desc with
  (* Opaque should not be treated as a move, really. However, there is a bug in
     the current Cfg_selectgen, where let a = opaque x in let b = opaque x in
     ... is translated as let a = opaque x in let b = opaque a in ... *)
  (* CR ttebbi: Fix Cfg_selectgen and remove Opaque here. *)
  | Op (Move | Opaque) ->
    Array.length i.arg = 1
    && Array.length i.res = 1
    && Reg.equal_location i.arg.(0).Reg.loc Reg.Unknown
    && Reg.equal_location i.res.(0).Reg.loc Reg.Unknown
  | _ -> false

(** A [Pushtrap]/[Poptrap] whose target has no predecessors is unreachable as a
    trap handler at runtime. [Cfg_selectgen] keeps an [unreachable_handler]
    block as the target while [Cfg_of_ssa] drops the trap push/pop entirely; we
    treat both options as equivalent by skipping the op on either side when the
    handler has no predecessors. *)
let is_skippable_trap cfg (i : Cfg.basic Cfg.instruction) =
  match i.desc with
  | Pushtrap { lbl_handler } | Poptrap { lbl_handler } ->
    let target = Cfg.get_block_exn cfg lbl_handler in
    Label.Set.is_empty target.predecessors
  | _ -> false

(* === Phase 1: structural matching === *)

let basic_desc_match ~map_label (old_d : Cfg.basic) (new_d : Cfg.basic) =
  match old_d, new_d with
  | Pushtrap { lbl_handler = ol }, Pushtrap { lbl_handler = nl }
  | Poptrap { lbl_handler = ol }, Poptrap { lbl_handler = nl } ->
    map_label ol nl;
    true
  | _ -> Cfg.equal_basic old_d new_d

(** Compare the non-register, non-desc fields of a [Cfg.instruction] pair.
    [desc] is compared by the caller (its type and equality depend on whether it
    is a basic or terminator instruction); [arg]/[res] are compared in phase 2
    ([process_block_backward]); [id] is allowed to differ; [stack_offset] can
    legitimately differ because the SSA pipeline removes [Pushtrap]/[Poptrap]
    instructions when the handler is unreachable. *)
let compare_instruction_fields ~ppf_m ~kind ~ol ~nl (oi : _ Cfg.instruction)
    (ni : _ Cfg.instruction) =
  let[@warning "+9"] { Cfg.desc = _;
                       id = oi_id;
                       arg = _;
                       res = _;
                       dbg = oi_dbg;
                       fdo = oi_fdo;
                       live = oi_live;
                       stack_offset = _;
                       available_before = oi_avail_before;
                       available_across = oi_avail_across
                     } =
    oi
  in
  let[@warning "+9"] { Cfg.desc = _;
                       id = ni_id;
                       arg = _;
                       res = _;
                       dbg = ni_dbg;
                       fdo = ni_fdo;
                       live = ni_live;
                       stack_offset = _;
                       available_before = ni_avail_before;
                       available_across = ni_avail_across
                     } =
    ni
  in
  let report_with field pp_value oi_v ni_v =
    Format.fprintf ppf_m
      "%s %s mismatch at old=%a(id:%a) new=%a(id:%a): %a vs %a@." kind field
      Label.format ol InstructionId.print oi_id Label.format nl
      InstructionId.print ni_id pp_value oi_v pp_value ni_v
  in
  let report field =
    Format.fprintf ppf_m "%s %s mismatch at old=%a(id:%a) new=%a(id:%a)@." kind
      field Label.format ol InstructionId.print oi_id Label.format nl
      InstructionId.print ni_id
  in
  if Debuginfo.compare oi_dbg ni_dbg <> 0
  then report_with "dbg" Debuginfo.print_compact oi_dbg ni_dbg;
  if not (Fdo_info.equal oi_fdo ni_fdo) then report "fdo";
  if not (Reg.Set.equal oi_live ni_live)
  then report_with "live" Printreg.regset oi_live ni_live;
  let pp_avail = Reg_availability_set.print ~print_reg:Printreg.reg in
  if not (Reg_availability_set.equal oi_avail_before ni_avail_before)
  then report_with "available_before" pp_avail oi_avail_before ni_avail_before;
  if not (Reg_availability_set.equal oi_avail_across ni_avail_across)
  then report_with "available_across" pp_avail oi_avail_across ni_avail_across

let compare_body ~ppf_m ~map_label ~old_cfg ~new_cfg ~ol ~nl old_body new_body =
  let rec advance cfg cell =
    match cell with
    | Some c when is_move (DLL.value c) || is_skippable_trap cfg (DLL.value c)
      ->
      advance cfg (DLL.next c)
    | _ -> cell
  in
  let rec loop oc nc =
    match advance old_cfg oc, advance new_cfg nc with
    | None, None -> ()
    | Some _, None | None, Some _ ->
      Format.fprintf ppf_m "Body length mismatch at old=%a new=%a@."
        Label.format ol Label.format nl
    | Some oc, Some nc ->
      let (oi : Cfg.basic Cfg.instruction) = DLL.value oc in
      let (ni : Cfg.basic Cfg.instruction) = DLL.value nc in
      if not (basic_desc_match ~map_label oi.desc ni.desc)
      then
        Format.fprintf ppf_m
          "Instruction mismatch at old=%a(id:%a) new=%a(id:%a): %a vs %a@."
          Label.format ol InstructionId.print oi.id Label.format nl
          InstructionId.print ni.id Cfg.dump_basic oi.desc Cfg.dump_basic
          ni.desc
      else compare_instruction_fields ~ppf_m ~kind:"Instruction" ~ol ~nl oi ni;
      loop (DLL.next oc) (DLL.next nc)
  in
  loop (DLL.hd_cell old_body) (DLL.hd_cell new_body)

let terminator_structure_match ~map_label (old_t : Cfg.terminator)
    (new_t : Cfg.terminator) =
  match old_t, new_t with
  | Never, Never -> true
  | Always ol, Always nl ->
    map_label ol nl;
    true
  | ( Parity_test { ifso = ot_ifso; ifnot = ot_ifnot },
      Parity_test { ifso = nt_ifso; ifnot = nt_ifnot } )
  | ( Truth_test { ifso = ot_ifso; ifnot = ot_ifnot },
      Truth_test { ifso = nt_ifso; ifnot = nt_ifnot } ) ->
    map_label ot_ifso nt_ifso;
    map_label ot_ifnot nt_ifnot;
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
        { lt = ot_lt;
          eq = ot_eq;
          gt = ot_gt;
          is_signed = ot_is_signed;
          imm = ot_imm
        },
      Int_test
        { lt = nt_lt;
          eq = nt_eq;
          gt = nt_gt;
          is_signed = nt_is_signed;
          imm = nt_imm
        } ) ->
    map_label ot_lt nt_lt;
    map_label ot_eq nt_eq;
    map_label ot_gt nt_gt;
    Scalar.Signedness.equal ot_is_signed nt_is_signed
    && Option.equal Int.equal ot_imm nt_imm
  | ( Float_test
        { width = ot_width; lt = ot_lt; eq = ot_eq; gt = ot_gt; uo = ot_uo },
      Float_test
        { width = nt_width; lt = nt_lt; eq = nt_eq; gt = nt_gt; uo = nt_uo } )
    ->
    map_label ot_lt nt_lt;
    map_label ot_eq nt_eq;
    map_label ot_gt nt_gt;
    map_label ot_uo nt_uo;
    Cmm.equal_float_width ot_width nt_width
  | Switch ols, Switch nls ->
    if Array.length ols = Array.length nls
    then (
      Array.iter2 map_label ols nls;
      true)
    else false
  | Return, Return -> true
  | Raise ok, Raise nk -> Lambda.equal_raise_kind ok nk
  | Tailcall_self { destination = od }, Tailcall_self { destination = nd } ->
    map_label od nd;
    true
  | Tailcall_func oo, Tailcall_func no -> Cfg.equal_func_call_operation oo no
  | Call_no_return oo, Call_no_return no ->
    Cfg.equal_external_call_operation oo no
  | ( Call { op = ot_op; label_after = ot_la },
      Call { op = nt_op; label_after = nt_la } ) ->
    map_label ot_la nt_la;
    Cfg.equal_func_call_operation ot_op nt_op
  | ( Prim { op = ot_op; label_after = ot_la },
      Prim { op = nt_op; label_after = nt_la } ) ->
    map_label ot_la nt_la;
    Cfg.equal_prim_call_operation ot_op nt_op
  | ( Invalid
        { message = ot_msg;
          stack_ofs = ot_so;
          stack_align = ot_sa;
          label_after = ot_la
        },
      Invalid
        { message = nt_msg;
          stack_ofs = nt_so;
          stack_align = nt_sa;
          label_after = nt_la
        } ) ->
    let labels_match =
      match ot_la, nt_la with
      | None, None -> true
      | Some ol, Some nl ->
        map_label ol nl;
        true
      | Some _, None | None, Some _ -> false
    in
    labels_match && String.equal ot_msg nt_msg && Int.equal ot_so nt_so
    && Cmm.equal_stack_align ot_sa nt_sa
  | ( ( Never | Always _ | Parity_test _ | Truth_test _ | Int_test _
      | Float_test _ | Switch _ | Return | Raise _ | Tailcall_self _
      | Tailcall_func _ | Call_no_return _ | Call _ | Prim _ | Invalid _ ),
      _ ) ->
    false

let collect_matching_blocks ~ppf_m ~old_cfg ~new_cfg =
  (* new_to_old: the primary label mapping (new_label -> old_label) *)
  let new_to_old = Label.Tbl.create 64 in
  (* old_to_new: inverse mapping, used only to assert bijectivity *)
  let old_to_new = Label.Tbl.create 64 in
  let queue = Queue.create () in
  let map_label ol nl =
    (match Label.Tbl.find_opt new_to_old nl with
    | Some mapped ->
      if not (Label.equal mapped ol)
      then
        Format.fprintf ppf_m
          "Label mapping conflict: new=%a already maps to old=%a, not old=%a@."
          Label.format nl Label.format mapped Label.format ol
    | None -> Label.Tbl.replace new_to_old nl ol);
    (match Label.Tbl.find_opt old_to_new ol with
    | Some mapped ->
      if not (Label.equal mapped nl)
      then
        Format.fprintf ppf_m
          "Label mapping conflict: old=%a already maps to new=%a, not new=%a@."
          Label.format ol Label.format mapped Label.format nl
    | None -> Label.Tbl.replace old_to_new ol nl);
    Queue.add (ol, nl) queue
  in
  map_label (Cfg.entry_label old_cfg) (Cfg.entry_label new_cfg);
  let visited = ref Label.Set.empty in
  while not (Queue.is_empty queue) do
    let ol, nl = Queue.pop queue in
    if not (Label.Set.mem ol !visited)
    then begin
      visited := Label.Set.add ol !visited;
      let ob = Cfg.get_block old_cfg ol in
      let nb = Cfg.get_block new_cfg nl in
      match ob, nb with
      | None, _ | _, None ->
        Format.fprintf ppf_m "Missing block: old=%a(%s) new=%a(%s)@."
          Label.format ol
          (if Option.is_none ob then "missing" else "ok")
          Label.format nl
          (if Option.is_none nb then "missing" else "ok")
      | Some ob, Some nb ->
        if not (Bool.equal ob.is_trap_handler nb.is_trap_handler)
        then
          Format.fprintf ppf_m "Trap handler mismatch at old=%a new=%a@."
            Label.format ol Label.format nl;
        if not (Bool.equal ob.can_raise nb.can_raise)
        then
          Format.fprintf ppf_m "can_raise mismatch at old=%a(%b) new=%a(%b)@."
            Label.format ol ob.can_raise Label.format nl nb.can_raise;
        if not (Bool.equal ob.cold nb.cold)
        then
          Format.fprintf ppf_m "cold mismatch at old=%a(%b) new=%a(%b)@."
            Label.format ol ob.cold Label.format nl nb.cold;
        (* [stack_offset] is intentionally not compared: the legacy pipeline
           keeps unreachable trap handlers as [Pushtrap]/[Poptrap] pairs while
           [Cfg_of_ssa] drops them, so the trap-stack-derived component of
           [stack_offset] can legitimately differ. *)
        (match ob.exn, nb.exn with
        | Some oe, Some ne -> map_label oe ne
        | None, None -> ()
        | _ ->
          Format.fprintf ppf_m "Exn presence mismatch at old=%a new=%a@."
            Label.format ol Label.format nl);
        (* Compare body structure, debuginfo, and map labels *)
        compare_body ~ppf_m ~map_label ~old_cfg ~new_cfg ~ol ~nl ob.body nb.body;
        (* Compare terminator structure and map labels *)
        if
          not
            (terminator_structure_match ~map_label ob.terminator.desc
               nb.terminator.desc)
        then
          Format.fprintf ppf_m
            "Terminator mismatch at old=%a(id:%a) new=%a(id:%a): %a vs %a@."
            Label.format ol InstructionId.print ob.terminator.id Label.format nl
            InstructionId.print nb.terminator.id
            (Cfg.dump_terminator ~sep:"")
            ob.terminator.desc
            (Cfg.dump_terminator ~sep:"")
            nb.terminator.desc
        else
          compare_instruction_fields ~ppf_m ~kind:"Terminator" ~ol ~nl
            ob.terminator nb.terminator
    end
  done;
  (* Validate predecessors match under the label mapping *)
  new_to_old
  |> Label.Tbl.iter (fun nl ol ->
      let ob = Cfg.get_block_exn old_cfg ol in
      let nb = Cfg.get_block_exn new_cfg nl in
      (* We ignore unvisited predecessors, which could be eliminated handler
         blocks and their successors.*)
      let mapped_old_preds =
        Label.Set.filter_map
          (fun op -> Label.Tbl.find_opt old_to_new op)
          ob.predecessors
      in
      let visited_new_preds =
        Label.Set.filter (Label.Tbl.mem new_to_old) nb.predecessors
      in
      if not (Label.Set.equal mapped_old_preds visited_new_preds)
      then
        Format.fprintf ppf_m "Predecessor mismatch at old=%a new=%a@."
          Label.format ol Label.format nl);
  new_to_old

(* === Phase 2: register equivalence ===

   Backward fixpoint over matched block pairs. Processes each block in reverse
   to propagate register equations. Only handles register tracking — all
   structural checks (desc, debuginfo) are done in phase 1. *)

let effective_arg (i : Cfg.basic Cfg.instruction) =
  match i.desc with
  | Op (Name_for_debugger { regs; _ }) ->
    assert (Array.length i.arg = 0);
    regs
  | _ -> i.arg

let process_block_backward ~ppf_m ~old_cfg ~new_cfg eqs
    ~(old_block : Cfg.basic_block) ~(new_block : Cfg.basic_block) =
  let ol = old_block.start in
  let nl = new_block.start in
  let process_reg_pairs eqs ~old_regs ~new_regs ~(old_instr : _ Cfg.instruction)
      ~(new_instr : _ Cfg.instruction) f =
    let eqs = ref eqs in
    for i = 0 to Array.length old_regs - 1 do
      let old_r = old_regs.(i) in
      let new_r = new_regs.(i) in
      if
        Reg.equal_location old_r.Reg.loc Reg.Unknown
        && Reg.equal_location new_r.Reg.loc Reg.Unknown
      then eqs := f !eqs ~old_r ~new_r
      else if not (Reg.same_loc old_r new_r)
      then
        Format.fprintf ppf_m
          "Physical reg mismatch at old=%a(id:%a) new=%a(id:%a): %a vs %a@."
          Label.format ol InstructionId.print old_instr.id Label.format nl
          InstructionId.print new_instr.id Printreg.reg old_r Printreg.reg new_r
    done;
    !eqs
  in
  (* Process a matched instruction pair: remove output equations, then add input
     equations. *)
  let process_instruction eqs ~(old_instr : _ Cfg.instruction)
      ~(new_instr : _ Cfg.instruction) ~old_arg ~new_arg =
    (* Check physical result registers match, then remove matched output
       pairs *)
    let eqs =
      let n = Array.length old_instr.res in
      if n <> Array.length new_instr.res
      then (
        Format.fprintf ppf_m
          "Result register count mismatch at old=%a(id:%a) new=%a(id:%a): old \
           has %d res, new has %d res@."
          Label.format ol InstructionId.print old_instr.id Label.format nl
          InstructionId.print new_instr.id n
          (Array.length new_instr.res);
        eqs)
      else
        process_reg_pairs eqs ~old_regs:old_instr.res ~new_regs:new_instr.res
          ~old_instr ~new_instr (fun eqs ~old_r ~new_r ->
            Equations.remove_pair eqs ~old_r ~new_r)
    in
    (* Check for residual output equations *)
    let check_side res ~find =
      Array.iter
        (fun r ->
          match find eqs r with
          | None -> ()
          | Some partners ->
            Reg.Set.iter
              (fun r' ->
                Format.fprintf ppf_m
                  "Residual output equation at old=%a(id:%a) new=%a(id:%a): %a \
                   still paired with %a@."
                  Label.format ol InstructionId.print old_instr.id Label.format
                  nl InstructionId.print new_instr.id Printreg.reg r
                  Printreg.reg r')
              partners)
        res
    in
    check_side old_instr.res ~find:Equations.find_partners_of_old;
    check_side new_instr.res ~find:Equations.find_partners_of_new;
    (* Remove all remaining output equations *)
    let eqs = Array.fold_left Equations.remove_old eqs old_instr.res in
    let eqs = Array.fold_left Equations.remove_new eqs new_instr.res in
    (* Add input equations *)
    if Array.length old_arg <> Array.length new_arg
    then (
      Format.fprintf ppf_m
        "Arg length mismatch at old=%a(id:%a) new=%a(id:%a): %d vs %d@."
        Label.format ol InstructionId.print old_instr.id Label.format nl
        InstructionId.print new_instr.id (Array.length old_arg)
        (Array.length new_arg);
      eqs)
    else
      process_reg_pairs eqs ~old_regs:old_arg ~new_regs:new_arg ~old_instr
        ~new_instr (fun eqs ~old_r ~new_r ->
          Equations.add_pair eqs ~old_r ~new_r)
  in
  (* Process terminator *)
  let old_t = old_block.terminator in
  let new_t = new_block.terminator in
  let eqs =
    process_instruction eqs ~old_instr:old_t ~new_instr:new_t ~old_arg:old_t.arg
      ~new_arg:new_t.arg
  in
  (* Process body backwards, skipping moves and unreachable trap handlers *)
  let old_instrs = List.rev (DLL.to_list old_block.body) in
  let new_instrs = List.rev (DLL.to_list new_block.body) in
  let rec loop eqs ~old_instrs ~new_instrs =
    match old_instrs, new_instrs with
    | [], [] -> eqs
    | (old_instr : _ Cfg.instruction) :: rest, _ when is_move old_instr ->
      loop
        (Equations.subst_old_move eqs ~src:old_instr.arg.(0)
           ~dst:old_instr.res.(0))
        ~old_instrs:rest ~new_instrs
    | _, (new_instr : _ Cfg.instruction) :: rest when is_move new_instr ->
      loop
        (Equations.subst_new_move eqs ~src:new_instr.arg.(0)
           ~dst:new_instr.res.(0))
        ~old_instrs ~new_instrs:rest
    | old_instr :: rest, _ when is_skippable_trap old_cfg old_instr ->
      loop eqs ~old_instrs:rest ~new_instrs
    | _, new_instr :: rest when is_skippable_trap new_cfg new_instr ->
      loop eqs ~old_instrs ~new_instrs:rest
    | old_instr :: old_rest, new_instr :: new_rest ->
      let eqs =
        process_instruction eqs ~old_instr ~new_instr
          ~old_arg:(effective_arg old_instr) ~new_arg:(effective_arg new_instr)
      in
      loop eqs ~old_instrs:old_rest ~new_instrs:new_rest
    | _ :: _, [] | [], _ :: _ ->
      Format.fprintf ppf_m
        "Body length mismatch at old=%a new=%a: old_remaining=%d \
         new_remaining=%d@."
        Label.format ol Label.format nl (List.length old_instrs)
        (List.length new_instrs);
      eqs
  in
  loop eqs ~old_instrs ~new_instrs

let verify_register_equivalence ~ppf_m ~old_cfg ~new_cfg ~new_to_old =
  (* block_eqs and worklist are keyed by new labels. new_to_old is used to find
     the corresponding old block. Predecessors come from the new CFG's
     block.predecessors. *)
  let block_eqs = Label.Tbl.create 64 in
  let worklist = Queue.create () in
  Label.Tbl.iter
    (fun nl _ol ->
      Label.Tbl.replace block_eqs nl Equations.empty;
      Queue.add nl worklist)
    new_to_old;
  while not (Queue.is_empty worklist) do
    let nl = Queue.pop worklist in
    let ol = Label.Tbl.find new_to_old nl in
    let ob = Cfg.get_block_exn old_cfg ol in
    let nb = Cfg.get_block_exn new_cfg nl in
    let eqs = Label.Tbl.find block_eqs nl in
    let start_eqs =
      process_block_backward ~ppf_m ~old_cfg ~new_cfg eqs ~old_block:ob
        ~new_block:nb
    in
    Label.Set.iter
      (fun pred_nl ->
        match Label.Tbl.find_opt block_eqs pred_nl with
        | None -> ()
        | Some prev ->
          let merged = Equations.union prev start_eqs in
          if not (Equations.equal prev merged)
          then (
            Label.Tbl.replace block_eqs pred_nl merged;
            Queue.add pred_nl worklist))
      (Label.Set.filter (Label.Tbl.mem new_to_old) nb.predecessors)
  done;
  (* Check entry equations are empty *)
  let new_entry = Cfg.entry_label new_cfg in
  match Label.Tbl.find_opt block_eqs new_entry with
  | None -> ()
  | Some entry_eqs ->
    if not (Equations.equal entry_eqs Equations.empty)
    then (
      Format.fprintf ppf_m "Undischarged equations at entry:@.";
      Equations.iter_old_to_new entry_eqs ~f:(fun old_r new_r ->
          Format.fprintf ppf_m "  %a -> %a@." Printreg.reg old_r Printreg.reg
            new_r))

(* === Entry point === *)

let compare ~fun_name ~old_cfg ~new_cfg ppf =
  let mismatches = Buffer.create 256 in
  let ppf_m = Format.formatter_of_buffer mismatches in
  begin
    (* We did run [Cfg_simplify] already in both pipelines, but it does not
       always reach a fixed-point and the SSA pipeline differs in more
       aggressive removal of unreachable blocks. *)
    let old_cfg = Cfg_with_layout.cfg (Cfg_simplify.run old_cfg) in
    let new_cfg = Cfg_with_layout.cfg (Cfg_simplify.run new_cfg) in
    (* Phase 1: structural matching *)
    let new_to_old = collect_matching_blocks ~ppf_m ~old_cfg ~new_cfg in
    (* Phase 2: register equivalence *)
    verify_register_equivalence ~ppf_m ~old_cfg ~new_cfg ~new_to_old;
    (* Check CFG metadata. [fun_contains_calls] is intentionally not compared:
       dropping unreachable trap-handler blocks (which contain a [Raise]) can
       legitimately change whether the function appears to contain calls. *)
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
    Format.fprintf ppf "*** CFG comparison MISMATCH for %s:@.%s@." fun_name msg;
    Format.fprintf ppf "*** Old CFG:@.%a@." Printcfg.cfg_with_layout old_cfg;
    Format.fprintf ppf "*** New CFG (from SSA):@.%a@." Printcfg.cfg_with_layout
      new_cfg;
    Format.pp_print_flush ppf ();
    Format.print_flush ();
    Misc.fatal_errorf "CFG comparison MISMATCH for %s" fun_name)
