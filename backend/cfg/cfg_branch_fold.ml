[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module C = Cfg
module DLL = Oxcaml_utils.Doubly_linked_list

type fold_result =
  | Goto of Label.t
  | Replace of
      { desc : C.terminator;
        arg : Reg.t array
      }

let apply_fold (block : C.basic_block) (result : fold_result) =
  match result with
  | Goto target ->
    block.terminator
      <- { block.terminator with desc = Always target; arg = [||]; res = [||] }
  | Replace { desc; arg } ->
    block.terminator <- { block.terminator with desc; arg; res = [||] }

let predecessor_if_unique (block : C.basic_block) ~cfg =
  if Label.Set.cardinal block.predecessors = 1
  then Some (Cfg.get_block_exn cfg (Label.Set.choose block.predecessors))
  else None

(** [find_reaching_def reg blocks] searches for the most recent non-move
    definition of [reg], walking backwards from the end of the given basic
    block, right before the termintor. It can walk multiple blocks as long as
    there is a single predecessor.

    [Move] instructions defining the current target are chased through: when we
    encounter [target = src], the search continues for [src]. A register is
    considered clobbered if it has been written to between the candidate
    definition and the use point. The function returns the defining instruction
    only when none of its arguments has been clobbered after it, so that the
    arguments can be still be used at the use point. *)
let find_reaching_def ~(reg : Reg.t) ~(block : C.basic_block) ~cfg :
    C.basic C.instruction option =
  let clobbered = ref Reg.Set.empty in
  let add_to_clobbered arr =
    clobbered := Array.fold_right Reg.Set.add arr !clobbered
  in
  let rec loop ~pos ~(next_block : C.basic_block option) ~reg :
      C.basic C.instruction option =
    match pos, next_block with
    | None, Some next_block
      when not (Array.exists (Reg.same reg) next_block.terminator.res) ->
      add_to_clobbered next_block.terminator.res;
      loop
        ~pos:(DLL.last_cell next_block.body)
        ~next_block:(predecessor_if_unique next_block ~cfg)
        ~reg
    | Some cell, _ ->
      let instr : C.basic C.instruction = DLL.value cell in
      add_to_clobbered instr.res;
      if Array.length instr.res >= 1 && Reg.same reg instr.res.(0)
      then
        begin match[@ocaml.warning "-4"] instr.desc with
        | Op Move
          when Array.length instr.arg = 1
               && Array.length instr.res = 1
               && not (Reg.is_preassigned instr.arg.(0)) ->
          assert (Reg.is_unknown instr.arg.(0));
          loop ~pos:(DLL.prev cell) ~next_block ~reg:instr.arg.(0)
        | _ ->
          let inputs_safe =
            Array.for_all (fun a -> not (Reg.Set.mem a !clobbered)) instr.arg
          in
          if inputs_safe then Some instr else None
        end
      else if Array.exists (Reg.same reg) instr.res
      then None
      else loop ~pos:(DLL.prev cell) ~next_block ~reg
    | _ -> None
  in
  if Reg.is_preassigned reg
  then None
  else
    loop ~pos:(DLL.last_cell block.body)
      ~next_block:(predecessor_if_unique block ~cfg)
      ~reg

let int_test_of_compare (cmp : Operation.integer_comparison) ~imm ~ifso ~ifnot :
    C.terminator =
  let module S = Scalar.Signedness in
  let lt, eq, gt, is_signed =
    match cmp with
    | Ceq -> ifnot, ifso, ifnot, S.Signed
    | Cne -> ifso, ifnot, ifso, S.Signed
    | Clt -> ifso, ifnot, ifnot, S.Signed
    | Cgt -> ifnot, ifnot, ifso, S.Signed
    | Cle -> ifso, ifso, ifnot, S.Signed
    | Cge -> ifnot, ifso, ifso, S.Signed
    | Cult -> ifso, ifnot, ifnot, S.Unsigned
    | Cugt -> ifnot, ifnot, ifso, S.Unsigned
    | Cule -> ifso, ifso, ifnot, S.Unsigned
    | Cuge -> ifnot, ifso, ifso, S.Unsigned
  in
  Int_test { lt; eq; gt; is_signed; imm }

let float_test_of_compare (cmp : Operation.float_comparison) ~width ~ifso ~ifnot
    : C.terminator =
  let lt, eq, gt, uo =
    match cmp with
    | CFeq -> ifnot, ifso, ifnot, ifnot
    | CFneq -> ifso, ifnot, ifso, ifso
    | CFlt -> ifso, ifnot, ifnot, ifnot
    | CFnlt -> ifnot, ifso, ifso, ifso
    | CFgt -> ifnot, ifnot, ifso, ifnot
    | CFngt -> ifso, ifso, ifnot, ifso
    | CFle -> ifso, ifso, ifnot, ifnot
    | CFnle -> ifnot, ifnot, ifso, ifso
    | CFge -> ifnot, ifso, ifso, ifnot
    | CFnge -> ifso, ifnot, ifnot, ifso
  in
  Float_test { width; lt; eq; gt; uo }

(** Fold [term] using reaching definitions visible at [pos].

    - For [Truth_test] whose argument was set by an [Icomp]/[Icompf] (with its
      inputs unclobbered), produces a [Replace] folding the comparison into the
      test.
    - For tests whose arguments are known constants, produces a [Goto] to the
      statically determined arm. *)
let evaluate_terminator ~block (term : C.terminator C.instruction) ~cfg :
    fold_result option =
  let constant_arg arg_idx =
    match[@ocaml.warning "-4"]
      find_reaching_def ~reg:term.arg.(arg_idx) ~block ~cfg
    with
    | Some { desc = Op (Const_int c); _ } -> Some c
    | Some _ | None -> None
  in
  match term.desc with
  | Truth_test { ifso; ifnot } -> (
    match[@ocaml.warning "-4"]
      find_reaching_def ~reg:term.arg.(0) ~block ~cfg
    with
    | Some { desc = Op (Const_int c); _ } ->
      if Nativeint.equal c 0n then Some (Goto ifnot) else Some (Goto ifso)
    | Some ({ desc = Op (Intop (Icomp cmp)); _ } as def) ->
      Some
        (Replace
           { desc = int_test_of_compare cmp ~imm:None ~ifso ~ifnot;
             arg = Array.copy def.arg
           })
    | Some ({ desc = Op (Intop_imm (Icomp cmp, imm)); _ } as def) ->
      Some
        (Replace
           { desc = int_test_of_compare cmp ~imm:(Some imm) ~ifso ~ifnot;
             arg = Array.copy def.arg
           })
    | Some ({ desc = Op (Floatop (width, Icompf cmp)); _ } as def) ->
      Some
        (Replace
           { desc = float_test_of_compare cmp ~width ~ifso ~ifnot;
             arg = Array.copy def.arg
           })
    | Some _ | None -> None)
  | Parity_test { ifso; ifnot } ->
    Option.bind (constant_arg 0) (fun c ->
        if Nativeint.equal (Nativeint.logand c 1n) 0n
        then Some (Goto ifso)
        else Some (Goto ifnot))
  | Int_test { lt; eq; gt; is_signed; imm } -> (
    let left = constant_arg 0 in
    let right =
      match imm with
      | Some n -> Some (Nativeint.of_int n)
      | None -> constant_arg 1
    in
    match left, right with
    | Some left, Some right ->
      let result =
        match is_signed with
        | Signed -> Nativeint.compare left right
        | Unsigned -> Nativeint.unsigned_compare left right
      in
      Some (Goto (if result < 0 then lt else if result > 0 then gt else eq))
    | None, _ | _, None -> None)
  | Switch labels ->
    Option.bind (constant_arg 0) (fun c ->
        if Nativeint.equal c (Nativeint.of_int (Nativeint.to_int c))
        then
          let idx = Nativeint.to_int c in
          if idx >= 0 && idx < Array.length labels
          then Some (Goto labels.(idx))
          else None
        else None)
  | Float_test _ | Never | Always _ | Return | Raise _ | Tailcall_self _
  | Tailcall_func _ | Call_no_return _ | Call _ | Prim _ | Invalid _ ->
    None

(* Apply a fold to [block]'s terminator and report whether the successor set
   actually changed. *)
let apply_and_check_change (block : C.basic_block) (result : fold_result) : bool
    =
  let old_successors = C.successor_labels ~normal:true ~exn:false block in
  apply_fold block result;
  not
    (Label.Set.equal old_successors
       (C.successor_labels ~normal:true ~exn:false block))

let process_block ~(is_loop_header : Label.t -> bool) (cfg : C.t)
    (block : C.basic_block) : bool =
  (* 1. Fold the block's own terminator (e.g. [Truth_test] → [Int_test] when the
     test argument is a comparison computed in this block). *)
  let reduced_terminator =
    match evaluate_terminator ~block block.terminator ~cfg with
    | None -> false
    | Some result -> apply_and_check_change block result
  in
  (* 2. Thread through an empty merge successor when safe. *)
  let skipped_successor =
    match[@ocaml.warning "-4"] block.terminator.desc with
    | Always successor_label
      when (not (Label.equal block.start cfg.entry_label))
           && (cfg.allowed_to_be_irreducible
              || not (is_loop_header successor_label)) -> (
      let successor_block = C.get_block_exn cfg successor_label in
      if not (DLL.is_empty successor_block.body)
      then false
      else
        match evaluate_terminator ~block successor_block.terminator ~cfg with
        | None -> false
        | Some result -> apply_and_check_change block result)
    | _ -> false
  in
  reduced_terminator || skipped_successor

let run cfg_with_infos =
  if not !Oxcaml_flags.cfg_value_propagation
  then cfg_with_infos
  else
    let cfg =
      Cfg_with_layout.cfg (Cfg_with_infos.cfg_with_layout cfg_with_infos)
    in
    assert (not cfg.register_locations_are_set);
    let header_map = (Cfg_with_infos.loop_infos cfg_with_infos).header_map in
    let is_loop_header label = Label.Map.mem label header_map in
    let registration_needed =
      C.fold_blocks cfg ~init:false ~f:(fun _ block acc ->
          process_block ~is_loop_header cfg block || acc)
    in
    if registration_needed
    then (
      (* We may need to remove predecessors, and
         [register_predecessors_for_all_blocks] only adds predecessors, so clear
         them all first to avoid stale entries. *)
      C.iter_blocks cfg ~f:(fun _label block ->
          block.predecessors <- Label.Set.empty);
      Cfg.register_predecessors_for_all_blocks cfg;
      (* The CFG structure changed, so cached dominators and loop infos must be
         discarded. *)
      Cfg_with_infos.invalidate_dominators_and_loop_infos cfg_with_infos;
      (* We extended the live ranges of comparison inputs. *)
      Cfg_with_infos.invalidate_liveness cfg_with_infos);
    cfg_with_infos
