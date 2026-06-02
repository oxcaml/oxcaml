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

let apply_fold (block : C.basic_block) (result : fold_result option) : bool =
  match result with
  | None -> false
  | Some result ->
    let new_terminator =
      match result with
      | Goto target ->
        { block.terminator with desc = C.Always target; arg = [||]; res = [||] }
      | Replace { desc; arg } -> { block.terminator with desc; arg; res = [||] }
    in
    block.terminator <- new_terminator;
    true

(** [find_reaching_def reg block dominators cfg] searches for the definition of
    [reg] visible at the end of [block]'s body (right before its terminator)
    while looking through moves. The search walks up the immediate-dominator
    chain of [block]; at each step it also visits any blocks between the current
    block and its immediate dominator (blocks reverse-reachable from the current
    block and dominated by the immediate dominator) — these are only consulted
    to invalidate registers, never to source the definition. This lets the
    search cross control-flow patterns like if-then-else.

    [Move] instructions defining the current target are chased through: when we
    encounter [target = src], the search continues for [src]. A register is
    considered clobbered if it has been written to between the candidate
    definition and the use point. The function returns the defining instruction
    only when none of its arguments has been clobbered after it, so that the
    arguments can still be used at the use point. *)
let find_reaching_def ~(reg : Reg.t) ~(block : C.basic_block)
    ~(cfg_with_infos : Cfg_with_infos.t) : C.basic C.instruction option =
  let dominators = Cfg_with_infos.dominators cfg_with_infos in
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  let clobbered = ref Reg.Set.empty in
  let add_to_clobbered arr =
    clobbered := Array.fold_right Reg.Set.add arr !clobbered
  in
  (* For a non-dominator, we cannot find a definition, but we might have to
     invalidate one. *)
  let visit_non_dominator (b : C.basic_block) ~reg =
    (not (Array.exists (Reg.same reg) b.terminator.res))
    &&
    (add_to_clobbered b.terminator.res;
     DLL.for_all b.body ~f:(fun (instr : C.basic C.instruction) ->
         add_to_clobbered instr.res;
         not (Array.exists (Reg.same reg) instr.res)))
  in
  (* Visit blocks reverse-reachable from [from_block], stopping at dominators of
     the block for which we search a definition. *)
  let visit_non_dominating_predecessors ~(from_block : C.basic_block) ~reg :
      bool =
    let visited = ref Label.Set.empty in
    let stack = Stack.create () in
    let push lbl =
      if
        (not (Label.Set.mem lbl !visited))
        && not (Cfg_dominators.is_dominating dominators lbl block.start)
      then (
        visited := Label.Set.add lbl !visited;
        Stack.push lbl stack)
    in
    Label.Set.iter push from_block.predecessors;
    let rec loop () =
      if not (Stack.is_empty stack)
      then (
        let label = Stack.pop stack in
        let b = Cfg.get_block_exn cfg label in
        Label.Set.iter push b.predecessors;
        visit_non_dominator b ~reg && loop ())
      else true
    in
    loop ()
  in
  let rec walk ~(block : C.basic_block) ~pos ~reg : C.basic C.instruction option
      =
    match pos with
    | Some cell ->
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
          walk ~block ~pos:(DLL.prev cell) ~reg:instr.arg.(0)
        | _ ->
          let inputs_safe =
            Array.for_all (fun a -> not (Reg.Set.mem a !clobbered)) instr.arg
          in
          if inputs_safe then Some instr else None
        end
      else if Array.exists (Reg.same reg) instr.res
      then None
      else walk ~block ~pos:(DLL.prev cell) ~reg
    | None -> (
      match Cfg_dominators.immediate_dominator dominators block.start with
      | None -> None
      | Some idom_label ->
        let idom_block = Cfg.get_block_exn cfg idom_label in
        if
          (not (visit_non_dominating_predecessors ~from_block:block ~reg))
          || Array.exists (Reg.same reg) idom_block.terminator.res
        then None
        else (
          add_to_clobbered idom_block.terminator.res;
          walk ~block:idom_block ~pos:(DLL.last_cell idom_block.body) ~reg))
  in
  if Reg.is_preassigned reg
  then None
  else walk ~block ~pos:(DLL.last_cell block.body) ~reg

(** Fold [term] using reaching definitions visible at [pos].

    - For [Truth_test] whose argument was set by an [Icomp]/[Icompf] (with its
      inputs unclobbered), produces a [Replace] folding the comparison into the
      test.
    - For tests whose arguments are known constants, produces a [Goto] to the
      statically determined arm. *)
let evaluate_terminator ~block (term : C.terminator C.instruction)
    ~cfg_with_infos : fold_result option =
  let constant_arg arg_idx =
    match[@ocaml.warning "-4"]
      find_reaching_def ~reg:term.arg.(arg_idx) ~block ~cfg_with_infos
    with
    | Some { desc = Op (Const_int c); _ } -> Some c
    | Some _ | None -> None
  in
  match term.desc with
  | Truth_test { ifso; ifnot } -> (
    match[@ocaml.warning "-4"]
      find_reaching_def ~reg:term.arg.(0) ~block ~cfg_with_infos
    with
    | Some { desc = Op (Const_int c); _ } ->
      if Nativeint.equal c 0n then Some (Goto ifnot) else Some (Goto ifso)
    | Some { desc = Op (Intop (Icomp cmp)); arg; _ } ->
      Some
        (Replace
           { desc =
               Int_test
                 (Select_utils.int_test_of_integer_comparison cmp
                    ~immediate:None ~label_false:ifnot ~label_true:ifso);
             arg = Array.copy arg
           })
    | Some { desc = Op (Intop_imm (Icomp cmp, imm)); arg; _ } ->
      Some
        (Replace
           { desc =
               Int_test
                 (Select_utils.int_test_of_integer_comparison cmp
                    ~immediate:(Some imm) ~label_false:ifnot ~label_true:ifso);
             arg = Array.copy arg
           })
    | Some { desc = Op (Floatop (width, Icompf cmp)); arg; _ } ->
      Some
        (Replace
           { desc =
               Float_test
                 (Select_utils.float_test_of_float_comparison width cmp
                    ~label_false:ifnot ~label_true:ifso);
             arg = Array.copy arg
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
        if
          Nativeint.compare c 0n >= 0
          && Nativeint.compare c (Nativeint.of_int Int.max_int) <= 0
        then
          let idx = Nativeint.to_int c in
          if idx < Array.length labels then Some (Goto labels.(idx)) else None
        else None)
  | Float_test _ | Never | Always _ | Return | Raise _ | Tailcall_self _
  | Tailcall_func _ | Call_no_return _ | Call _ | Prim _ | Invalid _ ->
    None

let process_block ~(is_loop_header : Label.t -> bool)
    ~(cfg_with_infos : Cfg_with_infos.t) (block : C.basic_block) : bool =
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  (* 1. Fold the block's own terminator. *)
  let reduce_terminator () =
    evaluate_terminator ~block block.terminator ~cfg_with_infos
    |> apply_fold block
  in
  (* 2. Thread through an empty merge successor when safe. *)
  let rec skip_successor () =
    match[@ocaml.warning "-4"] block.terminator.desc with
    | Always successor_label
      when (not (Label.equal block.start cfg.entry_label))
           && (cfg.allowed_to_be_irreducible
              || not (is_loop_header successor_label)) ->
      let successor_block = C.get_block_exn cfg successor_label in
      if not (DLL.is_empty successor_block.body)
      then false
      else
        let changed =
          evaluate_terminator ~block successor_block.terminator ~cfg_with_infos
          |> apply_fold block
        in
        if changed then skip_successor () |> ignore;
        true
    | _ -> false
  in
  let folded_own = reduce_terminator () in
  let folded_successor = skip_successor () in
  folded_own || folded_successor

let run cfg_with_infos =
  if not !Oxcaml_flags.cfg_jump_threading
  then cfg_with_infos
  else
    let cfg =
      Cfg_with_layout.cfg (Cfg_with_infos.cfg_with_layout cfg_with_infos)
    in
    if cfg.register_locations_are_set
    then
      Misc.fatal_error "Cfg_jump_threading: must run before register allocation";
    let header_map = (Cfg_with_infos.loop_infos cfg_with_infos).header_map in
    let is_loop_header label = Label.Map.mem label header_map in
    let changed =
      C.fold_blocks cfg ~init:false ~f:(fun _ block acc ->
          process_block ~is_loop_header ~cfg_with_infos block || acc)
    in
    if changed
    then (
      (* We may need to remove predecessors, and
         [register_predecessors_for_all_blocks] only adds predecessors, so clear
         them all first to avoid stale entries. *)
      C.iter_blocks cfg ~f:(fun _label block ->
          block.predecessors <- Label.Set.empty);
      Cfg.register_predecessors_for_all_blocks cfg;
      (* Folding a conditional terminator to [Always] can leave behind an
         unreachable strongly-connected component (e.g. a cycle whose only entry
         from outside was the eliminated arm). Remove unreachable blocks so
         later passes that require every weakly-connected component to contain a
         block with no predecessors (notably [Cfg_dominators]) do not fail. *)
      let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
      let dead_labels = Cfg_simplify.Eliminate_dead_code.run cfg_with_layout in
      Cfg_with_layout.remove_blocks cfg_with_layout dead_labels;
      (* The CFG structure changed, so cached dominators and loop infos must be
         discarded. *)
      Cfg_with_infos.invalidate_dominators_and_loop_infos cfg_with_infos;
      (* We extended the live ranges of comparison inputs. *)
      Cfg_with_infos.invalidate_liveness cfg_with_infos);
    cfg_with_infos
