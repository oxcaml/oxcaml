[@@@ocaml.warning "+a-4-40-41-42-44"]

module Make (S : Ssa.Finished_graph) = struct
  module IV = Induction_var.Make (S)

  type t =
    | Terminates
    | Unknown

  type exit_branch =
    { condition : S.Instruction.t;
      continue_when_true : bool;
      exit_target : S.Block.t
    }

  (* The exit branch must be the loop header's [Branch] terminator, with exactly
     one of its two targets inside the loop and the other outside (if both are
     in or both are out, we give up). *)
  let find_exit_branch (loop : IV.loop) : exit_branch option =
    match loop.header.terminator with
    | Branch { cond; ifso; ifnot } -> (
      let true_in = S.Block.Set.mem ifso loop.body in
      let false_in = S.Block.Set.mem ifnot loop.body in
      match true_in, false_in with
      | true, false ->
        Some
          { condition = cond; continue_when_true = true; exit_target = ifnot }
      | false, true ->
        Some
          { condition = cond; continue_when_true = false; exit_target = ifso }
      | true, true | false, false -> None)
    | Goto _ | Switch _ | Return _ | Raise _ | Tailcall_self _ | Tailcall_func _
    | Call _ | Invalid _ ->
      None

  let direction_of_biv (biv : IV.biv) : Loop_comparisons.direction option =
    Option.bind (IV.signed_step biv) Loop_comparisons.direction_of_step

  let biv_implies_termination ~op_def (biv : IV.biv) : bool =
    match find_exit_branch biv.loop, direction_of_biv biv with
    | None, _ | _, None -> false
    | Some exit_info, Some dir -> (
      let header = biv.loop.header in
      let body = biv.loop.body in
      let is_self = IV.is_header_param header biv.param_index in
      let is_iv_val v = is_self v || List.exists (IV.instr_same v) biv.update in
      (* The IV progresses monotonically, but that only forces the comparison to
         flip if the operand it is tested against stays put. A loop-variant
         other operand (e.g. a second counter in [while i < j]) can keep the
         comparison true forever, so we require it to be loop-invariant. *)
      let is_bound v = IV.is_loop_invariant op_def body v in
      let extract =
        match exit_info.condition with
        | Op { op = Intop_imm (Icomp cmp, _); args = [| x |]; _ } ->
          if is_iv_val x then Some (cmp, `Iv_left) else None
        | Op { op = Intop (Icomp cmp); args = [| x; y |]; _ } ->
          if is_iv_val x && is_bound y
          then Some (cmp, `Iv_left)
          else if is_iv_val y && is_bound x
          then Some (cmp, `Iv_right)
          else None
        | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
        | Stack_check _ | Name_for_debugger _ ->
          None
      in
      match extract with
      | None -> false
      | Some (cmp, side) ->
        let iv_is_left =
          match side with `Iv_left -> true | `Iv_right -> false
        in
        let continue_cmp =
          Loop_comparisons.oriented_continue_comparison ~iv_is_left
            ~continue_when_true:exit_info.continue_when_true cmp
        in
        Loop_comparisons.continue_terminates dir continue_cmp)

  let analyze (_loop : IV.loop) (bivs : IV.biv list) : t =
    let op_def = IV.op_def () in
    if List.exists (biv_implies_termination ~op_def) bivs
    then Terminates
    else Unknown

  let print_one ppf = function
    | Terminates -> Format.fprintf ppf "terminates"
    | Unknown -> Format.fprintf ppf "termination unknown"

  let print ppf (results : (IV.loop * t) list) =
    Format.fprintf ppf "@[<v>termination:";
    match results with
    | [] -> Format.fprintf ppf " <no loops>@]"
    | _ ->
      List.iter
        (fun ((loop : IV.loop), term) ->
          Format.fprintf ppf "@,  loop header=%d (%a)"
            (loop.header.id :> int)
            print_one term)
        results;
      Format.fprintf ppf "@]"
end
