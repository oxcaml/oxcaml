[@@@ocaml.warning "+a-4-40-41-42-44"]

module Make (S : Ssa.Finished_graph) = struct
  module IV = Induction_var.Make (S)

  type t =
    | Terminates
    | Unknown

  type direction =
    | Up
    | Down

  type exit_branch =
    { condition : S.Instruction.t;
      continue_when_true : bool
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
      | true, false -> Some { condition = cond; continue_when_true = true }
      | false, true -> Some { condition = cond; continue_when_true = false }
      | true, true | false, false -> None)
    | Goto _ | Switch _ | Return _ | Raise _ | Tailcall_self _ | Tailcall_func _
    | Call _ | Invalid _ ->
      None

  let direction_of_biv (biv : IV.biv) : direction option =
    match biv.step, biv.sign with
    | Step_const c, `Add ->
      if c > 0 then Some Up else if c < 0 then Some Down else None
    | Step_const c, `Sub ->
      if c > 0 then Some Down else if c < 0 then Some Up else None
    | Step_var _, _ -> None

  (* Continue-condition (oriented with the IV operand on the left): the loop
     continues iff this comparison holds. Decides whether monotonic progression
     in [dir] must eventually break the comparison. We only handle signed
     comparisons; unsigned wrap-around invalidates monotonicity, so those are
     reported as Unknown. *)
  let continue_terminates dir (cmp : Cmm.integer_comparison) =
    match dir, cmp with
    | Up, (Clt | Cle) -> true
    | Down, (Cgt | Cge) -> true
    | (Up | Down), Ceq -> true
    | Up, (Cgt | Cge | Cne | Cult | Cugt | Cule | Cuge)
    | Down, (Clt | Cle | Cne | Cult | Cugt | Cule | Cuge) ->
      false

  let biv_implies_termination (biv : IV.biv) : bool =
    match find_exit_branch biv.loop, direction_of_biv biv with
    | None, _ | _, None -> false
    | Some exit_info, Some dir -> (
      let header = biv.loop.header in
      let is_self = IV.is_header_param header biv.param_index in
      let is_iv_val v = is_self v || List.exists (IV.instr_same v) biv.update in
      let extract =
        match exit_info.condition with
        | Op { op = Intop_imm (Icomp cmp, _); args = [| x |]; _ } ->
          if is_iv_val x then Some (cmp, `Iv_left) else None
        | Op { op = Intop (Icomp cmp); args = [| x; y |]; _ } ->
          if is_iv_val x
          then Some (cmp, `Iv_left)
          else if is_iv_val y
          then Some (cmp, `Iv_right)
          else None
        | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
        | Stack_check _ | Name_for_debugger _ ->
          None
      in
      match extract with
      | None -> false
      | Some (cmp, side) ->
        let cmp_iv_left =
          match side with
          | `Iv_left -> cmp
          | `Iv_right -> Cmm.swap_integer_comparison cmp
        in
        let continue_cmp =
          if exit_info.continue_when_true
          then cmp_iv_left
          else Cmm.negate_integer_comparison cmp_iv_left
        in
        continue_terminates dir continue_cmp)

  let analyze (_loop : IV.loop) (bivs : IV.biv list) : t =
    if List.exists biv_implies_termination bivs then Terminates else Unknown

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
