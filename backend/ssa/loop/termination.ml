[@@@ocaml.warning "+a-4-40-41-42-44"]

module Make (S : Ssa.Finished_graph) = struct
  module IV = Induction_var.Make (S)
  module AS = Affine_ssa.Make (S)
  module Affine = Fourier_motzkin.Affine

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

  (* Discharge {!Loop_comparisons.Terminates_if_bound_in_range}: the bound's
     machine value must satisfy [b <= max_int] (positive step) or [b >=
     -max_int] (negative step), which keeps the IV's increment from ever
     wrapping at 64 bits (see the argument in {!Loop_comparisons}). Literal
     bounds are checked directly; register bounds are proved by Fourier-Motzkin
     from the guards dominating the loop header — e.g. a bound compared against
     an array length (whose [lsr]-of-header shape yields a [<= 2^54 - 1] range
     fact) or any other dominating range check. *)
  let bound_in_range ~step ~(header : S.Block.t)
      (bound : [`Value of S.Instruction.t | `Const of int]) : bool =
    match bound with
    | `Const k -> if step > 0 then k <= max_int else k >= -max_int
    | `Value v -> (
      let ctx = AS.new_ctx () in
      let side = ref [] in
      let form = AS.linearize ctx side v in
      let facts = AS.guards_at ctx side header @ !side in
      match
        if step > 0
        then Affine.add_const_checked (Affine.scale_checked (-1) form) max_int
        else Affine.add_const_checked form max_int
      with
      | goal ->
        let r = Fourier_motzkin.entails facts goal in
        Format.eprintf "DBG bir: step=%d nfacts=%d goal={c=%d;%s} r=%b@." step
          (List.length facts) goal.Affine.const
          (String.concat ","
             (List.map
                (fun (a, c) -> Printf.sprintf "%d*a%d" c a)
                goal.Affine.terms))
          r;
        List.iter
          (fun (f : Affine.t) ->
            Format.eprintf "DBG   f {c=%d;%s}@." f.Affine.const
              (String.concat ","
                 (List.map
                    (fun (a, c) -> Printf.sprintf "%d*a%d" c a)
                    f.Affine.terms)))
          facts;
        r
      | exception Fourier_motzkin.Overflow -> false)

  let biv_implies_termination ~op_def (biv : IV.biv) : bool =
    match find_exit_branch biv.loop, IV.signed_step biv with
    | None, _ | _, None -> false
    | Some exit_info, Some step -> (
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
        | Op { op = Intop_imm (Icomp cmp, k); args = [| x |]; _ } ->
          if is_iv_val x then Some (cmp, true, `Const k) else None
        | Op { op = Intop (Icomp cmp); args = [| x; y |]; _ } ->
          if is_iv_val x && is_bound y
          then Some (cmp, true, `Value y)
          else if is_iv_val y && is_bound x
          then Some (cmp, false, `Value x)
          else None
        | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
        | Stack_check _ | Name_for_debugger _ ->
          None
      in
      match extract with
      | None -> false
      | Some (cmp, iv_is_left, bound) -> (
        let continue_cmp =
          Loop_comparisons.oriented_continue_comparison ~iv_is_left
            ~continue_when_true:exit_info.continue_when_true cmp
        in
        match Loop_comparisons.continue_terminates ~step continue_cmp with
        | Terminates -> true
        | Terminates_if_bound_in_range -> bound_in_range ~step ~header bound
        | Unknown -> false))

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
