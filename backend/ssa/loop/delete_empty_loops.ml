[@@@ocaml.warning "+a-4-40-41-42-44"]

module Make (S : Ssa.Finished_graph) = struct
  module IV = Induction_var.Make (S)
  module Term = Termination.Make (S)
  module Dead = Dead_induction_var.Make (S)

  type deletion =
    { loop : IV.loop;
      exit_target : S.Block.t
    }

  (* If the header's terminator matches the simple exit shape the dead-IV
     analysis also requires (exactly one in-loop target), return the out-of-loop
     target. *)
  let exit_target_of_loop (loop : IV.loop) : S.Block.t option =
    match loop.header.terminator with
    | Branch { cond = _; ifso; ifnot } -> (
      let true_in = S.Block.Set.mem ifso loop.body in
      let false_in = S.Block.Set.mem ifnot loop.body in
      match true_in, false_in with
      | true, false -> Some ifnot
      | false, true -> Some ifso
      | true, true | false, false -> None)
    | Goto _ | Switch _ | Return _ | Raise _ | Tailcall_self _ | Tailcall_func _
    | Call _ | Invalid _ ->
      None

  let run () : deletion list =
    let loops = IV.analyze () in
    let dead = Dead.analyze loops in
    let deletions = ref [] in
    List.iter2
      (fun ((loop : IV.loop), bivs) (dr : Dead.loop_result) ->
        let terminates =
          match Term.analyze loop bivs with
          | Term.Terminates -> true
          | Term.Unknown -> false
        in
        if dr.useless && terminates
        then
          match exit_target_of_loop loop with
          | Some exit_target ->
            S.Block.set_terminator loop.header
              (Goto { goto = exit_target; args = [||] });
            deletions := { loop; exit_target } :: !deletions
          | None -> ())
      loops dead;
    List.rev !deletions

  let print ppf (ds : deletion list) =
    Format.fprintf ppf "@[<v>deleted empty loops:";
    match ds with
    | [] -> Format.fprintf ppf " <none>@]"
    | _ ->
      List.iter
        (fun (d : deletion) ->
          Format.fprintf ppf "@,  header=%d bypassed -> goto block %d"
            (d.loop.header.id :> int)
            (d.exit_target.id :> int))
        ds;
      Format.fprintf ppf "@]"
end
