[@@@ocaml.warning "+a-4-40-41-42-44"]

module Make (S : Ssa.Finished_graph) = struct
  module IV = Induction_var.Make (S)
  module Term = Termination.Make (S)
  module Dead = Dead_induction_var.Make (S)

  type deletion =
    { loop : IV.loop;
      exit_target : S.Block.t
    }

  (* A loop may only be deleted if, besides spinning its (dead) induction
     variables, it performs no observable work. [Dead_induction_var.useless]
     only checks the header parameters; it says nothing about the body, so a
     loop like [while i < n do print_int 7; i <- i + 1 done] has a dead IV [i]
     yet is emphatically not empty. Here we additionally require every body
     block to contain only pure operations and to end in a pure control-flow
     terminator, ruling out stores, calls, raises, allocations, early returns
     and trap manipulation. *)
  let instr_effect_free (i : S.Instruction.t) : bool =
    match i with
    | Op { op; _ } -> Operation.is_pure op
    | Name_for_debugger _ | Stack_check _ -> true
    | Push_trap _ | Pop_trap _ -> false
    | Block_param _ | Proj _ | Tuple _ -> true

  let terminator_pure_control (t : S.Terminator.t) : bool =
    match t with
    | Goto _ | Branch _ | Switch _ -> true
    | Return _ | Raise _ | Tailcall_self _ | Tailcall_func _ | Call _
    | Invalid _ ->
      false

  let body_effect_free (loop : IV.loop) : bool =
    S.Block.Set.for_all
      (fun (bl : S.Block.t) ->
        Array.for_all instr_effect_free bl.body
        && terminator_pure_control bl.terminator)
      loop.body

  (* If the header's terminator matches the simple exit shape the dead-IV
     analysis also requires (exactly one in-loop target), return the out-of-loop
     target. *)
  let exit_target_of_loop (loop : IV.loop) : S.Block.t option =
    Option.map
      (fun (eb : Term.exit_branch) -> eb.exit_target)
      (Term.find_exit_branch loop)

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
        if dr.useless && terminates && body_effect_free loop
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

let run (input : (module Ssa.Finished_graph)) :
    (module Ssa.Finished_graph) * int =
  let module S = (val input : Ssa.Finished_graph) in
  let module D = Make (S) in
  match D.run () with
  | [] -> input, 0
  | deletions ->
    (* [D.run] rewrote header terminators in place, which leaves the graph's
       cached metadata stale and the loop bodies unreachable. A trivial reducer
       pass rebuilds a fresh finished graph (recomputing predecessors,
       dominators and use counts, and pruning the now-dead blocks), so the
       result is safe to feed to the next pass. *)
    ( Ssa_reducer.run (module Ssa_reducer.Default : Ssa_reducer.Reducer) input,
      List.length deletions )
