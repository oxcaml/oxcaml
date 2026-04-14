[@@@ocaml.warning "+a-4-40-41-42-44"]

open Ssa

type deletion =
  { loop : Induction_var.loop;
    exit_target : block
  }

(* If the header's terminator matches the simple exit shape the dead-IV
   analysis also requires (single condition, exactly one in-loop target),
   return the out-of-loop target. *)
let exit_target_of_loop (loop : Induction_var.loop) : block option =
  match loop.header.terminator with
  | Branch { conditions = [| _cond, t_true |]; else_goto } -> (
    let true_in = Block.Set.mem t_true loop.body in
    let false_in = Block.Set.mem else_goto loop.body in
    match true_in, false_in with
    | true, false -> Some else_goto
    | false, true -> Some t_true
    | true, true | false, false -> None)
  | Branch _ | Never | Goto _ | Switch _ | Return _ | Raise _
  | Tailcall_self _ | Tailcall_func _ | Call _ | Prim _ | Invalid _ ->
    None

let run (t : Ssa.t) : deletion list =
  let loops = Induction_var.analyze t in
  let dead = Dead_induction_var.analyze t loops in
  let deletions = ref [] in
  List.iter2
    (fun ((loop : Induction_var.loop), bivs)
         (dr : Dead_induction_var.loop_result) ->
      if dr.useless && Termination.analyze loop bivs = Termination.Terminates
      then
        match exit_target_of_loop loop with
        | Some exit_target ->
          let header : block = loop.header in
          header.terminator <- Goto { goto = exit_target; args = [||] };
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
      (fun d ->
        Format.fprintf ppf "@,  header=%d bypassed -> goto block %d"
          d.loop.Induction_var.header.id d.exit_target.id)
      ds;
    Format.fprintf ppf "@]"
