open DeadInductionVar
open InductionVar
open LoopTermination
open NaturalLoop
open OxCamlList
open SsaGraph
open SsaModel
open SsaSemantics

(** val graph_supported : block list -> bool **)

let graph_supported blocks =
  length_at_most 4_294_967_296 blocks
  && List.for_all
       (fun b -> List.for_all context_instruction_supported (block_body b))
       blocks

(** val body_supported : natural_loop -> bool **)

let body_supported loop0 = Ssa.Block.Set.for_all supported_block loop0.body

(** val single_exit : natural_loop -> block -> bool **)

let single_exit loop0 exit =
  Ssa.Block.Set.for_all
    (fun block0 ->
      List.for_all
        (fun succ ->
          Ssa.Block.Set.mem succ loop0.body || Ssa.Block.equal succ exit)
        (Ssa.Block.all_successors block0))
    loop0.body

(** val loop_terminates : op_def -> biv list -> bool **)

let loop_terminates op_def0 bivs0 =
  match analyze op_def0 bivs0 with Terminates -> true | Unknown -> false

(** val find_deletions : op_def -> block list -> (block * block) list **)

let find_deletions op_def0 blocks =
  if graph_supported blocks
  then
    let loops = InductionVar.analyze op_def0 blocks in
    let dead = DeadInductionVar.analyze blocks loops in
    List.filter_map
      (fun result ->
        let loop0 = result.DeadInductionVar.loop in
        let bivs0 = List.map (fun biv_result -> biv_result.biv) result.bivs in
        if result.useless && body_supported loop0
        then
          match find_exit_branch loop0 with
          | Some exit_info -> (
            match params exit_info.exit_target with
            | [] ->
              if
                (single_exit loop0 exit_info.exit_target
                && acyclic_except blocks loop0.body loop0.header)
                && loop_terminates op_def0 bivs0
              then Some (loop0.header, exit_info.exit_target)
              else None
            | _ :: _ -> None)
          | None -> None
        else None)
      dead
  else []
