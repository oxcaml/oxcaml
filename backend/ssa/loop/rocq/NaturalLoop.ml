open SsaModel

type natural_loop =
  { header : block;
    body : blockset;
    back_edge_sources : block list
  }

(** val add_new_body_block : (blockset * block list) -> block -> blockset *
    block list **)

let add_new_body_block acc pred =
  let body0, added = acc in
  if Ssa.Block.Set.mem pred body0
  then body0, added
  else Ssa.Block.Set.add pred body0, pred :: added

(** val walk_predecessors : block list -> blockset -> block list -> blockset **)

let rec walk_predecessors fuel body0 worklist =
  match fuel with
  | [] -> body0
  | _ :: fuel0 -> (
    match worklist with
    | [] -> body0
    | block0 :: rest ->
      let body1, added =
        List.fold_left add_new_body_block (body0, rest)
          (Ssa.Block.predecessors block0)
      in
      walk_predecessors fuel0 body1 added)

(** val natural_loop_body : block list -> block -> block list -> blockset **)

let natural_loop_body fuel header0 back_edge_sources0 =
  let body0, seeds =
    List.fold_left add_new_body_block
      (Ssa.Block.Set.singleton header0, [])
      back_edge_sources0
  in
  walk_predecessors fuel body0 seeds

(** val add_back_edge : block -> block -> (block * block list) list -> (block *
    block list) list **)

let rec add_back_edge header0 source = function
  | [] -> (header0, source :: []) :: []
  | p :: table0 ->
    let header', back_edge_sources0 = p in
    if Ssa.Block.equal header' header0
    then (header', source :: back_edge_sources0) :: table0
    else (header', back_edge_sources0) :: add_back_edge header0 source table0

(** val record_back_edge : block -> (block * block list) list -> block -> (block
    * block list) list **)

let record_back_edge source table succ =
  if Ssa.Block.dominates succ source
  then add_back_edge succ source table
  else table

(** val record_back_edges : (block * block list) list -> block -> (block * block
    list) list **)

let record_back_edges table source =
  List.fold_left (record_back_edge source) table
    (Ssa.Block.all_successors source)

(** val find_loops : block list -> natural_loop list **)

let find_loops blocks =
  let header_table = List.fold_left record_back_edges [] blocks in
  List.map
    (fun pat ->
      let header0, back_edge_sources0 = pat in
      { header = header0;
        body = natural_loop_body blocks header0 back_edge_sources0;
        back_edge_sources = back_edge_sources0
      })
    header_table

type exit_branch =
  { condition : Ssa.finished Ssa.Value.t;
    continue_when_true : bool;
    exit_target : block
  }

(** val find_exit_branch : natural_loop -> exit_branch option **)

let find_exit_branch loop =
  (fun f_continue
    f_switch
    f_call
    f_invalid
    (t : Ssa.finished Ssa.Terminator.t)
  ->
    match t with
    | Ssa.Terminator.Continue { continuation; args } ->
      f_continue continuation (Array.to_list args)
    | Ssa.Terminator.Switch { index; targets } ->
      f_switch index (Array.to_list targets)
    | Ssa.Terminator.Call { args; _ } -> f_call (Array.to_list args)
    | Ssa.Terminator.Invalid { args; _ } -> f_invalid (Array.to_list args))
    (fun _ _ -> None)
    (fun index targets ->
      match targets with
      | [] -> None
      | ifnot :: l -> (
        match l with
        | [] -> None
        | ifso :: l0 -> (
          match l0 with
          | [] ->
            if Ssa.Block.Set.mem ifso loop.body
            then
              if Ssa.Block.Set.mem ifnot loop.body
              then None
              else
                Some
                  { condition = index;
                    continue_when_true = true;
                    exit_target = ifnot
                  }
            else if Ssa.Block.Set.mem ifnot loop.body
            then
              Some
                { condition = index;
                  continue_when_true = false;
                  exit_target = ifso
                }
            else None
          | _ :: _ -> None)))
    (fun _ -> None)
    (fun _ -> None)
    (Ssa.Block.terminator loop.header)
