[@@@ocaml.warning "+a-4-40-41-42-44"]

open Ssa

let successors (b : block) : block list =
  match b.terminator with
  | Never | Return _ | Raise _ | Tailcall_func _ -> []
  | Goto { goto; _ } -> [goto]
  | Branch { conditions; else_goto } ->
    let succs = ref [else_goto] in
    Array.iter (fun (_, dst) -> succs := dst :: !succs) conditions;
    !succs
  | Switch (targets, _) -> Array.to_list targets
  | Tailcall_self { destination; _ } -> [destination]
  | Call { continuation; exn_continuation; _ }
  | Prim { continuation; exn_continuation; _ } ->
    continuation
    :: (match exn_continuation with Some l -> [l] | None -> [])
  | Invalid { continuation; _ } -> (
    match continuation with Some l -> [l] | None -> [])

type t =
  { dom_in : int Block.Tbl.t;
    dom_out : int Block.Tbl.t;
    preds : block list Block.Tbl.t
  }

let compute (graph : Ssa.t) : t =
  let preds = Block.Tbl.create 16 in
  List.iter (fun bl -> Block.Tbl.replace preds bl []) graph.blocks;
  let add_pred ~src ~dst =
    match Block.Tbl.find_opt preds dst with
    | Some ps -> Block.Tbl.replace preds dst (src :: ps)
    | None -> ()
  in
  List.iter
    (fun bl -> List.iter (fun dst -> add_pred ~src:bl ~dst) (successors bl))
    graph.blocks;
  let rpo =
    let visited = Block.Tbl.create 16 in
    let order = ref [] in
    let rec dfs bl =
      if not (Block.Tbl.mem visited bl)
      then (
        Block.Tbl.replace visited bl ();
        List.iter dfs (successors bl);
        order := bl :: !order)
    in
    dfs graph.entry;
    !order
  in
  let rpo_index = Block.Tbl.create 16 in
  List.iteri (fun i bl -> Block.Tbl.replace rpo_index bl i) rpo;
  let idom = Block.Tbl.create 16 in
  Block.Tbl.replace idom graph.entry graph.entry;
  let intersect b1 b2 =
    let b1 = ref b1 and b2 = ref b2 in
    while not (block_equal !b1 !b2) do
      while Block.Tbl.find rpo_index !b1 > Block.Tbl.find rpo_index !b2 do
        b1 := Block.Tbl.find idom !b1
      done;
      while Block.Tbl.find rpo_index !b2 > Block.Tbl.find rpo_index !b1 do
        b2 := Block.Tbl.find idom !b2
      done
    done;
    !b1
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun bl ->
        if not (block_equal bl graph.entry)
        then
          let ps = Block.Tbl.find preds bl in
          let processed = List.filter (fun p -> Block.Tbl.mem idom p) ps in
          match processed with
          | [] -> ()
          | first :: rest ->
            let new_idom = List.fold_left intersect first rest in
            if
              not
                (match Block.Tbl.find_opt idom bl with
                | Some d -> block_equal d new_idom
                | None -> false)
            then (
              Block.Tbl.replace idom bl new_idom;
              changed := true))
      rpo
  done;
  let dom_children = Block.Tbl.create 16 in
  Block.Tbl.iter
    (fun bl parent ->
      if not (block_equal bl parent)
      then
        let kids =
          match Block.Tbl.find_opt dom_children parent with
          | Some l -> l
          | None -> []
        in
        Block.Tbl.replace dom_children parent (bl :: kids))
    idom;
  let dom_in = Block.Tbl.create 16 in
  let dom_out = Block.Tbl.create 16 in
  let time = ref 0 in
  let rec compute_times bl =
    Block.Tbl.replace dom_in bl !time;
    incr time;
    (match Block.Tbl.find_opt dom_children bl with
    | Some kids -> List.iter compute_times kids
    | None -> ());
    Block.Tbl.replace dom_out bl !time;
    incr time
  in
  compute_times graph.entry;
  { dom_in; dom_out; preds }

let dominates t a b =
  Block.Tbl.find t.dom_in a <= Block.Tbl.find t.dom_in b
  && Block.Tbl.find t.dom_out a >= Block.Tbl.find t.dom_out b

let predecessors t b =
  match Block.Tbl.find_opt t.preds b with Some ps -> ps | None -> []
