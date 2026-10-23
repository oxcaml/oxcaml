open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

open Ssa.Export

module Loop = struct
  type t =
    { header : finished Block.t;
      blocks : Block.Set.t
    }

  let header t = t.header

  let contains t block = Block.Set.mem block t.blocks
end

type t = { innermost_loop : Loop.t Block.Tbl.t }

(* CR xclerc for xclerc: try and share back edge computation with the CFG
   version. *)

type dfs_state =
  | Visiting
  | Visited

(* The back edges, as [(source, header)] pairs, or [None] if a retreating edge
   of the depth-first search is not a back edge, which happens exactly when the
   graph is irreducible. *)
let back_edges (graph : finished Ssa.graph) :
    (finished Block.t * finished Block.t) list option =
  let state = Block.Tbl.create 64 in
  let back_edges = ref [] in
  let reducible = ref true in
  let stack = Stack.create () in
  let entry = Ssa.entry graph in
  Block.Tbl.replace state entry Visiting;
  Stack.push (entry, Block.all_successors entry) stack;
  while not (Stack.is_empty stack) do
    match Stack.pop stack with
    | block, [] -> Block.Tbl.replace state block Visited
    | block, succ :: succs -> (
      Stack.push (block, succs) stack;
      match Block.Tbl.find_opt state succ with
      | None ->
        Block.Tbl.replace state succ Visiting;
        Stack.push (succ, Block.all_successors succ) stack
      | Some Visited -> ()
      | Some Visiting ->
        if Block.dominates succ block
        then back_edges := (block, succ) :: !back_edges
        else reducible := false)
  done;
  if !reducible then Some !back_edges else None

let natural_loop ~(source : finished Block.t) ~(header : finished Block.t) :
    Block.Set.t =
  let rec walk blocks = function
    | [] -> blocks
    | block :: rest ->
      if Block.Set.mem block blocks
      then walk blocks rest
      else
        walk
          (Block.Set.add block blocks)
          (List.rev_append (Block.predecessors block) rest)
  in
  walk (Block.Set.singleton header) [source]

let compute (graph : finished Ssa.graph) : t option =
  match back_edges graph with
  | None -> None
  | Some back_edges ->
    let loops =
      List.fold_left
        (fun loops (source, header) ->
          let blocks = natural_loop ~source ~header in
          Block.Map.update header
            (function
              | None -> Some blocks
              | Some other_blocks -> Some (Block.Set.union blocks other_blocks))
            loops)
        Block.Map.empty back_edges
    in
    (* Any two loops being nested or disjoint, recording the loops from the
       largest to the smallest leaves each block mapped to its innermost
       loop. *)
    let loops_by_decreasing_size =
      Block.Map.fold
        (fun header blocks acc ->
          (Block.Set.cardinal blocks, { Loop.header; blocks }) :: acc)
        loops []
      |> List.sort (fun (size1, _) (size2, _) -> Int.compare size2 size1)
    in
    let innermost_loop = Block.Tbl.create 64 in
    List.iter
      (fun (_size, (loop : Loop.t)) ->
        Block.Set.iter
          (fun block -> Block.Tbl.replace innermost_loop block loop)
          loop.blocks)
      loops_by_decreasing_size;
    Some { innermost_loop }

let innermost_loop t block = Block.Tbl.find_opt t.innermost_loop block
