open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

open Ssa.Export

(* Post-dominators are the dominators of the reverse graph, rooted at a virtual
   exit node that every exit block flows into. They are computed using the same
   algorithm used in `Cfg_dominators`. *)

(* CR-soon xclerc for xclerc: see what can be shared with the computations of
   dominators (CFG and SSA). *)

type t =
  { index : int Block.Tbl.t;
        (* [0 .. n-1] are the graph's blocks, and [n] is the virtual exit. *)
    immediate_post_dominator : int array;
        (* [-1] for the blocks from which no exit can be reached. *)
    depth : int array
        (* Depth in the post-dominator tree, the virtual exit being at [0]. *)
  }

(* Depth-first search from [root] of the graph with nodes [0 .. num_nodes - 1]
   and the given [successors]. Returns the postorder number of each node ([-1]
   for the nodes not reachable from [root]), and the reachable nodes in reverse
   postorder. *)
let compute_postorder ~num_nodes ~(successors : int -> int list) ~root :
    int array * int list =
  let postorder = Array.make num_nodes (-1) in
  let reverse_postorder = ref [] in
  let visited = Array.make num_nodes false in
  let stack = Stack.create () in
  let next_number = ref 0 in
  visited.(root) <- true;
  Stack.push (root, successors root) stack;
  while not (Stack.is_empty stack) do
    match Stack.pop stack with
    | node, [] ->
      postorder.(node) <- !next_number;
      incr next_number;
      reverse_postorder := node :: !reverse_postorder
    | node, child :: children ->
      Stack.push (node, children) stack;
      if not visited.(child)
      then begin
        visited.(child) <- true;
        Stack.push (child, successors child) stack
      end
  done;
  postorder, !reverse_postorder

(* Immediate dominators of the graph with the given [predecessors], rooted at
   [root], where [postorder] and [reverse_postorder] are the result of
   [compute_postorder] on that graph. Returns the immediate dominator of each
   node: [root] for the root itself, and [-1] for the nodes not reachable from
   it. *)
let compute_immediate_dominators ~(predecessors : int -> int list) ~root
    ~postorder ~reverse_postorder : int array =
  let immediate_dominator = Array.make (Array.length postorder) (-1) in
  immediate_dominator.(root) <- root;
  let rec intersect a b =
    if a = b
    then a
    else if postorder.(a) < postorder.(b)
    then intersect immediate_dominator.(a) b
    else intersect a immediate_dominator.(b)
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun node ->
        if node <> root
        then begin
          let new_immediate_dominator =
            List.fold_left
              (fun acc pred ->
                if immediate_dominator.(pred) < 0
                then acc
                else if acc < 0
                then pred
                else intersect pred acc)
              (-1) (predecessors node)
          in
          if new_immediate_dominator <> immediate_dominator.(node)
          then begin
            immediate_dominator.(node) <- new_immediate_dominator;
            changed := true
          end
        end)
      reverse_postorder
  done;
  immediate_dominator

let compute (graph : finished Ssa.graph) : t =
  let blocks = Array.of_list (Ssa.blocks graph) in
  let num_blocks = Array.length blocks in
  let exit = num_blocks in
  let index = Block.Tbl.create num_blocks in
  Array.iteri (fun i block -> Block.Tbl.replace index block i) blocks;
  let successors =
    Array.map
      (fun block ->
        List.map (Block.Tbl.find index) (Block.non_exn_successors block))
      blocks
  in
  let predecessors = Array.make num_blocks [] in
  Array.iteri
    (fun i succs ->
      List.iter
        (fun succ -> predecessors.(succ) <- i :: predecessors.(succ))
        succs)
    successors;
  let is_exit i = match successors.(i) with [] -> true | _ :: _ -> false in
  let exits = List.filter is_exit (List.init num_blocks Fun.id) in
  (* Successors and predecessors of a node in the reverse graph. *)
  let reverse_successors node =
    if node = exit then exits else predecessors.(node)
  in
  let reverse_predecessors node =
    if is_exit node then [exit] else successors.(node)
  in
  let postorder, reverse_postorder =
    compute_postorder ~num_nodes:(num_blocks + 1) ~successors:reverse_successors
      ~root:exit
  in
  let immediate_post_dominator =
    compute_immediate_dominators ~predecessors:reverse_predecessors ~root:exit
      ~postorder ~reverse_postorder
  in
  (* In reverse postorder, a node's immediate post-dominator comes before it. *)
  let depth = Array.make (num_blocks + 1) 0 in
  List.iter
    (fun node ->
      if node <> exit
      then depth.(node) <- depth.(immediate_post_dominator.(node)) + 1)
    reverse_postorder;
  { index; immediate_post_dominator; depth }

let post_dominates t (a : finished Block.t) (b : finished Block.t) : bool =
  let a = Block.Tbl.find t.index a in
  let b = Block.Tbl.find t.index b in
  if t.immediate_post_dominator.(b) < 0
  then a = b
  else if t.immediate_post_dominator.(a) < 0
  then false
  else
    let rec climb node =
      if t.depth.(node) > t.depth.(a)
      then climb t.immediate_post_dominator.(node)
      else node
    in
    climb b = a
