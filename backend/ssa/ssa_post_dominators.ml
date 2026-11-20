open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

open Ssa.Export

(* Post-dominators are the dominators of the reverse graph, rooted at a virtual
   exit node that every exit block flows into. They are computed using the same
   algorithm used in `Cfg_dominators`. *)

(* CR-soon xclerc for xclerc: see what can be shared with the computations of
   dominators (CFG and SSA). More precisely, the goal is to share along two
   dimensions: IR (CFG vs SSA) and kind ("regular" vs "post-"). The refactoring
   will happen after we merge the AFL fuzzer stress testing the CFG version. *)

module Node = struct
  type t =
    | Block of finished Block.t
    | Exit

  let equal a b =
    match a, b with
    | Block a, Block b -> Block.equal a b
    | Exit, Exit -> true
    | Block _, Exit | Exit, Block _ -> false

  let hash = function Block block -> Block.hash block | Exit -> 0

  module Tbl = Hashtbl.Make (struct
    type nonrec t = t

    let equal = equal

    let hash = hash
  end)
end

type t =
  { immediate_post_dominator : Node.t Node.Tbl.t;
        (* No entry for blocks from which no exit can be reached. *)
    depth : int Node.Tbl.t
        (* Depth in the post-dominator tree, the virtual exit being at [0]. No
           entry for blocks from which no exit can be reached. *)
  }

(* Depth-first search from [root] of the graph with the given [successors].
   Returns the postorder number of each reachable node, and the reachable nodes
   in reverse postorder. Postorder numbers increase as DFS finishes each node,
   after its children in the DFS tree. Reverse postorder lists nodes in
   decreasing postorder number. *)
let compute_postorder ~num_nodes ~(successors : Node.t -> Node.t list) ~root :
    int Node.Tbl.t * Node.t list =
  let postorder = Node.Tbl.create num_nodes in
  let reverse_postorder = ref [] in
  let visited = Node.Tbl.create num_nodes in
  let stack = Stack.create () in
  let next_number = ref 0 in
  Node.Tbl.add visited root ();
  Stack.push (root, successors root) stack;
  while not (Stack.is_empty stack) do
    match Stack.pop stack with
    | node, [] ->
      Node.Tbl.add postorder node !next_number;
      incr next_number;
      reverse_postorder := node :: !reverse_postorder
    | node, child :: children ->
      Stack.push (node, children) stack;
      if not (Node.Tbl.mem visited child)
      then begin
        Node.Tbl.add visited child ();
        Stack.push (child, successors child) stack
      end
  done;
  postorder, !reverse_postorder

(* Immediate dominators of the graph with the given [predecessors], rooted at
   [root], where [postorder] and [reverse_postorder] are the result of
   [compute_postorder] on that graph. Returns the immediate dominator of each
   reachable node, with [root] for the root itself. *)
let compute_immediate_dominators ~(predecessors : Node.t -> Node.t list) ~root
    ~postorder ~reverse_postorder : Node.t Node.Tbl.t =
  let immediate_dominator = Node.Tbl.create (Node.Tbl.length postorder) in
  Node.Tbl.add immediate_dominator root root;
  let rec intersect a b =
    if Node.equal a b
    then a
    else if Node.Tbl.find postorder a < Node.Tbl.find postorder b
    then intersect (Node.Tbl.find immediate_dominator a) b
    else intersect a (Node.Tbl.find immediate_dominator b)
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun node ->
        if not (Node.equal node root)
        then begin
          let new_immediate_dominator =
            List.fold_left
              (fun acc pred ->
                if not (Node.Tbl.mem immediate_dominator pred)
                then acc
                else
                  Some
                    (match acc with
                    | None -> pred
                    | Some acc -> intersect pred acc))
              None (predecessors node)
          in
          match new_immediate_dominator with
          | None -> ()
          | Some new_immediate_dominator ->
            if
              not
                (Option.equal Node.equal (Some new_immediate_dominator)
                   (Node.Tbl.find_opt immediate_dominator node))
            then begin
              Node.Tbl.replace immediate_dominator node new_immediate_dominator;
              changed := true
            end
        end)
      reverse_postorder
  done;
  immediate_dominator

let compute (graph : finished Ssa.graph) : t =
  let nodes =
    Node.Exit :: List.map (fun block -> Node.Block block) (Ssa.blocks graph)
  in
  let num_nodes = List.length nodes in
  let successors = Node.Tbl.create num_nodes in
  let predecessors = Node.Tbl.create num_nodes in
  List.iter (fun node -> Node.Tbl.add predecessors node []) nodes;
  List.iter
    (fun node ->
      let succs =
        match node with
        | Node.Exit -> []
        | Node.Block block -> (
          match Block.non_exn_successors block with
          | [] -> [Node.Exit]
          | blocks -> List.map (fun block -> Node.Block block) blocks)
      in
      Node.Tbl.add successors node succs;
      List.iter
        (fun succ ->
          let preds = Node.Tbl.find predecessors succ in
          Node.Tbl.replace predecessors succ (node :: preds))
        succs)
    nodes;
  (* Successors and predecessors of a node in the reverse graph. *)
  let reverse_successors node = Node.Tbl.find predecessors node in
  let reverse_predecessors node = Node.Tbl.find successors node in
  let postorder, reverse_postorder =
    compute_postorder ~num_nodes ~successors:reverse_successors ~root:Node.Exit
  in
  let immediate_post_dominator =
    compute_immediate_dominators ~predecessors:reverse_predecessors
      ~root:Node.Exit ~postorder ~reverse_postorder
  in
  (* In reverse postorder, a node's immediate post-dominator comes before it. *)
  let depth = Node.Tbl.create (Node.Tbl.length postorder) in
  Node.Tbl.add depth Node.Exit 0;
  List.iter
    (fun node ->
      if not (Node.equal node Node.Exit)
      then begin
        let parent = Node.Tbl.find immediate_post_dominator node in
        Node.Tbl.add depth node (Node.Tbl.find depth parent + 1)
      end)
    reverse_postorder;
  { immediate_post_dominator; depth }

let post_dominates t (a : finished Block.t) (b : finished Block.t) : bool =
  let a = Node.Block a in
  let b = Node.Block b in
  match Node.Tbl.find_opt t.depth a, Node.Tbl.find_opt t.depth b with
  | None, _ | _, None -> Node.equal a b
  | Some depth_a, Some _ ->
    let rec climb node =
      if Node.Tbl.find t.depth node > depth_a
      then climb (Node.Tbl.find t.immediate_post_dominator node)
      else node
    in
    Node.equal (climb b) a
