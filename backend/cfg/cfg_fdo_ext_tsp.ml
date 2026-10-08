[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module Int_tbl = Hashtbl.Make (Int)

(* The ext-TSP block layout of Newell and Pupyrev, "Improved Basic Block
   Reordering" (IEEE Transactions on Computers, 2020), as implemented in LLVM's
   CodeLayout.cpp (which is what BOLT uses). Every block starts as a chain of
   its own; pairs of blocks forced together (single successor, single
   predecessor) are merged first; then, repeatedly, the pair of chains whose
   merge increases the score the most is merged, trying not only the
   concatenation but also splitting the first chain around the second, until no
   merge gains anything; finally the chains are laid out by decreasing density,
   the entry block's chain first.

   The score of a layout is the sum over the jumps of their count times a factor
   depending on the distance from the end of the source block to the target: 1
   for a fallthrough (1.05 for an unconditional one, so that removing a jump
   counts a little more than orienting a conditional branch), 0.1 decaying
   linearly to 0 over [forward_distance] bytes for a forward jump and over
   [backward_distance] bytes for a backward one, 0 beyond. Block sizes are
   estimates (bytes per instruction), BOLT having the real ones. *)

let forward_weight_cond = 0.1

let forward_weight_uncond = 0.1

let backward_weight_cond = 0.1

let backward_weight_uncond = 0.1

let fallthrough_weight_cond = 1.0

let fallthrough_weight_uncond = 1.05

let forward_distance = 1024

let backward_distance = 640

(* The maximum size of a chain for splitting it at every position. *)
let chain_split_threshold = 128

(* The maximum size of a chain. *)
let max_chain_size = 4096

let eps = 1e-8

let jump_score ~src_addr ~src_size ~dst_addr ~count ~conditional =
  let count = Int64.to_float count in
  if src_addr + src_size = dst_addr
  then
    count
    *.
    if conditional then fallthrough_weight_cond else fallthrough_weight_uncond
  else if src_addr < dst_addr
  then
    let dist = dst_addr - (src_addr + src_size) in
    if dist <= forward_distance
    then
      count
      *. (1. -. (float_of_int dist /. float_of_int forward_distance))
      *. if conditional then forward_weight_cond else forward_weight_uncond
    else 0.
  else
    let dist = src_addr + src_size - dst_addr in
    if dist <= backward_distance
    then
      count
      *. (1. -. (float_of_int dist /. float_of_int backward_distance))
      *. if conditional then backward_weight_cond else backward_weight_uncond
    else 0.

type node =
  { index : int; (* 0 is the entry *)
    label : Label.t;
    size : int;
    mutable count : int64;
    mutable chain : int;
    mutable pos : int; (* in its chain *)
    mutable forced_succ : node option;
    mutable forced_pred : node option;
    mutable succs : node list; (* all successors, self excluded *)
    mutable preds : node list;
    mutable out_jumps : jump list; (* the edges of positive count *)
    mutable in_jumps : jump list
  }

and jump =
  { src : node;
    dst : node;
    jcount : int64;
    conditional : bool
  }

type merge_type =
  | X_Y
  | X1_Y_X2
  | Y_X2_X1
  | X2_X1_Y

type gain =
  { score : float;
    offset : int;
    merge_type : merge_type
  }

let no_gain = { score = 0.; offset = 0; merge_type = X_Y }

type chain =
  { id : int;
    mutable nodes : node array;
    mutable ccount : int64; (* sum of the nodes' counts *)
    mutable csize : int;
    mutable cscore : float; (* over the jumps within the chain *)
    mutable edges : (int * chain_edge) list (* by other chain, oldest first *)
  }

and chain_edge =
  { mutable c1 : int; (* one of the two chains *)
    mutable jumps : jump list; (* between the two chains, both directions *)
    mutable gain1 : gain option; (* cached gain of merging with [c1] first *)
    mutable gain2 : gain option (* ... with the other chain first *)
  }

let is_entry_chain chain = chain.nodes.(0).index = 0

(* The merged sequence of nodes: [x] split at [offset] into [x1] and [x2],
   arranged with [y] as [merge_type] says. *)
let merge_nodes (x : node array) (y : node array) ~offset merge_type =
  let x1 = Array.sub x 0 offset in
  let x2 = Array.sub x offset (Array.length x - offset) in
  match merge_type with
  | X_Y -> Array.concat [x; y]
  | X1_Y_X2 -> Array.concat [x1; y; x2]
  | Y_X2_X1 -> Array.concat [y; x2; x1]
  | X2_X1_Y -> Array.concat [x2; x1; y]

(* The score of [jumps] when the nodes are laid out in the order of [nodes] (all
   the jumps' endpoints must be among them). *)
let layout_score (nodes : node array) (jumps : jump list) =
  let addr = Int_tbl.create (Array.length nodes) in
  let cur = ref 0 in
  Array.iter
    (fun node ->
      Int_tbl.replace addr node.index !cur;
      cur := !cur + node.size)
    nodes;
  List.fold_left
    (fun acc jump ->
      acc
      +. jump_score
           ~src_addr:(Int_tbl.find addr jump.src.index)
           ~src_size:jump.src.size
           ~dst_addr:(Int_tbl.find addr jump.dst.index)
           ~count:jump.jcount ~conditional:jump.conditional)
    0. jumps

let layout ~(blocks : (Label.t * int * int64) array)
    ~(edges : (Label.t * Label.t * int64) list) : Label.t list =
  let num_nodes = Array.length blocks in
  let chains : chain Int_tbl.t = Int_tbl.create num_nodes in
  let chain id = Int_tbl.find chains id in
  let nodes =
    Array.mapi
      (fun index (label, size, count) ->
        { index;
          label;
          size = max size 1;
          count;
          chain = index;
          pos = 0;
          forced_succ = None;
          forced_pred = None;
          succs = [];
          preds = [];
          out_jumps = [];
          in_jumps = []
        })
      blocks
  in
  let by_label = Label.Tbl.create num_nodes in
  Array.iter (fun node -> Label.Tbl.replace by_label node.label node) nodes;
  (* Edges, oldest first. *)
  let edges = List.rev edges in
  List.iter
    (fun (src, dst, _) ->
      let src = Label.Tbl.find by_label src
      and dst = Label.Tbl.find by_label dst in
      if not (Label.equal src.label dst.label)
      then (
        src.succs <- dst :: src.succs;
        dst.preds <- src :: dst.preds))
    edges;
  let jumps =
    List.filter_map
      (fun (src, dst, count) ->
        let src = Label.Tbl.find by_label src
        and dst = Label.Tbl.find by_label dst in
        if Label.equal src.label dst.label || Int64.compare count 0L <= 0
        then None
        else (
          src.count <- Int64.max src.count count;
          dst.count <- Int64.max dst.count count;
          Some
            { src;
              dst;
              jcount = count;
              conditional = List.length src.succs > 1
            }))
      edges
  in
  List.iter
    (fun jump ->
      jump.src.out_jumps <- jump :: jump.src.out_jumps;
      jump.dst.in_jumps <- jump :: jump.dst.in_jumps)
    (List.rev jumps);
  Array.iter
    (fun node ->
      Int_tbl.replace chains node.index
        { id = node.index;
          nodes = [| node |];
          ccount = node.count;
          csize = node.size;
          cscore = 0.;
          edges = []
        })
    nodes;
  let find_edge (c : chain) other = List.assoc_opt other c.edges in
  let add_edge (c : chain) other edge = c.edges <- c.edges @ [other, edge] in
  List.iter
    (fun jump ->
      let a = jump.src.chain and b = jump.dst.chain in
      match find_edge (chain a) b with
      | Some edge -> edge.jumps <- edge.jumps @ [jump]
      | None ->
        let edge = { c1 = a; jumps = [jump]; gain1 = None; gain2 = None } in
        add_edge (chain a) b edge;
        add_edge (chain b) a edge)
    jumps;
  (* The jumps within a chain. *)
  let internal_jumps (c : chain) =
    Array.fold_left
      (fun acc node ->
        List.fold_left
          (fun acc jump -> if jump.dst.chain = c.id then jump :: acc else acc)
          acc node.out_jumps)
      [] c.nodes
    |> List.rev
  in
  let merge_chains (into : chain) (from : chain) ~offset merge_type =
    into.nodes <- merge_nodes into.nodes from.nodes ~offset merge_type;
    Array.iteri
      (fun pos node ->
        node.chain <- into.id;
        node.pos <- pos)
      into.nodes;
    into.ccount <- Int64.add into.ccount from.ccount;
    into.csize <- into.csize + from.csize;
    into.cscore <- layout_score into.nodes (internal_jumps into);
    into.edges <- List.remove_assoc from.id into.edges;
    List.iter
      (fun (other_id, edge) ->
        if other_id <> into.id
        then (
          let other = chain other_id in
          other.edges <- List.remove_assoc from.id other.edges;
          match find_edge into other_id with
          | Some existing -> existing.jumps <- existing.jumps @ edge.jumps
          | None ->
            if edge.c1 = from.id then edge.c1 <- into.id;
            add_edge into other_id edge;
            add_edge other into.id edge))
      from.edges;
    List.iter
      (fun (_, edge) ->
        edge.gain1 <- None;
        edge.gain2 <- None)
      into.edges;
    Int_tbl.remove chains from.id
  in
  (* Pairs of blocks that are the only successor and predecessor of each other
     are merged first; the entry is never a successor. Cycles in these
     dependencies (profiles are not exact) are broken at the node of smallest
     index. *)
  Array.iter
    (fun node ->
      match node.succs with
      | [succ]
        when (match succ.preds with [_] -> true | [] | _ :: _ :: _ -> false)
             && succ.index <> 0 ->
        node.forced_succ <- Some succ;
        succ.forced_pred <- Some node
      | [] | _ :: _ -> ())
    nodes;
  Array.iter
    (fun node ->
      match node.forced_succ, node.forced_pred with
      | Some succ, Some pred ->
        let rec in_cycle (cur : node option) =
          match cur with
          | None -> false
          | Some cur -> cur.index = node.index || in_cycle cur.forced_succ
        in
        if in_cycle (Some succ)
        then (
          pred.forced_succ <- None;
          node.forced_pred <- None)
      | (Some _ | None), (Some _ | None) -> ())
    nodes;
  Array.iter
    (fun node ->
      if Option.is_none node.forced_pred
      then
        let rec follow (cur : node) =
          match cur.forced_succ with
          | Some next ->
            merge_chains (chain node.chain) (chain next.chain) ~offset:0 X_Y;
            follow next
          | None -> ()
        in
        follow node)
    nodes;
  (* The gain of merging [succ] into [pred], the best over the merge types and
     split positions considered. *)
  let compute_merge_gain (pred : chain) (succ : chain) jumps ~offset merge_type
      =
    let merged = merge_nodes pred.nodes succ.nodes ~offset merge_type in
    if (is_entry_chain pred || is_entry_chain succ) && merged.(0).index <> 0
    then no_gain
    else
      { score = layout_score merged jumps -. pred.cscore; offset; merge_type }
  in
  let best_merge_gain (pred : chain) (succ : chain) (edge : chain_edge) =
    let cached = if edge.c1 = pred.id then edge.gain1 else edge.gain2 in
    match cached with
    | Some gain -> gain
    | None ->
      let jumps = edge.jumps @ internal_jumps pred in
      let best = ref no_gain in
      let consider gain =
        if Float.compare gain.score !best.score > 0 then best := gain
      in
      let try_merging ~offset merge_types =
        if
          offset <> 0
          && offset <> Array.length pred.nodes
          && Option.is_none pred.nodes.(offset - 1).forced_succ
        then
          List.iter
            (fun merge_type ->
              consider (compute_merge_gain pred succ jumps ~offset merge_type))
            merge_types
      in
      consider (compute_merge_gain pred succ jumps ~offset:0 X_Y);
      (* Splitting [pred] along the jumps into the head and out of the tail of
         [succ]. *)
      List.iter
        (fun jump ->
          if jump.src.chain = pred.id
          then try_merging ~offset:(jump.src.pos + 1) [X1_Y_X2; X2_X1_Y])
        succ.nodes.(0).in_jumps;
      List.iter
        (fun jump ->
          if jump.dst.chain = pred.id
          then try_merging ~offset:jump.dst.pos [X1_Y_X2; Y_X2_X1])
        succ.nodes.(Array.length succ.nodes - 1).out_jumps;
      if Array.length pred.nodes <= chain_split_threshold
      then
        for offset = 1 to Array.length pred.nodes - 1 do
          try_merging ~offset [X1_Y_X2; Y_X2_X1; X2_X1_Y]
        done;
      if edge.c1 = pred.id
      then edge.gain1 <- Some !best
      else edge.gain2 <- Some !best;
      !best
  in
  (* On ties, the pair with fewer samples, then the lower identifiers. *)
  let prefer (pred1 : chain) (succ1 : chain) (pred2 : chain) (succ2 : chain) =
    let samples1 = Int64.add pred1.ccount succ1.ccount
    and samples2 = Int64.add pred2.ccount succ2.ccount in
    if not (Int64.equal samples1 samples2)
    then Int64.compare samples1 samples2 < 0
    else if pred1.id <> pred2.id
    then pred1.id < pred2.id
    else succ1.id < succ2.id
  in
  let hot =
    ref
      (List.filter
         (fun (c : chain) -> Int64.compare c.ccount 0L > 0)
         (List.init num_nodes (fun i -> i)
         |> List.filter_map (Int_tbl.find_opt chains)))
  in
  let rec merge_pairs () =
    let best = ref None in
    List.iter
      (fun (pred : chain) ->
        List.iter
          (fun (succ_id, edge) ->
            if succ_id <> pred.id
            then
              let succ = chain succ_id in
              if
                Array.length pred.nodes + Array.length succ.nodes
                < max_chain_size
              then
                let gain = best_merge_gain pred succ edge in
                let better =
                  match !best with
                  | None -> true
                  | Some (best_pred, best_succ, best_gain) ->
                    Float.compare gain.score best_gain.score > 0
                    || Float.compare
                         (Float.abs (gain.score -. best_gain.score))
                         eps
                       < 0
                       && prefer pred succ best_pred best_succ
                in
                if better then best := Some (pred, succ, gain))
          pred.edges)
      !hot;
    match !best with
    | Some (pred, succ, gain) when Float.compare gain.score eps > 0 ->
      merge_chains pred succ ~offset:gain.offset gain.merge_type;
      hot := List.filter (fun (c : chain) -> c.id <> succ.id) !hot;
      merge_pairs ()
    | Some _ | None -> ()
  in
  merge_pairs ();
  (* The entry's chain first, then by decreasing density, ties by identifier. *)
  let density (c : chain) = Int64.to_float c.ccount /. float_of_int c.csize in
  let remaining =
    List.init num_nodes (fun i -> i)
    |> List.filter_map (Int_tbl.find_opt chains)
  in
  let sorted =
    List.stable_sort
      (fun (l : chain) (r : chain) ->
        match is_entry_chain l, is_entry_chain r with
        | true, false -> -1
        | false, true -> 1
        | (true | false), _ ->
          let c = Float.compare (density r) (density l) in
          if c <> 0 then c else Int.compare l.id r.id)
      remaining
  in
  List.concat_map
    (fun (c : chain) ->
      Array.to_list (Array.map (fun node -> node.label) c.nodes))
    sorted
