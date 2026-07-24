[@@@ocaml.warning "+a-4-40-41-42-44"]

(* See [natural_loop.mli] for the interface. *)

module type Graph = sig
  type node

  module Set : Set.S with type elt = node

  module Tbl : Hashtbl.S with type key = node

  val equal : node -> node -> bool

  val nodes : node list

  val successors : node -> node list

  val predecessors : node -> node list

  val dominates : node -> node -> bool
end

module Make (G : Graph) = struct
  (* An edge u -> v is a back edge iff v dominates u. The natural loop of such
     an edge consists of the header v plus every node that can reach u in the
     graph without passing through v. *)

  type loop =
    { header : G.node;
      body : G.Set.t;
      back_edges : G.node list
    }

  let natural_loop_body ~header ~back_preds =
    let body = ref (G.Set.singleton header) in
    let rec walk worklist =
      match worklist with
      | [] -> ()
      | bl :: rest ->
        let ps = G.predecessors bl in
        let added = ref rest in
        List.iter
          (fun p ->
            if not (G.Set.mem p !body)
            then (
              body := G.Set.add p !body;
              added := p :: !added))
          ps;
        walk !added
    in
    let seeds =
      List.filter
        (fun bp ->
          if G.Set.mem bp !body
          then false
          else (
            body := G.Set.add bp !body;
            true))
        back_preds
    in
    walk seeds;
    !body

  let find_loops () : loop list =
    let header_tbl : G.node list G.Tbl.t = G.Tbl.create 8 in
    List.iter
      (fun bl ->
        List.iter
          (fun succ ->
            if G.dominates succ bl
            then
              let existing =
                match G.Tbl.find_opt header_tbl succ with
                | Some l -> l
                | None -> []
              in
              G.Tbl.replace header_tbl succ (bl :: existing))
          (G.successors bl))
      G.nodes;
    G.Tbl.fold
      (fun header back_preds acc ->
        let body = natural_loop_body ~header ~back_preds in
        { header; body; back_edges = back_preds } :: acc)
      header_tbl []

  let edge_dominates ~src ~succ ~target =
    G.dominates succ target
    &&
    match G.predecessors succ with
    | [p] -> G.equal p src
    | [] | _ :: _ :: _ -> false
end
