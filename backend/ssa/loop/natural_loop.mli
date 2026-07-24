[@@@ocaml.warning "+a-40-41-42"]

(** Natural-loop discovery over an abstract directed graph.

    This module is pure graph theory: it knows nothing about SSA values or
    instruction semantics, only nodes, edges and a dominance oracle. It is
    instantiated by {!Induction_var} with the SSA block graph.

    A loop's header is a node to which a back edge points (an edge [u -> v]
    where [v] dominates [u]); the loop's body is the header plus every node that
    can reach a back-edge source without passing through the header. *)

module type Graph = sig
  type node

  module Set : Set.S with type elt = node

  module Tbl : Hashtbl.S with type key = node

  val equal : node -> node -> bool

  (** All nodes of the graph. *)
  val nodes : node list

  val successors : node -> node list

  val predecessors : node -> node list

  (** [dominates a b] is [true] iff every path from the graph's entry to [b]
      passes through [a]. *)
  val dominates : node -> node -> bool
end

module Make (G : Graph) : sig
  type loop =
    { header : G.node;
      body : G.Set.t;
      back_edges : G.node list
          (** The back-edge {e sources} (nodes with an edge to [header]). *)
    }

  (** The natural loop of every back edge in the graph, grouped by header. *)
  val find_loops : unit -> loop list

  (** [edge_dominates ~src ~succ ~target] is a sufficient condition for the edge
      [src -> succ] to dominate [target], i.e. for every path from the graph's
      entry to [target] to traverse that specific edge: [succ] dominates
      [target] (all paths pass through [succ]) and [src] is [succ]'s only
      predecessor (the only way into [succ] is that edge). Node-dominance of
      [succ] alone would not be enough: control could reach [succ] via another
      in-edge when paths reconverge. *)
  val edge_dominates : src:G.node -> succ:G.node -> target:G.node -> bool
end
