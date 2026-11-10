[@@@ocaml.warning "+a-40-41-42"]

(** Dominator-related utility functions. *)

type dominator_tree = private
  { label : Label.t;
    children : dominator_tree list
  }

type t

(** Computes immediate dominators, dominance frontiers, and the dominator tree.
    All blocks must be reachable from the CFG's entry through normal or
    exceptional edges. *)
val build : Cfg.t -> t

(** [is_dominating doms x y] is [true] iff [x] is dominating [y] according to
    [doms]. That is, all paths from the entry node to [y] go through [x]. All
    edges, regular and exceptional are treated the same way. *)
val is_dominating : t -> Label.t -> Label.t -> bool

(** [is_strictly_dominating doms x y] is [true] iff [x] is strictly dominating
    [y] according to [doms]. That is, [is_dominating doms x y = true] and [x] is
    not equal [y]. All edges, regular and exceptional are treated the same way.*)
val is_strictly_dominating : t -> Label.t -> Label.t -> bool

(** [find_dominance_frontier doms label] returns the dominance frontier for
    [label] according to [doms]. The definition we use is the following: "the
    dominance frontier of a node n is the set of all nodes m such that n
    dominates a predecessor of m, but does not strictly dominate m itself". *)
val find_dominance_frontier : t -> Label.t -> Label.Set.t

(** Returns the dominator tree rooted at the CFG's entry. *)
val dominator_tree : t -> dominator_tree

(** Visits the labels of a dominator tree in breadth-first order. *)
val iter_breadth_dominator_tree : dominator_tree -> f:(Label.t -> unit) -> unit
