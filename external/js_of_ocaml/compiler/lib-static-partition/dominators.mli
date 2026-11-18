(** Immediate dominators of a rooted directed graph, by the iterative algorithm of
    Cooper, Harvey and Kennedy. *)

val immediate_dominators : successors:(int -> int list) -> root:int -> int -> int
(** [immediate_dominators ~successors ~root] returns the immediate dominator of each
    node reachable from [root]. [root] is its own immediate dominator. Raises
    [Not_found] for nodes that are not reachable. *)
