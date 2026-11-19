[@@@ocaml.warning "+a-40-41-42"]

(** Normalized graph inputs for the CFG shape validators. *)
type t

(** Every byte string is a valid input; there is no reject path.

    Input format:
    - Bytes 0-1 are an unsigned 16-bit little-endian seed for the node count:
      [node_count = 1 + (seed mod max_nodes)]. Fewer than two bytes means a
      one-node graph.
    - The remaining bytes are 4-byte edge records. The source is unsigned 16-bit
      little-endian, with its top bit marking an exceptional edge; the target is
      unsigned 16-bit little-endian. After removing the flag bit, both endpoints
      are taken modulo [node_count]. A trailing partial record is ignored.

    Normalization is deterministic: edges into node 0 are dropped (the entry
    block must have no predecessors and cannot be a trap handler), duplicate
    edges are dropped, and only the first exceptional successor of a node is
    kept. A node with an exceptional successor keeps at most its first normal
    successor, so that a raising terminator fits.

    Parsing retains unreachable nodes. The harness selects the nodes reachable
    from the entry before validation to satisfy the validators' no-dead-code
    precondition.

    Requires [max_nodes > 0]. *)
val parse : max_nodes:int -> string -> t

(** Selects every node, including those unreachable from the entry. *)
val keep_all_nodes : t -> bool array

(** Selects nodes reachable from node 0 over normal and exceptional edges. *)
val reachable_from_entry : t -> bool array

(** Prints the selected graph. [keep] must be an unchanged mask returned by
    [keep_all_nodes] or [reachable_from_entry] for this graph. *)
val print_dot : Format.formatter -> t -> keep:bool array -> unit

(** Builds a body-less CFG from the selected graph. [keep] has the same
    requirements as for [print_dot]. *)
val to_cfg : t -> keep:bool array -> Cfg.t
