[@@@ocaml.warning "+a-30-40-41-42"]

(** Interference graph for register allocation.

    This module manages the interference graph structure, tracking which
    registers interfere (cannot be assigned the same physical register).

    The graph maintains both an edge set (for fast membership testing) and
    adjacency lists (for iteration), along with degree information. This dual
    representation trades memory for performance - the duplication is
    intentional and necessary for the IRC algorithm's performance. *)

(** {1 Core types} *)

(** Undirected edge between registers *)
module Edge : sig
  type t

  val make : Reg.Stamp.t -> Reg.Stamp.t -> t

  val equal : t -> t -> bool

  val hash : t -> int

  val fst : t -> Reg.Stamp.t

  val snd : t -> Reg.Stamp.t

  val to_string : t -> string
end

(** Common interface for edge set implementations.

    This module type defines the operations supported by both EdgeSet (hash
    table-based) and BitMatrix (bit array-based) implementations. *)
module type S = sig
  type t

  val make : num_registers:int -> t

  val clear : t -> unit

  val mem : t -> Edge.t -> bool

  val add : t -> Edge.t -> unit

  val capacity : t -> int

  module For_debug : sig
    val cardinal : t -> int

    val iter : t -> f:(Edge.t -> unit) -> unit
  end
end

(** Legacy representation for edge sets.

    The implementation is based on a hash table built by applying `Hashtbl.Make`
    to the `Edge` module above. *)
module EdgeSet : S

(** Alternative bit matrix representation for edge sets.

    This module provides the same interface as EdgeSet but uses a compact bit
    matrix stored in a bytes value. Since the interference graph is symmetric,
    only the upper triangle is stored (edges where i < j).

    Memory usage: O(n²/8) bytes where n is the number of registers. This is more
    compact than EdgeSet for dense graphs.

    Trade-offs compared to EdgeSet:
    - Smaller memory footprint (n²/8 bytes vs hash table overhead)
    - Better cache locality for membership testing
    - O(n²) cardinal operation (vs O(1) for EdgeSet)
    - O(n²) iter operation (vs O(edges) for EdgeSet) *)
module BitMatrix : S

(** Degree tracking *)
module Degree : sig
  type t = int

  val infinite : t

  val to_string : t -> string

  val to_float : t -> float
end

(** {1 Interference graph} *)

(** The interference graph type.

    Internal representation:
    - adj_set: edge set for O(1) membership testing
    - adj_list: adjacency lists for iteration (DUPLICATES adj_set information)
    - degree: degree counts per register

    The duplication between adj_set and adj_list is intentional: adj_set
    provides fast membership testing while adj_list enables efficient iteration.
*)
type t

(** {2 Construction} *)

(** Create a new empty interference graph.

    The graph size is based on the current register stamp
    (Reg.For_testing.get_stamp). This ensures it can accommodate all registers
    that have been allocated so far. *)
val make : unit -> t

(** Clear all edges from the graph, resetting it to empty state.

    If new registers have been allocated since graph creation (detected via
    Reg.For_testing.get_stamp), the underlying BitMatrix representation (if
    used) will be reallocated to accommodate the larger stamp range. EdgeSet
    grows dynamically so no reallocation is needed. *)
val clear : t -> unit

(** [add_edge graph u v] adds an undirected edge between registers [u] and [v].

    This function:
    - Checks that u ≠ v
    - Filters out uninteresting registers (stack-allocated locals, etc.)
    - Only adds edges between registers of the same class
    - Updates both adj_set and adj_list
    - Increments degrees for both endpoints (unless degree is infinite)

    Note: Registers with infinite degree (precolored) do not have their
    adjacency lists updated. *)
val add_edge : t -> Reg.t -> Reg.t -> unit

(** [mem_edge graph u v] tests whether an edge exists between [u] and [v]. This
    is a fast O(1) operation using the edge set. *)
val mem_edge : t -> Reg.t -> Reg.t -> bool

(** {2 Adjacency queries} *)

(** [adj_list graph reg] returns the complete adjacency list for [reg]. This
    returns ALL neighbors with no filtering. *)
val adj_list : t -> Reg.t -> Reg.t list

(** [iter_adjacent graph reg ~f] iterates over all neighbors of [reg], applying
    [f] to each. No filtering is performed - visits all neighbors. *)
val iter_adjacent : t -> Reg.t -> f:(Reg.t -> unit) -> unit

(** [iter_adjacent_if graph reg ~should_visit ~f] iterates over neighbors of
    [reg], applying [f] only to neighbors where [should_visit] returns true.

    Example:
    {[
      iter_adjacent_if graph reg
        ~should_visit:(fun r ->
          match State.reg_work_list state r with
          | Select_stack | Coalesced -> false
          | _ -> true)
        ~f:(fun adj -> process adj)
    ]}

    This is the primary mechanism for IRC-specific filtering. *)
val iter_adjacent_if :
  t -> Reg.t -> should_visit:(Reg.t -> bool) -> f:(Reg.t -> unit) -> unit

(** [for_all_adjacent graph reg ~f] tests whether [f] holds for all neighbors.
    Returns [true] if [f] returns [true] for every neighbor. *)
val for_all_adjacent : t -> Reg.t -> f:(Reg.t -> bool) -> bool

(** [for_all_adjacent_if graph reg ~should_visit ~f] tests whether [f] holds for
    all visited neighbors (those where [should_visit] returns true).

    Returns [true] if [f] returns [true] for every visited neighbor. Neighbors
    that are filtered out (should_visit returns false) are implicitly treated as
    satisfying the predicate. *)
val for_all_adjacent_if :
  t -> Reg.t -> should_visit:(Reg.t -> bool) -> f:(Reg.t -> bool) -> bool

(** {2 Degree queries} *)

(** [degree graph reg] returns the degree of [reg] in the graph. The degree
    equals the number of edges incident to [reg]. *)
val degree : t -> Reg.t -> int

(** [set_degree graph reg d] directly sets the degree of [reg] to [d].

    WARNING: This should be used with extreme care as it can break graph
    invariants if used incorrectly. Only use when you know what you're doing
    (e.g., setting to 0 during cleanup, or setting to [Degree.infinite] for
    precolored registers). *)
val set_degree : t -> Reg.t -> int -> unit

(** [incr_degree graph reg] increments the degree of [reg] by 1. This operation
    does NOT affect edges - it only updates the degree counter. Typically used
    when adding edges in ways that bypass [add_edge]. *)
val incr_degree : t -> Reg.t -> unit

(** [decr_degree graph reg] decrements the degree of [reg] by 1. This operation
    does NOT affect edges - it only updates the degree counter. Typically used
    during IRC simplification phase. Does nothing if the degree is infinite. *)
val decr_degree : t -> Reg.t -> unit

(** {2 Initialization} *)

(** [init_register graph reg] initializes storage for [reg] in the graph. Sets
    empty adjacency list and degree 0.

    This must be called before [reg] can be used with other graph operations. *)
val init_register : t -> Reg.t -> unit

(** [init_register_with_infinite_degree graph reg] initializes [reg] with the
    infinite degree value.

    This is used for precolored registers. *)
val init_register_with_infinite_degree : t -> Reg.t -> unit

(** {2 Debugging} *)

module For_debug : sig
  (** [cardinal_edges graph] returns the total number of edges in the graph. *)
  val cardinal_edges : t -> int

  (** [iter_edges graph ~f] iterates over all edges in the graph, applying [f]
      to each edge of register stamps. *)
  val iter_edges : t -> f:(Edge.t -> unit) -> unit
end
