[@@@ocaml.warning "+a-30-40-41-42"]

(** Interference graph for register allocation.

    This module manages the interference graph structure, tracking which
    registers interfere (cannot be assigned the same physical register).

    The graph maintains both an edge set (for fast membership testing) and
    adjacency lists (for iteration), along with degree information. This dual
    representation trades memory for performance - the duplication is
    intentional and necessary for the IRC algorithm's performance. *)

(** {1 Core types} *)

(** Register stamps - unique identifiers for registers *)
module RegisterStamp : sig
  type t = int

  type pair

  val pair : t -> t -> pair

  val fst : pair -> t

  val snd : pair -> t

  module PairSet : sig
    type t

    val make : num_registers:int -> t

    val clear : t -> unit

    val mem : t -> pair -> bool

    val add : t -> pair -> unit

    val cardinal : t -> int

    val iter : t -> f:(pair -> unit) -> unit
  end
end

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

    @param num_registers Initial capacity hint for the number of registers *)
val make : num_registers:int -> t

(** Clear all edges from the graph, resetting it to empty state. *)
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

(** {2 Bulk operations} *)

(** [adj_set graph] returns the underlying edge set.

    This is exposed for iteration and debugging purposes. Modifications to the
    returned set will affect the graph (no defensive copy is made). *)
val adj_set : t -> RegisterStamp.PairSet.t

(** [cardinal graph] returns the total number of edges in the graph. *)
val cardinal : t -> int

(** {2 Initialization} *)

(** [init_register graph reg] initializes storage for [reg] in the graph. Sets
    empty adjacency list and degree 0.

    This must be called before [reg] can be used with other graph operations. *)
val init_register : t -> Reg.t -> unit

(** [init_register_with_degree graph reg ~degree] initializes [reg] with a
    specific degree value.

    This is used for precolored registers which have infinite degree. *)
val init_register_with_degree : t -> Reg.t -> degree:int -> unit
