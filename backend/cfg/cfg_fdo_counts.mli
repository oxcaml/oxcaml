[@@@ocaml.warning "+a-40-41-42"]

(** Profile-derived execution frequencies of a function's blocks and edges: the
    input shared by the profile-guided optimizations of the CFG backend (block
    layout in [Cfg_fdo_layout], the call graph for the linker in
    [Cfg_fdo_call_graph]).

    The profile measures edges: the function's entry, and the conditional and
    switch edges whose pseudo-instrumentation counters the profiled build had (a
    counter whose functions have changed body since is unknown, not zero). The
    remaining edges are solved by flow conservation, exactly where the
    measurements determine them, and otherwise by the profile's estimates for
    counters it has in related inlining contexts and by spreading the flow
    evenly where it has none. A block executes as often as flow passes through
    it. Measurements the solved flow contradicts beyond a tolerance for sampling
    noise are a fatal error: the profile does not describe this code. *)

type t

val compute : Source_position_profile.t -> Cfg_with_layout.t -> t

(** Whether the profiled build compiled the function under this id. Without
    that, the profile says nothing about it. *)
val is_hot : t -> bool

(** The count of a block. *)
val block_count : t -> Label.t -> int64

(** The normal successor edges of a block, with their weights. *)
val successor_edge_weights : t -> Label.t -> (Label.t * int64) list

(** Print the function's body status and entry counters, every block's count,
    and the edges with their counters and weights, solved ones marked (the
    [-dfdo] flag). *)
val dump : Format.formatter -> t -> unit

val print_counter : Format.formatter -> Fdo_counter.t -> unit
