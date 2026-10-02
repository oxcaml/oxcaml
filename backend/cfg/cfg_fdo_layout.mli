[@@@ocaml.warning "+a-40-41-42"]

(** Profile-guided basic-block layout.

    [reorder_blocks counts cl] lays the blocks out so that the hottest edges
    become fallthroughs, loops are closed by backwards conditional branches, and
    cold blocks sink to the end (the entry block stays first), given the block
    counts and edge weights of [Cfg_fdo_counts], by ext-TSP ([Cfg_fdo_ext_tsp]).
    A function the profile does not cover ([Cfg_fdo_counts.is_hot]) is left
    untouched. *)
val reorder_blocks : Cfg_fdo_counts.t -> Cfg_with_layout.t -> unit
