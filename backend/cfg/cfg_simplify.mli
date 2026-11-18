[@@@ocaml.warning "+a-40-41-42"]

(** Merge blocks and eliminate dead blocks, including unreachable trap handlers.
    On return, every block is reachable from the entry through normal or
    exceptional edges. Trap instructions referring to removed handlers are
    deleted and stack offsets are updated. *)
val run : Cfg_with_layout.t -> Cfg_with_layout.t
