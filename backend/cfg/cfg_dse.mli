[@@@ocaml.warning "+a-40-41-42"]

(** Dead store elimination over basic blocks: a store is deleted when a later
    store in the same block entirely overwrites the same location, with no
    intervening instruction that could observe the location's contents.

    This pass should be run after CSE ([Cfg_cse]): loads that CSE's
    store-to-load forwarding has satisfied have by then been rewritten into
    register moves, over which this pass can step, whereas an actual load acts
    as a barrier. The pass only runs when [-cfg-dse] is enabled (which
    [-experimental-optimizations] also does) and, like [Cfg_cse], it leaves
    functions carrying the [Cfg.No_CSE] codegen option untouched. *)
val run : Cfg_with_layout.t -> Cfg_with_layout.t
