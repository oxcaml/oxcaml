[@@@ocaml.warning "+a-40-41-42"]

(** This pass is a basic implementation of copy propagation, that will remove
    temporaries used as (very) short-lived intermediaries. Since this pass
    deletes moves but makes register allocation slightly more difficult, the
    balance between the two aspects is not obvious. The current heuristics has
    been calibrated on the compiler distribution for the linscan and greedy
    allocators. Copy propagation should not be used with IRC; indeed, the
    allocation is made more difficult (resulting in both more compile-time work
    and spills/reloads) and there are basically no benefits in terms of move
    instructions since what copy propagation would achieve is performed by the
    "coalescing" part of IRC anyway. *)
val run : Cfg_with_infos.t -> Cfg_with_infos.t
