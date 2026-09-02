(* Re-expose [Tr_dep_a.scale] so that the main module's uses of it, and hence
   its needs for [Tr_dep_a]'s metadata, arrive via this module. *)
let[@inline always] get () = Tr_dep_a.scale
