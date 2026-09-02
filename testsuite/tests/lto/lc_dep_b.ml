(* Force the projections of [Lc_dep_a]'s value slots to be compiled into
   dependent modules by inlining. *)
let[@inline always] via n = Lc_dep_a.used n
