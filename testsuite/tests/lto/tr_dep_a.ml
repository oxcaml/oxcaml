(* A closure whose value slots may be projected by modules that only see it
   through [Tr_dep_b]. *)

let p = Sys.opaque_identity 7

let q = Sys.opaque_identity 5

let[@inline always] scale n = (n * p) + q
