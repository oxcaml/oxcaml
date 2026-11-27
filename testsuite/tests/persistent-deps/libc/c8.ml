(* A's description is first looked up in the load path for A.f, then through
   the location recorded in b.cmx when inlining B.h. *)
let y = A.f 0

let z = B.h 1
