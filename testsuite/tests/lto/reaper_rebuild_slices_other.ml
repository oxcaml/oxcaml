(* Not referenced by the other units. *)
let[@inline never] independent x = x * 3

let () = ignore (Sys.opaque_identity (independent 14) : int)
