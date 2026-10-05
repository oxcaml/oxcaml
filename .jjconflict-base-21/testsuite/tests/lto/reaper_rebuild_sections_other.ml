(* Not referenced by the other units; its section must not be loaded when
   rebuilding them, and vice versa. *)
let[@inline never] independent x = x * 3

let () = ignore (Sys.opaque_identity (independent 14) : int)
