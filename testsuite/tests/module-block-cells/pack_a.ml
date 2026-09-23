(* First member of [Cellpack]: a dynamic field and a mutable one. *)

let x = Sys.opaque_identity 11
let r = ref 0
let () = r := x + 1
