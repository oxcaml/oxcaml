(* A unit whose initialiser never returns. Its cells are defined in the
   initialiser's return continuation, which the compiler drops, so it defines
   placeholders for the clients' references instead. *)

let f x = x + 1
let n = Sys.opaque_identity 2
let r = ref 3
let () = print_endline "raising_lib: initialising"; raise Exit
