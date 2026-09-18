(* Uses B which transitively depends on A.
   Typechecks only when the correct version of A's cmi is visible. *)
let x = B.x + 1

let () = Printf.printf "%d\n" x
