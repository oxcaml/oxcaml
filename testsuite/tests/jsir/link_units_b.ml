(* Paired with link_units.ml: re-exports symbols of Link_units_a, and defines
   symbols of its own, in a unit linked between the two. *)

let () = print_endline "init b"

let alias = Link_units_a.constant

let add3 x = Link_units_a.add x 3

let first = Link_units_a.incr_counter ()

let apply_twice f x = f (f x)
