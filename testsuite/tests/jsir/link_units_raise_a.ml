(* Paired with link_units_raise.ml: a unit whose initialisation always raises,
   so that the units linked after it never run. *)

let () = print_endline "init a"

let value = if Sys.opaque_identity true then failwith "init failed" else 1
