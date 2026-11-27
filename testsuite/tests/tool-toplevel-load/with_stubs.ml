external stub : unit -> int = "toplevel_load_stub"

let () = Printf.printf "archive loaded, stub returned %d\n%!" (stub ())
