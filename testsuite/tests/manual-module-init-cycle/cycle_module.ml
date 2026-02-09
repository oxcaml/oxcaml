(* Cycle_module: module that tries to re-init itself via C stub, triggering cycle detection *)
let () = Printf.printf "Cycle_module: initializing\n%!"

external try_init_self : unit -> int = "try_init_self_stub"

let () = Printf.printf "Cycle_module: calling C stub that will try to init Cycle_module\n%!"

let () =
  match try_init_self () with
  | result ->
    Printf.printf "Cycle_module: ERROR - should have detected cycle! result=%d\n%!" result
  | exception Failure msg ->
    Printf.printf "Cycle_module: caught cycle exception: Failure(\"%s\")\n%!" msg
