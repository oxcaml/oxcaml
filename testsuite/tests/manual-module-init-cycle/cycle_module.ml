(* Cycle_module: module that tries to re-init itself via C stub, triggering cycle detection *)
let () = Printf.printf "Cycle_module: initializing\n%!"

external try_init_self : unit -> int = "try_init_self_stub"

let () = Printf.printf "Cycle_module: calling C stub that will try to init Cycle_module\n%!"

(* This will trigger cycle detection and abort *)
let result = try_init_self ()

(* Should not reach here *)
let () = Printf.printf "Cycle_module: ERROR - should have aborted! result=%d\n%!" result
