(* Reentrant_b: module that calls C stub which re-enters caml_init_module *)
let () = Printf.printf "Reentrant_b: initializing\n%!"

(* This C stub will call caml_init_module("Reentrant_c") - a module we don't depend on *)
external init_reentrant_c : unit -> int = "init_reentrant_c_stub"

let () = Printf.printf "Reentrant_b: calling C stub that will init Reentrant_c (no dependency)\n%!"

let magic_from_c = init_reentrant_c ()

let () = Printf.printf "Reentrant_b: C stub returned magic_value=%d from Reentrant_c\n%!" magic_from_c

(* Now also init Reentrant_a via C stub (Reentrant_a IS a dependency, so already initialized) *)
external init_reentrant_a : unit -> int = "init_reentrant_a_stub"

let () = Printf.printf "Reentrant_b: calling C stub that will init Reentrant_a (already a dependency)\n%!"

let result_from_a = init_reentrant_a ()

let () = Printf.printf "Reentrant_b: C stub for Reentrant_a returned %d\n%!" result_from_a

(* Use Reentrant_a's value directly (this creates the dependency) *)
let combined = magic_from_c + Reentrant_a.value_a + result_from_a

let () = Printf.printf "Reentrant_b: combined = %d\n%!" combined

let () = Printf.printf "Reentrant_b: initialization complete\n%!"
