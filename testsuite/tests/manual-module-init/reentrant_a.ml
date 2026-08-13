(* Reentrant_a: module that will be initialized via re-entrant call *)
let () = Printf.printf "Reentrant_a: initializing\n%!"

let value_a = 999

let () = Printf.printf "Reentrant_a: initialized with value_a=%d\n%!" value_a
