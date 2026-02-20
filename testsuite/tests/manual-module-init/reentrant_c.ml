(* Reentrant_c: module initialized purely via re-entrant caml_init_module call *)
(* This module has NO dependency relationship with Reentrant_b *)
let () = Printf.printf "Reentrant_c: initializing (via re-entrant call)\n%!"

let magic_value = 777

let () = Printf.printf "Reentrant_c: initialized with magic_value=%d\n%!" magic_value

(* Register a callback so C code can get our value *)
let get_magic () = magic_value
let () = Callback.register "get_reentrant_c_magic" get_magic [@@alert "-unsafe_multidomain"]
