(* Ocaml_init: Tests calling caml_init_module_from_ocaml from OCaml code *)

(* Note: We intentionally don't have compile-time dependencies on Reentrant_c
   or Reentrant_a, so we can test initializing them via init_module. *)

external init_module : string -> unit = "caml_init_module_from_ocaml"

(* C stub to get the magic value from Reentrant_c after it's initialized *)
external get_reentrant_c_magic : unit -> int = "get_reentrant_c_magic_stub"

let () = Printf.printf "Ocaml_init: initializing\n%!"

(* Initialize Reentrant_c from OCaml - it's not a compile-time dependency *)
let () = Printf.printf "Ocaml_init: calling init_module \"Reentrant_c\" from OCaml\n%!"
let () = init_module "Reentrant_c"
let () = Printf.printf "Ocaml_init: Reentrant_c initialized from OCaml\n%!"

(* Get the magic value via C stub to verify it's initialized *)
let magic = get_reentrant_c_magic ()
let () = Printf.printf "Ocaml_init: got Reentrant_c magic_value = %d via C stub\n%!" magic

(* Initialize Reentrant_a from OCaml - also not a compile-time dependency *)
let () = Printf.printf "Ocaml_init: calling init_module \"Reentrant_a\" from OCaml\n%!"
let () = init_module "Reentrant_a"
let () = Printf.printf "Ocaml_init: Reentrant_a initialized from OCaml\n%!"

(* Initialize Base - this IS a compile-time dependency (used below), so already initialized *)
let () = Printf.printf "Ocaml_init: calling init_module \"Base\" (should be no-op)\n%!"
let () = init_module "Base"
let () = Printf.printf "Ocaml_init: Base init returned\n%!"

(* Verify we can access Base's value (Base is a compile-time dependency) *)
let () = Printf.printf "Ocaml_init: Base.base_value = %d\n%!" Base.base_value

let () = Printf.printf "Ocaml_init: initialization complete\n%!"
