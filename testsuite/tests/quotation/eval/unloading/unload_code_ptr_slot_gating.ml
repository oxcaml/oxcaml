(* TEST
  modules = "unload_code_ptr_slot_gating_.c";
  no-address-sanitizer;
  { native; }
*)

external check : unit -> unit = "caml_test_unload_code_ptr_slot_gating"

let () =
  print_endline "check code-ptr slot gating";
  check ();
  print_endline "ok"

