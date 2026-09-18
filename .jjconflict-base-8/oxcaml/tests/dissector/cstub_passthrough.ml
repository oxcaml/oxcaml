(* Test that C stubs are passed through to the final linker *)
external get_magic_number : unit -> int = "caml_cstub_test_get_magic_number"

let () =
  let n = get_magic_number () in
  if n = 42
  then print_endline "C stub passthrough test passed"
  else Printf.printf "ERROR: expected 42, got %d\n" n
