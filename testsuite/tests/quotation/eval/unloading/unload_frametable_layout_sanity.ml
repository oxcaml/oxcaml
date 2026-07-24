(* TEST
  modules = "unload_frametable_layout_sanity_.c";
  no-address-sanitizer;
  { native; }
*)

external check : unit -> unit = "caml_test_frametable_layout_sanity"

let () =
  print_endline "check frametable layout sanity";
  check ();
  print_endline "ok"

