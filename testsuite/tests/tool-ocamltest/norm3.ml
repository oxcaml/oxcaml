(* TEST
   reason = "Disabled due to differences between ocamltest and dune's diff behaviour";
   skip;
  *)
let () = set_binary_mode_out stdout true in
(* ocamltest must normalise the \r\n but preserve the final \r *)
print_string "line1\r\nline2\r"; flush stdout
