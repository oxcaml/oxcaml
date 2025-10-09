let[@inline never] [@local never] [@loop never] rec loop x =
  print_int x;
  print_newline ();
  if x = 0
  then 0
  else loop (x - 1)
;;

let () = loop (Sys.opaque_identity 3) |> ignore
