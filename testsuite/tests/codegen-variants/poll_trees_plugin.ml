let () =
  Printf.printf "%s\n%!" (String.concat "," (List.map string_of_int [1; 2; 3]))
