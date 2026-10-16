let () =
  let squares = List.init 5 (fun i -> i * i) in
  assert (List.fold_left ( + ) 0 squares = 30);
  Printf.printf "JSIR (ocamlopt -target js_of_ocaml) from the compiler package: %s\n"
    (String.concat " " (List.map string_of_int squares))
