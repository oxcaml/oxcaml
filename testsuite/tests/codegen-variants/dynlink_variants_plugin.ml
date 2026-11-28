let () =
  Printf.printf "plugin: %d %s\n%!" 42
    (Format.asprintf "%a" Format.pp_print_int 7)
