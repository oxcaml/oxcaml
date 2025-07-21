let () =
  Array_rev.rev ();
  Array.to_list Array_rev_data.arr
  |> Format.asprintf "%a\n"
       (Format.pp_print_list ~pp_sep:Format.pp_print_newline Format.pp_print_int)
  |> print_endline
