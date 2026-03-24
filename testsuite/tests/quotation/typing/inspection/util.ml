#syntax quotations on

let test ?(eval = true) e : unit =
  print_endline (Quote.string_of_expr e); print_newline ();
  if eval then [%eval: unit] e else ()
