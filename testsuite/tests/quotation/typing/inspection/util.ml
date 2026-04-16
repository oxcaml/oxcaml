#syntax quotations on

let test ?(eval = true) (e : <[unit]> expr) =
  print_endline (Quote.string_of_expr e); print_newline ();
  if eval then Eval.eval e else ()
