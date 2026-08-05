(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  native;
*)

#syntax quotations on

let answer = 42

let () =
  let e = <[ This_module.answer ]> in
  print_string (Eval.string_of_expr e);
  print_newline ();
  print_int (Eval.eval e);
  print_newline ();
;;
