(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  {
    setup-ocamlopt.opt-build-env;
    ocamlopt_opt_exit_status = "2";
    ocamlopt.opt;
    check-ocamlopt.opt-output;
  }
*)

#syntax quotations on

let answer = 42

let () =
  let e = <[ answer ]> in
  print_string (Quote.string_of_expr e);
  print_newline ();
  print_int (Eval.eval e);
  print_newline ();
;;
