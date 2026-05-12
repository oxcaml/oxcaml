(* TEST
 include eval;
 flags = "-extension runtime_metaprogramming -uses-metaprogramming";
 native;
*)

#syntax quotations on

let test e =
  let e = Obj.magic_many e in
  print_endline (Quote.string_of_expr e); print_newline ();
  ignore (Eval.eval e)

let () = test <[ ($(<[42]>) [@magic_staged_modes]) ]>
