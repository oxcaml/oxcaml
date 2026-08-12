(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  native;
*)

#syntax quotations on

module M = struct
  let f = ( * )
  let f' = <[ With_module.M.f ]>
end

let () =
  let e = Obj.magic_many <[ $M.f' 6 7 ]> in
  print_string (Quote.string_of_expr e);
  print_newline ();
  print_int (Eval.eval e);
  print_newline ();
;;
