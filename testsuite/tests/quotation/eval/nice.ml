(* TEST
  flags = "-extension runtime_metaprogramming";
  arch_amd64;
  native;
*)

#syntax quotations on

let () =
  let output = (42 : <[ int ]> eval) in
  print_int output; print_newline ()
;;

let () =
  let output = [%eval: int] <[ 42 ]> in
  print_int output; print_newline ()
;;
