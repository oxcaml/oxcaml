(* TEST
  include camlinternaleval;
  flags = "-extension runtime_metaprogramming";
  arch_amd64;
  native;
*)

#syntax quotations on
open Camlinternaleval

let () =
  let output = (42 : <[ int ]> eval) in
  print_int output; print_newline ()
;;

let () =
  let output = eval <[ 42 ]> in
  print_int output; print_newline ()
;;
