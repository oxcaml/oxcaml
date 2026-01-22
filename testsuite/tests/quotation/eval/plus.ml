(* TEST
  include camlinternaleval;
  flags = "-extension runtime_metaprogramming";
  arch_amd64;
  native;
*)

#syntax quotations on
open Camlinternaleval

(* This test must be in it's own file so that no dependencies are pulled in
  accidentally. It's testing that (+) is pulled in correctly. *)

let () =
  Printf.printf "\nTest eval +\n";
  let output = eval <[ 1 + 2 ]> in
  Printf.printf "Output: %d\n" output;
;;
