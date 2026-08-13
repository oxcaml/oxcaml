(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  native;
*)

#syntax quotations on

(* This test must be in it's own file so that no dependencies are pulled in
  accidentally. It's testing that (+) is pulled in correctly. *)

let () =
  Printf.printf "\nTest eval +\n";
  let output : int = Eval.eval <[ 1 + 2 ]> in
  Printf.printf "Output: %d\n" output;
;;
