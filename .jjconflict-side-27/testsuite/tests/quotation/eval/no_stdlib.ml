(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  native;
*)

#syntax quotations on

(* This test must be in its own file as the built cmx must not have any quotes
   referencing the stdlib *)

let () =
  Printf.printf "\nTest eval with no references to Stdlib\n";
  let output = Eval.eval (<[ fun x -> x ]> : <[int -> int]> expr) in
  Printf.printf "Output: %d\n" (output 42);
;;
