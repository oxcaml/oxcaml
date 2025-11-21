(* TEST
  arch_amd64;
  native;
*)

#syntax quotations on

(* This test must be in its own file as the built cmx must not have any quotes
   referencing the stdlib *)

let () =
  Printf.printf "\nTest eval with no references to Stdlib\n";
  let output = [%eval: int -> int] <[ fun x -> x ]> in
  Printf.printf "Output: %d\n" (output 42);
;;
