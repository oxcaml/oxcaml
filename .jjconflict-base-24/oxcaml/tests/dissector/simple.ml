(* Simple single-file test for the dissector. This tests that a basic program
   compiles and runs correctly when the dissector is enabled. *)

let () =
  print_endline "Hello from dissector test";
  (* Do some computation to ensure code is actually executed *)
  let x = 42 in
  let y = x * 2 in
  Printf.printf "Result: %d\n" y
