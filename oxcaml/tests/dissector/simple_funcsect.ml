(* Simple single-file test for the dissector with -function-sections. This is a
   copy of simple.ml to avoid race conditions when both tests compile in
   parallel. *)

let () =
  print_endline "Hello from dissector test";
  (* Do some computation to ensure code is actually executed *)
  let x = 42 in
  let y = x * 2 in
  Printf.printf "Result: %d\n" y
