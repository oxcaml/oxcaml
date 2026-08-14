(* TEST
 {
   reference = "${test_source_directory}/erasure_runtime.reference";
   bytecode;
 }{
   reference = "${test_source_directory}/erasure_runtime.reference";
   native;
 }
*)

(* Runtime semantics of erasure.

   The erased expression's effects are gone. This pins the deliberate
   unsoundness of erased_ before the totality piece requires e @ total: if
   deletion of effects ever changes, this test's reference must change with
   it. *)

let f (x : int @ erased) (n : int) (m : int) = n + m

let g (u : unit) (z : int @ erased) = print_string "g ran\n"

let () =
  (* erased_ deletes evaluation, including effects and exceptions *)
  let x = erased_ (print_string "DELETED\n"; failwith "never") in
  g () x;
  (* an erased argument built with erased_ is never evaluated *)
  g () (erased_ (print_string "ALSO DELETED\n"; 6));
  (* a retained argument at an erased parameter is evaluated for its
     effects, then dropped at the boundary *)
  g () (print_string "kept effect\n"; 7);
  (* partial application across an erased parameter uses the ABI arity *)
  let h = f x 10 in
  print_int (h 20); print_newline ();
  (* a function whose only parameter is erased *)
  let use (y : int @ erased) = f y 1 in
  print_int (use x 2); print_newline ();
  (* a retained closure capturing an erased value is an ordinary closure *)
  let clo = fun u -> g u x in
  clo ();
  print_string "done\n"
