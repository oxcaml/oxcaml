(* TEST
 {
   flags = "-dlambda -dno-unique-ids";
   compiler_reference2 =
     "${test_source_directory}/try_reset_dst.lambda.reference";
   native;
 }{
   flags = "-runtime-variant=d";
   native;
 }
*)

(* The dps version of the [try] body writes the destination before the
   recursive call. When a deeper call raises, the handler writes the same
   destination again, so it must first reset it to the placeholder with
   [caml_modify]: TMC writes the destination with [caml_initialize], which
   requires an immediate in the field. The debug runtime checks this. *)

let[@inline never] fail () : unit = raise_notrace Exit

let[@tail_mod_cons] rec f n =
  try
    if n = 0 then fail ();
    n :: f (n - 1)
  with Exit when n = 2 -> [ n * 100 ]

let () = 
  let result = f 4 in
  let as_str = String.concat " " (List.map string_of_int result) in 
  (*
   * f 0 raises Exit
   * f 1 raises Exit
   * f 2 catches the exception, returns [200]
   * f 3 returns 3 :: f 2
   * f 4 returns 4 :: f 3
   *)
  print_endline "Should be: 4 3 200";
  print_string "f 4 = ";
  print_endline as_str
