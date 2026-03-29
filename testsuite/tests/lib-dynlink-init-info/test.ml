(* TEST
 include dynlink;
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run;
   check-program-output;
 }{
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 }
*)

(* Make sure dynlink state info is accurate before any load
   occurs #9338. *)

let test () =
  assert (List.mem "Dynlink" (Dynlink.main_program_units ()));
  assert (List.mem "Dynlink" (Dynlink.all_units ()));
  ()

let () = test (); print_endline "OK"
