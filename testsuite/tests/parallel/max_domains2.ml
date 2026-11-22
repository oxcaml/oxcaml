(* TEST
 multicore;
 {
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   ocamlrunparam += ",d=129";
   run;
   check-program-output;
 }
*)

(* The domain limit is set for the test program only: the compiler would
   warn about it when it is not itself compiled with poll insertion. *)

let m = Mutex.create ()

let _ =
  Mutex.lock m;
  (* The default max domains limit is 128. In this test, we make this limit 129
     and spawn 128 domains in addition to the main domain. *)
  for i = 1 to 128 do
    Domain.spawn (fun _ -> Mutex.lock m) |> ignore
  done;
  print_endline "ok"
