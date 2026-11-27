(* TEST
 include systhreads;
 hassysthreads;
 not-target-windows;
 {
   bytecode;
 }{
   native;
 }
*)

(* this test checks that the domain lock is properly reinitialized
   in the child process after fork.
   See: https://github.com/ocaml-multicore/ocaml-multicore/issues/471 *)

let () =
  let th = Thread.create (fun () -> Thread.delay 0.5) () in
  let fd = Unix.dup Unix.stdout in
  match Unix.fork () with
  | 0 ->
     Unix.close fd;
     print_endline "OK"
  | pid ->
     Unix.close fd;
     Thread.join th;
     (* The child prints the test's output: do not exit before it has. *)
     ignore (Unix.waitpid [] pid)
