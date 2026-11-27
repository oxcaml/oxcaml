(* TEST
 modules = "test_c_thread_register_cstubs.c";
 include systhreads;
 no-tsan; (* Flaky under TSan, disable until fixed (see issue #13472) *)
 hassysthreads;
 not-bsd;
 {
   bytecode;
 }{
   native;
 }
*)

(* spins a external thread from C and register it to the OCaml runtime *)

external spawn_thread : (unit -> unit) -> unit = "spawn_thread"

let main_thread_id = Thread.self_id ()

let passed () =
  assert (Thread.self_id () <> main_thread_id);
  Printf.printf "passed\n"

let _ =
  let finished = Atomic.make false in
  spawn_thread (fun () ->
      passed ();
      Atomic.set finished true);
  while not (Atomic.get finished) do
    Thread.yield ()
  done
