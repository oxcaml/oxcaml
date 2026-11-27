(* TEST
 modules = "test_c_thread_register_cstubs.c";
 multicore;
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)

(* spins a external thread from C and register it to the OCaml runtime *)

external spawn_thread : (unit -> unit) -> unit = "spawn_thread"

let passed () = Printf.printf "passed\n"

let _ =
  let finished = Atomic.make false in
  let d =
    Domain.spawn begin fun () ->
      spawn_thread (fun () ->
          passed ();
          Atomic.set finished true);
      Thread.delay 0.5
    end
  in
  let t = Thread.create (fun () -> Thread.delay 1.0) () in
  Thread.join t;
  Domain.join d;
  (* Wait here, after [Domain.join]: it re-raises if [spawn_thread] failed in
     the domain, in which case nothing would set the flag. *)
  while not (Atomic.get finished) do
    Thread.yield ()
  done
