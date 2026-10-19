(* TEST
 modules = "test_c_thread_register_tls_stubs.c";
 include systhreads;
 hassysthreads;
 {
   native;
 }
*)

(* Spawn an external thread using C which registers itself using caml_c_thread_register,
   then accesses TLS.

   This is interesting as it's an entrypoint to systhreads that doesn't go through
   [Thread.create ()]
*)

external spawn_thread : (unit -> unit) -> unit = "spawn_thread"

let thread finished k =
  Thread.TLS.access k (fun s -> print_endline !s);
  Atomic.set finished true

let _ =
  let finished = Atomic.make true in
  let k = Thread.TLS.new_key (fun () -> ref "foo") in
  spawn_thread (fun () -> thread finished k);
  let t = Thread.create (fun () -> Thread.delay 1.0) () in
  while not (Atomic.get finished) do Thread.yield () done;
  Thread.join t
