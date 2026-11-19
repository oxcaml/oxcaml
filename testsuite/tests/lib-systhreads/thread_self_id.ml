(* TEST
 modules = "test_c_thread_register_cstubs.c";
 include systhreads;
 hassysthreads;
 multicore;
 not-bsd;
 {
   bytecode;
 }{
   native;
 }
*)

external spawn_c_thread : (unit -> unit) -> unit = "spawn_thread"

let check () =
  let expected = Thread.id (Thread.self ()) in
  for _ = 1 to 1000 do
    let actual = Thread.self_id () in
    if actual <> expected
    then Printf.printf "expected %d, got %d\n%!" expected actual;
    (* Yield between checks so that a stale id left behind by a thread switch
       would be noticed. *)
    Thread.yield ()
  done

let check_in_threads () =
  List.init 4 (fun _ -> Thread.create check ()) |> List.iter Thread.join

let check_in_c_thread () =
  let finished = Atomic.make false in
  spawn_c_thread (fun () ->
      check ();
      Atomic.set finished true);
  while not (Atomic.get finished) do
    Thread.yield ()
  done

let () =
  Printf.printf "main thread: %d\n" (Thread.self_id ());
  check ();
  check_in_threads ();
  check_in_c_thread ();
  Domain.join
    (Domain.spawn (fun () ->
         check ();
         check_in_threads ()));
  print_endline "done"
