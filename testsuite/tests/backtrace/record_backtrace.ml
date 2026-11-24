(* TEST
 multicore;
 include systhreads;
 hassysthreads;
 flags += "-g";
 {
   bytecode;
 }{
   native;
 }
*)

exception Exception

let test ~trace =
  Printexc.record_backtrace trace;
  let verbosely () =
    try
      raise Exception
    with
    | _exn ->
      let bt = Printexc.get_raw_backtrace () in
      let entries = Printexc.raw_backtrace_entries bt in
      Printf.printf "on domain %d, trace %b\n"
        (Domain.self_index ())
        (Array.length entries > 0)
  in
  verbosely ();
  let finished = Atomic.make false in
  (match Multicore.spawn_on ~domain:1 (fun () ->
     verbosely ();
     Atomic.set finished true) ()
   with
   | Failed ((), exn, bt) -> Printexc.raise_with_backtrace exn bt
   | Spawned -> ());
  while not (Atomic.get finished) do
    Thread.yield ()
  done

let () =
  test ~trace:false;
  test ~trace:true
