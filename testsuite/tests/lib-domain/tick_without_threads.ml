(* TEST
   modules = "tick_without_threads_stubs.c";
   runtime5;
   flags = "-alert -unsafe_multidomain";
   { native; }
*)

(* Test that the ticker thread and tick hook mechanism still work when the systhreads
   library isn't linked in *)

external set_tick_hook : (unit -> unit) -> unit = "set_tick_hook" [@@noalloc]

external poll : unit -> unit = "%poll"

let () =
  Printf.printf "initial tick interval: %d\n%!"
    (Domain.Tick.local_requested_interval_usec ());
  Printf.printf "initial effective tick interval: %d\n%!"
    (Domain.Tick.global_effective_interval_usec ());
  let ticks = Atomic.make 0 in
  set_tick_hook (fun () -> Atomic.incr ticks);
  let tick = Domain.Tick.acquire ~interval_usec:1_000 in
  assert (Domain.Tick.local_requested_interval_usec () = 1_000);
  Printf.printf "tick interval after set: %d\n%!"
    (Domain.Tick.local_requested_interval_usec ());
  Printf.printf "effective tick interval after set: %d\n%!"
    (Domain.Tick.global_effective_interval_usec ());
  let start = Sys.time () in
  while Atomic.get ticks = 0 do
    if (Sys.time () -. start) > 5.0
    then failwith "Timed out"
    else poll ()
  done;
  Domain.Tick.release tick;
  Printf.printf "tick interval after set back to 0: %d\n%!"
    (Domain.Tick.local_requested_interval_usec ());
  Printf.printf "effective tick interval after set back to 0: %d\n%!"
    (Domain.Tick.global_effective_interval_usec ())
