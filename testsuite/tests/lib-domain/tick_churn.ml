(* TEST
   modules = "tick_stubs.c";
   include unix;
   hasunix;
   not-windows;
   multicore;
   { native; }
*)

(* Test that changing a domain's tick interval more often than the effective
   interval doesn't stop the tick thread from ticking for the domains that
   want ticks. *)

external set_tick_hook : (unit -> unit) -> unit = "set_tick_hook" [@@noalloc]

external poll : unit -> unit = "%poll"

let ticks = Atomic.make 0

let wait_for_ticks ~name ~churn =
  let target = Atomic.get ticks + 10 in
  let start = Unix.gettimeofday () in
  while Atomic.get ticks < target do
    if Unix.gettimeofday () -. start > 5.0
    then failwith (name ^ ": timed out");
    churn ();
    poll ()
  done

let () =
  set_tick_hook (fun () -> Atomic.incr ticks);
  Domain.Tick.with_ ~interval_usec:1_000 (fun () ->
    (* The effective interval alternates between 1ms and 500μs *)
    wait_for_ticks ~name:"nested" ~churn:(fun () ->
      Domain.Tick.with_ ~interval_usec:500 (fun () -> ());
      ());
    (* Another domain repeatedly acquires and releases ticks, without changing
       the effective interval *)
    let stop = Atomic.make false in
    let d =
      Domain.spawn (fun () ->
        while not (Atomic.get stop) do
          Domain.Tick.with_ ~interval_usec:2_000 (fun () -> ());
          ()
        done)
    in
    wait_for_ticks ~name:"other domain" ~churn:ignore;
    Atomic.set stop true;
    Domain.join d)
