(* TEST
   modules = "tick_stubs.c";
   include unix;
   hasunix;
   not-windows;
   runtime5;
   flags = "-alert -unsafe_multidomain";
   { native; }
*)

(* Test that the file descriptors for the ticker thread are properly reset on
   fork *)

external set_tick_hook : (unit -> unit) -> unit = "set_tick_hook" [@@noalloc]

external poll : unit -> unit = "%poll"

let () =
  let ticks = Atomic.make 0 in
  set_tick_hook (fun () -> Atomic.incr ticks);
  let tick = Domain.Tick.acquire ~interval_usec:1_000 in
  match Unix.fork () with
  | 0 -> (* in child *)
    let start = Sys.time () in
    while Atomic.get ticks = 0 do
      if (Sys.time () -. start) > 5.0
      then failwith "Timed out"
      else poll ()
    done;
    Domain.Tick.release tick
  | child_pid -> (* in parent *)
    (* If we release the tick in the parent, the child should still tick *)
    Domain.Tick.release tick;
    ignore (Unix.waitpid [ ] child_pid)
