(* TEST
   modules = "tick_stubs.c";
   include unix;
   hasunix;
   not-windows;
   { native; }
*)

(* Test that the file descriptors for the ticker thread are properly reset on
   fork *)

external set_tick_hook : (unit -> unit) -> unit = "set_tick_hook" [@@noalloc]

external poll : unit -> unit = "%poll"

let () =
  let ticks = Atomic.make 0 in
  set_tick_hook (fun () -> Atomic.incr ticks);
  let wait_for_child = Domain.Tick.with_ ~interval_usec:1_000 (fun () ->
    match Unix.fork () with
    | 0 -> (* in child *)
      let start = Sys.time () in
      while Atomic.get ticks = 0 do
        if (Sys.time () -. start) > 5.0
        then failwith "Timed out"
        else poll ()
      done;
      (fun () -> ())
    | child_pid -> (* in parent *)
      (fun () -> ignore (Unix.waitpid [ ] child_pid))
  ) in
  (* After releasing the tick in the parent, the child should still tick *)
  wait_for_child ()
