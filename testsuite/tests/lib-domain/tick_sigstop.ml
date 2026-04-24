(* TEST
   modules = "tick_stubs.c";
   include unix;
   hasunix;
   not-windows;
   runtime5;
   flags = "-alert -unsafe_multidomain";
   { native; }
*)

(* Test that the tick thread continues operation even if the process is SIGSTOPped and
   SIGCONTed.

   Despite what the man page says, epoll_wait *can* actually return EINTR even if the
   thread has signals masked, if the process gets SIGSTOPped and SIGCONTed. This test
   checks that the tick thread keeps operating if this happens.
*)

external set_tick_hook : (unit -> unit) -> unit = "set_tick_hook" [@@noalloc]

external poll : unit -> unit = "%poll"

let () =
  match Unix.fork () with
  | 0 -> (* in child *)
    (* start ticking... *)
    let ticks = Atomic.make 0 in
    set_tick_hook (fun () -> Atomic.incr ticks);
    let tick = Domain.Tick.acquire ~interval_usec:150 in
    let start = Sys.time () in
    while Atomic.get ticks < 1_000 do
      if (Sys.time () -. start) > 5.0
      then failwith "Timed out"
      else poll ()
    done;
    Domain.Tick.release tick
  | child_pid -> (* in parent *)
    for _ = 0 to 10_000 do
      Unix.kill child_pid Sys.sigstop;
      Unix.kill child_pid Sys.sigcont;
    done;
    (* If we release the tick in the parent, the child should still tick *)
    let _pid, status =  Unix.waitpid [ ] child_pid in
    begin match status with
    | WEXITED 0 -> ((* OK *))
    | WEXITED i -> Printf.printf "child exited with nonzero status: %d\n%!" i
    | WSIGNALED i -> Printf.printf "child received signal: %d\n%!" i
    | WSTOPPED _ -> Printf.printf "child stopped (but we didn't pass WUNTRACED?)\n"
    end
