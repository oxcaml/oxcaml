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

let print_tick_interval () =
  match Domain.Tick.global_effective_interval_usec () with
  | Null -> Printf.printf "effective tick interval: none\n%!"
  | This n -> Printf.printf "effective tick interval: %d\n%!" n

let () =
  print_tick_interval ();
  let ticks = Atomic.make 0 in
  set_tick_hook (fun () -> Atomic.incr ticks);
  let tick = Domain.Tick.acquire ~interval_usec:1_000 in
  print_endline "after set to 1000";
  print_tick_interval ();
  let start = Sys.time () in
  while Atomic.get ticks = 0 do
    if (Sys.time () -. start) > 5.0
    then failwith "Timed out"
    else poll ()
  done;
  Domain.Tick.release tick;
  print_endline "after release";
  print_tick_interval ()
