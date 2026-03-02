(* TEST
   modules = "tick_without_threads_stubs.c";
   runtime5;
   flags = "-alert -unsafe_multidomain";
   { native; }
*)

(* Test that the ticker thread and tick hook mechanism still work when the systhreads
   library isn't linked in *)

external set_tick_hook : (unit -> unit) -> unit = "set_tick_hook" [@@noalloc]

external set_interval_usec
  : int -> unit
  = "caml_domain_set_tick_interval_usec"
[@@noalloc]

external get_interval_usec
  : unit -> int
  = "caml_domain_get_tick_interval_usec"
[@@noalloc]

external poll : unit -> unit = "%poll"

let () =
  let ticks = Atomic.make 0 in
  set_tick_hook (fun () -> Atomic.incr ticks);
  set_interval_usec 1_000;
  assert (get_interval_usec () = 1_000);
  let start = Sys.time () in
  while Atomic.get ticks = 0 do
    if (Sys.time () -. start) > 5.0
    then failwith "Timed out"
    else poll ()
  done
