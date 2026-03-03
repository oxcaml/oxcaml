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

external effective_interval_usec
  : (unit[@untagged]) -> (int[@untagged])
  = "caml_effective_tick_interval_usec_bytecode" "caml_effective_tick_interval_usec"

[@@noalloc]

external poll : unit -> unit = "%poll"

let () =
  Printf.printf "initial tick interval: %d\n%!"
    (get_interval_usec ());
  Printf.printf "initial effective tick interval: %d\n%!"
    (effective_interval_usec ());
  let ticks = Atomic.make 0 in
  set_tick_hook (fun () -> Atomic.incr ticks);
  set_interval_usec 1_000;
  assert (get_interval_usec () = 1_000);
  Printf.printf "tick interval after set: %d\n%!"
    (get_interval_usec ());
  Printf.printf "effective tick interval after set: %d\n%!"
    (effective_interval_usec ());
  let start = Sys.time () in
  while Atomic.get ticks = 0 do
    if (Sys.time () -. start) > 5.0
    then failwith "Timed out"
    else poll ()
  done;
  set_interval_usec 0;
  Printf.printf "tick interval after set back to 0: %d\n%!"
    (get_interval_usec ());
  Printf.printf "effective tick interval after set back to 0: %d\n%!"
    (effective_interval_usec ())
