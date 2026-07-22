(* TEST
   modules = "preemption_util.ml";
   include unix;
   hasunix;
   poll_insertion;
   flags += "-w -21";
   { native; }
*)

open Preemption_util

let () =
  let count = ref 0 in
  let iters = ref 0 in

  ignore (run_with_tick_handler
    ~interval_usec:50_000
    ~repeating:true
    ~on_preemption:(fun _resume -> incr count; Resume)
    (fun () ->
       let start_at = Sys.time () in
       while !count < 5 do
         if Sys.time () -. start_at > 10.
         then failwith "Timed out";
         incr iters
       done;
       !count));

  assert (!count >= 5);
