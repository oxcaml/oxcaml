(* TEST
   modules = "preemption_util.ml";
   include unix;
   hasunix;
   runtime5;
   poll_insertion;
   flags += "-alert -unsafe_multidomain -w -21";
   { native; }
*)

open Preemption_util

let () =
  let preempted = ref false in
  let x = ref 0 in
  run_with_tick_handler
    ~on_preemption:(fun _resume -> preempted := true; Resume)
    (fun () ->
       let start_at = Sys.time () in
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         x := !x + 1
       done;
       assert (!x > 0))
