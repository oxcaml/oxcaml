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
  let count = ref 0 in
  let iters = ref 0 in

  ignore (run_with_tick_handler
    ~interval:0.05
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
