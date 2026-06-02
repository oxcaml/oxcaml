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
  let allocations = ref [] in

  let result = run_with_tick_handler
    ~interval:0.001
    ~repeating:true
    ~on_preemption:(fun _resume ->
      incr count;
      let _ = ref !count in
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       while !count < 50 do
         if Sys.time () -. start_at > 10.
         then failwith "Rapid preemptions timed out!";
         allocations := Array.make 10 !count :: !allocations;
         if List.length !allocations > 100 then allocations := []
       done;
       !count)
  in

  assert (result >= 50);
