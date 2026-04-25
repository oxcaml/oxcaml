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
  let exception Handler_exn in

  let result = try
    run_with_tick_handler
      ~on_preemption:(fun _resume ->
        preempted := true;
        raise Handler_exn)
      (fun () ->
         let start_at = Sys.time () in
         let counter = ref 0 in
         while not !preempted do
           if Sys.time () -. start_at > 5.
           then failwith "Didn't get preempted after 5s!";
           incr counter
         done;
         !counter)
  with Handler_exn ->
    -1
  in

  assert (result = -1);
