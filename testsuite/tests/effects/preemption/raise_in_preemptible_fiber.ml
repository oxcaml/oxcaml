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
  let exception Test_exn of int in

  let result = try
    run_with_tick_handler
      ~on_preemption:(fun _resume ->
        preempted := true;
        Gc.full_major ();
        Resume)
      (fun () ->
         let start_at = Sys.time () in
         let counter = ref 0 in
         while not !preempted do
           if Sys.time () -. start_at > 5.
           then failwith "Didn't get preempted after 5s!";
           incr counter;
           if !counter > 10000 then raise (Test_exn !counter)
         done;
         "should not reach")
  with Test_exn n ->
    assert (n > 10000);
    "caught exception"
  in

  assert (result = "caught exception");
