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
  let exception Inner_exn of int in
  let exception Outer_exn of string in

  let result = try
    try
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
             try
               if !counter > 5000 then raise (Inner_exn !counter)
             with Inner_exn n ->
               if n > 10000 then raise (Outer_exn "outer")
           done;
           !counter)
    with Outer_exn s -> -1
  with Failure s -> -2
  in

  assert (result <> -2);
