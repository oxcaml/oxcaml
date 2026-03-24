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

  let result = run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      Gc.full_major ();
      Gc.compact ();
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let huge = ref None in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         huge := Some (Array.init 1000000 (fun i -> ref i))
       done;
       !huge)
  in

  (match result with
  | Some arr -> assert (Array.length arr = 1000000)
  | None -> failwith "Huge allocation: no result");
