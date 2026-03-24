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
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let arr = ref [||] in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         arr := Array.init 1000 (fun i -> (Random.int 10000, ref i));
         Array.sort (fun (a, _) (b, _) -> compare a b) !arr
       done;
       !arr)
  in

  assert (Array.length result = 1000);
