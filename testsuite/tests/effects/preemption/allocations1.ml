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
  let (data @ global) = ref [] in

  ignore (run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      let _ = Array.make 10 0 in
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         data := [1; 2; 3; 4; 5] :: !data;
         if List.length !data > 100 then data := []
       done;
       List.iter (fun l ->
         if List.length l <> 5 then failwith "Data corrupted"
       ) !data;
       "done"));
