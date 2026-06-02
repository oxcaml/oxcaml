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
  let finalizer_ran = ref false in
  let survivor = ref None in

  let make_short_lived () =
    let obj = ref 42 in
    Gc.finalise (fun r ->
      finalizer_ran := true;
      let _ = Array.make 100 (!r + 1) in
      ()
    ) obj;
    obj
  in

  let result = run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      Gc.full_major ();
      Gc.full_major ();
      survivor := Some (Array.make 1000 0);
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let keep_alive = ref 100 in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         let _ = make_short_lived () in
         incr keep_alive
       done;
       keep_alive)
  in

  assert (!result > 100);
  ignore (Sys.opaque_identity (result, survivor))
