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
  let finalized_count = ref 0 in

  let make_finalizable n =
    let obj = (n, ref 0) in
    Gc.finalise (fun _ -> incr finalized_count) obj;
    obj
  in

  let result = run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      Gc.full_major ();
      Gc.compact ();
      let _ = Array.init 1000 (fun i -> (i, ref i)) in
      Gc.full_major ();
      Gc.compact ();
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let obj1 = make_finalizable 1 in
       let obj2 = make_finalizable 2 in
       let obj3 = make_finalizable 3 in
       let obj4 = make_finalizable 4 in
       let obj5 = make_finalizable 5 in
       let obj6 = make_finalizable 6 in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         let (_, r1) = obj1 in incr r1;
         let (_, r2) = obj2 in incr r2;
         let (_, r3) = obj3 in incr r3;
         let (_, r4) = obj4 in incr r4;
         let (_, r5) = obj5 in incr r5;
         let (_, r6) = obj6 in incr r6
       done;

       (obj1, obj2, obj3, obj4, obj5, obj6))
  in

  let (obj1, obj2, obj3, obj4, obj5, obj6) = result in
  let (_, r1) = obj1 in assert (!r1 > 0);
  let (_, r2) = obj2 in assert (!r2 > 0);
  let (_, r3) = obj3 in assert (!r3 > 0);
  let (_, r4) = obj4 in assert (!r4 > 0);
  let (_, r5) = obj5 in assert (!r5 > 0);
  let (_, r6) = obj6 in assert (!r6 > 0);

  Gc.full_major ();

  if !finalized_count > 0 then
    failwith (Printf.sprintf
      "GC register test: FAILED - %d objects finalized too early!"
      !finalized_count)
  else ();

  ignore (Sys.opaque_identity (obj1, obj2, obj3, obj4, obj5, obj6))
