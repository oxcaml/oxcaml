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
  let finalized_count = ref 0 in

  let make_finalizable n =
    let obj = (n, ref 0) in
    Gc.finalise (fun _ -> incr finalized_count) obj;
    obj
  in

  let result = run_with_tick_handler
    ~interval:0.02
    ~repeating:true
    ~on_preemption:(fun _resume ->
      incr count;
      Gc.full_major ();
      if !count mod 3 = 0 then Gc.compact ();
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let obj1 = make_finalizable 1 in
       let obj2 = make_finalizable 2 in
       let obj3 = make_finalizable 3 in

       while !count < 10 do
         if Sys.time () -. start_at > 10.
         then failwith "Repeated GC timed out!";
         let (_, r1) = obj1 in incr r1;
         let (_, r2) = obj2 in incr r2;
         let (_, r3) = obj3 in incr r3
       done;
       (obj1, obj2, obj3))
  in

  let (obj1, obj2, obj3) = result in
  let (_, r1) = obj1 in assert (!r1 > 0);
  let (_, r2) = obj2 in assert (!r2 > 0);
  let (_, r3) = obj3 in assert (!r3 > 0);

  Gc.full_major ();

  if !finalized_count > 0 then
    failwith (Printf.sprintf "Repeated GC: %d objects finalized!" !finalized_count)
  else if !count < 10 then
    failwith "Repeated GC: insufficient preemptions"
  else ();

  ignore (Sys.opaque_identity (obj1, obj2, obj3))
