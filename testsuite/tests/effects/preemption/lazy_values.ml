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
  let finalized = ref false in

  let make_finalizable v =
    let obj = ref v in
    Gc.finalise (fun _ -> finalized := true) obj;
    obj
  in

  let result = run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      Gc.full_major ();
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let obj = make_finalizable 100 in
       let lazy_val = lazy (
         incr obj;
         let arr = Array.init 100 (fun i -> !obj + i) in
         (obj, arr)
       ) in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         incr obj
       done;
       lazy_val)
  in

  let (obj, arr) = Lazy.force result in
  assert (!obj > 100);
  assert (Array.length arr = 100);
  Gc.full_major ();

  if !finalized then
    failwith "Lazy values: object finalized!"
  else ();

  ignore (Sys.opaque_identity (obj, arr))
