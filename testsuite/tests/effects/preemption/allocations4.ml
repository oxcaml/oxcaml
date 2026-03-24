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

  let make_finalizable () =
    let obj = ref [1; 2; 3; 4; 5] in
    Gc.finalise (fun _ -> finalized := true) obj;
    obj
  in

  let result = run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      let _ = Array.init 10000 (fun i ->
        Array.init 100 (fun j -> (i, j, ref (i + j)))
      ) in
      Gc.full_major ();
      Gc.compact ();
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let obj = make_finalizable () in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         obj := List.map (fun x -> x + 1) !obj
       done;
       obj)
  in

  assert (List.length !result = 5);
  Gc.full_major ();

  if !finalized then
    failwith "Large allocation: object finalized!"
  else ();

  ignore (Sys.opaque_identity result)
