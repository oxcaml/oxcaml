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
  let stored_resume : (unit -> int * int * int) option ref =
    ref None
  in
  let result = run_with_tick_handler
    ~on_preemption:(fun resume ->
      preempted := true;
      stored_resume := Some resume;
      Handled (0, 0, 0))
    (fun () ->
      let obj1 = ref 1 in
      let obj2 = ref 2 in
      let obj3 = ref 3 in
      let start_at = Sys.time () in
      while not !preempted do
        if Sys.time () -. start_at > 5. then failwith "timeout";
        incr obj1; incr obj2; incr obj3
      done;
      (!obj1, !obj2, !obj3))
  in
  assert (result = (0, 0, 0));
  Gc.full_major ();
  Gc.compact ();
  let _ = Array.init 10000 (fun i -> ref i) in
  Gc.full_major ();
  Gc.compact ();
  let resume = Option.get !stored_resume in
  stored_resume := None;
  let (v1, v2, v3) = resume () in
  assert (v1 > 1);
  assert (v2 > 2);
  assert (v3 > 3);
