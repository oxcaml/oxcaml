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
       let obj1 = ref 100 in
       let obj2 = ref 200 in
       let obj3 = ref 300 in
       let weak = Weak.create 3 in
       Weak.set weak 0 (Some obj1);
       Weak.set weak 1 (Some obj2);
       Weak.set weak 2 (Some obj3);

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         incr obj1; incr obj2; incr obj3
       done;
       (obj1, obj2, obj3, weak))
  in

  Gc.full_major ();
  let (obj1, obj2, obj3, weak) = (Sys.opaque_identity result) in

  (match Weak.get weak 0, Weak.get weak 1, Weak.get weak 2 with
  | Some w1, Some w2, Some w3 ->
      assert (!w1 > 100);
      assert (!w2 > 200);
      assert (!w3 > 300);
  | _ -> failwith "Weak references: objects were collected!");

  ignore (Sys.opaque_identity (obj1, obj2, obj3, weak))
