(* TEST
   modules = "preemption_util.ml";
   include unix;
   hasunix;
   runtime5;
   poll_insertion;
   flags += "-alert -unsafe_multidomain -w -21";
   { native; }
*)

open Effect
open Effect.Deep
open Preemption_util

type _ Effect.t += Nested : int -> int Effect.t

let () =
  let preempted = ref false in

  let rec nest_handlers depth acc =
    if depth = 0 then acc
    else
      try_with
        (fun () -> nest_handlers (depth - 1) (acc + depth))
        ()
        { effc = (fun (type a) (e : a t) ->
            match e with
            | Nested n -> Some (fun (k : (a, _) continuation) ->
              continue k (n * 2))
            | _ -> None) }
  in

  let result = run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      Gc.full_major ();
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let acc = ref 0 in
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         acc := nest_handlers 10 !acc;
         if !acc > 10000 then acc := 0
       done;
       !acc)
  in

  assert (result >= 0);
