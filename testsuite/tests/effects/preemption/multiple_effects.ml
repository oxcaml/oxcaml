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

type _ Effect.t += Inc : int -> int Effect.t
type _ Effect.t += Dec : int -> int Effect.t

let () =
  let preempted = ref false in

  let result = with_preemption_setup (fun () ->
    try_with
      (fun () ->
         let start_at = Sys.time () in
         let counter = ref 0 in

         while not !preempted do
           if Sys.time () -. start_at > 5.
           then failwith "Didn't get preempted after 5s!";
           counter := Effect.perform (Inc !counter);
           if !counter > 100 then
             counter := Effect.perform (Dec !counter)
         done;
         !counter)
      ()
      { effc = (fun (type a) (e : a t) ->
          match e with
          | Preemption -> Some (fun (k : (a, _) continuation) ->
            preempted := true;
            Gc.full_major ();
            continue k ())
          | Inc n -> Some (fun (k : (a, _) continuation) ->
            continue k (n + 1))
          | Dec n -> Some (fun (k : (a, _) continuation) ->
            continue k (n - 50))
          | _ -> None) })
  in

  assert (result > 0);
