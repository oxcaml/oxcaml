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
  let finalized = ref false in

  let make_finalizable v =
    let obj = ref v in
    Gc.finalise (fun _ -> finalized := true) obj;
    obj
  in

  let result = with_preemption_setup (fun () ->
    try_with
      (fun () ->
         let obj = make_finalizable 42 in
         try_with
           (fun () ->
              let start_at = Sys.time () in
              while not !preempted do
                if Sys.time () -. start_at > 5.
                then failwith "Didn't get preempted after 5s!";
                incr obj;
                let _ = Effect.perform (Nested !obj) in
                ()
              done;
              obj)
           ()
           { effc = (fun (type a) (e : a t) ->
               match e with
               | Nested n -> Some (fun (k : (a, _) continuation) ->
                   continue k (n * 2))
               | _ -> None) })
      ()
      { effc = (fun (type a) (e : a t) ->
          match e with
          | Preemption -> Some (fun (k : (a, _) continuation) ->
            preempted := true;
            Gc.full_major ();
            continue k ())
          | _ -> None) })
  in

  assert (!result > 42);
  Gc.full_major ();

  if !finalized then
    failwith "Nested handlers: object finalized!"
  else ();

  ignore (Sys.opaque_identity result)
