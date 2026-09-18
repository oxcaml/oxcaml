(* TEST
   poll_insertion;
   flags += "-w -21";
   { native; }
*)

open Effect
open Effect.Deep

type _ Effect.t += Inc : int -> int Effect.t
type _ Effect.t += Dec : int -> int Effect.t

let () =
  let preempted = ref false in

  let result = Domain.Tick.with_ ~interval_usec:100_000 (fun _ ->
    Preemptible.try_with
      ~on_tick:(fun () -> Preempt)
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
