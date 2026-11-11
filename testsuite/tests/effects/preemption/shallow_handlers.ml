(* TEST
   include unix;
   hasunix;
   poll_insertion;
   flags += "-alert -unsafe_multidomain -w -21";
   { native; }
*)

open Effect
open Effect.Shallow

let busy_wait_for flag =
  let start_at = Sys.time () in
  while not !flag do
    if Sys.time () -. start_at > 5.
    then failwith "Timed out after 5s!"
  done

type _ Effect.t += Yield : unit Effect.t

(* Resume a saved preemptible continuation with a different [tickc] (and
   different [effc]): only the new [tickc] should fire, and the [Yield]
   performed after resume should be handled by the new handler. *)
let resume_preemptible_with_different_tickc () =
  print_endline "# Resume preemptible cont with different on_tick";
  let saved_k : (unit, unit) Preemptible.continuation option ref = ref None in
  let count_a = ref 0 in
  let count_b = ref 0 in
  let yields_a = ref 0 in
  let yields_b = ref 0 in
  let preempted_a = ref false in
  let preempted_b = ref false in
  let f = Preemptible.fiber (fun () ->
    perform Yield;
    busy_wait_for preempted_a;
    perform Yield;
    busy_wait_for preempted_b)
  in
  let rec handler_a k =
    Preemptible.continue_with k ()
      { retc = (fun () -> failwith "should be preempted, not return")
      ; exnc = raise
      ; tickc = This (fun () -> incr count_a; Preempt)
      ; effc = (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Preemption -> Some (fun (k : (a, _) Preemptible.continuation) ->
            preempted_a := true;
            saved_k := Some k)
          | Yield -> Some (fun (k : (a, _) Preemptible.continuation) ->
            incr yields_a;
            handler_a k)
          | _ -> None)
      }
  in
  handler_a f;
  assert !preempted_a;
  let k = match !saved_k with
    | Some k -> k
    | None -> failwith "No continuation saved"
  in
  let count_a_at_resume = !count_a in
  let yields_a_at_resume = !yields_a in
  let rec handler_b k =
    Preemptible.continue_with k ()
      { retc = (fun () -> ())
      ; exnc = raise
      ; tickc = This (fun () -> incr count_b; Preempt)
      ; effc = (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Preemption -> Some (fun (k : (a, _) Preemptible.continuation) ->
            preempted_b := true;
            handler_b k)
          | Yield -> Some (fun (k : (a, _) Preemptible.continuation) ->
            incr yields_b;
            handler_b k)
          | _ -> None)
      }
  in
  handler_b k;
  assert !preempted_b;
  assert (!count_b > 0);
  assert (!count_a = count_a_at_resume);
  assert (!yields_a = yields_a_at_resume);
  assert (yields_a_at_resume = 1);
  assert (!yields_b = 1);
  print_endline "OK"
;;

let () =
  Domain.Tick.with_ ~interval_usec:1_000 (fun _ ->
    resume_preemptible_with_different_tickc ())
;;
