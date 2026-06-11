(* TEST
   include unix;
   hasunix;
   runtime5;
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

let busy_wait_for_seconds n =
  let start_at = Sys.time () in
  while Sys.time () -. start_at < n do () done

type _ Effect.t += Suspend : unit Effect.t
type _ Effect.t += Yield : unit Effect.t

(* Resume a saved preemptible continuation with non-preemptible
   [continue_with]: the resumed body should run uninterrupted, and the
   original [tickc] should not fire again. The [Yield] performed after
   resume should be handled by the new (non-preemptible) handler. *)
let resume_preemptible_as_nonpreemptible () =
  print_endline "# Resume preemptible cont as non-preemptible";
  let saved_k : (unit, unit) continuation option ref = ref None in
  let preempted = ref false in
  let tick_count = ref 0 in
  let yields_a = ref 0 in
  let yields_b = ref 0 in
  let f = fiber (fun () ->
    perform Yield;
    busy_wait_for preempted;
    perform Yield;
    (* If the original preemptible handler were still active, this body
       would tick ~50 times at the 1ms interval. *)
    busy_wait_for_seconds 0.05)
  in
  let rec handler_a k =
    Preemptible.continue_with k ()
      { retc = (fun () -> failwith "should be preempted, not return")
      ; exnc = raise
      ; tickc = (fun () -> incr tick_count; Preempt)
      ; effc = (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Preemption -> Some (fun (k : (a, _) continuation) ->
            preempted := true;
            saved_k := Some k)
          | Yield -> Some (fun (k : (a, _) continuation) ->
            incr yields_a;
            handler_a k)
          | _ -> None)
      }
  in
  handler_a f;
  let k = match !saved_k with
    | Some k -> k
    | None -> failwith "No continuation saved"
  in
  let count_at_resume = !tick_count in
  let yields_a_at_resume = !yields_a in
  let returned = ref false in
  let rec handler_b k =
    continue_with k ()
      { retc = (fun () -> returned := true)
      ; exnc = raise
      ; effc = (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Yield -> Some (fun (k : (a, _) continuation) ->
            incr yields_b;
            handler_b k)
          | _ -> None)
      }
  in
  handler_b k;
  assert !returned;
  assert (!tick_count = count_at_resume);
  assert (!yields_a = yields_a_at_resume);
  assert (yields_a_at_resume = 1);
  assert (!yields_b = 1);
  print_endline "OK"
;;

(* Resume a saved non-preemptible continuation with
   [Preemptible.continue_with]: the new [tickc] should fire and
   [Preemption] should be performed. The [Yield] performed after resume
   should be handled by the new (preemptible) handler. *)
let resume_nonpreemptible_as_preemptible () =
  print_endline "# Resume non-preemptible cont as preemptible";
  let saved_k : (unit, unit) continuation option ref = ref None in
  let preempted = ref false in
  let yields_a = ref 0 in
  let yields_b = ref 0 in
  let f = fiber (fun () ->
    perform Yield;
    perform Suspend;
    perform Yield;
    busy_wait_for preempted)
  in
  let rec handler_a k =
    continue_with k ()
      { retc = (fun () -> failwith "should be suspended")
      ; exnc = raise
      ; effc = (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Suspend -> Some (fun (k : (a, _) continuation) ->
            saved_k := Some k)
          | Yield -> Some (fun (k : (a, _) continuation) ->
            incr yields_a;
            handler_a k)
          | _ -> None)
      }
  in
  handler_a f;
  let k = match !saved_k with
    | Some k -> k
    | None -> failwith "No continuation saved"
  in
  let yields_a_at_resume = !yields_a in
  let rec handler_b k =
    Preemptible.continue_with k ()
      { retc = (fun () -> ())
      ; exnc = raise
      ; tickc = (fun () -> Preempt)
      ; effc = (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Preemption -> Some (fun (k : (a, _) continuation) ->
            preempted := true;
            handler_b k)
          | Yield -> Some (fun (k : (a, _) continuation) ->
            incr yields_b;
            handler_b k)
          | _ -> None)
      }
  in
  handler_b k;
  assert !preempted;
  assert (!yields_a = yields_a_at_resume);
  assert (yields_a_at_resume = 1);
  assert (!yields_b = 1);
  print_endline "OK"
;;

(* Resume a saved preemptible continuation with a different [tickc] (and
   different [effc]): only the new [tickc] should fire, and the [Yield]
   performed after resume should be handled by the new handler. *)
let resume_preemptible_with_different_tickc () =
  print_endline "# Resume preemptible cont with different on_tick";
  let saved_k : (unit, unit) continuation option ref = ref None in
  let count_a = ref 0 in
  let count_b = ref 0 in
  let yields_a = ref 0 in
  let yields_b = ref 0 in
  let preempted_a = ref false in
  let preempted_b = ref false in
  let f = fiber (fun () ->
    perform Yield;
    busy_wait_for preempted_a;
    perform Yield;
    busy_wait_for preempted_b)
  in
  let rec handler_a k =
    Preemptible.continue_with k ()
      { retc = (fun () -> failwith "should be preempted, not return")
      ; exnc = raise
      ; tickc = (fun () -> incr count_a; Preempt)
      ; effc = (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Preemption -> Some (fun (k : (a, _) continuation) ->
            preempted_a := true;
            saved_k := Some k)
          | Yield -> Some (fun (k : (a, _) continuation) ->
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
      ; tickc = (fun () -> incr count_b; Preempt)
      ; effc = (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Preemption -> Some (fun (k : (a, _) continuation) ->
            preempted_b := true;
            handler_b k)
          | Yield -> Some (fun (k : (a, _) continuation) ->
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
    resume_preemptible_as_nonpreemptible ();
    resume_nonpreemptible_as_preemptible ();
    resume_preemptible_with_different_tickc ())
;;
