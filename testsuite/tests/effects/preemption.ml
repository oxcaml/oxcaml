(* TEST
   include unix;
   hasunix;
   flags += "-alert -unsafe_multidomain";
   { native; }
*)

open Effect
open Effect.Deep

external preempt_self : unit -> unit = "caml_domain_preempt_self"

let () =
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 1. } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
  let preempted = ref false in
  let start_at = Sys.time () in
  try_with
    (fun () ->
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!"
       done)
    ()
    { effc = (fun (type a) (e : a t) ->
        match e with
        | Tick -> Some (fun (k : (a, _) continuation) ->
          preempted := true;
          continue k ())
        | _ -> None) }
