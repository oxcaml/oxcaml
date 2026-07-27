(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   { bytecode; }
   { native; }
*)

(* TLS state travels with a preemptible fiber across suspension and
   resumption: the effect handler (running on the parent) sees the parent's
   state while the fiber is suspended, and the fiber sees its own state
   again after being resumed. *)

open Effect
open Effect.Deep

type _ Effect.t += Suspend : unit Effect.t

let k = Domain.TLS.new_key (fun () -> "init")

let () =
  Domain.TLS.set k "parent";
  Preemptible.match_with (fun () ->
      Domain.TLS.set k "fiber";
      perform Suspend;
      Printf.printf "after resume: %s\n" (Domain.TLS.get k);
      perform Suspend;
      assert (Domain.TLS.get k = "fiber"))
    ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (e : a Effect.t) ->
        match e with
        | Suspend -> Some (fun (k' : (a, _) continuation) ->
            Printf.printf "in handler: %s\n" (Domain.TLS.get k);
            (* Writes in the handler go to the parent's state, not the
               suspended fiber's. *)
            Domain.TLS.set k "handler";
            continue k' ())
        | _ -> None);
      tickc = (fun () -> Continue) };
  assert (Domain.TLS.get k = "handler");
  print_endline "OK"
