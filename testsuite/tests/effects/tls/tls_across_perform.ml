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

module TLS = Domain.Safe.TLS

let k = TLS.new_key (fun () -> "init")

let () =
  TLS.set k "parent";
  Preemptible.match_with (fun () ->
      TLS.set k "fiber";
      perform Suspend;
      Printf.printf "after resume: %s\n" (TLS.get k);
      perform Suspend;
      assert (TLS.get k = "fiber"))
    ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (e : a Effect.t) ->
        match e with
        | Suspend -> Some (fun (k' : (a, _) continuation) ->
            Printf.printf "in handler: %s\n" (TLS.get k);
            (* Writes in the handler go to the parent's state, not the
               suspended fiber's. *)
            TLS.set k "handler";
            continue k' ())
        | _ -> None);
      tickc = This (fun () -> Continue) };
  assert (TLS.get k = "handler");
  print_endline "OK"
