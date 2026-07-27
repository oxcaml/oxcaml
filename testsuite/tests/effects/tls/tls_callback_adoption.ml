(* TEST
   modules = "tls_callback_adoption_stubs.c";
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   { bytecode; }
   { native; }
*)

(* Callbacks from C cut the fiber parent chain, separating a non-owner fiber
   from its TLS owner. The callback stack temporarily adopts the state:
   writes (including ones that grow the array) made during the callback must
   be written back to the real owner on exit, and preemptible fibers created
   inside the callback must work. *)

open Effect
open Effect.Deep

type _ Effect.t += Suspend : unit Effect.t

external call_me_back : (unit -> unit) -> unit = "call_me_back"

let k0 = Domain.TLS.new_key (fun () -> "init")
(* Enough keys that setting the last ones forces the array to grow inside
   the callback. *)
let more_keys = Array.init 100 (fun _ -> Domain.TLS.new_key (fun () -> -1))

let () =
  Domain.TLS.set k0 "outer";

  (* Call back from C inside a non-owner fiber. *)
  match_with (fun () ->
      call_me_back (fun () ->
          assert (Domain.TLS.get k0 = "outer");
          Domain.TLS.set k0 "set-in-callback";
          (* Growing writes during adoption. *)
          Array.iteri (fun i k -> Domain.TLS.set k i) more_keys;

          (* A preemptible fiber created, suspended, resumed and completed
             inside the callback (under the cut chain). *)
          let r =
            Preemptible.match_with (fun () ->
                assert (Domain.TLS.get k0 = "init");
                Domain.TLS.set k0 "fiber-in-callback";
                perform Suspend;
                Domain.TLS.get k0)
              ()
              { retc = Fun.id; exnc = raise;
                effc = (fun (type a) (e : a Effect.t) ->
                  match e with
                  | Suspend -> Some (fun (k : (a, _) continuation) ->
                      assert (Domain.TLS.get k0 = "set-in-callback");
                      continue k ())
                  | _ -> None);
                tickc = (fun () -> Continue) }
          in
          assert (r = "fiber-in-callback"));
      (* Back in the fiber: writes made during the callback are visible. *)
      assert (Domain.TLS.get k0 = "set-in-callback");
      Array.iteri (fun i k -> assert (Domain.TLS.get k = i)) more_keys)
    ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (_ : a Effect.t) -> None) };

  (* And at the thread level too. *)
  assert (Domain.TLS.get k0 = "set-in-callback");
  Array.iteri (fun i k -> assert (Domain.TLS.get k = i)) more_keys;
  print_endline "OK"
