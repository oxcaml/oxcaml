(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   poll_insertion;
   { native; }
*)

(* [Domain.Safe.TLS.access] gives the current TLS owner uncontended access to
   its value, and raises if the same owner re-enters [access]. *)

open Effect
open Effect.Deep

module TLS = Domain.Safe.TLS

exception Boom

let k = TLS.new_key (fun () -> ref 0)
let k2 = TLS.new_key (fun () -> ref 0)
let ki = TLS.new_key (fun () -> 0)

let read (k : int ref TLS.key) = TLS.access k (fun r -> !r) [@nontail]

let bump (k : int ref TLS.key) =
  TLS.access k (fun r -> incr r; !r) [@nontail]

let reenters f =
  match f () with
  | () -> false
  | exception Failure _ -> true

let entered = Atomic.make false
let released = Atomic.make false

let () =
  (* Initialisation, mutation, and results. *)
  assert (read k = 0);
  assert (bump k = 1);
  assert (bump k = 2);
  assert (read k = 2);
  assert (read k2 = 0);
  TLS.set ki 5;
  assert (TLS.access ki (fun x -> x + 1) = 6);

  (* Re-entry raises, on the same key or another one. *)
  assert (reenters (fun () ->
    TLS.access k (fun _ -> TLS.access k (fun _ -> ()) [@nontail]) [@nontail]));
  assert (reenters (fun () ->
    TLS.access k (fun _ -> TLS.access k2 (fun _ -> ()) [@nontail]) [@nontail]));

  (* The guard is released when [f] raises. *)
  (match TLS.access k (fun _ -> raise Boom) with
   | () -> failwith "expected Boom"
   | exception Boom -> ());
  assert (bump k = 3);

  (* A preemptible fiber owns its own value and its own guard: while it is
     preempted inside [access], the thread can still [access] the key. *)
  Domain.Tick.with_ ~interval_usec:1_000 (fun _ ->
    Preemptible.match_with (fun () ->
        assert (read k = 0);
        TLS.access k (fun r ->
          r := 100;
          Atomic.set entered true;
          let start = Sys.time () in
          while not (Atomic.get released) do
            if Sys.time () -. start > 5. then failwith "Timed out after 5s"
          done;
          assert (!r = 100));
        assert (read k = 100))
      ()
      { retc = Fun.id; exnc = raise;
        effc = (fun (type a) (e : a Effect.t) ->
          match e with
          | Preemption ->
            Some (fun (c : (a, _) continuation) ->
              (* Runs on the thread. *)
              if Atomic.get entered && not (Atomic.get released) then begin
                assert (bump k = 4);
                Atomic.set released true
              end;
              continue c ())
          | _ -> None);
        tickc = This (fun () -> Preempt) });
  assert (read k = 4);

  print_endline "OK"
