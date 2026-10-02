(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects -w -21";
   include unix;
   hasunix;
   poll_insertion;
   { native; }
*)

(* A [Preemption] effect that no handler handles is resumed by the runtime
   after switching back to the performer. The cache must be recomputed for
   the performer, both when it is the owner and when it is a non-owner
   inside the owner. *)

open Effect
open Effect.Deep

let k = Domain.TLS.new_key (fun () -> "init")
let get () = Domain.TLS.get k
let set v = Domain.TLS.set k v

(* Number of [Preemption] effects that reached the top and were resumed. *)
let unhandled = ref 0

let wait_for_preemptions ~expect =
  let start_at = Sys.time () in
  let target = !unhandled + 3 in
  while !unhandled < target do
    assert (get () = expect);
    if Sys.time () -. start_at > 5. then failwith "Timed out after 5s"
  done;
  assert (get () = expect)

let () =
  set "thread";
  Domain.Tick.with_ ~interval_usec:1_000 (fun _ ->
      Preemptible.match_with (fun () ->
          set "P";
          (* Performed by the owner. *)
          wait_for_preemptions ~expect:"P";
          (* Performed by a non-owner inside the owner, through a handler
             that forwards it. *)
          match_with (fun () -> wait_for_preemptions ~expect:"P") ()
            { retc = Fun.id; exnc = raise;
              effc = (fun (type a) (_ : a Effect.t) ->
                (* Runs on P. *)
                assert (get () = "P");
                None) };
          assert (get () = "P"))
        ()
        { retc = Fun.id; exnc = raise;
          effc = (fun (type a) (e : a Effect.t) ->
            match e with
            | Preemption ->
              (* Runs on the thread; forward to nobody. *)
              assert (get () = "thread");
              incr unhandled;
              None
            | _ -> None);
          tickc = This (fun () -> Preempt) });
  assert (get () = "thread");
  print_endline "OK"
