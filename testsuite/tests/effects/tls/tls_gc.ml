(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   { bytecode; }
   { native; }
*)

(* TLS state survives minor GCs, major GCs and compactions at every point
   where it is only reachable through the runtime's own roots: a suspended
   continuation's stack, the domain's cached state while a non-owner fiber
   is running, and the thread's main stack. After each GC, the cached state
   and the owner's state must still be the same array: a write through the
   cache must be visible once the cache is recomputed from the owner. *)

open Effect
open Effect.Deep

type _ Effect.t += Suspend : unit Effect.t

let k = Domain.TLS.new_key (fun () -> "init")
let far_keys = Array.init 64 (fun _ -> Domain.TLS.new_key (fun () -> -1))

(* A fresh, young, heap-allocated value with checkable contents. *)
let payload tag = String.concat "" [ "<"; tag; ">"; String.make 64 '.' ]

(* Overwrite the minor heap so that dangling references are noticed. *)
let churn () =
  for i = 1 to 20_000 do
    ignore (Sys.opaque_identity (ref i))
  done

let full_gc () =
  Gc.minor ();
  churn ();
  Gc.full_major ();
  churn ();
  Gc.compact ();
  churn ()

let non_preemptible f =
  match_with f ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (_ : a Effect.t) -> None) }

(* Runs [f] in a preemptible fiber whose [Suspend]s run [on_suspend] on the
   parent before resuming. *)
let preemptible ~on_suspend f =
  Preemptible.match_with f ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (e : a Effect.t) ->
        match e with
        | Suspend -> Some (fun (k' : (a, _) continuation) ->
            on_suspend ();
            continue k' ())
        | _ -> None);
      tickc = This (fun () -> Continue) }

(* The suspended fiber's stack holds the only reference to its state. *)
let suspended_owner () =
  preemptible ~on_suspend:full_gc (fun () ->
      Domain.TLS.set k (payload "suspended");
      perform Suspend;
      assert (Domain.TLS.get k = payload "suspended"))

(* Same, but the continuation outlives the handler and is resumed later from
   the thread, after the parent has done more work and GCs. *)
let stashed_owner () =
  let stash : (unit, string) continuation option ref = ref None in
  let r =
    Preemptible.match_with (fun () ->
        Domain.TLS.set k (payload "stashed");
        perform Suspend;
        Domain.TLS.get k)
      ()
      { retc = Fun.id; exnc = raise;
        effc = (fun (type a) (e : a Effect.t) ->
          match e with
          | Suspend -> Some (fun (k' : (a, _) continuation) ->
              stash := Some k';
              "suspended")
          | _ -> None);
        tickc = This (fun () -> Continue) }
  in
  assert (r = "suspended");
  Domain.TLS.set k (payload "thread");
  full_gc ();
  match !stash with
  | None -> assert false
  | Some k' ->
    assert (continue k' () = payload "stashed");
    assert (Domain.TLS.get k = payload "thread")

(* GC while a non-owner fiber runs inside an owner: the cache and the
   owner's [tls_state] must be moved together. The write after the GC goes
   through the cache; the suspension recomputes the cache from the owner. *)
let nested_non_owner () =
  preemptible ~on_suspend:full_gc (fun () ->
      Domain.TLS.set k (payload "owner");
      non_preemptible (fun () ->
          Domain.TLS.set k (payload "non-owner");
          full_gc ();
          assert (Domain.TLS.get k = payload "non-owner");
          Domain.TLS.set k (payload "after-gc");
          (* Grow the array after the GC too. *)
          Array.iteri (fun i k -> Domain.TLS.set k i) far_keys;
          full_gc ());
      perform Suspend;
      assert (Domain.TLS.get k = payload "after-gc");
      Array.iteri (fun i k -> assert (Domain.TLS.get k = i)) far_keys)

(* Many suspended owners at once, resumed in reverse order. *)
let many_suspended () =
  let n = 50 in
  let stash : (unit, string) continuation list ref = ref [] in
  for i = 1 to n do
    let r =
      Preemptible.match_with (fun () ->
          Domain.TLS.set k (payload (string_of_int i));
          perform Suspend;
          assert (Domain.TLS.get k = payload (string_of_int i));
          "done")
        ()
        { retc = Fun.id; exnc = raise;
          effc = (fun (type a) (e : a Effect.t) ->
            match e with
            | Suspend -> Some (fun (k' : (a, _) continuation) ->
                stash := k' :: !stash;
                "suspended")
            | _ -> None);
          tickc = This (fun () -> Continue) }
    in
    assert (r = "suspended")
  done;
  full_gc ();
  List.iter (fun k' -> assert (continue k' () = "done")) !stash

(* The thread's own state: GC with it cached, then switch to a fiber and
   back (which recomputes the cache from the main stack). *)
let thread_owner () =
  Domain.TLS.set k (payload "thread-1");
  full_gc ();
  Domain.TLS.set k (payload "thread-2");
  preemptible ~on_suspend:ignore (fun () ->
      assert (Domain.TLS.get k = "init");
      full_gc ());
  assert (Domain.TLS.get k = payload "thread-2");
  non_preemptible full_gc;
  assert (Domain.TLS.get k = payload "thread-2")

let () =
  suspended_owner ();
  stashed_owner ();
  nested_non_owner ();
  many_suspended ();
  thread_owner ();
  print_endline "OK"
