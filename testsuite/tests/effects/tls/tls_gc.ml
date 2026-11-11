(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   { bytecode; }
   { native; }
*)

(* TLS state survives minor GCs, major GCs and compactions at every point
   where it is only reachable through the runtime's own roots: a suspended
   continuation's stack, the domain's cached state while a non-owner fiber
   is running, and the thread's main stack. After each GC, the cached state
   and the owner's state must still be the same array: a slot initialised
   through the cache must be visible once the cache is recomputed from the
   owner. *)

open Effect
open Effect.Deep

type _ Effect.t += Suspend : unit Effect.t

module TLS = Domain.Safe.TLS

let k = TLS.new_key (fun () -> "init")
let far_keys = Array.init 64 (fun _ -> TLS.new_key (fun () -> -1))

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
      TLS.set k (payload "suspended");
      perform Suspend;
      assert (TLS.get k = payload "suspended"))

(* Same, but the continuation outlives the handler and is resumed later from
   the thread, after the parent has done more work and GCs. *)
let stashed_owner () =
  let stash : (unit, string) continuation option ref = ref None in
  let r =
    Preemptible.match_with (fun () ->
        TLS.set k (payload "stashed");
        perform Suspend;
        TLS.get k)
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
  TLS.set k (payload "thread");
  full_gc ();
  match !stash with
  | None -> assert false
  | Some k' ->
    assert (continue k' () = payload "stashed");
    assert (TLS.get k = payload "thread")

(* GC while a non-owner fiber runs inside an owner: the cache and the
   owner's [tls_state] must be moved together. The writes after the GC go
   through the cache (the first [TLS.set] of each far key initialises its slot,
   growing the array); the suspension recomputes the cache from the owner. *)
let nested_non_owner () =
  preemptible ~on_suspend:full_gc (fun () ->
      TLS.set k (payload "owner");
      non_preemptible (fun () ->
          TLS.set k (payload "non-owner");
          full_gc ();
          assert (TLS.get k = payload "non-owner");
          TLS.set k (payload "after-gc");
          (* Grow the array after the GC too. *)
          Array.iteri (fun i k -> TLS.set k i) far_keys;
          full_gc ());
      perform Suspend;
      assert (TLS.get k = payload "after-gc");
      Array.iteri (fun i k -> assert (i = TLS.get k)) far_keys)

(* Many suspended owners at once, resumed in reverse order. *)
let many_suspended () =
  let n = 50 in
  let stash : (unit, string) continuation list ref = ref [] in
  for i = 1 to n do
    let r =
      Preemptible.match_with (fun () ->
          TLS.set k (payload (string_of_int i));
          perform Suspend;
          assert (TLS.get k = payload (string_of_int i));
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
  TLS.set k (payload "thread-1");
  full_gc ();
  TLS.set k (payload "thread-2");
  preemptible ~on_suspend:ignore (fun () ->
      assert (TLS.get k = "init");
      full_gc ());
  assert (TLS.get k = payload "thread-2");
  non_preemptible full_gc;
  assert (TLS.get k = payload "thread-2")

let () =
  suspended_owner ();
  stashed_owner ();
  nested_non_owner ();
  many_suspended ();
  thread_owner ();
  print_endline "OK"
