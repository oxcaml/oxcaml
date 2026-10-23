(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects \
             -alert -do_not_spawn_domains";
   poll_insertion;
   multicore;
   { native; }
*)

(* [Domain.Safe.TLS.access] gives the current TLS owner uncontended access to
   its value. While [f] runs, the key's slot holds a sentinel, so accessing,
   getting, setting, or splitting that key again raises. See
   [tls_access_interrupt.ml] for an access from an interrupt. *)

open Effect
open Effect.Deep

module TLS = Domain.Safe.TLS

exception Boom
exception Init_failed

let k = TLS.new_key (fun () -> ref 0)
let k2 = TLS.new_key (fun () -> ref 0)
let ki = TLS.new_key (fun () -> 0)
let k_break = TLS.new_key (fun () -> ref 0)

(* Splits by copying the current value into the child. *)
let k_split =
  TLS.new_key ~split_from_parent:(fun (v : int) -> fun () -> v) (fun () -> 0)

let read (k : int ref TLS.key) = TLS.access k (fun r -> !r) [@nontail]

let bump (k : int ref TLS.key) =
  TLS.access k (fun r -> incr r; !r) [@nontail]

let fails f =
  match f () with
  | () -> print_endline "no exception"
  | exception Failure msg -> print_endline msg
  | exception Sys.Break -> print_endline "Break"

let entered = Atomic.make false
let released = Atomic.make false

(* An initialiser that fails the first time it runs. *)
let init_runs = Atomic.make 0
let k_bad =
  TLS.new_key (fun () ->
    Atomic.incr init_runs;
    if Atomic.get init_runs = 1 then raise Init_failed else ref 42)

let () =
  (* Initialisation, mutation, and results. *)
  assert (read k = 0);
  assert (bump k = 1);
  assert (bump k = 2);
  assert (read k = 2);
  assert (read k2 = 0);
  assert (TLS.access ki (fun x -> x + 1) = 1);

  (* Nesting on different keys is fine. *)
  TLS.access k (fun r ->
    let x = !r in
    TLS.access k2 (fun r2 -> r2 := x + 10) [@nontail]);
  assert (read k2 = 12);

  (* Re-entering on the same key raises, and the slot is restored. *)
  fails (fun () ->
    TLS.access k (fun _ -> TLS.access k (fun _ -> ()) [@nontail]) [@nontail]);
  assert (read k = 2);

  (* [get] and [set] reject borrowed slots, preserving their values. *)
  assert (TLS.get ki = 0);
  TLS.set ki 10;
  fails (fun () ->
    TLS.access ki (fun _ -> ignore (TLS.get ki)) [@nontail]);
  assert (TLS.get ki = 10);
  fails (fun () ->
    TLS.access ki (fun _ -> TLS.set ki 20) [@nontail]);
  assert (TLS.get ki = 10);

  (* Other keys can be read and written, even when this grows the state.
     Catching a failed [set] must leave the slot borrowed until [access]
     returns. *)
  TLS.access ki (fun x ->
    let keys = Array.init 100 (fun _ -> TLS.new_key (fun () -> -1)) in
    Array.iteri (fun i k -> TLS.set k i) keys;
    Array.iteri (fun i k -> assert (i = TLS.get k)) keys;
    fails (fun () -> TLS.set ki 20);
    fails (fun () -> ignore (TLS.get ki));
    fails (fun () -> TLS.access ki (fun _ -> ()) [@nontail]);
    assert (x = 10));
  assert (TLS.get ki = 10);
  TLS.set ki 30;
  assert (TLS.get ki = 30);

  (* The slot is restored when [f] raises. *)
  (match TLS.access k (fun _ -> raise Boom) with
   | () -> failwith "expected Boom"
   | exception Boom -> ());
  assert (bump k = 3);

  (* The slot is restored when the initialiser raises. *)
  (match TLS.access k_bad (fun _ -> ()) with
   | () -> failwith "expected Init_failed"
   | exception Init_failed -> ());
  assert (read k_bad = 42);
  assert (Atomic.get init_runs = 2);

  (* Splitting the TLS state while a split key is being accessed raises
     (before the domain is created); afterwards, splitting works again. *)
  fails (fun () ->
    TLS.access k_split (fun _ ->
      ignore (Domain.Safe.spawn (fun () -> ()) : unit Domain.t)) [@nontail]);
  assert (Domain.join (Domain.Safe.spawn (fun () ->
    TLS.access k_split (fun x -> x) [@nontail])) = 0);

  (* A preemptible fiber owns its own value: while it is preempted inside
     [access], the thread can still [access] the same key. *)
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

  (* An asynchronous exception unwinding through [access] skips its handler
     (as with [Mutex.protect]), so the slot stays borrowed. *)
  Sys.catch_break true;
  let finalised = ref false in
  Gc.finalise_last (fun () -> finalised := true; raise Sys.Break) (ref 0);
  fails (fun () ->
    Sys.with_async_exns (fun () ->
      TLS.access k_break (fun r ->
        Gc.full_major ();
        Gc.full_major ();
        r := 1000) [@nontail]));
  assert !finalised;
  fails (fun () -> ignore (read k_break));

  print_endline "OK"
