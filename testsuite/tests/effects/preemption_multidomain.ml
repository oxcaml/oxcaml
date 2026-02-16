(* TEST
   include unix;
   hasunix;
   runtime5;
   poll_insertion;
   flags += "-alert -unsafe_multidomain -alert -do_not_spawn_domains -w -21";
   { native; }
*)

(* Minimal reproducer for preemption segfault.

   All 5 ingredients are required (removing any one causes the test to pass):
   1. >= 2 domains
   2. Nested effect handlers (Bump inside Preemption)
   3. BOTH SIGALRM-driven AND explicit preempt_self (reentrant preemption)
   4. Tiny minor heap (1024 words) to trigger frequent GC during alloc_3
   5. Gc.full_major() in the Preemption handler

   The bug: SIGALRM fires while a domain is mid-way through handling an
   explicit preempt_self(), creating reentrant preemption. Combined with
   a tiny heap (which triggers minor GC during caml_alloc_3 in
   caml_domain_setup_preemption), this can promote a preemption continuation
   to the major heap before gc_regs is populated. Another domain doing
   Gc.full_major() then scans the continuation via caml_darken_cont(),
   reading uninitialized gc_regs -> segfault in caml_scan_stack.
*)

open Effect
open Effect.Deep

external preempt_self : unit -> unit = "caml_domain_preempt_self" [@@noalloc]

type _ Effect.t += Bump : int -> int Effect.t

let () =
  let stop = Atomic.make false in
  (* SIGALRM every 0.1ms calling preempt_self -- creates reentrant preemption *)
  let _ = Unix.setitimer ITIMER_REAL
    { it_interval = 0.0001; it_value = 0.0001 } in
  let _ = Sys.set_signal Sys.sigalrm
    (Signal_handle (fun _ -> preempt_self ())) in
  let workers = Array.init 2 (fun i ->
    Domain.spawn (fun () ->
      (* Tiny heap: forces GC during caml_alloc_3 in setup_preemption *)
      Gc.set { (Gc.get ()) with minor_heap_size = 1024 };
      (* Outer handler: catches Preemption effect *)
      try_with (fun () ->
        (* Inner handler: catches Bump effect (nesting is required) *)
        try_with (fun () ->
          let start = Unix.gettimeofday () in
          while not (Atomic.get stop) do
            if Unix.gettimeofday () -. start > 30. then
              failwith "timed out";
            (* Explicit preempt_self + SIGALRM = reentrant preemption *)
            preempt_self ();
            let e = Bump i in
            let n = perform e in
            ignore (Sys.opaque_identity n)
          done)
        () { effc = (fun (type a) (e : a t) -> match e with
          | Bump n -> Some (fun (k : (a,_) continuation) ->
            let _ = Sys.opaque_identity (ref (n+1)) in
            continue k (n + 1))
          | _ -> None) })
      () { effc = (fun (type a) (e : a t) -> match e with
        | Preemption -> Some (fun (k : (a,_) continuation) ->
          (* GC in handler: triggers cross-domain scanning of
             potentially uninitialized preemption continuations *)
          Gc.full_major ();
          continue k ())
        | _ -> None) }
    )
  ) in
  Unix.sleepf 10.0;
  Atomic.set stop true;
  Array.iter Domain.join workers;
  let _ = Unix.setitimer ITIMER_REAL
    { it_interval = 0.; it_value = 0. } in
  Printf.printf "PASSED\n%!"
