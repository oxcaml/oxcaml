(* TEST
   include unix;
   hasunix;
   runtime5;
   poll_insertion;
   flags += "-alert -unsafe_multidomain -w -21";
   { native; }
*)

open Effect
open Effect.Deep

external preempt_self : unit -> unit = "caml_domain_preempt_self"
[@@noalloc]

type _ Effect.t += Nested : int -> int Effect.t

let with_preemption_setup ?(interval = 0.1) ?(repeating = false) f =
  let it_interval = if repeating then interval else 0. in
  Unix.setitimer ITIMER_REAL { it_interval; it_value = interval } |> ignore;
  Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ()))
  |> ignore;
  Fun.protect f
    ~finally:(fun () ->
      Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0. } |> ignore)

type 'a preemption_action =
  | Resume
  | Handled of 'a

let run_with_tick_handler ?(interval = 0.1) ?(repeating = false)
    ~on_preemption computation =
  with_preemption_setup ~interval ~repeating (fun () ->
    try_with
      computation
      ()
      { effc = (fun (type a) (e : a t) ->
          match e with
          | Preemption -> Some (fun (k : (a, _) continuation) ->
            let resume () = continue k () in
            match on_preemption resume with
            | Resume -> resume ()
            | Handled result -> result)
          | _ -> None) }
  )

(* Basic preemption test: preempt quickly, then immediately resume *)
let test_basic () =
  print_endline "Test 1: Basic preemption";
  let preempted = ref false in
  let x = ref 0 in
  run_with_tick_handler
    ~on_preemption:(fun _resume -> preempted := true; Resume)
    (fun () ->
       let start_at = Sys.time () in
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         x := !x + 1
       done;
       assert (!x > 0);
       print_endline "  Basic preemption: PASSED")

(* Test delayed continuation resume - registers might be clobbered *)
let test_delayed_resume () =
  print_endline "Test 2: Delayed resume with live values";
  let preempted = ref false in
  let result = ref None in
  ignore (run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      (* Do some work before resuming *)
      let _ = Sys.opaque_identity (ref 1) in
      let _ = Sys.opaque_identity (ref 2) in
      let _ = Sys.opaque_identity (ref 3) in
      Resume)
    (fun () ->
       (* Create lots of live values that should survive preemption *)
       let a = ref 1 in
       let b = ref 2 in
       let c = ref 3 in
       let d = ref 4 in
       let e = ref 5 in
       let f = ref 6 in
       let g = ref 7 in
       let h = ref 8 in

       let start_at = Sys.time () in
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Touch all values to keep them live *)
         a := !a + !b; b := !b + !c; c := !c + !d; d := !d + !e;
         e := !e + !f; f := !f + !g; g := !g + !h; h := !h + 1;
         (* Prevent overflow *)
         if !h > 1000 then begin
           a := 1; b := 2; c := 3; d := 4; e := 5; f := 6; g := 7; h := 8
         end
       done;

       (* Values should be preserved across preemption *)
       result := Some (!a, !b, !c, !d, !e, !f, !g, !h);
       "completed"));

  match !result with
  | Some _ -> print_endline "  Delayed resume: PASSED"
  | None -> failwith "Delayed resume: FAILED - no result"

(* Test multiple preemptions in sequence *)
let test_multiple_preemptions () =
  print_endline "Test 4: Multiple preemptions";
  let count = ref 0 in
  let iters = ref 0 in

  ignore (run_with_tick_handler
    ~interval:0.05
    ~repeating:true
    ~on_preemption:(fun _resume -> incr count; Resume)
    (fun () ->
       let start_at = Sys.time () in
       while !count < 5 do
         if Sys.time () -. start_at > 10.
         then failwith "Timed out";
         incr iters
       done;
       !count));

  assert (!count >= 5);
  print_endline "  Multiple preemptions: PASSED"

(* Test preemption with allocation *)
let test_with_allocations () =
  print_endline "Test 5: Preemption with allocations";
  let preempted = ref false in
  let (data @ global) = ref [] in

  ignore (run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      (* Allocate during preemption handling *)
      let _ = Array.make 10 0 in
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Allocate lots of short-lived objects *)
         data := [1; 2; 3; 4; 5] :: !data;
         if List.length !data > 100 then data := []
       done;
       (* Verify data integrity *)
       List.iter (fun l ->
         if List.length l <> 5 then failwith "Data corrupted"
       ) !data;
       "done"));

  print_endline "  Preemption with allocations: PASSED"

(* Test preemption with many live registers *)
let test_many_live_registers () =
  print_endline "Test 6: Many live registers";
  let preempted = ref false in

  ignore (run_with_tick_handler
    ~on_preemption:(fun _resume -> preempted := true; Resume)
    (fun () ->
       let r0 = ref 100 in let r1 = ref 101 in let r2 = ref 102 in let r3 = ref 103 in
       let r4 = ref 104 in let r5 = ref 105 in let r6 = ref 106 in let r7 = ref 107 in
       let r8 = ref 108 in let r9 = ref 109 in let r10 = ref 110 in let r11 = ref 111 in
       let r12 = ref 112 in let r13 = ref 113 in let r14 = ref 114 in let r15 = ref 115 in

       let start_at = Sys.time () in
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Keep all registers live *)
         r0 := !r0 + 1; r1 := !r1 + 1; r2 := !r2 + 1; r3 := !r3 + 1;
         r4 := !r4 + 1; r5 := !r5 + 1; r6 := !r6 + 1; r7 := !r7 + 1;
         r8 := !r8 + 1; r9 := !r9 + 1; r10 := !r10 + 1; r11 := !r11 + 1;
         r12 := !r12 + 1; r13 := !r13 + 1; r14 := !r14 + 1; r15 := !r15 + 1
       done;

       (* Verify all values *)
       let expected = [100; 101; 102; 103; 104; 105; 106; 107;
                       108; 109; 110; 111; 112; 113; 114; 115] in
       let actual = [!r0; !r1; !r2; !r3; !r4; !r5; !r6; !r7;
                     !r8; !r9; !r10; !r11; !r12; !r13; !r14; !r15] in
       List.iter2 (fun e a ->
         if a < e then failwith "Register value lost"
       ) expected actual;
       "ok"));

  print_endline "  Many live registers: PASSED"

(* Test that register-only values survive GC during preemption *)
let test_gc_register_values () =
  print_endline "Test 7: Register values survive GC";
  let preempted = ref false in
  let finalized_count = ref 0 in

  (* Create objects with finalizers that we'll keep live *)
  let make_finalizable n =
    let obj = (n, ref 0) in
    Gc.finalise (fun _ -> incr finalized_count) obj;
    obj
  in

  let result = run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      (* Force aggressive GC while suspended *)
      Gc.full_major ();
      Gc.compact ();
      (* Allocate some garbage to stress the GC *)
      let _ = Array.init 1000 (fun i -> (i, ref i)) in
      Gc.full_major ();
      Gc.compact ();
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       (* Create multiple finalizable objects *)
       let obj1 = make_finalizable 1 in
       let obj2 = make_finalizable 2 in
       let obj3 = make_finalizable 3 in
       let obj4 = make_finalizable 4 in
       let obj5 = make_finalizable 5 in
       let obj6 = make_finalizable 6 in

       (* Keep them all live through computation *)
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Touch all objects to keep them live *)
         let (_, r1) = obj1 in incr r1;
         let (_, r2) = obj2 in incr r2;
         let (_, r3) = obj3 in incr r3;
         let (_, r4) = obj4 in incr r4;
         let (_, r5) = obj5 in incr r5;
         let (_, r6) = obj6 in incr r6
       done;

       (* Return all objects to keep them live until the end *)
       (obj1, obj2, obj3, obj4, obj5, obj6))
  in

  (* Objects should still be alive, so touch them *)
  let (obj1, obj2, obj3, obj4, obj5, obj6) = result in
  let (_, r1) = obj1 in assert (!r1 > 0);
  let (_, r2) = obj2 in assert (!r2 > 0);
  let (_, r3) = obj3 in assert (!r3 > 0);
  let (_, r4) = obj4 in assert (!r4 > 0);
  let (_, r5) = obj5 in assert (!r5 > 0);
  let (_, r6) = obj6 in assert (!r6 > 0);

  (* Run GC again to be sure *)
  Gc.full_major ();

  if !finalized_count > 0 then
    failwith (Printf.sprintf
      "GC register test: FAILED - %d objects finalized too early!"
      !finalized_count)
  else
    print_endline "  Register values survive GC: PASSED";

  (* Keep objects alive until after the check *)
  ignore (Sys.opaque_identity (obj1, obj2, obj3, obj4, obj5, obj6))

(* Test 8: Repeated preemptions with GC each time *)
let test_repeated_gc () =
  print_endline "Test 8: Repeated preemptions with GC";
  let count = ref 0 in
  let finalized_count = ref 0 in

  let make_finalizable n =
    let obj = (n, ref 0) in
    Gc.finalise (fun _ -> incr finalized_count) obj;
    obj
  in

  let result = run_with_tick_handler
    ~interval:0.02
    ~repeating:true
    ~on_preemption:(fun _resume ->
      incr count;
      (* GC on every preemption! *)
      Gc.full_major ();
      if !count mod 3 = 0 then Gc.compact ();
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let obj1 = make_finalizable 1 in
       let obj2 = make_finalizable 2 in
       let obj3 = make_finalizable 3 in

       while !count < 10 do
         if Sys.time () -. start_at > 10.
         then failwith "Repeated GC timed out!";
         let (_, r1) = obj1 in incr r1;
         let (_, r2) = obj2 in incr r2;
         let (_, r3) = obj3 in incr r3
       done;
       (obj1, obj2, obj3))
  in

  let (obj1, obj2, obj3) = result in
  let (_, r1) = obj1 in assert (!r1 > 0);
  let (_, r2) = obj2 in assert (!r2 > 0);
  let (_, r3) = obj3 in assert (!r3 > 0);

  Gc.full_major ();

  if !finalized_count > 0 then
    failwith (Printf.sprintf "Repeated GC: %d objects finalized!" !finalized_count)
  else if !count < 10 then
    failwith "Repeated GC: insufficient preemptions"
  else
    print_endline "  Repeated preemptions with GC: PASSED";

  ignore (Sys.opaque_identity (obj1, obj2, obj3))

(* Test 9: Massive allocation during preemption *)
let test_massive_allocation () =
  print_endline "Test 9: Massive allocation during preemption";
  let preempted = ref false in
  let finalized = ref false in

  let make_finalizable () =
    let obj = ref [1; 2; 3; 4; 5] in
    Gc.finalise (fun _ -> finalized := true) obj;
    obj
  in

  let result = run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      (* Allocate ~10MB of data *)
      let _ = Array.init 10000 (fun i ->
        Array.init 100 (fun j -> (i, j, ref (i + j)))
      ) in
      Gc.full_major ();
      Gc.compact ();
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let obj = make_finalizable () in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         obj := List.map (fun x -> x + 1) !obj
       done;
       obj)
  in

  assert (List.length !result = 5);
  Gc.full_major ();

  if !finalized then
    failwith "Massive allocation: object finalized!"
  else
    print_endline "  Massive allocation: PASSED";

  ignore (Sys.opaque_identity result)

(* Test 10: Large complex data structures in registers *)
let test_large_structures () =
  print_endline "Test 10: Large complex data structures";
  let preempted = ref false in
  let finalized_count = ref 0 in

  let make_complex_structure n =
    let arr = Array.init 100 (fun i -> (n * 1000 + i, ref i)) in
    Gc.finalise (fun _ -> incr finalized_count) arr;
    arr
  in

  let result = run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      Gc.full_major ();
      Gc.compact ();
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let s1 = make_complex_structure 1 in
       let s2 = make_complex_structure 2 in
       let s3 = make_complex_structure 3 in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Touch all structures *)
         let (_, r) = s1.(0) in incr r;
         let (_, r) = s2.(50) in incr r;
         let (_, r) = s3.(99) in incr r
       done;
       (s1, s2, s3))
  in

  let (s1, s2, s3) = result in
  assert (Array.length s1 = 100);
  assert (Array.length s2 = 100);
  assert (Array.length s3 = 100);
  Gc.full_major ();

  if !finalized_count > 0 then
    failwith "Large structures: objects finalized!"
  else
    print_endline "  Large structures: PASSED";

  ignore (Sys.opaque_identity (s1, s2, s3))

(* Test 11: Weak references with strong refs in registers *)
let test_weak_references () =
  print_endline "Test 11: Weak references";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      (* Aggressive GC should not clear weak refs *)
      Gc.full_major ();
      Gc.compact ();
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       (* Create objects and weak refs *)
       let obj1 = ref 100 in
       let obj2 = ref 200 in
       let obj3 = ref 300 in
       let weak = Weak.create 3 in
       Weak.set weak 0 (Some obj1);
       Weak.set weak 1 (Some obj2);
       Weak.set weak 2 (Some obj3);

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         incr obj1; incr obj2; incr obj3
       done;
       (obj1, obj2, obj3, weak))
  in

  Gc.full_major ();
  let (obj1, obj2, obj3, weak) = (Sys.opaque_identity result) in

  (* Objects should still be accessible via weak array *)
  match Weak.get weak 0, Weak.get weak 1, Weak.get weak 2 with
  | Some w1, Some w2, Some w3 ->
      assert (!w1 > 100);
      assert (!w2 > 200);
      assert (!w3 > 300);
      print_endline "  Weak references: PASSED"
  | _ -> failwith "Weak references: objects were collected!";

  ignore (Sys.opaque_identity (obj1, obj2, obj3, weak))

(* Test 12: Nested effect handlers with preemption *)
let test_nested_handlers () =
  print_endline "Test 12: Nested effect handlers";
  let preempted = ref false in
  let finalized = ref false in

  let make_finalizable v =
    let obj = ref v in
    Gc.finalise (fun _ -> finalized := true) obj;
    obj
  in

  let result = with_preemption_setup (fun () ->
    try_with
      (fun () ->
         let obj = make_finalizable 42 in
         try_with
           (fun () ->
              let start_at = Sys.time () in
              while not !preempted do
                if Sys.time () -. start_at > 5.
                then failwith "Didn't get preempted after 5s!";
                incr obj;
                let _ = Effect.perform (Nested !obj) in
                ()
              done;
              obj)
           ()
           { effc = (fun (type a) (e : a t) ->
               match e with
               | Nested n -> Some (fun (k : (a, _) continuation) ->
                   continue k (n * 2))
               | _ -> None) })
      ()
      { effc = (fun (type a) (e : a t) ->
          match e with
          | Preemption -> Some (fun (k : (a, _) continuation) ->
            preempted := true;
            Gc.full_major ();
            continue k ())
          | _ -> None) })
  in

  assert (!result > 42);
  Gc.full_major ();

  if !finalized then
    failwith "Nested handlers: object finalized!"
  else
    print_endline "  Nested handlers: PASSED";

  ignore (Sys.opaque_identity result)

(* Test 13: Lazy values forced during preemption *)
let test_lazy_values () =
  print_endline "Test 13: Lazy values";
  let preempted = ref false in
  let finalized = ref false in

  let make_finalizable v =
    let obj = ref v in
    Gc.finalise (fun _ -> finalized := true) obj;
    obj
  in

  let result = run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      Gc.full_major ();
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let obj = make_finalizable 100 in
       let lazy_val = lazy (
         incr obj;
         let arr = Array.init 100 (fun i -> !obj + i) in
         (obj, arr)
       ) in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         incr obj
       done;
       lazy_val)
  in

  (* Force the lazy value after preemption *)
  let (obj, arr) = Lazy.force result in
  assert (!obj > 100);
  assert (Array.length arr = 100);
  Gc.full_major ();

  if !finalized then
    failwith "Lazy values: object finalized!"
  else
    print_endline "  Lazy values: PASSED";

  ignore (Sys.opaque_identity (obj, arr))

(* Test 14: Preemption continuation stored on heap. We use Handled to return a dummy value
   without resuming, which pops the effc frame. The continuation is then only reachable
   via the stored_resume ref on the heap. *)
let test_heap_stored_continuation () =
  print_endline "Test 14: Heap-stored continuation with GC";
  let preempted = ref false in
  let stored_resume : (unit -> int * int * int) option ref =
    ref None
  in
  let result = run_with_tick_handler
    ~on_preemption:(fun resume ->
      preempted := true;
      (* Store resume closure (captures the continuation) on the heap
         and return immediately. The effc frame is popped, so the
         continuation is only reachable via stored_resume. *)
      stored_resume := Some resume;
      Handled (0, 0, 0))
    (fun () ->
      let obj1 = ref 1 in
      let obj2 = ref 2 in
      let obj3 = ref 3 in
      let start_at = Sys.time () in
      while not !preempted do
        if Sys.time () -. start_at > 5. then failwith "timeout";
        incr obj1; incr obj2; incr obj3
      done;
      (!obj1, !obj2, !obj3))
  in
  assert (result = (0, 0, 0));
  (* Outside run_with_tick_handler — effc frame is gone. The
     continuation is only reachable via stored_resume on the heap. *)
  Gc.full_major ();
  Gc.compact ();
  let _ = Array.init 10000 (fun i -> ref i) in
  Gc.full_major ();
  Gc.compact ();
  let resume = Option.get !stored_resume in
  stored_resume := None;
  let (v1, v2, v3) = resume () in
  assert (v1 > 1);
  assert (v2 > 2);
  assert (v3 > 3);
  print_endline "  Heap-stored continuation: PASSED"

(* Test 15: Exception handling across preemption *)
let test_exception_handling () =
  print_endline "Test 15: Exception handling across preemption";
  let preempted = ref false in
  let exception Test_exn of int in

  let result = try
    run_with_tick_handler
      ~on_preemption:(fun _resume ->
        preempted := true;
        Gc.full_major ();
        Resume)
      (fun () ->
         let start_at = Sys.time () in
         let counter = ref 0 in
         while not !preempted do
           if Sys.time () -. start_at > 5.
           then failwith "Didn't get preempted after 5s!";
           incr counter;
           if !counter > 10000 then raise (Test_exn !counter)
         done;
         "should not reach")
  with Test_exn n ->
    assert (n > 10000);
    "caught exception"
  in

  assert (result = "caught exception");
  print_endline "  Exception handling: PASSED"

(* Test 16: Finalizer execution during preemption handler *)
let test_finalizer_in_handler () =
  print_endline "Test 16: Finalizer in preemption handler";
  let preempted = ref false in
  let finalizer_ran = ref false in
  let survivor = ref None in

  let make_short_lived () =
    let obj = ref 42 in
    Gc.finalise (fun r ->
      finalizer_ran := true;
      (* Allocate in finalizer *)
      let _ = Array.make 100 (!r + 1) in
      ()
    ) obj;
    obj
  in

  let result = run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      (* Force GC to run finalizers *)
      Gc.full_major ();
      Gc.full_major ();
      (* Allocate during handler *)
      survivor := Some (Array.make 1000 0);
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let keep_alive = ref 100 in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Create and drop short-lived objects *)
         let _ = make_short_lived () in
         incr keep_alive
       done;
       keep_alive)
  in

  assert (!result > 100);
  print_endline "  Finalizer in handler: PASSED";
  ignore (Sys.opaque_identity (result, survivor))

(* Test 17: Raising exception in preemption handler *)
let test_exception_in_handler () =
  print_endline "Test 17: Exception in preemption handler";
  let preempted = ref false in
  let exception Handler_exn in

  let result = try
    run_with_tick_handler
      ~on_preemption:(fun _resume ->
        preempted := true;
        (* Raise exception instead of continuing *)
        raise Handler_exn)
      (fun () ->
         let start_at = Sys.time () in
         let counter = ref 0 in
         while not !preempted do
           if Sys.time () -. start_at > 5.
           then failwith "Didn't get preempted after 5s!";
           incr counter
         done;
         !counter)
  with Handler_exn ->
    -1
  in

  assert (result = -1);
  print_endline "  Exception in handler: PASSED"

(* Test 18: Multiple effects interleaved with preemption *)
type _ Effect.t += Inc : int -> int Effect.t
type _ Effect.t += Dec : int -> int Effect.t

let test_multiple_effects () =
  print_endline "Test 18: Multiple effects with preemption";
  let preempted = ref false in

  let result = with_preemption_setup (fun () ->
    try_with
      (fun () ->
         let start_at = Sys.time () in
         let counter = ref 0 in

         while not !preempted do
           if Sys.time () -. start_at > 5.
           then failwith "Didn't get preempted after 5s!";
           counter := Effect.perform (Inc !counter);
           if !counter > 100 then
             counter := Effect.perform (Dec !counter)
         done;
         !counter)
      ()
      { effc = (fun (type a) (e : a t) ->
          match e with
          | Preemption -> Some (fun (k : (a, _) continuation) ->
            preempted := true;
            Gc.full_major ();
            continue k ())
          | Inc n -> Some (fun (k : (a, _) continuation) ->
            continue k (n + 1))
          | Dec n -> Some (fun (k : (a, _) continuation) ->
            continue k (n - 50))
          | _ -> None) })
  in

  assert (result > 0);
  print_endline "  Multiple effects: PASSED"

(* Test 19: Deep effect handler nesting with preemption *)
let test_deep_effect_nesting () =
  print_endline "Test 19: Deep effect nesting with preemption";
  let preempted = ref false in

  let rec nest_handlers depth acc =
    if depth = 0 then acc
    else
      try_with
        (fun () -> nest_handlers (depth - 1) (acc + depth))
        ()
        { effc = (fun (type a) (e : a t) ->
            match e with
            | Nested n -> Some (fun (k : (a, _) continuation) ->
              continue k (n * 2))
            | _ -> None) }
  in

  let result = run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      Gc.full_major ();
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let acc = ref 0 in
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         acc := nest_handlers 10 !acc;
         if !acc > 10000 then acc := 0
       done;
       !acc)
  in

  assert (result >= 0);
  print_endline "  Deep effect nesting: PASSED"

(* Test 20: Very fast repeated preemptions *)
let test_rapid_preemptions () =
  print_endline "Test 20: Rapid repeated preemptions";
  let count = ref 0 in
  let allocations = ref [] in

  let result = run_with_tick_handler
    ~interval:0.001
    ~repeating:true
    ~on_preemption:(fun _resume ->
      incr count;
      (* Small allocation in handler *)
      let _ = ref !count in
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       while !count < 50 do
         if Sys.time () -. start_at > 10.
         then failwith "Rapid preemptions timed out!";
         (* Allocate on every iteration *)
         allocations := Array.make 10 !count :: !allocations;
         if List.length !allocations > 100 then allocations := []
       done;
       !count)
  in

  assert (result >= 50);
  print_endline "  Rapid preemptions: PASSED"

(* Test 21: Nested try-with during preemption *)
let test_nested_try_blocks () =
  print_endline "Test 21: Nested try-with blocks during preemption";
  let preempted = ref false in
  let exception Inner_exn of int in
  let exception Outer_exn of string in

  let result = try
    try
      run_with_tick_handler
        ~on_preemption:(fun _resume ->
          preempted := true;
          Gc.full_major ();
          Resume)
        (fun () ->
           let start_at = Sys.time () in
           let counter = ref 0 in
           while not !preempted do
             if Sys.time () -. start_at > 5.
             then failwith "Didn't get preempted after 5s!";
             incr counter;
             try
               if !counter > 5000 then raise (Inner_exn !counter)
             with Inner_exn n ->
               if n > 10000 then raise (Outer_exn "outer")
           done;
           !counter)
    with Outer_exn s -> -1
  with Failure s -> -2
  in

  assert (result <> -2);
  print_endline "  Nested try-with blocks: PASSED"

(* Test 22: Mixed allocation sizes *)
let test_mixed_allocations () =
  print_endline "Test 22: Mixed allocation sizes";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      (* Also mix allocation sizes in handler *)
      let _ = ref 1 in
      let _ = Array.make 5000 0 in
      Gc.full_major ();
      Gc.compact ();
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let data = ref [] in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Mix tiny and huge allocations *)
         let tiny = ref 1 in
         let small = Array.make 10 (ref 0) in
         let medium = Array.make 100 (ref 0) in
         let large = Array.init 1000 (fun i -> (i, ref i)) in
         data := tiny :: small.(0) :: medium.(0) :: (snd large.(0))
                 :: !data;
         if List.length !data > 1000 then data := []
       done;
       !data)
  in

  assert (List.length result > 0);
  print_endline "  Mixed allocations: PASSED"

(* Test 23: Really huge single allocation *)
let test_huge_allocation () =
  print_endline "Test 23: Huge single allocation";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      Gc.full_major ();
      Gc.compact ();
      Resume)
    (fun () ->
       let start_at = Sys.time () in
       let huge = ref None in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Allocate 10MB+ array that definitely exceeds young heap *)
         huge := Some (Array.init 1000000 (fun i -> ref i))
       done;
       !huge)
  in

  (match result with
  | Some arr -> assert (Array.length arr = 1000000)
  | None -> failwith "Huge allocation: no result");
  print_endline "  Huge allocation: PASSED"

(* Test 24: Multiple live continuations on the heap simultaneously.
   Collects 5 preemption continuations into a list, runs GC while all
   are on the heap, then resumes them in reverse order. *)
let test_multiple_live_continuations () =
  print_endline "Test 24: Multiple live continuations on heap";
  let n = 5 in
  let conts : (unit, unit) continuation list ref = ref [] in
  let values : int ref list ref = ref [] in
  let rec collect remaining =
    if remaining = 0 then begin
      (* All n continuations are on the heap in the list.
         GC must scan each one's gc_regs. *)
      Gc.full_major ();
      Gc.compact ();
      let _ = Array.init 10000 (fun i -> ref i) in
      Gc.full_major ();
      Gc.compact ();
      (* Resume in reverse collection order *)
      List.iter (fun k -> continue k ()) (List.rev !conts)
    end else begin
      let done_ = ref false in
      let v = ref 0 in
      values := v :: !values;
      match_with
        (fun () ->
          let start_at = Sys.time () in
          while not !done_ do
            if Sys.time () -. start_at > 5. then failwith "timeout";
            incr v
          done;
          assert (!v > 0))
        ()
        { retc = (fun () -> ());
          exnc = raise;
          effc = fun (type a) (e : a t) ->
            match e with
            | Preemption -> Some (fun (k : (a, _) continuation) ->
              conts := k :: !conts;
              done_ := true;
              collect (remaining - 1))
            | _ -> None }
    end
  in
  with_preemption_setup ~interval:0.01 ~repeating:true (fun () ->
    collect n);
  (* Verify all computations ran *)
  List.iter (fun v -> assert (!v > 0)) !values;
  print_endline "  Multiple live continuations: PASSED"

(* Run all tests *)
let () =
  test_basic ();
  test_delayed_resume ();
  test_multiple_preemptions ();
  test_with_allocations ();
  test_many_live_registers ();
  test_gc_register_values ();
  test_repeated_gc ();
  test_massive_allocation ();
  test_large_structures ();
  test_weak_references ();
  test_nested_handlers ();
  test_lazy_values ();
  test_heap_stored_continuation ();
  test_exception_handling ();
  test_finalizer_in_handler ();
  test_exception_in_handler ();
  test_multiple_effects ();
  test_deep_effect_nesting ();
  test_rapid_preemptions ();
  test_nested_try_blocks ();
  test_mixed_allocations ();
  test_huge_allocation ();
  test_multiple_live_continuations ();
  (* Disable timer *)
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0. } in
  print_endline "\nAll preemption tests PASSED!"
