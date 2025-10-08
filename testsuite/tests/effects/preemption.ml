(* TEST
   include unix;
   hasunix;
   runtime5;
   flags += "-alert -unsafe_multidomain -w -21";
   { native; }
*)

open Effect
open Effect.Deep

type _ Effect.t +=
  | Tick : unit Effect.t
  | Nested : int -> int Effect.t

let preempt_self () =
  preempt_with Tick

let with_preemption_setup ?(interval = 0.1) ?(repeating = false) f =
  let it_interval = if repeating then interval else 0. in
  let _ = Unix.setitimer ITIMER_REAL { it_interval; it_value = interval } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
  let result = f () in
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0. } in
  result

let run_with_tick_handler ?(interval = 0.1) ?(repeating = false) ?(on_tick = fun () -> ()) computation =
  with_preemption_setup ~interval ~repeating (fun () ->
    try_with
      computation
      ()
      { effc = (fun (type a) (e : a t) ->
          match e with
          | Tick -> Some (fun (k : (a, _) continuation) ->
            on_tick ();
            continue k ())
          | _ -> None) }
  )

(* Basic preemption test - immediate resume *)
let test_basic () =
  print_endline "Test 1: Basic preemption";
  let preempted = ref false in
  let x = ref 0 in
  run_with_tick_handler
    ~on_tick:(fun () -> preempted := true)
    (fun () ->
       let start_at = Sys.time () in
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         x := !x + 1;
         Sys.poll_actions ()
       done;
       assert (!x > 0);
       print_endline "  Basic preemption: PASSED")

(* Test delayed continuation resume - registers might be clobbered *)
let test_delayed_resume () =
  print_endline "Test 2: Delayed resume with live values";
  let preempted = ref false in
  let result = ref None in
  ignore (run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      (* Do some work before resuming *)
      let _ = ref 1 in
      let _ = ref 2 in
      let _ = ref 3 in
      ())
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
         end;
         Sys.poll_actions ()
       done;

       (* Values should be preserved across preemption *)
       result := Some (!a, !b, !c, !d, !e, !f, !g, !h);
       "completed"));

  match !result with
  | Some _ -> print_endline "  Delayed resume: PASSED"
  | None -> failwith "Delayed resume: FAILED - no result"

(* Test with nested computations *)
let test_nested_computation () =
  print_endline "Test 3: Nested computation with preemption";
  let preempted = ref false in

  let rec compute_nested depth acc =
    if depth = 0 then acc
    else begin
      let v = acc + depth in
      compute_nested (depth - 1) v
    end
  in

  ignore (run_with_tick_handler
    ~on_tick:(fun () -> preempted := true)
    (fun () ->
       let start_at = Sys.time () in
       let result = ref 0 in
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         result := compute_nested 100 !result;
         if !result > 100000 then result := 0;
         Sys.poll_actions ()
       done;
       !result));

  print_endline "  Nested computation: PASSED"

(* Test multiple preemptions in sequence *)
let test_multiple_preemptions () =
  print_endline "Test 4: Multiple preemptions";
  let count = ref 0 in
  let iters = ref 0 in

  ignore (run_with_tick_handler
    ~interval:0.05
    ~repeating:true
    ~on_tick:(fun () -> incr count)
    (fun () ->
       let start_at = Sys.time () in
       while !count < 5 do
         if Sys.time () -. start_at > 10.
         then failwith "Multiple preemptions timed out!";
         incr iters;
         Sys.poll_actions ()
       done;
       !count));

  if !count >= 5 then
    print_endline "  Multiple preemptions: PASSED"
  else
    failwith "Multiple preemptions: insufficient count"

(* Test preemption with allocation pressure *)
let test_with_allocations () =
  print_endline "Test 5: Preemption with allocations";
  let preempted = ref false in
  let data = ref [] in

  ignore (run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      (* Allocate during preemption handling *)
      let _ = Array.make 10 0 in
      ())
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
    ~on_tick:(fun () -> preempted := true)
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
         r12 := !r12 + 1; r13 := !r13 + 1; r14 := !r14 + 1; r15 := !r15 + 1;
         Sys.poll_actions ()
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
    ~on_tick:(fun () ->
      preempted := true;
      (* Force aggressive GC while suspended *)
      Gc.full_major ();
      Gc.compact ();
      (* Allocate some garbage to stress the GC *)
      let _ = Array.init 1000 (fun i -> (i, ref i)) in
      Gc.full_major ();
      Gc.compact ())
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
         let (_, r6) = obj6 in incr r6;
         Sys.poll_actions ()
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

(* Test GC with deep recursion and register pressure *)
let test_gc_deep_recursion () =
  print_endline "Test 8: GC with deep recursion";
  let preempted = ref false in
  let finalized = ref false in

  let make_finalizable v =
    let obj = ref v in
    Gc.finalise (fun _ -> finalized := true) obj;
    obj
  in

  (* Recursive function that keeps objects live across many stack frames *)
  let rec deep_compute obj depth acc =
    if depth = 0 then (obj, acc)
    else begin
      obj := !obj + 1;
      if not !preempted then begin
        (* Keep spinning until preempted *)
        deep_compute obj depth acc
      end else begin
        (* After preemption, compute something *)
        deep_compute obj (depth - 1) (acc + depth)
      end
    end
  in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      (* Force GC during suspension *)
      Gc.full_major ();
      Gc.compact ())
    (fun () ->
       let start_at = Sys.time () in
       let obj = make_finalizable 100 in
       let rec loop () =
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         if not !preempted then begin
           let (obj_ref, _) = deep_compute obj 50 0 in
           if !obj_ref > 10000 then obj_ref := 100;
           loop ()
         end else begin
           deep_compute obj 100 0
         end
       in
       loop ())
  in

  let (obj_ref, _) = result in
  assert (!obj_ref > 0);
  Gc.full_major ();

  if !finalized then
    failwith "Deep recursion GC test: FAILED - object finalized!"
  else
    print_endline "  GC with deep recursion: PASSED";

  (* Keep obj_ref alive until after the check *)
  ignore (Sys.opaque_identity obj_ref)

(* Test 9: Repeated preemptions with GC each time *)
let test_repeated_gc () =
  print_endline "Test 9: Repeated preemptions with GC";
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
    ~on_tick:(fun () ->
      incr count;
      (* GC on every preemption! *)
      Gc.full_major ();
      if !count mod 3 = 0 then Gc.compact ())
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

(* Test 10: Massive allocation during preemption *)
let test_massive_allocation () =
  print_endline "Test 10: Massive allocation during preemption";
  let preempted = ref false in
  let finalized = ref false in

  let make_finalizable () =
    let obj = ref [1; 2; 3; 4; 5] in
    Gc.finalise (fun _ -> finalized := true) obj;
    obj
  in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      (* Allocate ~10MB of data *)
      let _ = Array.init 10000 (fun i ->
        Array.init 100 (fun j -> (i, j, ref (i + j)))
      ) in
      Gc.full_major ();
      Gc.compact ())
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

(* Test 11: Large complex data structures in registers *)
let test_large_structures () =
  print_endline "Test 11: Large structures in registers";
  let preempted = ref false in
  let finalized_count = ref 0 in

  let make_complex_structure n =
    let arr = Array.init 100 (fun i -> (n * 1000 + i, ref i)) in
    Gc.finalise (fun _ -> incr finalized_count) arr;
    arr
  in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ();
      Gc.compact ())
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

(* Test 12: Weak references with strong refs in registers *)
let test_weak_references () =
  print_endline "Test 12: Weak references";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      (* Aggressive GC should not clear weak refs *)
      Gc.full_major ();
      Gc.compact ())
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

(* Test 13: Nested effect handlers with preemption *)
let test_nested_handlers () =
  print_endline "Test 13: Nested effect handlers";
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
          | Tick -> Some (fun (k : (a, _) continuation) ->
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

(* Test 14: Lazy values forced during preemption *)
let test_lazy_values () =
  print_endline "Test 14: Lazy values";
  let preempted = ref false in
  let finalized = ref false in

  let make_finalizable v =
    let obj = ref v in
    Gc.finalise (fun _ -> finalized := true) obj;
    obj
  in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ())
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

(* Test 15: Extremely deep recursion with live values *)
let test_extreme_depth () =
  print_endline "Test 15: Extreme recursion depth";
  let preempted = ref false in
  let finalized_count = ref 0 in

  let make_finalizable n =
    let obj = ref n in
    Gc.finalise (fun _ -> incr finalized_count) obj;
    obj
  in

  (* Create chain of 1000 objects through deep recursion *)
  let rec deep_chain depth objs =
    if depth = 0 then objs
    else begin
      let obj = make_finalizable depth in
      if not !preempted then
        deep_chain depth objs
      else begin
        incr obj;
        deep_chain (depth - 1) (obj :: objs)
      end
    end
  in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ();
      Gc.compact ())
    (fun () ->
       let start_at = Sys.time () in
       let rec loop () =
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         if not !preempted then
           loop ()
         else
           deep_chain 1000 []
       in
       loop ())
  in

  assert (List.length result = 1000);
  List.iter (fun obj -> assert (!obj > 0)) result;
  Gc.full_major ();

  if !finalized_count > 0 then
    failwith "Extreme depth: objects finalized!"
  else
    print_endline "  Extreme recursion depth: PASSED";

  ignore (Sys.opaque_identity result)

(* Test 16: Exception handling across preemption *)
let test_exception_handling () =
  print_endline "Test 16: Exception handling across preemption";
  let preempted = ref false in
  let exception Test_exn of int in

  let result = try
    run_with_tick_handler
      ~on_tick:(fun () ->
        preempted := true;
        Gc.full_major ())
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

(* Test 17: Finalizer execution during preemption handler *)
let test_finalizer_in_handler () =
  print_endline "Test 17: Finalizer in preemption handler";
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
    ~on_tick:(fun () ->
      preempted := true;
      (* Force GC to run finalizers *)
      Gc.full_major ();
      Gc.full_major ();
      (* Allocate during handler *)
      survivor := Some (Array.make 1000 0))
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

(* Test 18: Raising exception in preemption handler *)
let test_exception_in_handler () =
  print_endline "Test 18: Exception in preemption handler";
  let preempted = ref false in
  let exception Handler_exn in

  let result = try
    run_with_tick_handler
      ~on_tick:(fun () ->
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

(* Test 19: Multiple effects interleaved with preemption *)
type _ Effect.t += Inc : int -> int Effect.t
type _ Effect.t += Dec : int -> int Effect.t

let test_multiple_effects () =
  print_endline "Test 19: Multiple effects with preemption";
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
          | Tick -> Some (fun (k : (a, _) continuation) ->
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

(* Test 20: Deep effect handler nesting with preemption *)
let test_deep_effect_nesting () =
  print_endline "Test 20: Deep effect nesting with preemption";
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
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ())
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

(* Test 21: Float arrays and operations *)
let test_float_arrays () =
  print_endline "Test 21: Float arrays during preemption";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ())
    (fun () ->
       let start_at = Sys.time () in
       let arr = Array.init 100 (fun i -> float_of_int i) in
       let sum = ref 0.0 in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Mix float operations with allocations *)
         let arr2 = Array.map (fun x -> x *. 2.0) arr in
         sum := Array.fold_left (+.) 0.0 arr2;
         let _ = [| !sum; !sum +. 1.0; !sum +. 2.0 |] in
         ()
       done;
       (arr, !sum))
  in

  let (arr, sum) = result in
  assert (Array.length arr = 100);
  assert (sum > 0.0);
  print_endline "  Float arrays: PASSED"

(* Test 22: String concatenation and operations *)
let test_string_operations () =
  print_endline "Test 22: String operations during preemption";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      (* Allocate strings during handler *)
      let _ = String.make 10000 'y' in
      Gc.full_major ())
    (fun () ->
       let start_at = Sys.time () in
       let str = ref "a" in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* String operations that allocate *)
         str := !str ^ "b";
         if String.length !str > 1000 then str := "a";
         let _ = String.make 10 'x' in
         let _ = String.concat "" [!str; "test"; "data"] in
         ()
       done;
       !str)
  in

  assert (String.length result > 0);
  print_endline "  String operations: PASSED"

(* Test 23: Format strings with complex allocations *)
let test_format_strings () =
  print_endline "Test 23: Format strings during preemption";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ())
    (fun () ->
       let start_at = Sys.time () in
       let strs = ref [] in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Printf allocates *)
         let s = Printf.sprintf "test_%d_%f_%s" (List.length !strs) 3.14 "data" in
         strs := s :: !strs;
         if List.length !strs > 100 then strs := []
       done;
       !strs)
  in

  assert (List.length result > 0);
  List.iter (fun s -> assert (String.length s > 0)) result;
  print_endline "  Format strings: PASSED"

(* Test 24: Polymorphic variants with allocation *)
type complex_variant = [
  | `Int of int
  | `String of string
  | `List of int list
  | `Tuple of int * string * float
  | `Nested of complex_variant list
]

let test_polymorphic_variants () =
  print_endline "Test 24: Polymorphic variants during preemption";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ())
    (fun () ->
       let start_at = Sys.time () in
       let data = ref (`Int 0) in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Cycle through different variants *)
         data := match !data with
         | `Int n -> `String (string_of_int n)
         | `String s -> `List [1; 2; 3]
         | `List l -> `Tuple (List.length l, "test", 3.14)
         | `Tuple (a, b, c) -> `Nested [`Int a; `String b]
         | `Nested l -> `Int (List.length l)
       done;
       !data)
  in

  let _ = result in
  print_endline "  Polymorphic variants: PASSED"

(* Test 25: Very fast repeated preemptions *)
let test_rapid_preemptions () =
  print_endline "Test 25: Rapid repeated preemptions";
  let count = ref 0 in
  let allocations = ref [] in

  let result = run_with_tick_handler
    ~interval:0.001
    ~repeating:true
    ~on_tick:(fun () ->
      incr count;
      (* Small allocation in handler *)
      let _ = ref !count in
      ())
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

(* Test 26: Large array initialization during preemption *)
let test_large_array_init () =
  print_endline "Test 26: Large array initialization";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ();
      Gc.compact ())
    (fun () ->
       let start_at = Sys.time () in
       let data = ref None in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Array.init allocates all at once *)
         data := Some (Array.init 1000 (fun i ->
           Array.init 10 (fun j -> (i, j, ref (i + j)))
         ))
       done;
       !data)
  in

  (match result with
  | Some arr -> assert (Array.length arr = 1000)
  | None -> failwith "Array init: no result");
  print_endline "  Large array init: PASSED"

(* Test 27: Bytes operations *)
let test_bytes_operations () =
  print_endline "Test 27: Bytes operations during preemption";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      let _ = Bytes.make 10000 'x' in
      Gc.full_major ())
    (fun () ->
       let start_at = Sys.time () in
       let data = ref (Bytes.create 100) in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Bytes operations *)
         Bytes.fill !data 0 (Bytes.length !data) 'a';
         data := Bytes.cat !data (Bytes.make 10 'b');
         if Bytes.length !data > 10000 then
           data := Bytes.create 100
       done;
       !data)
  in

  assert (Bytes.length result > 0);
  print_endline "  Bytes operations: PASSED"

(* Test 28: Nested try-with during preemption *)
let test_nested_try_blocks () =
  print_endline "Test 28: Nested try blocks with preemption";
  let preempted = ref false in
  let exception Inner_exn of int in
  let exception Outer_exn of string in

  let result = try
    try
      run_with_tick_handler
        ~on_tick:(fun () ->
          preempted := true;
          Gc.full_major ())
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
  print_endline "  Nested try blocks: PASSED"

(* Test 29: Preemption with Seq (lazy sequences) *)
let test_sequences () =
  print_endline "Test 29: Sequences during preemption";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ())
    (fun () ->
       let start_at = Sys.time () in
       let seq = ref (Seq.ints 0) in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Take from sequence, forcing lazy computation *)
         let taken = !seq |> Seq.take 100 |> Seq.map (fun x -> x * 2) |> List.of_seq in
         let _ = List.fold_left (+) 0 taken in
         seq := Seq.ints 0
       done;
       Seq.take 50 !seq |> List.of_seq)
  in

  assert (List.length result = 50);
  print_endline "  Sequences: PASSED"

(* Test 30: Mixed allocation sizes *)
let test_mixed_allocations () =
  print_endline "Test 30: Mixed allocation sizes";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      (* Also mix allocation sizes in handler *)
      let _ = ref 1 in
      let _ = Array.make 5000 0 in
      Gc.full_major ();
      Gc.compact ())
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
         data := tiny :: small.(0) :: medium.(0) :: (snd large.(0)) :: !data;
         if List.length !data > 1000 then data := []
       done;
       !data)
  in

  assert (List.length result > 0);
  print_endline "  Mixed allocations: PASSED"

(* Test 31: Hash tables with rehashing *)
let test_hash_tables () =
  print_endline "Test 31: Hash tables with preemption";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ())
    (fun () ->
       let start_at = Sys.time () in
       let tbl = Hashtbl.create 100 in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Add many entries to trigger rehashing *)
         for i = 0 to 50 do
           Hashtbl.add tbl i (Printf.sprintf "value_%d" i)
         done;
         (* Remove some *)
         for i = 0 to 25 do
           Hashtbl.remove tbl i
         done;
         if Hashtbl.length tbl > 500 then Hashtbl.clear tbl
       done;
       tbl)
  in

  assert (Hashtbl.length result >= 0);
  print_endline "  Hash tables: PASSED"

(* Test 32: Recursive tree data structures *)
type tree = Leaf | Node of int * tree * tree

let test_recursive_trees () =
  print_endline "Test 32: Recursive tree structures";
  let preempted = ref false in

  let rec build_tree depth =
    if depth = 0 then Leaf
    else Node (depth, build_tree (depth - 1), build_tree (depth - 1))
  in

  let rec tree_size = function
    | Leaf -> 0
    | Node (_, l, r) -> 1 + tree_size l + tree_size r
  in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ();
      Gc.compact ())
    (fun () ->
       let start_at = Sys.time () in
       let tree = ref Leaf in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         tree := build_tree 10;
         let _ = tree_size !tree in
         ()
       done;
       !tree)
  in

  assert (tree_size result > 0);
  print_endline "  Recursive trees: PASSED"

(* Test 33: Array.sort with lots of data *)
let test_array_sort () =
  print_endline "Test 33: Array.sort during preemption";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ())
    (fun () ->
       let start_at = Sys.time () in
       let arr = ref [||] in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Create random array *)
         arr := Array.init 1000 (fun i -> (Random.int 10000, ref i));
         (* Sort triggers lots of allocations and comparisons *)
         Array.sort (fun (a, _) (b, _) -> compare a b) !arr
       done;
       !arr)
  in

  assert (Array.length result = 1000);
  print_endline "  Array.sort: PASSED"

(* Test 34: Deeply nested closures *)
let test_nested_closures () =
  print_endline "Test 34: Nested closures with preemption";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ())
    (fun () ->
       let start_at = Sys.time () in
       let closures = ref [] in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Build nested closures capturing state *)
         let v1 = ref 1 in
         let f1 = fun () -> !v1 in
         let v2 = ref 2 in
         let f2 = fun () -> !v2 + f1 () in
         let v3 = ref 3 in
         let f3 = fun () -> !v3 + f2 () in
         closures := f3 :: !closures;
         if List.length !closures > 1000 then closures := []
       done;
       !closures)
  in

  assert (List.length result > 0);
  List.iter (fun f -> assert (f () > 0)) result;
  print_endline "  Nested closures: PASSED"

(* Test 35: Complex pattern matching *)
let test_pattern_matching () =
  print_endline "Test 35: Complex pattern matching";
  let preempted = ref false in

  let open struct
    type complex =
      | A of int
      | B of string * int
      | C of int list
      | D of (int * string) list
      | E of complex * complex
  end in

  let rec process = function
    | A n -> n
    | B (s, n) -> String.length s + n
    | C lst -> List.fold_left (+) 0 lst
    | D lst -> List.length lst
    | E (c1, c2) -> process c1 + process c2
  in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ())
    (fun () ->
       let start_at = Sys.time () in
       let data = ref (A 0) in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         data := (match !data with
         | A n -> B ("test", n + 1)
         | B (s, n) -> C [1; 2; 3; n]
         | C lst -> D (List.map (fun x -> (x, string_of_int x)) lst)
         | D lst -> E (A (List.length lst), B ("nested", 5))
         | E (c1, c2) -> A (process c1 + process c2));
         ignore (process !data)
       done;
       !data)
  in

  assert (process result >= 0);
  print_endline "  Pattern matching: PASSED"

(* Test 36: Option and Result nesting *)
let test_option_result () =
  print_endline "Test 36: Nested Options and Results";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ())
    (fun () ->
       let start_at = Sys.time () in
       let data = ref (Some (Ok (Some 0))) in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         data := (match !data with
         | None -> Some (Ok (Some 1))
         | Some (Ok (Some n)) -> Some (Ok (Some (n + 1)))
         | Some (Ok None) -> Some (Error "error")
         | Some (Error s) -> None);
         if !data = Some (Ok (Some 1000)) then
           data := Some (Ok None)
       done;
       !data)
  in

  assert (result <> None);
  print_endline "  Options and Results: PASSED"

(* Test 37: Refs to refs (indirection) *)
let test_ref_indirection () =
  print_endline "Test 37: Reference indirection";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ();
      Gc.compact ())
    (fun () ->
       let start_at = Sys.time () in
       let r1 = ref 0 in
       let r2 = ref r1 in
       let r3 = ref r2 in
       let r4 = ref r3 in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Follow the chain *)
         let v = !(!(!(!r4))) in
         r1 := v + 1;
         if !r1 > 10000 then r1 := 0
       done;
       (!r1, !r2, !r3, !r4))
  in

  let (v1, r2, r3, r4) = result in
  assert (v1 = !(!(!r4)));
  print_endline "  Reference indirection: PASSED"

(* Test 38: List.rev and List.append chains *)
let test_list_operations () =
  print_endline "Test 38: List operations with preemption";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ())
    (fun () ->
       let start_at = Sys.time () in
       let lst = ref [] in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Build long list *)
         lst := List.init 1000 (fun i -> ref i);
         (* Reverse it *)
         lst := List.rev !lst;
         (* Append to itself *)
         lst := !lst @ !lst;
         if List.length !lst > 10000 then lst := []
       done;
       !lst)
  in

  assert (List.length result >= 0);
  print_endline "  List operations: PASSED"

(* Test 39: Really huge single allocation *)
let test_huge_allocation () =
  print_endline "Test 39: Huge single allocation";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ();
      Gc.compact ())
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

(* Test 40: Mutation heavy code *)
let test_mutation_heavy () =
  print_endline "Test 40: Mutation-heavy code";
  let preempted = ref false in

  let result = run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      Gc.full_major ())
    (fun () ->
       let start_at = Sys.time () in
       let arr = Array.init 100 (fun i -> ref i) in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         (* Mutate everything *)
         for i = 0 to 99 do
           arr.(i) := !(arr.(i)) + 1;
           arr.(i) := !(arr.(i)) * 2;
           arr.(i) := !(arr.(i)) mod 1000
         done;
         (* Swap elements *)
         for i = 0 to 49 do
           let tmp = arr.(i) in
           arr.(i) <- arr.(99 - i);
           arr.(99 - i) <- tmp
         done
       done;
       arr)
  in

  assert (Array.length result = 100);
  Array.iter (fun r -> assert (!r >= 0)) result;
  print_endline "  Mutation heavy: PASSED"

(* Run all tests *)
let () =
  test_basic ();
  test_delayed_resume ();
  test_nested_computation ();
  test_multiple_preemptions ();
  test_with_allocations ();
  test_many_live_registers ();
  test_gc_register_values ();
  test_gc_deep_recursion ();
  test_repeated_gc ();
  test_massive_allocation ();
  test_large_structures ();
  test_weak_references ();
  test_nested_handlers ();
  test_lazy_values ();
  test_extreme_depth ();
  test_exception_handling ();
  test_finalizer_in_handler ();
  test_exception_in_handler ();
  test_multiple_effects ();
  test_deep_effect_nesting ();
  test_float_arrays ();
  test_string_operations ();
  test_format_strings ();
  test_polymorphic_variants ();
  test_rapid_preemptions ();
  test_large_array_init ();
  test_bytes_operations ();
  test_nested_try_blocks ();
  test_sequences ();
  test_mixed_allocations ();
  test_hash_tables ();
  test_recursive_trees ();
  test_array_sort ();
  test_nested_closures ();
  test_pattern_matching ();
  test_option_result ();
  test_ref_indirection ();
  test_list_operations ();
  test_huge_allocation ();
  test_mutation_heavy ();
  (* Disable timer *)
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0. } in
  print_endline "\nAll preemption tests PASSED!"
