(* TEST
   include unix;
   hasunix;
   flags += "-alert -unsafe_multidomain -w -21";
   { native; }
*)

open Effect
open Effect.Deep

external preempt_self : unit -> unit = "caml_domain_preempt_self"

type _ Effect.t += Nested : int -> int Effect.t

(* Basic preemption test - immediate resume *)
let test_basic () =
  print_endline "Test 1: Basic preemption";
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0.1 } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
  let preempted = ref false in
  let x = ref 0 in
  let _ = try_with
    (fun () ->
       let start_at = Sys.time () in
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         x := !x + 1
       done;
       assert (!x > 0);
       print_endline "  Basic preemption: PASSED")
    ()
    { effc = (fun (type a) (e : a t) ->
        match e with
        | Tick -> Some (fun (k : (a, _) continuation) ->
          preempted := true;
          continue k ())
        | _ -> None) } in
  ()

(* Test delayed continuation resume - registers might be clobbered *)
let test_delayed_resume () =
  print_endline "Test 2: Delayed resume with live values";
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0.1 } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
  let preempted = ref false in
  let result = ref None in
  let _ = try_with
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
       "completed")
    ()
    { effc = (fun (type a) (e : a t) ->
        match e with
        | Tick -> Some (fun (k : (a, _) continuation) ->
          preempted := true;
          (* Do some work before resuming *)
          let _ = ref 1 in
          let _ = ref 2 in
          let _ = ref 3 in
          continue k ())
        | _ -> None) } in

  match !result with
  | Some _ -> print_endline "  Delayed resume: PASSED"
  | None -> failwith "Delayed resume: FAILED - no result"

(* Test with nested computations *)
let test_nested_computation () =
  print_endline "Test 3: Nested computation with preemption";
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0.1 } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
  let preempted = ref false in

  let rec compute_nested depth acc =
    if depth = 0 then acc
    else begin
      let v = acc + depth in
      compute_nested (depth - 1) v
    end
  in

  let _ = try_with
    (fun () ->
       let start_at = Sys.time () in
       let result = ref 0 in
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         result := compute_nested 100 !result;
         if !result > 100000 then result := 0
       done;
       !result)
    ()
    { effc = (fun (type a) (e : a t) ->
        match e with
        | Tick -> Some (fun (k : (a, _) continuation) ->
          preempted := true;
          continue k ())
        | _ -> None) } in

  print_endline "  Nested computation: PASSED"

(* Test multiple preemptions in sequence *)
let test_multiple_preemptions () =
  print_endline "Test 4: Multiple preemptions";
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.05; it_value = 0.05 } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
  let count = ref 0 in
  let iters = ref 0 in

  let _ = try_with
    (fun () ->
       let start_at = Sys.time () in
       while !count < 5 do
         if Sys.time () -. start_at > 10.
         then failwith "Multiple preemptions timed out!";
         incr iters
       done;
       !count)
    ()
    { effc = (fun (type a) (e : a t) ->
        match e with
        | Tick -> Some (fun (k : (a, _) continuation) ->
          incr count;
          continue k ())
        | _ -> None) } in

  if !count >= 5 then
    print_endline "  Multiple preemptions: PASSED"
  else
    failwith "Multiple preemptions: insufficient count"

(* Test preemption with allocation pressure *)
let test_with_allocations () =
  print_endline "Test 5: Preemption with allocations";
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0.1 } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
  let preempted = ref false in
  let data = ref [] in

  let _ = try_with
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
       "done")
    ()
    { effc = (fun (type a) (e : a t) ->
        match e with
        | Tick -> Some (fun (k : (a, _) continuation) ->
          preempted := true;
          (* Allocate during preemption handling *)
          let _ = Array.make 10 0 in
          continue k ())
        | _ -> None) } in

  print_endline "  Preemption with allocations: PASSED"

(* Test preemption with many live registers *)
let test_many_live_registers () =
  print_endline "Test 6: Many live registers";
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0.1 } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
  let preempted = ref false in

  let _ =
    try_with
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
         "ok")
      ()
      { effc = (fun (type a) (e : a t) ->
          match e with
          | Tick -> Some (fun (k : (a, _) continuation) ->
            preempted := true;
            continue k ())
          | _ -> None) }
    in

    print_endline "  Many live registers: PASSED"

(* Test that register-only values survive GC during preemption *)
let test_gc_register_values () =
  print_endline "Test 7: Register values survive GC";
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0.1 } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
  let preempted = ref false in
  let finalized_count = ref 0 in

  (* Create objects with finalizers that we'll keep live *)
  let make_finalizable n =
    let obj = (n, ref 0) in
    Gc.finalise (fun _ -> incr finalized_count) obj;
    obj
  in

  let result = try_with
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
    ()
    { effc = (fun (type a) (e : a t) ->
        match e with
        | Tick -> Some (fun (k : (a, _) continuation) ->
          preempted := true;
          (* Force aggressive GC while suspended *)
          Gc.full_major ();
          Gc.compact ();
          (* Allocate some garbage to stress the GC *)
          let _ = Array.init 1000 (fun i -> (i, ref i)) in
          Gc.full_major ();
          Gc.compact ();
          continue k ())
        | _ -> None) }
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
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0.1 } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
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

  let result = try_with
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
    ()
    { effc = (fun (type a) (e : a t) ->
        match e with
        | Tick -> Some (fun (k : (a, _) continuation) ->
          preempted := true;
          (* Force GC during suspension *)
          Gc.full_major ();
          Gc.compact ();
          continue k ())
        | _ -> None) }
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
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.02; it_value = 0.02 } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
  let count = ref 0 in
  let finalized_count = ref 0 in

  let make_finalizable n =
    let obj = (n, ref 0) in
    Gc.finalise (fun _ -> incr finalized_count) obj;
    obj
  in

  let result = try_with
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
    ()
    { effc = (fun (type a) (e : a t) ->
        match e with
        | Tick -> Some (fun (k : (a, _) continuation) ->
          incr count;
          (* GC on every preemption! *)
          Gc.full_major ();
          if !count mod 3 = 0 then Gc.compact ();
          continue k ())
        | _ -> None) }
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
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0.1 } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
  let preempted = ref false in
  let finalized = ref false in

  let make_finalizable () =
    let obj = ref [1; 2; 3; 4; 5] in
    Gc.finalise (fun _ -> finalized := true) obj;
    obj
  in

  let result = try_with
    (fun () ->
       let start_at = Sys.time () in
       let obj = make_finalizable () in

       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         obj := List.map (fun x -> x + 1) !obj
       done;
       obj)
    ()
    { effc = (fun (type a) (e : a t) ->
        match e with
        | Tick -> Some (fun (k : (a, _) continuation) ->
          preempted := true;
          (* Allocate ~10MB of data *)
          let _ = Array.init 10000 (fun i ->
            Array.init 100 (fun j -> (i, j, ref (i + j)))
          ) in
          Gc.full_major ();
          Gc.compact ();
          continue k ())
        | _ -> None) }
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
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0.1 } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
  let preempted = ref false in
  let finalized_count = ref 0 in

  let make_complex_structure n =
    let arr = Array.init 100 (fun i -> (n * 1000 + i, ref i)) in
    Gc.finalise (fun _ -> incr finalized_count) arr;
    arr
  in

  let result = try_with
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
    ()
    { effc = (fun (type a) (e : a t) ->
        match e with
        | Tick -> Some (fun (k : (a, _) continuation) ->
          preempted := true;
          Gc.full_major ();
          Gc.compact ();
          continue k ())
        | _ -> None) }
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
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0.1 } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
  let preempted = ref false in

  let result = try_with
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
    ()
    { effc = (fun (type a) (e : a t) ->
        match e with
        | Tick -> Some (fun (k : (a, _) continuation) ->
          preempted := true;
          (* Aggressive GC should not clear weak refs *)
          Gc.full_major ();
          Gc.compact ();
          continue k ())
        | _ -> None) }
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
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0.1 } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
  let preempted = ref false in
  let finalized = ref false in

  let make_finalizable v =
    let obj = ref v in
    Gc.finalise (fun _ -> finalized := true) obj;
    obj
  in

  let result = try_with
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
        | _ -> None) }
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
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0.1 } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
  let preempted = ref false in
  let finalized = ref false in

  let make_finalizable v =
    let obj = ref v in
    Gc.finalise (fun _ -> finalized := true) obj;
    obj
  in

  let result = try_with
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
    ()
    { effc = (fun (type a) (e : a t) ->
        match e with
        | Tick -> Some (fun (k : (a, _) continuation) ->
          preempted := true;
          Gc.full_major ();
          continue k ())
        | _ -> None) }
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
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0.1 } in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in
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

  let result = try_with
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
    ()
    { effc = (fun (type a) (e : a t) ->
        match e with
        | Tick -> Some (fun (k : (a, _) continuation) ->
          preempted := true;
          Gc.full_major ();
          Gc.compact ();
          continue k ())
        | _ -> None) }
  in

  assert (List.length result = 1000);
  List.iter (fun obj -> assert (!obj > 0)) result;
  Gc.full_major ();

  if !finalized_count > 0 then
    failwith "Extreme depth: objects finalized!"
  else
    print_endline "  Extreme recursion depth: PASSED";

  ignore (Sys.opaque_identity result)

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
  (* Disable timer *)
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0. } in
  print_endline "\nAll preemption tests PASSED!"
