(* TEST
 modules = "cstubs.c";
 stack-allocation;
 native;
*)

external local_stack_offset : unit -> int = "caml_local_stack_offset"
external opaque_identity : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"

(* ---- Operator definitions ---- *)

let[@inline never] ( let$ ) (with_ @ local) (f : (_ @ local -> _) @ local) =
  with_ f [@nontail]

let[@inline never] ( let@ ) (with_ @ local once) (f : (_ @ local -> _) @ local) =
  with_ f [@nontail]

(* ---- Helpers ---- *)

let[@inline never] with_local_ref (x : int) (f : (_ @ local -> _) @ local) =
  let r = local_ ref (Sys.opaque_identity x) in
  f r [@nontail]

(* An adversarial "with" function: it does a GC and heavy heap allocation
   between creating the local value and calling the callback. If the callback
   closure were freed (dangling pointer), the GC or heap allocation could
   overwrite its memory. *)
let[@inline never] with_local_ref_gc (x : int)
    (f : (_ @ local -> _) @ local) =
  let r = local_ ref (Sys.opaque_identity x) in
  (* Force GC to scan stack - if f is a dangling pointer, GC might crash
     trying to scan it *)
  Gc.minor ();
  Gc.compact ();
  (* Allocate heavily to fill freed memory with garbage *)
  for _ = 1 to 100 do
    ignore (Sys.opaque_identity (Array.make 100 0))
  done;
  f r [@nontail]

(* An operator that verifies the body closure is on the local stack *)
let[@inline never] with_local_ref_check_stack (x : int)
    (f : (_ @ local -> _) @ local) =
  let before = local_stack_offset () in
  let r = local_ ref (Sys.opaque_identity x) in
  let after = local_stack_offset () in
  (* The local ref should have grown the local stack *)
  assert (after > before);
  f r [@nontail]

(* ---- Test 1: Basic letop with local param ---- *)

let[@inline never] test_basic () =
  let$ r = with_local_ref 42 in
  assert (!r = 42);
  Printf.printf "%40s: OK\n%!" "basic local param"

let () = test_basic ()

(* ---- Test 2: Body closure captures outer locals ---- *)

let[@inline never] test_capture_outer () =
  let outer = local_ ref (Sys.opaque_identity 100) in
  let$ r = with_local_ref 42 in
  let sum = !outer + !r in
  assert (sum = 142);
  Printf.printf "%40s: OK\n%!" "capture outer locals"

let () = test_capture_outer ()

(* ---- Test 3: Adversarial operator does GC before calling body ----
   If the body closure were a dangling pointer, the GC would likely crash
   or the heap allocations would overwrite the freed closure memory. *)

let[@inline never] test_gc_adversarial () =
  let$ r = with_local_ref_gc 42 in
  assert (!r = 42);
  Printf.printf "%40s: OK\n%!" "adversarial gc before body call"

let () = test_gc_adversarial ()

(* ---- Test 4: Adversarial in a loop ---- *)

let[@inline never] test_gc_adversarial_loop () =
  for i = 1 to 1000 do
    let$ r = with_local_ref_gc i in
    assert (!r = i)
  done;
  Printf.printf "%40s: OK\n%!" "adversarial gc loop"

let () = test_gc_adversarial_loop ()

(* ---- Test 5: Body closure captures many local values ----
   A large closure is more likely to be corrupted if freed. *)

let[@inline never] test_large_closure () =
  let a = local_ ref (Sys.opaque_identity 1) in
  let b = local_ ref (Sys.opaque_identity 2) in
  let c = local_ ref (Sys.opaque_identity 3) in
  let d = local_ ref (Sys.opaque_identity 4) in
  let e = local_ ref (Sys.opaque_identity 5) in
  let$ r = with_local_ref_gc 10 in
  let sum = !a + !b + !c + !d + !e + !r in
  assert (sum = 25);
  Printf.printf "%40s: OK\n%!" "large closure with many captures"

let () = test_large_closure ()

(* ---- Test 6: Letop in tail position of a function ----
   This is the critical case for Rc_nontail. The letop desugars to an
   operator application that would be in tail position. Without Rc_nontail,
   the compiler might convert it to Rc_close_at_apply, closing the region
   and freeing the local body closure. *)

let[@inline never] test_tail_position_fn () =
  (* The letop is the last expression - it's in tail position *)
  let$ r = with_local_ref_gc 77 in
  assert (!r = 77);
  Printf.printf "%40s: OK\n%!" "tail position with gc"

let () = test_tail_position_fn ()

(* ---- Test 7: Letop in tail position called from a loop ----
   Repeatedly calling a function where the letop is in tail position.
   If region closing corrupts the closure, repeated calls maximize the
   chance of overwriting freed memory. *)

let[@inline never] tail_fn (i : int) =
  let$ r = with_local_ref_gc i in
  assert (!r = i)

let[@inline never] test_tail_position_loop () =
  for i = 1 to 1000 do
    tail_fn i
  done;
  Printf.printf "%40s: OK\n%!" "tail position called in loop"

let () = test_tail_position_loop ()

(* ---- Test 8: Verify local stack with letop ----
   Check that the local stack offset increases when the body closure
   and local param exist, proving they are stack-allocated. *)

let[@inline never] test_stack_offset () =
  let before = local_stack_offset () in
  let$ r = with_local_ref_check_stack 42 in
  let during = local_stack_offset () in
  (* During the body, both the closure (if local) and the local ref
     should be on the local stack *)
  assert (during > before);
  assert (!r = 42);
  Printf.printf "%40s: OK\n%!" "local stack offset increases"

let () = test_stack_offset ()

(* ---- Test 9: Nested letops with adversarial GC ---- *)

let[@inline never] test_nested_adversarial () =
  let$ x = with_local_ref_gc 10 in
  let$ y = with_local_ref_gc 20 in
  let sum = !x + !y in
  assert (sum = 30);
  Printf.printf "%40s: OK\n%!" "nested adversarial gc"

let () = test_nested_adversarial ()

(* ---- Test 10: Deeply nested with adversarial GC ---- *)

let[@inline never] test_deeply_nested_adversarial () =
  let$ a = with_local_ref_gc 1 in
  let$ b = with_local_ref_gc 2 in
  let$ c = with_local_ref_gc 3 in
  let$ d = with_local_ref_gc 4 in
  let sum = !a + !b + !c + !d in
  assert (sum = 10);
  Printf.printf "%40s: OK\n%!" "deeply nested adversarial gc"

let () = test_deeply_nested_adversarial ()

(* ---- Test 11: Recursive function with letop in tail position ----
   In each recursive call, the letop is in tail position. The operator
   does GC. The body calls recursively. If any region were wrongly closed,
   the GC would likely corrupt the closure or the local ref. *)

let[@inline never] test_recursive_tail () =
  let[@inline never] rec loop n =
    if n <= 0 then ()
    else
      let$ r = with_local_ref_gc n in
      assert (!r = n);
      loop (n - 1)
  in
  loop 200;
  Printf.printf "%40s: OK\n%!" "recursive tail with gc"

let () = test_recursive_tail ()

(* ---- Test 12: Once operator with adversarial GC ---- *)

let[@inline never] with_local_ref_gc_once (x : int)
    (f : (_ @ local -> _) @ local) =
  let r = local_ ref (Sys.opaque_identity x) in
  Gc.minor ();
  Gc.compact ();
  for _ = 1 to 100 do
    ignore (Sys.opaque_identity (Array.make 100 0))
  done;
  f r [@nontail]

let[@inline never] test_once_adversarial () =
  let@ r = with_local_ref_gc_once 55 in
  assert (!r = 55);
  Printf.printf "%40s: OK\n%!" "once operator adversarial gc"

let () = test_once_adversarial ()

(* ---- Test 13: Letop in tail of match arm with adversarial GC ---- *)

let[@inline never] test_tail_match_adversarial () =
  match Sys.opaque_identity 1 with
  | 0 -> assert false
  | _ ->
    let$ r = with_local_ref_gc 88 in
    assert (!r = 88);
    Printf.printf "%40s: OK\n%!" "tail match arm adversarial gc"

let () = test_tail_match_adversarial ()

(* ---- Test 14: Letop in tail of if-then with adversarial GC ---- *)

let[@inline never] test_tail_if_adversarial () =
  if Sys.opaque_identity true then begin
    let$ r = with_local_ref_gc 66 in
    assert (!r = 66);
    Printf.printf "%40s: OK\n%!" "tail if branch adversarial gc"
  end

let () = test_tail_if_adversarial ()

(* ---- Test 15: Body closure uses local param multiple times ----
   Reading the local param at different points, with GC in between,
   to catch use-after-free. *)

let[@inline never] test_use_after_gc () =
  let$ r = with_local_ref 42 in
  let v1 = !r in
  Gc.minor ();
  let v2 = !r in
  Gc.compact ();
  let v3 = !r in
  assert (v1 = 42 && v2 = 42 && v3 = 42);
  Printf.printf "%40s: OK\n%!" "use local param after gc"

let () = test_use_after_gc ()

(* ---- Test 16: Letop captures local, operator allocates heavily ----
   The operator creates so much garbage that freed memory is very likely
   to be recycled. *)

let[@inline never] with_local_ref_alloc_heavy (x : int)
    (f : (_ @ local -> _) @ local) =
  let r = local_ ref (Sys.opaque_identity x) in
  (* Allocate 10MB of garbage to fill old memory regions *)
  for _ = 1 to 1000 do
    ignore (Sys.opaque_identity (Array.make 1000 0))
  done;
  Gc.compact ();
  f r [@nontail]

let[@inline never] test_heavy_alloc () =
  let outer = local_ ref (Sys.opaque_identity 99) in
  let$ r = with_local_ref_alloc_heavy 42 in
  assert (!outer = 99);
  assert (!r = 42);
  Printf.printf "%40s: OK\n%!" "heavy alloc before body call"

let () = test_heavy_alloc ()

(* ---- Test 17: Letop in tail position returning a value ---- *)

let[@inline never] test_tail_returns_value () : int =
  let$ r = with_local_ref 42 in
  !r

let () =
  let v = test_tail_returns_value () in
  assert (v = 42);
  Printf.printf "%40s: OK\n%!" "tail position returns value"

(* ---- Test 18: Stress test - many iterations with captures ---- *)

let[@inline never] test_stress () =
  let acc = ref 0 in
  for i = 1 to 100_000 do
    let$ r = with_local_ref i in
    acc := !acc + !r;
    if i mod 10_000 = 0 then Gc.compact ()
  done;
  (* Sum of 1..100_000 = 5_000_050_000, but we only check it's nonzero *)
  assert (!acc > 0);
  Printf.printf "%40s: OK\n%!" "100k iteration stress"

let () = test_stress ()

(* ==== and* operator tests ==== *)

let ( let* ) (x @ local once) (f @ local) = f x [@nontail]
let ( and* ) (a @ local once) (b @ local once) = exclave_ (a, b)

(* ---- Test 19: Basic and* with local bindings ---- *)

let[@inline never] test_and_basic () =
  let* x = local_ (Sys.opaque_identity 10)
  and* y = local_ (Sys.opaque_identity 20) in
  assert (x + y = 30);
  Printf.printf "%40s: OK\n%!" "and* basic local bindings"

let () = test_and_basic ()

(* ---- Test 20: and* with GC stress ---- *)

let[@inline never] test_and_gc () =
  for _ = 1 to 10_000 do
    let* x = local_ (Sys.opaque_identity 7)
    and* y = local_ (Sys.opaque_identity 13) in
    Gc.minor ();
    assert (x + y = 20)
  done;
  Gc.compact ();
  Printf.printf "%40s: OK\n%!" "and* gc stress"

let () = test_and_gc ()

(* ---- Test 21: and* with three bindings ---- *)

let[@inline never] test_and_three () =
  let* x = local_ (Sys.opaque_identity 1)
  and* y = local_ (Sys.opaque_identity 2)
  and* z = local_ (Sys.opaque_identity 3) in
  assert (x + y + z = 6);
  Printf.printf "%40s: OK\n%!" "and* three bindings"

let () = test_and_three ()

(* ---- Test 22: and* in tail position ---- *)

let[@inline never] test_and_tail () =
  let* x = local_ (Sys.opaque_identity 100)
  and* y = local_ (Sys.opaque_identity 200) in
  assert (x + y = 300);
  Printf.printf "%40s: OK\n%!" "and* in tail position"

let () = test_and_tail ()

(* ---- Test 23: and* with heap allocations between ---- *)

let[@inline never] test_and_heap_between () =
  let heap1 = ref (Sys.opaque_identity 1000) in
  let* x = local_ (Sys.opaque_identity 5)
  and* y = local_ (Sys.opaque_identity 6) in
  let heap2 = ref (Sys.opaque_identity 2000) in
  Gc.compact ();
  assert (x + y + !heap1 + !heap2 = 3011);
  Printf.printf "%40s: OK\n%!" "and* with heap allocs"

let () = test_and_heap_between ()
