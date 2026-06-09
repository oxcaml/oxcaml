(* Test that YMM registers are correctly saved and restored during preemption *)

open Effect
open Effect.Deep

external preempt_self : unit -> unit = "caml_domain_preempt_self" [@@noalloc]

external int64x4_of_int64s : int64 -> int64 -> int64 -> int64 -> int64x4#
  = "" "vec256_of_int64s"
[@@noalloc] [@@unboxed]

external int64x4_second_int64 : int64x4# -> int64 = "" "vec256_second_int64"
[@@noalloc] [@@unboxed]

external int64x4_third_int64 : int64x4# -> int64 = "" "vec256_third_int64"
[@@noalloc] [@@unboxed]

external int64x4_fourth_int64 : int64x4# -> int64 = "" "vec256_fourth_int64"
[@@noalloc] [@@unboxed]

(* SIMD integer add: [@@builtin] compiles to vpaddq, not a C call, so it does
   not clobber YMM registers. This lets us keep 16 accumulators live in
   ymm0..ymm15 across the loop back edge where the poll point sits. *)
external int64x4_add : int64x4# -> int64x4# -> int64x4#
  = "caml_vec256_unreachable" "caml_avx2_int64x4_add"
[@@noalloc] [@@unboxed] [@@builtin]

(* Ignores the low lane, which our accumulators use as an iteration counter
   (unpredictable value after preemption). *)
let[@inline never] check_ymm_upper_3 name v b c d =
  let v2 = int64x4_second_int64 v in
  let v3 = int64x4_third_int64 v in
  let v4 = int64x4_fourth_int64 v in
  if v2 <> b || v3 <> c || v4 <> d
  then begin
    Printf.printf
      "%s upper lanes: got (_, %Ld, %Ld, %Ld), expected (_, %Ld, %Ld, %Ld)\n"
      name v2 v3 v4 b c d;
    exit 1
  end

let with_preemption_setup ?(interval = 0.1) ?(repeating = false) f =
  let it_interval = if repeating then interval else 0. in
  let _ = Unix.setitimer ITIMER_REAL { it_interval; it_value = interval } in
  let _ =
    Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ()))
  in
  let result = f () in
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0. } in
  result

let run_with_tick_handler ?(interval = 0.1) ?(repeating = false)
    ?(on_tick = fun () -> ()) computation =
  with_preemption_setup ~interval ~repeating (fun () ->
      try_with computation ()
        { effc =
            (fun (type a) (e : a t) ->
              match e with
              | Preemption ->
                Some
                  (fun (k : (a, _) continuation) ->
                    on_tick ();
                    continue k ())
              | _ -> None)
        })

(* Spins 16 unboxed int64x4 accumulators in a tail-recursive loop, then checks
   their upper lanes. The whole thing is one function because int64x4# can't
   flow out through a normal (boxed) tuple.

   At the back-edge poll point the accumulators are live in YMM registers:
   the tail call is a direct jump with unboxed args, the body is 16 vpaddq
   ([@@builtin], no call), and the loop condition is a single scalar load
   of [!stop]. Nothing in the hot path clobbers YMMs. We have 17 SIMD values
   live (16 accumulators + [one]) against 16 YMM regs so one accumulator does
   get spilled to stack, but ~15 flow through the poll directly in YMMs.

   [one] increments only the low lane. The upper three lanes of each
   accumulator are therefore loop-invariant, so if preemption corrupts a
   YMM we'll see it in the upper-lane check.

   [max_iters] is a safety net: if preemption fails to fire we exit rather
   than hanging. *)
let[@inline never] spin_and_check stop max_iters
    a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 =
  let one = int64x4_of_int64s 1L 0L 0L 0L in
  let rec loop n a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 =
    if !stop || n = 0
    then begin
      if not !stop
      then
        failwith
          (Printf.sprintf "Stop condition never fired after %d iters!"
             (max_iters - n));
      check_ymm_upper_3 "a0" a0 2L 3L 4L;
      check_ymm_upper_3 "a1" a1 6L 7L 8L;
      check_ymm_upper_3 "a2" a2 10L 11L 12L;
      check_ymm_upper_3 "a3" a3 14L 15L 16L;
      check_ymm_upper_3 "a4" a4 18L 19L 20L;
      check_ymm_upper_3 "a5" a5 22L 23L 24L;
      check_ymm_upper_3 "a6" a6 26L 27L 28L;
      check_ymm_upper_3 "a7" a7 30L 31L 32L;
      check_ymm_upper_3 "a8" a8 34L 35L 36L;
      check_ymm_upper_3 "a9" a9 38L 39L 40L;
      check_ymm_upper_3 "a10" a10 42L 43L 44L;
      check_ymm_upper_3 "a11" a11 46L 47L 48L;
      check_ymm_upper_3 "a12" a12 50L 51L 52L;
      check_ymm_upper_3 "a13" a13 54L 55L 56L;
      check_ymm_upper_3 "a14" a14 58L 59L 60L;
      check_ymm_upper_3 "a15" a15 62L 63L 64L
    end
    else
      loop (n - 1)
        (int64x4_add a0 one) (int64x4_add a1 one)
        (int64x4_add a2 one) (int64x4_add a3 one)
        (int64x4_add a4 one) (int64x4_add a5 one)
        (int64x4_add a6 one) (int64x4_add a7 one)
        (int64x4_add a8 one) (int64x4_add a9 one)
        (int64x4_add a10 one) (int64x4_add a11 one)
        (int64x4_add a12 one) (int64x4_add a13 one)
        (int64x4_add a14 one) (int64x4_add a15 one)
  in
  loop max_iters a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15

let run_spin_and_check stop =
  spin_and_check stop 1_000_000_000
    (int64x4_of_int64s 1L 2L 3L 4L)
    (int64x4_of_int64s 5L 6L 7L 8L)
    (int64x4_of_int64s 9L 10L 11L 12L)
    (int64x4_of_int64s 13L 14L 15L 16L)
    (int64x4_of_int64s 17L 18L 19L 20L)
    (int64x4_of_int64s 21L 22L 23L 24L)
    (int64x4_of_int64s 25L 26L 27L 28L)
    (int64x4_of_int64s 29L 30L 31L 32L)
    (int64x4_of_int64s 33L 34L 35L 36L)
    (int64x4_of_int64s 37L 38L 39L 40L)
    (int64x4_of_int64s 41L 42L 43L 44L)
    (int64x4_of_int64s 45L 46L 47L 48L)
    (int64x4_of_int64s 49L 50L 51L 52L)
    (int64x4_of_int64s 53L 54L 55L 56L)
    (int64x4_of_int64s 57L 58L 59L 60L)
    (int64x4_of_int64s 61L 62L 63L 64L)

let test_ymm_preservation () =
  let preempted = ref false in
  run_with_tick_handler
    ~on_tick:(fun () -> preempted := true)
    (fun () -> run_spin_and_check preempted)

let test_multiple_preemptions () =
  let count = ref 0 in
  let stop = ref false in
  run_with_tick_handler ~interval:0.05 ~repeating:true
    ~on_tick:(fun () ->
      incr count;
      if !count >= 5 then stop := true)
    (fun () -> run_spin_and_check stop)

(* Test: GC stress during preemption *)
let test_gc_stress () =
  let preempted = ref false in
  run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      (* Allocate during preemption to trigger GC *)
      let _ = Array.init 10000 (fun i -> i) in
      Gc.minor ();
      ())
    (fun () -> run_spin_and_check preempted)

external runtime5 : unit -> bool = "%runtime5"

(* Dune has no poll-insertion condition available in [enabled_if] on this dune
   lang version, so [make runtest] passes the configure-time value through
   the environment and we gate the body on it here. The tight SIMD loop has
   no C-call or allocation-site polls, so without compiler-inserted back-edge
   polls preemption would never fire and the safety net would [failwith]. *)
let poll_insertion_enabled () =
  match Sys.getenv_opt "OXCAML_POLL_INSERTION" with
  | Some "true" -> true
  | _ -> false

let () =
  if runtime5 () && poll_insertion_enabled ()
  then (
    test_ymm_preservation ();
    test_multiple_preemptions ();
    test_gc_stress ())
  else ()
