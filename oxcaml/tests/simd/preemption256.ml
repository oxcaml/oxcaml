(* Test that YMM (256-bit) registers are correctly saved and restored during
   native runtime preemption via effects. *)

open Effect
open Effect.Deep

external preempt_self : unit -> unit = "caml_domain_preempt_self" [@@noalloc]

external int64x4_of_int64s : int64 -> int64 -> int64 -> int64 -> int64x4
  = "" "vec256_of_int64s"
[@@noalloc] [@@unboxed]

external int64x4_first_int64 : int64x4 -> int64 = "" "vec256_first_int64"
[@@noalloc] [@@unboxed]

external int64x4_second_int64 : int64x4 -> int64 = "" "vec256_second_int64"
[@@noalloc] [@@unboxed]

external int64x4_third_int64 : int64x4 -> int64 = "" "vec256_third_int64"
[@@noalloc] [@@unboxed]

external int64x4_fourth_int64 : int64x4 -> int64 = "" "vec256_fourth_int64"
[@@noalloc] [@@unboxed]

(* This function takes 16 YMM arguments, forcing them all to be in registers *)
external lots_of_vectors :
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 = "" "lots_of_vectors256"
[@@noalloc] [@@unboxed]

let[@inline never] check_ymm name v a b c d =
  let v1 = int64x4_first_int64 v in
  let v2 = int64x4_second_int64 v in
  let v3 = int64x4_third_int64 v in
  let v4 = int64x4_fourth_int64 v in
  if v1 <> a || v2 <> b || v3 <> c || v4 <> d
  then begin
    Printf.printf
      "%s: got (%Ld, %Ld, %Ld, %Ld), expected (%Ld, %Ld, %Ld, %Ld)\n" name v1 v2
      v3 v4 a b c d;
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

(* Test: YMM preservation across preemption with all 16 registers live. The key
   is using lots_of_vectors which takes 16 YMM arguments, forcing them all to be
   in registers at the call site. *)
let test_ymm_preservation () =
  let preempted = ref false in
  let sum = ref (int64x4_of_int64s 0L 0L 0L 0L) in
  run_with_tick_handler
    ~on_tick:(fun () -> preempted := true)
    (fun () ->
      let v0 = int64x4_of_int64s 1L 2L 3L 4L in
      let v1 = int64x4_of_int64s 5L 6L 7L 8L in
      let v2 = int64x4_of_int64s 9L 10L 11L 12L in
      let v3 = int64x4_of_int64s 13L 14L 15L 16L in
      let v4 = int64x4_of_int64s 17L 18L 19L 20L in
      let v5 = int64x4_of_int64s 21L 22L 23L 24L in
      let v6 = int64x4_of_int64s 25L 26L 27L 28L in
      let v7 = int64x4_of_int64s 29L 30L 31L 32L in
      let v8 = int64x4_of_int64s 33L 34L 35L 36L in
      let v9 = int64x4_of_int64s 37L 38L 39L 40L in
      let v10 = int64x4_of_int64s 41L 42L 43L 44L in
      let v11 = int64x4_of_int64s 45L 46L 47L 48L in
      let v12 = int64x4_of_int64s 49L 50L 51L 52L in
      let v13 = int64x4_of_int64s 53L 54L 55L 56L in
      let v14 = int64x4_of_int64s 57L 58L 59L 60L in
      let v15 = int64x4_of_int64s 61L 62L 63L 64L in
      let start_at = Sys.time () in
      while not !preempted do
        if Sys.time () -. start_at > 5.
        then failwith "Didn't get preempted after 5s!";
        (* Call lots_of_vectors to force all 16 YMM values into registers. This
           creates a poll point with live YMM registers. *)
        sum
          := lots_of_vectors v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14
               v15
      done;
      (* After preemption, verify the sum is correct. Expected sum: each
         component is sum of (1..64 step 4) = 496, 512, 528, 544 *)
      check_ymm "sum" !sum 496L 512L 528L 544L;
      (* Also verify individual vectors are preserved *)
      check_ymm "v0" v0 1L 2L 3L 4L;
      check_ymm "v1" v1 5L 6L 7L 8L;
      check_ymm "v2" v2 9L 10L 11L 12L;
      check_ymm "v3" v3 13L 14L 15L 16L;
      check_ymm "v4" v4 17L 18L 19L 20L;
      check_ymm "v5" v5 21L 22L 23L 24L;
      check_ymm "v6" v6 25L 26L 27L 28L;
      check_ymm "v7" v7 29L 30L 31L 32L;
      check_ymm "v8" v8 33L 34L 35L 36L;
      check_ymm "v9" v9 37L 38L 39L 40L;
      check_ymm "v10" v10 41L 42L 43L 44L;
      check_ymm "v11" v11 45L 46L 47L 48L;
      check_ymm "v12" v12 49L 50L 51L 52L;
      check_ymm "v13" v13 53L 54L 55L 56L;
      check_ymm "v14" v14 57L 58L 59L 60L;
      check_ymm "v15" v15 61L 62L 63L 64L)

(* Test: Multiple preemptions with YMM registers *)
let test_multiple_preemptions () =
  let count = ref 0 in
  let sum = ref (int64x4_of_int64s 0L 0L 0L 0L) in
  run_with_tick_handler ~interval:0.05 ~repeating:true
    ~on_tick:(fun () -> incr count)
    (fun () ->
      let v0 = int64x4_of_int64s 1L 2L 3L 4L in
      let v1 = int64x4_of_int64s 5L 6L 7L 8L in
      let v2 = int64x4_of_int64s 9L 10L 11L 12L in
      let v3 = int64x4_of_int64s 13L 14L 15L 16L in
      let v4 = int64x4_of_int64s 17L 18L 19L 20L in
      let v5 = int64x4_of_int64s 21L 22L 23L 24L in
      let v6 = int64x4_of_int64s 25L 26L 27L 28L in
      let v7 = int64x4_of_int64s 29L 30L 31L 32L in
      let v8 = int64x4_of_int64s 33L 34L 35L 36L in
      let v9 = int64x4_of_int64s 37L 38L 39L 40L in
      let v10 = int64x4_of_int64s 41L 42L 43L 44L in
      let v11 = int64x4_of_int64s 45L 46L 47L 48L in
      let v12 = int64x4_of_int64s 49L 50L 51L 52L in
      let v13 = int64x4_of_int64s 53L 54L 55L 56L in
      let v14 = int64x4_of_int64s 57L 58L 59L 60L in
      let v15 = int64x4_of_int64s 61L 62L 63L 64L in
      let start_at = Sys.time () in
      while !count < 5 do
        if Sys.time () -. start_at > 10.
        then failwith "Multiple preemptions timed out!";
        sum
          := lots_of_vectors v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14
               v15
      done;
      check_ymm "sum" !sum 496L 512L 528L 544L)

(* Test: GC stress during preemption *)
let test_gc_stress () =
  let preempted = ref false in
  let sum = ref (int64x4_of_int64s 0L 0L 0L 0L) in
  run_with_tick_handler
    ~on_tick:(fun () ->
      preempted := true;
      (* Allocate during preemption to trigger GC *)
      let _ = Array.init 10000 (fun i -> i) in
      Gc.minor ();
      ())
    (fun () ->
      let v0 = int64x4_of_int64s 1L 2L 3L 4L in
      let v1 = int64x4_of_int64s 5L 6L 7L 8L in
      let v2 = int64x4_of_int64s 9L 10L 11L 12L in
      let v3 = int64x4_of_int64s 13L 14L 15L 16L in
      let v4 = int64x4_of_int64s 17L 18L 19L 20L in
      let v5 = int64x4_of_int64s 21L 22L 23L 24L in
      let v6 = int64x4_of_int64s 25L 26L 27L 28L in
      let v7 = int64x4_of_int64s 29L 30L 31L 32L in
      let v8 = int64x4_of_int64s 33L 34L 35L 36L in
      let v9 = int64x4_of_int64s 37L 38L 39L 40L in
      let v10 = int64x4_of_int64s 41L 42L 43L 44L in
      let v11 = int64x4_of_int64s 45L 46L 47L 48L in
      let v12 = int64x4_of_int64s 49L 50L 51L 52L in
      let v13 = int64x4_of_int64s 53L 54L 55L 56L in
      let v14 = int64x4_of_int64s 57L 58L 59L 60L in
      let v15 = int64x4_of_int64s 61L 62L 63L 64L in
      let start_at = Sys.time () in
      while not !preempted do
        if Sys.time () -. start_at > 5.
        then failwith "Didn't get preempted after 5s!";
        sum
          := lots_of_vectors v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14
               v15
      done;
      check_ymm "sum" !sum 496L 512L 528L 544L)

external runtime5 : unit -> bool = "%runtime5"

let () =
  if runtime5 ()
  then (
    test_ymm_preservation ();
    test_multiple_preemptions ();
    test_gc_stress ())
  else ()
