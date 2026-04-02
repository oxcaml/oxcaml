external set_gate : bool -> unit = "caml_tpcg_set_gate" [@@noalloc]

external reset : unit -> unit = "caml_tpcg_reset" [@@noalloc]

external num_guards : unit -> int = "caml_tpcg_num_guards" [@@noalloc]

external num_hit : unit -> int = "caml_tpcg_num_hit" [@@noalloc]

external init_calls : unit -> int = "caml_tpcg_init_calls" [@@noalloc]

external misaligned_calls : unit -> int = "caml_tpcg_misaligned_calls"
[@@noalloc]

let[@inline never] classify x = if x > 3 then "big" else "small"

let[@inline never] name = function
  | 0 -> "zero"
  | 1 -> "one"
  | n when n < 0 -> "negative"
  | _ -> "many"

let[@inline never] rec sum acc n = if n <= 0 then acc else sum (acc + n) (n - 1)

let[@inline never] safe_div x y = try x / y with Division_by_zero -> 0

(* Many values live across probes, including floats, which the C callback is
   free to clobber in registers (and the one in [stubs.c] does). *)
let[@inline never] mix a b c d =
  let x = (a *. 2.) +. b in
  let y = if c > 0 then x *. d else x -. d in
  let z = if Float.abs y > 10. then y /. 3. else y +. 1. in
  x +. y +. z +. a +. b +. d +. Float.of_int c

let[@inline never] mix_int a b c d e f g h =
  let x = if a > b then (a * c) + d else (b * d) - c in
  let y = if e land 1 = 0 then x + f else x - g in
  let z = match h with 0 -> y | 1 -> y * 2 | _ -> y + h in
  a + b + c + d + e + f + g + h + x + y + z

(* So many integers live across probes that the register allocator uses every
   register, including [r11], which the probe does not declare as destroyed.
   [Sys.opaque_identity] keeps the values from being computed later. *)
let[@inline never] many_live n =
  let o = Sys.opaque_identity in
  let a = o (n + 1) and b = o (n * 3) and c = o (n lxor 5) and d = o (n - 7) in
  let e = o (n * n)
  and f = o (n lsl 2)
  and g = o (n lor 9)
  and h = o (n * 11) in
  let i = o (n - 13)
  and j = o (n lxor 17)
  and k = o (n * 19)
  and l = o (n + 23) in
  let r = if n > 2 then (a * b) + c else d - e in
  let s = match n land 3 with 0 -> r + f | 1 -> r - g | _ -> r * h in
  a + b + c + d + e + f + g + h + i + j + k + l + r + s

external int64x2_of_int64 : int64 -> int64x2
  = "caml_vec128_unreachable" "caml_int64x2_low_of_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external int64x2_to_int64 : int64x2 -> int64
  = "caml_vec128_unreachable" "caml_int64x2_low_to_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external int64x2_add : int64x2 -> int64x2 -> int64x2
  = "caml_vec128_unreachable" "caml_sse2_int64x2_add"
[@@noalloc] [@@unboxed] [@@builtin]

external float64x2_of_float : float -> float64x2
  = "caml_vec128_unreachable" "caml_float64x2_low_of_float"
[@@noalloc] [@@unboxed] [@@builtin]

external float64x2_to_float : float64x2 -> float
  = "caml_vec128_unreachable" "caml_float64x2_low_to_float"
[@@noalloc] [@@unboxed] [@@builtin]

external float64x2_add : float64x2 -> float64x2 -> float64x2
  = "caml_vec128_unreachable" "caml_sse2_float64x2_add"
[@@noalloc] [@@unboxed] [@@builtin]

(* 128-bit vectors live in registers across probes, which the probe must save to
   suitably aligned stack slots. *)
let[@inline never] vec_int a b c =
  let v = int64x2_of_int64 a in
  let w = int64x2_of_int64 b in
  let r = if c > 0 then int64x2_add v w else int64x2_add w w in
  Int64.add (int64x2_to_int64 r) (int64x2_to_int64 v)

let[@inline never] vec_float a b c =
  let v = float64x2_of_float a in
  let w = float64x2_of_float b in
  let r = if c > 0 then float64x2_add v w else float64x2_add w w in
  float64x2_to_float r +. float64x2_to_float v

(* Number of distinct guards hit while running [f] with the gate open. *)
let hits f =
  reset ();
  set_gate true;
  let result = f () in
  set_gate false;
  let n = num_hit () in
  ignore (Sys.opaque_identity result);
  n

let () =
  Printf.printf "init calls: %d\n" (init_calls ());
  Printf.printf "guards registered: %b\n" (num_guards () > 0);
  reset ();
  ignore (Sys.opaque_identity (classify 1, name 2, sum 0 3, safe_div 1 0));
  Printf.printf "hits with the gate closed: %d\n" (num_hit ());
  let one_branch = hits (fun () -> classify 1) in
  let both_branches = hits (fun () -> classify 1 ^ classify 5) in
  Printf.printf "if: both arms cover more than one: %b\n"
    (both_branches > one_branch);
  let one_case = hits (fun () -> name 0) in
  let all_cases =
    hits (fun () -> String.concat "" [name 0; name 1; name (-1); name 7])
  in
  Printf.printf "match: all cases cover more than one: %b\n"
    (all_cases > one_case);
  let no_iteration = hits (fun () -> sum 0 0) in
  let iterations = hits (fun () -> sum 0 3) in
  Printf.printf "loop: iterating covers more than not: %b\n"
    (iterations > no_iteration);
  let no_raise = hits (fun () -> safe_div 6 3) in
  let raise_and_no_raise = hits (fun () -> safe_div 6 3 + safe_div 1 0) in
  Printf.printf "try: the handler adds coverage: %b\n"
    (raise_and_no_raise > no_raise);
  let expected = mix 1.5 2.5 3 4.5 +. mix 1.5 2.5 (-3) 40.5 in
  let actual = ref 0. in
  let (_ : int) =
    hits (fun () -> actual := mix 1.5 2.5 3 4.5 +. mix 1.5 2.5 (-3) 40.5)
  in
  let expected_int = mix_int 1 2 3 4 5 6 7 8 + mix_int 9 8 7 6 5 4 3 1 in
  let actual_int = ref 0 in
  let (_ : int) =
    hits (fun () ->
        actual_int := mix_int 1 2 3 4 5 6 7 8 + mix_int 9 8 7 6 5 4 3 1)
  in
  let expected_many = many_live 1 + many_live 6 in
  let actual_many = ref 0 in
  let (_ : int) = hits (fun () -> actual_many := many_live 1 + many_live 6) in
  Printf.printf "values preserved across probes: %b %b %b\n"
    (Float.equal expected !actual)
    (expected_int = !actual_int)
    (expected_many = !actual_many);
  let expected_vec_int = Int64.add (vec_int 3L 4L 1) (vec_int 3L 4L 0) in
  let actual_vec_int = ref 0L in
  let (_ : int) =
    hits (fun () ->
        actual_vec_int := Int64.add (vec_int 3L 4L 1) (vec_int 3L 4L 0))
  in
  let expected_vec_float = vec_float 1.5 2.5 1 +. vec_float 1.5 2.5 0 in
  let actual_vec_float = ref 0. in
  let (_ : int) =
    hits (fun () ->
        actual_vec_float := vec_float 1.5 2.5 1 +. vec_float 1.5 2.5 0)
  in
  Printf.printf "vectors preserved across probes: %b %b\n"
    (Int64.equal expected_vec_int !actual_vec_int)
    (Float.equal expected_vec_float !actual_vec_float);
  Printf.printf "callback called with an aligned stack: %b\n"
    (misaligned_calls () = 0);
  Printf.printf "results: %s %s %d %d\n" (classify 5) (name (-2)) (sum 0 10)
    (safe_div 7 0)
