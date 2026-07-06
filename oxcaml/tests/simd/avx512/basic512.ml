open Stdlib

(* 512-bit vectors *)

external int64x8_of_int64s :
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64x8 = "" "vec512_of_int64s"
[@@noalloc] [@@unboxed]

external int64x8_w0 : int64x8 -> int64 = "" "vec512_w0" [@@noalloc] [@@unboxed]

external int64x8_w1 : int64x8 -> int64 = "" "vec512_w1" [@@noalloc] [@@unboxed]

external int64x8_w2 : int64x8 -> int64 = "" "vec512_w2" [@@noalloc] [@@unboxed]

external int64x8_w3 : int64x8 -> int64 = "" "vec512_w3" [@@noalloc] [@@unboxed]

external int64x8_w4 : int64x8 -> int64 = "" "vec512_w4" [@@noalloc] [@@unboxed]

external int64x8_w5 : int64x8 -> int64 = "" "vec512_w5" [@@noalloc] [@@unboxed]

external int64x8_w6 : int64x8 -> int64 = "" "vec512_w6" [@@noalloc] [@@unboxed]

external int64x8_w7 : int64x8 -> int64 = "" "vec512_w7" [@@noalloc] [@@unboxed]

let eq l r = if l <> r then Printf.printf "%Ld <> %Ld\n" l r

let[@inline never] check v a b c d e f g h =
  eq (int64x8_w0 v) a;
  eq (int64x8_w1 v) b;
  eq (int64x8_w2 v) c;
  eq (int64x8_w3 v) d;
  eq (int64x8_w4 v) e;
  eq (int64x8_w5 v) f;
  eq (int64x8_w6 v) g;
  eq (int64x8_w7 v) h

let[@inline never] combine v0 v1 =
  let add f = Int64.add (f v0) (f v1) in
  int64x8_of_int64s (add int64x8_w0) (add int64x8_w1) (add int64x8_w2)
    (add int64x8_w3) (add int64x8_w4) (add int64x8_w5) (add int64x8_w6)
    (add int64x8_w7)

let[@inline never] combine_with_floats v0 f0 v1 f1 =
  let add f = Int64.add (f v0) (f v1) in
  let w0 = Int64.add (Int64.of_float f0) (add int64x8_w0) in
  let w1 = Int64.add (Int64.of_float f1) (add int64x8_w1) in
  int64x8_of_int64s w0 w1 (add int64x8_w2) (add int64x8_w3) (add int64x8_w4)
    (add int64x8_w5) (add int64x8_w6) (add int64x8_w7)

(* Identity *)
let () =
  let v = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let v = Sys.opaque_identity v in
  check v 1L 2L 3L 4L 5L 6L 7L 8L

(* Identity fn *)
let () =
  let v = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let[@inline never] id v = v in
  let v = id v in
  check v 1L 2L 3L 4L 5L 6L 7L 8L

(* Pass to function *)
let () =
  let v0 = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let v1 = int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L in
  let v = combine v0 v1 in
  check v 10L 12L 14L 16L 18L 20L 22L 24L

(* Pass to function (inlined) *)
let () =
  let v0 = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let v1 = int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L in
  let v = (combine [@inlined hint]) v0 v1 in
  check v 10L 12L 14L 16L 18L 20L 22L 24L

(* Pass to function with floats *)
let () =
  let v0 = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let v1 = int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L in
  let f0 = Sys.opaque_identity 100. in
  let v = combine_with_floats v0 f0 v1 200. in
  check v 110L 212L 14L 16L 18L 20L 22L 24L

(* Capture in closure *)
let () =
  let v0 = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let v1 = int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L in
  let f = combine v0 in
  let f = Sys.opaque_identity f in
  let v = f v1 in
  check v 10L 12L 14L 16L 18L 20L 22L 24L

(* Store in record *)
type record =
  { a : int64x8;
    mutable b : int64x8;
    c : float
  }

let () =
  let record =
    { a = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L;
      b = int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L;
      c = 17.
    }
  in
  check record.a 1L 2L 3L 4L 5L 6L 7L 8L;
  check record.b 9L 10L 11L 12L 13L 14L 15L 16L;
  let record = Sys.opaque_identity record in
  record.b <- int64x8_of_int64s 17L 18L 19L 20L 21L 22L 23L 24L;
  check record.a 1L 2L 3L 4L 5L 6L 7L 8L;
  check record.b 17L 18L 19L 20L 21L 22L 23L 24L;
  let v = combine_with_floats record.a record.c record.b 18. in
  check v 35L 38L 22L 24L 26L 28L 30L 32L

(* Store in variant *)
type variant =
  | A of int64x8
  | B of int64x8
  | C of float

let () =
  (match
     Sys.opaque_identity (A (int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L))
   with
  | A v -> check v 1L 2L 3L 4L 5L 6L 7L 8L
  | B _ | C _ -> print_endline "fail");
  (match
     Sys.opaque_identity (B (int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L))
   with
  | B v -> check v 9L 10L 11L 12L 13L 14L 15L 16L
  | A _ | C _ -> print_endline "fail");
  match Sys.opaque_identity (C 17.) with
  | C c -> eq (Int64.of_float c) 17L
  | A _ | B _ -> print_endline "fail"

(* Pass boxed vectors to an external *)
external boxed_combine : int64x8 -> int64x8 -> int64x8 = "" "boxed_combine512"
[@@noalloc]

let () =
  let v0 = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let v1 = int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L in
  let v = boxed_combine v0 v1 in
  check v 10L 12L 14L 16L 18L 20L 22L 24L

(* Pass lots of vectors to an external *)
external lots_of_vectors :
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 = "" "lots_of_vectors512"
[@@noalloc] [@@unboxed]

let () =
  let mk n = int64x8_of_int64s n n n n n n n n in
  let v =
    lots_of_vectors (mk 1L) (mk 2L) (mk 3L) (mk 4L) (mk 5L) (mk 6L) (mk 7L)
      (mk 8L) (mk 9L) (mk 10L) (mk 11L) (mk 12L) (mk 13L) (mk 14L) (mk 15L)
      (mk 16L)
  in
  check v 136L 136L 136L 136L 136L 136L 136L 136L

(* AVX512 mask registers *)

external mask_of_int64 : int64 -> mask
  = "caml_vec512_unreachable" "caml_mask_of_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external int64_of_mask : mask -> int64
  = "caml_vec512_unreachable" "caml_int64_of_mask"
[@@noalloc] [@@unboxed] [@@builtin]

let[@inline never] check_mask m bits = eq (int64_of_mask m) bits

(* Identity *)
let () =
  let m = mask_of_int64 0xAAL in
  let m = Sys.opaque_identity m in
  check_mask m 0xAAL

(* Identity fn *)
let () =
  let[@inline never] id m = m in
  let m = id (mask_of_int64 0x1234L) in
  check_mask m 0x1234L

(* Round-trip through int64 *)
let () =
  let bits = Sys.opaque_identity 0xFEDCBA9876543210L in
  check_mask (mask_of_int64 bits) bits

(* Pass to function *)
let[@inline never] pick c m0 m1 = if c then m0 else m1

let () =
  let m0 = mask_of_int64 0xF0L in
  let m1 = mask_of_int64 0x0FL in
  check_mask (pick (Sys.opaque_identity true) m0 m1) 0xF0L;
  check_mask (pick (Sys.opaque_identity false) m0 m1) 0x0FL

(* Capture in closure (forces a spill/reload of the mask register) *)
let () =
  let m = mask_of_int64 0x55L in
  let f () = int64_of_mask m in
  let f = Sys.opaque_identity f in
  eq (f ()) 0x55L

(* Keep a mask in a k-register live across many minor collections. The inline
   allocations below hit the GC entry point with the mask live, exercising the
   k-register save/restore in [caml_call_gc_avx512] (as opposed to a plain stack
   spill). *)
let[@inline never] gc_stress bits =
  let m = mask_of_int64 bits in
  let acc = ref [] in
  for i = 1 to 200_000 do
    acc := i :: !acc
  done;
  ignore (Sys.opaque_identity !acc);
  int64_of_mask m

let () =
  List.iter
    (fun bits -> eq (gc_stress bits) bits)
    [0x55L; 0xAAL; 0x1234L; 0xABCDL; 0xFFFFL; 0x8000L; 0x0L]

(* Store in record *)
type mrecord =
  { mutable m : mask;
    v : int64x8
  }

let () =
  let r =
    { m = mask_of_int64 0x0102L; v = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L }
  in
  check_mask r.m 0x0102L;
  check r.v 1L 2L 3L 4L 5L 6L 7L 8L;
  let r = Sys.opaque_identity r in
  r.m <- mask_of_int64 0x0304L;
  check_mask r.m 0x0304L

(* Store in variant *)
type mvariant =
  | M of mask
  | I of int64

let () =
  (match Sys.opaque_identity (M (mask_of_int64 0xABCDL)) with
  | M m -> check_mask m 0xABCDL
  | I _ -> print_endline "fail");
  match Sys.opaque_identity (I 0x1234L) with
  | I i -> eq i 0x1234L
  | M _ -> print_endline "fail"

(* Pass masks to an external: per the C ABI, masks are passed as integers (this
   exercises the [kmovq] mask-to-GPR conversion). *)
external mask_and : mask -> mask -> int64 = "" "mask_and"
[@@noalloc] [@@unboxed]

let () =
  let m0 = mask_of_int64 0xF0L in
  let m1 = mask_of_int64 0x3CL in
  eq (mask_and m0 m1) 0x30L
