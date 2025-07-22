let failmsg = ref (fun () -> ())

let eq lv hv l h =
  if l <> lv
  then Printf.printf "low: actual = 0x%016Lx <> 0x%016Lx = expected\n" lv l;
  if h <> hv
  then Printf.printf "high: actual = 0x%016Lx <> 0x%016Lx = expected\n" hv h;
  if l <> lv || h <> hv then !failmsg ()

let eql lv hv l h =
  if l <> lv then Printf.printf "%016lx <> %016lx\n" lv l;
  if h <> hv then Printf.printf "%016lx <> %016lx\n" hv h;
  if l <> lv || h <> hv then !failmsg ()

let eqi lv hv l h =
  if l <> lv
  then Printf.printf "low:  expected = %016x <> %016x = actual\n" lv l;
  if h <> hv
  then Printf.printf "high: expected = %016x <> %016x = actual\n" hv h;
  if l <> lv || h <> hv then !failmsg ()

let eqf lv hv l h =
  if l <> lv then Printf.printf "%f <> %f\n" l lv;
  if h <> hv then Printf.printf "%f <> %f\n" h hv;
  if l <> lv || h <> hv then !failmsg ()

let eqf32 lv hv l h =
  let f32 = Stdlib_stable.Float32.to_float in
  if l <> lv then Printf.printf "%f <> %f\n" (f32 l) (f32 lv);
  if h <> hv then Printf.printf "%f <> %f\n" (f32 h) (f32 hv);
  if l <> lv || h <> hv then !failmsg ()

let eq4 w0v w1v w2v w3v w0 w1 w2 w3 =
  if w0 <> w0v
  then Printf.printf "word0: actual = 0x%016Lx <> 0x%016Lx = expected\n" w0v w0;
  if w1 <> w1v
  then Printf.printf "word1: actual = 0x%016Lx <> 0x%016Lx = expected\n" w1v w1;
  if w2 <> w2v
  then Printf.printf "word2: actual = 0x%016Lx <> 0x%016Lx = expected\n" w2v w2;
  if w3 <> w3v
  then Printf.printf "word3: actual = 0x%016Lx <> 0x%016Lx = expected\n" w3v w3;
  if w0 <> w0v || w1 <> w1v || w2 <> w2v || w3 <> w3v then !failmsg ()

external abort : unit -> unit = "caml_test_abort" [@@noalloc]

let eqf' lv l =
  let fail = lv <> l && not (Float.is_nan lv && Float.is_nan l) in
  if fail then Printf.printf "expected = %f <> %f = actual\n" l lv;
  if fail then !failmsg ();
  (* if fail then abort (); *)
  ()

external int64x2_of_int64s : int64 -> int64 -> int64x2
  = "caml_vec128_unreachable" "vec128_of_int64s"
  [@@noalloc] [@@unboxed]

external int64x2_low_int64 : int64x2 -> int64
  = "caml_vec128_unreachable" "vec128_low_int64"
  [@@noalloc] [@@unboxed]

external int64x2_high_int64 : int64x2 -> int64
  = "caml_vec128_unreachable" "vec128_high_int64"
  [@@noalloc] [@@unboxed]

external int32x4_of_int64s : int64 -> int64 -> int32x4
  = "caml_vec128_unreachable" "vec128_of_int64s"
  [@@noalloc] [@@unboxed]

external int32x4_low_int64 : int32x4 -> int64
  = "caml_vec128_unreachable" "vec128_low_int64"
  [@@noalloc] [@@unboxed]

external int32x4_high_int64 : int32x4 -> int64
  = "caml_vec128_unreachable" "vec128_high_int64"
  [@@noalloc] [@@unboxed]

external int16x8_of_int64s : int64 -> int64 -> int16x8
  = "caml_vec128_unreachable" "vec128_of_int64s"
  [@@noalloc] [@@unboxed]

external int16x8_low_int64 : int16x8 -> int64
  = "caml_vec128_unreachable" "vec128_low_int64"
  [@@noalloc] [@@unboxed]

external int16x8_high_int64 : int16x8 -> int64
  = "caml_vec128_unreachable" "vec128_high_int64"
  [@@noalloc] [@@unboxed]

external int8x16_of_int64s : int64 -> int64 -> int8x16
  = "caml_vec128_unreachable" "vec128_of_int64s"
  [@@noalloc] [@@unboxed]

external int8x16_low_int64 : int8x16 -> int64
  = "caml_vec128_unreachable" "vec128_low_int64"
  [@@noalloc] [@@unboxed]

external int8x16_high_int64 : int8x16 -> int64
  = "caml_vec128_unreachable" "vec128_high_int64"
  [@@noalloc] [@@unboxed]

external float32x4_of_int64s : int64 -> int64 -> float32x4
  = "caml_vec128_unreachable" "vec128_of_int64s"
  [@@noalloc] [@@unboxed]

external float32x4_low_int64 : float32x4 -> int64
  = "caml_vec128_unreachable" "vec128_low_int64"
  [@@noalloc] [@@unboxed]

external float32x4_high_int64 : float32x4 -> int64
  = "caml_vec128_unreachable" "vec128_high_int64"
  [@@noalloc] [@@unboxed]

external float64x2_of_int64s : int64 -> int64 -> float64x2
  = "caml_vec128_unreachable" "vec128_of_int64s"
  [@@noalloc] [@@unboxed]

external float64x2_low_int64 : float64x2 -> int64
  = "caml_vec128_unreachable" "vec128_low_int64"
  [@@noalloc] [@@unboxed]

external float64x2_high_int64 : float64x2 -> int64
  = "caml_vec128_unreachable" "vec128_high_int64"
  [@@noalloc] [@@unboxed]

external float32x4_extract :
  (float32x4[@unboxed]) -> (int[@untagged]) -> (int32[@unboxed])
  = "caml_vec128_unreachable" "test_simd_vec128_extract_ps"
  [@@noalloc]

let eq_float32x4 ~result ~expect =
  for i = 0 to 3 do
    let r = float32x4_extract result i |> Int32.float_of_bits in
    let e = float32x4_extract expect i |> Int32.float_of_bits in
    eqf' r e
  done

let eq_float64x2 ~result ~expect =
  let lv = float64x2_low_int64 result |> Int64.float_of_bits in
  let hv = float64x2_high_int64 result |> Int64.float_of_bits in
  let l = float64x2_low_int64 expect |> Int64.float_of_bits in
  let h = float64x2_high_int64 expect |> Int64.float_of_bits in
  eqf' lv l;
  eqf' hv h;
  ()

let to_float64x2 f0 f1 =
  let v0, v1 = Int64.bits_of_float f0, Int64.bits_of_float f1 in
  float64x2_of_int64s v0 v1

let () =
  (failmsg := fun () -> Printf.printf "basic_checks!");
  let a : int8x16 = int8x16_of_int64s 1L 2L in
  let b : int16x8 = int16x8_of_int64s 3L 4L in
  let c : int32x4 = int32x4_of_int64s 5L 6L in
  let d : int64x2 = int64x2_of_int64s 7L 8L in
  let e : float32x4 = float32x4_of_int64s 9L 10L in
  let f : float64x2 = float64x2_of_int64s 11L 12L in
  let al, ah = int8x16_low_int64 a, int8x16_high_int64 a in
  eq al ah 1L 2L;
  let bl, bh = int16x8_low_int64 b, int16x8_high_int64 b in
  eq bl bh 3L 4L;
  let cl, ch = int32x4_low_int64 c, int32x4_high_int64 c in
  eq cl ch 5L 6L;
  let dl, dh = int64x2_low_int64 d, int64x2_high_int64 d in
  eq dl dh 7L 8L;
  let el, eh = float32x4_low_int64 e, float32x4_high_int64 e in
  eq el eh 9L 10L;
  let fl, fh = float64x2_low_int64 f, float64x2_high_int64 f in
  eq fl fh 11L 12L

module Vector_casts = struct
  external int64x2_of_int32x4 : int32x4 -> int64x2
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int64x2_of_int16x8 : int16x8 -> int64x2
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int64x2_of_int8x16 : int8x16 -> int64x2
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int64x2_of_float32x4 : float32x4 -> int64x2
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int64x2_of_float64x2 : float64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    (failmsg := fun () -> Printf.printf "vector casts 1! ");
    let _0 = int32x4_of_int64s 1L 2L in
    let _1 = int16x8_of_int64s 3L 4L in
    let _2 = int8x16_of_int64s 5L 6L in
    let _3 = float32x4_of_int64s 7L 8L in
    let _4 = float64x2_of_int64s 9L 10L in
    let _0 = int64x2_of_int32x4 (Sys.opaque_identity _0) in
    let _1 = int64x2_of_int16x8 (Sys.opaque_identity _1) in
    let _2 = int64x2_of_int8x16 (Sys.opaque_identity _2) in
    let _3 = int64x2_of_float32x4 (Sys.opaque_identity _3) in
    let _4 = int64x2_of_float64x2 (Sys.opaque_identity _4) in
    let a, b = int64x2_low_int64 _0, int64x2_high_int64 _0 in
    eq a b 1L 2L;
    let a, b = int64x2_low_int64 _1, int64x2_high_int64 _1 in
    eq a b 3L 4L;
    let a, b = int64x2_low_int64 _2, int64x2_high_int64 _2 in
    eq a b 5L 6L;
    let a, b = int64x2_low_int64 _3, int64x2_high_int64 _3 in
    eq a b 7L 8L;
    let a, b = int64x2_low_int64 _4, int64x2_high_int64 _4 in
    eq a b 9L 10L

  external int32x4_of_int64x2 : int64x2 -> int32x4
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int32x4_of_int16x8 : int16x8 -> int32x4
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int32x4_of_int8x16 : int8x16 -> int32x4
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int32x4_of_float32x4 : float32x4 -> int32x4
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int32x4_of_float64x2 : float64x2 -> int32x4
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    (failmsg := fun () -> Printf.printf "vector casts 2! ");
    let _0 = int64x2_of_int64s 1L 2L in
    let _1 = int16x8_of_int64s 3L 4L in
    let _2 = int8x16_of_int64s 5L 6L in
    let _3 = float32x4_of_int64s 7L 8L in
    let _4 = float64x2_of_int64s 9L 10L in
    let _0 = int32x4_of_int64x2 (Sys.opaque_identity _0) in
    let _1 = int32x4_of_int16x8 (Sys.opaque_identity _1) in
    let _2 = int32x4_of_int8x16 (Sys.opaque_identity _2) in
    let _3 = int32x4_of_float32x4 (Sys.opaque_identity _3) in
    let _4 = int32x4_of_float64x2 (Sys.opaque_identity _4) in
    let a, b = int32x4_low_int64 _0, int32x4_high_int64 _0 in
    eq a b 1L 2L;
    let a, b = int32x4_low_int64 _1, int32x4_high_int64 _1 in
    eq a b 3L 4L;
    let a, b = int32x4_low_int64 _2, int32x4_high_int64 _2 in
    eq a b 5L 6L;
    let a, b = int32x4_low_int64 _3, int32x4_high_int64 _3 in
    eq a b 7L 8L;
    let a, b = int32x4_low_int64 _4, int32x4_high_int64 _4 in
    eq a b 9L 10L

  external int16x8_of_int64x2 : int64x2 -> int16x8
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int16x8_of_int32x4 : int32x4 -> int16x8
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int16x8_of_int8x16 : int8x16 -> int16x8
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int16x8_of_float32x4 : float32x4 -> int16x8
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int16x8_of_float64x2 : float64x2 -> int16x8
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    (failmsg := fun () -> Printf.printf "vector casts 3! ");
    let _0 = int64x2_of_int64s 1L 2L in
    let _1 = int32x4_of_int64s 3L 4L in
    let _2 = int8x16_of_int64s 5L 6L in
    let _3 = float32x4_of_int64s 7L 8L in
    let _4 = float64x2_of_int64s 9L 10L in
    let _0 = int16x8_of_int64x2 (Sys.opaque_identity _0) in
    let _1 = int16x8_of_int32x4 (Sys.opaque_identity _1) in
    let _2 = int16x8_of_int8x16 (Sys.opaque_identity _2) in
    let _3 = int16x8_of_float32x4 (Sys.opaque_identity _3) in
    let _4 = int16x8_of_float64x2 (Sys.opaque_identity _4) in
    let a, b = int16x8_low_int64 _0, int16x8_high_int64 _0 in
    eq a b 1L 2L;
    let a, b = int16x8_low_int64 _1, int16x8_high_int64 _1 in
    eq a b 3L 4L;
    let a, b = int16x8_low_int64 _2, int16x8_high_int64 _2 in
    eq a b 5L 6L;
    let a, b = int16x8_low_int64 _3, int16x8_high_int64 _3 in
    eq a b 7L 8L;
    let a, b = int16x8_low_int64 _4, int16x8_high_int64 _4 in
    eq a b 9L 10L

  external int8x16_of_int64x2 : int64x2 -> int8x16
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int8x16_of_int32x4 : int32x4 -> int8x16
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int8x16_of_int16x8 : int16x8 -> int8x16
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int8x16_of_float32x4 : float32x4 -> int8x16
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external int8x16_of_float64x2 : float64x2 -> int8x16
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    (failmsg := fun () -> Printf.printf "vector casts 4! ");
    let _0 = int64x2_of_int64s 1L 2L in
    let _1 = int32x4_of_int64s 3L 4L in
    let _2 = int16x8_of_int64s 5L 6L in
    let _3 = float32x4_of_int64s 7L 8L in
    let _4 = float64x2_of_int64s 9L 10L in
    let _0 = int8x16_of_int64x2 (Sys.opaque_identity _0) in
    let _1 = int8x16_of_int32x4 (Sys.opaque_identity _1) in
    let _2 = int8x16_of_int16x8 (Sys.opaque_identity _2) in
    let _3 = int8x16_of_float32x4 (Sys.opaque_identity _3) in
    let _4 = int8x16_of_float64x2 (Sys.opaque_identity _4) in
    let a, b = int8x16_low_int64 _0, int8x16_high_int64 _0 in
    eq a b 1L 2L;
    let a, b = int8x16_low_int64 _1, int8x16_high_int64 _1 in
    eq a b 3L 4L;
    let a, b = int8x16_low_int64 _2, int8x16_high_int64 _2 in
    eq a b 5L 6L;
    let a, b = int8x16_low_int64 _3, int8x16_high_int64 _3 in
    eq a b 7L 8L;
    let a, b = int8x16_low_int64 _4, int8x16_high_int64 _4 in
    eq a b 9L 10L

  external float32x4_of_int64x2 : int64x2 -> float32x4
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float32x4_of_int32x4 : int32x4 -> float32x4
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float32x4_of_int16x8 : int16x8 -> float32x4
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float32x4_of_int8x16 : int8x16 -> float32x4
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float32x4_of_float64x2 : float64x2 -> float32x4
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    (failmsg := fun () -> Printf.printf "vector casts 5! ");
    let _0 = int64x2_of_int64s 1L 2L in
    let _1 = int32x4_of_int64s 3L 4L in
    let _2 = int16x8_of_int64s 5L 6L in
    let _3 = int8x16_of_int64s 7L 8L in
    let _4 = float64x2_of_int64s 9L 10L in
    let _0 = float32x4_of_int64x2 (Sys.opaque_identity _0) in
    let _1 = float32x4_of_int32x4 (Sys.opaque_identity _1) in
    let _2 = float32x4_of_int16x8 (Sys.opaque_identity _2) in
    let _3 = float32x4_of_int8x16 (Sys.opaque_identity _3) in
    let _4 = float32x4_of_float64x2 (Sys.opaque_identity _4) in
    let a, b = float32x4_low_int64 _0, float32x4_high_int64 _0 in
    eq a b 1L 2L;
    let a, b = float32x4_low_int64 _1, float32x4_high_int64 _1 in
    eq a b 3L 4L;
    let a, b = float32x4_low_int64 _2, float32x4_high_int64 _2 in
    eq a b 5L 6L;
    let a, b = float32x4_low_int64 _3, float32x4_high_int64 _3 in
    eq a b 7L 8L;
    let a, b = float32x4_low_int64 _4, float32x4_high_int64 _4 in
    eq a b 9L 10L

  external float64x2_of_int64x2 : int64x2 -> float64x2
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float64x2_of_int32x4 : int32x4 -> float64x2
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float64x2_of_int16x8 : int16x8 -> float64x2
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float64x2_of_int8x16 : int8x16 -> float64x2
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  external float64x2_of_float32x4 : float32x4 -> float64x2
    = "caml_vec128_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    (failmsg := fun () -> Printf.printf "vector casts 6! ");
    let v_0 = int64x2_of_int64s 1L 2L in
    let v_1 = int32x4_of_int64s 3L 4L in
    let v_2 = int16x8_of_int64s 5L 6L in
    let v_3 = int8x16_of_int64s 7L 8L in
    let v_4 = float32x4_of_int64s 9L 10L in
    let v_0 = float64x2_of_int64x2 (Sys.opaque_identity v_0) in
    let v_1 = float64x2_of_int32x4 (Sys.opaque_identity v_1) in
    let v_2 = float64x2_of_int16x8 (Sys.opaque_identity v_2) in
    let v_3 = float64x2_of_int8x16 (Sys.opaque_identity v_3) in
    let v_4 = float64x2_of_float32x4 (Sys.opaque_identity v_4) in
    let a, b = float64x2_low_int64 v_0, float64x2_high_int64 v_0 in
    eq a b 1L 2L;
    let a, b = float64x2_low_int64 v_1, float64x2_high_int64 v_1 in
    eq a b 3L 4L;
    let a, b = float64x2_low_int64 v_2, float64x2_high_int64 v_2 in
    eq a b 5L 6L;
    let a, b = float64x2_low_int64 v_3, float64x2_high_int64 v_3 in
    eq a b 7L 8L;
    let a, b = float64x2_low_int64 v_4, float64x2_high_int64 v_4 in
    eq a b 9L 10L
end

(* For testing *)
module Float32 = struct
  include Float32_reference

  let to_float32x4 t0 t1 t2 t3 =
    let i0 = Int64.of_int32 t0 |> Int64.logand 0xffffffffL in
    let i1 = Int64.of_int32 t1 |> Int64.logand 0xffffffffL in
    let i2 = Int64.of_int32 t2 |> Int64.logand 0xffffffffL in
    let i3 = Int64.of_int32 t3 |> Int64.logand 0xffffffffL in
    let i0 = Int64.logor (Int64.shift_left i1 32) i0 in
    let i1 = Int64.logor (Int64.shift_left i3 32) i2 in
    float32x4_of_int64s i0 i1
end

module Float64 = struct
  include Float64_reference

  module Tests = struct
    let () =
      (failmsg := fun () -> Printf.printf "Float64!");
      check_floats (fun l r ->
          eqf' (Builtins.Float64.max l r) (Float64_reference.c_max l r));
      check_floats (fun l r ->
          eqf' (Builtins.Float64.min l r) (Float64_reference.c_min l r));
      check_floats (fun l r ->
          eqf'
            (Builtins.Float64.max_match_sse l r)
            (Float64_reference.c_max_match_sse l r));
      check_floats (fun l r ->
          eqf'
            (Builtins.Float64.min_match_sse l r)
            (Float64_reference.c_min_match_sse l r));
      check_floats (fun l _ ->
          eqf' (Builtins.Float64.sqrt l) (Float64_reference.c_sqrt l));
      check_floats (fun l _ ->
          eqf' (Builtins.Float64.round_near l) (Float64_reference.c_round l));
      failmsg := fun () -> Printf.printf "Something else!"
  end
end

module Int64s = struct
  let check_ints f =
    let open Int64 in
    Random.set_state (Random.State.make [| 1234567890 |]);
    f zero zero;
    f zero one;
    f one one;
    f zero minus_one;
    f minus_one minus_one;
    f one minus_one;
    f max_int zero;
    f min_int zero;
    f max_int one;
    f min_int one;
    f max_int max_int;
    f min_int min_int;
    f max_int min_int;
    for _ = 0 to 100_000 do
      let i0 = Random.int64 Int64.max_int in
      let i1 = Random.int64 Int64.max_int in
      f
        (if Random.bool () then i0 else Int64.neg i0)
        (if Random.bool () then i1 else Int64.neg i1)
    done
end

module Int32s = struct
  type t = int32

  external max_unsigned : t -> t -> t = "caml_vec128_unreachable" "uint32_max"
    [@@noalloc] [@@unboxed]

  external min_unsigned : t -> t -> t = "caml_vec128_unreachable" "uint32_min"
    [@@noalloc] [@@unboxed]

  external mul_low : t -> t -> t = "caml_vec128_unreachable" "int32_mul_low"
    [@@noalloc] [@@unboxed]

  external cvt_si16 : (t[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "int32_si16"
    [@@noalloc]

  external cvt_su16 : (t[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "int32_su16"
    [@@noalloc]

  let of_int32s a b c d =
    let a = Int64.of_int32 a |> Int64.logand 0xffffffffL in
    let b = Int64.of_int32 b |> Int64.logand 0xffffffffL in
    let c = Int64.of_int32 c |> Int64.logand 0xffffffffL in
    let d = Int64.of_int32 d |> Int64.logand 0xffffffffL in
    int32x4_of_int64s
      Int64.(logor (shift_left b 32) a)
      Int64.(logor (shift_left d 32) c)

  let check_ints f =
    let open Int32 in
    Random.set_state (Random.State.make [| 1234567890 |]);
    f zero zero;
    f zero one;
    f one one;
    f zero minus_one;
    f minus_one minus_one;
    f one minus_one;
    f max_int zero;
    f min_int zero;
    f max_int one;
    f min_int one;
    f max_int max_int;
    f min_int min_int;
    f max_int min_int;
    for _ = 0 to 100_000 do
      let i0 = Random.int32 Int32.max_int in
      let i1 = Random.int32 Int32.max_int in
      f
        (if Random.bool () then i0 else Int32.neg i0)
        (if Random.bool () then i1 else Int32.neg i1)
    done
end

module Int16 = struct
  type t = int

  external abs : t -> t = "caml_vec128_unreachable" "int16_abs"
    [@@noalloc] [@@untagged]

  external add : t -> t -> t = "caml_vec128_unreachable" "int16_add"
    [@@noalloc] [@@untagged]

  external sub : t -> t -> t = "caml_vec128_unreachable" "int16_sub"
    [@@noalloc] [@@untagged]

  external adds : t -> t -> t = "caml_vec128_unreachable" "int16_adds"
    [@@noalloc] [@@untagged]

  external subs : t -> t -> t = "caml_vec128_unreachable" "int16_subs"
    [@@noalloc] [@@untagged]

  external mulsign : t -> t -> t = "caml_vec128_unreachable" "int16_mulsign"
    [@@noalloc] [@@untagged]

  external mul_round : t -> t -> t = "caml_vec128_unreachable" "int16_mul_round"
    [@@noalloc] [@@untagged]

  external addsu : t -> t -> t = "caml_vec128_unreachable" "int16_addsu"
    [@@noalloc] [@@untagged]

  external subsu : t -> t -> t = "caml_vec128_unreachable" "int16_subsu"
    [@@noalloc] [@@untagged]

  external min : t -> t -> t = "caml_vec128_unreachable" "int16_min"
    [@@noalloc] [@@untagged]

  external max : t -> t -> t = "caml_vec128_unreachable" "int16_max"
    [@@noalloc] [@@untagged]

  external minu : t -> t -> t = "caml_vec128_unreachable" "int16_minu"
    [@@noalloc] [@@untagged]

  external maxu : t -> t -> t = "caml_vec128_unreachable" "int16_maxu"
    [@@noalloc] [@@untagged]

  external cmpeq : t -> t -> t = "caml_vec128_unreachable" "int16_cmpeq"
    [@@noalloc] [@@untagged]

  external cmpgt : t -> t -> t = "caml_vec128_unreachable" "int16_cmpgt"
    [@@noalloc] [@@untagged]

  external avgu : t -> t -> t = "caml_vec128_unreachable" "int16_avgu"
    [@@noalloc] [@@untagged]

  external shift_left : t -> int -> t
    = "caml_vec128_unreachable" "int16_shift_left"
    [@@noalloc] [@@untagged]

  external shift_right : t -> int -> t
    = "caml_vec128_unreachable" "int16_shift_right"
    [@@noalloc] [@@untagged]

  external shift_right_logical : t -> int -> t
    = "caml_vec128_unreachable" "int16_shift_right_logical"
    [@@noalloc] [@@untagged]

  external logand : t -> t -> t = "caml_vec128_unreachable" "int16_logand"
    [@@noalloc] [@@untagged]

  external mul_high : t -> t -> t = "caml_vec128_unreachable" "int16_mul_high"
    [@@noalloc] [@@untagged]

  external mul_high_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "int16_mul_high_unsigned"
    [@@noalloc] [@@untagged]

  external mul_low : t -> t -> t = "caml_vec128_unreachable" "int16_mul_low"
    [@@noalloc] [@@untagged]

  external mul_i32 : (t[@untagged]) -> (t[@untagged]) -> (int32[@unboxed])
    = "caml_vec128_unreachable" "int16_mul_i32"
    [@@noalloc]

  external cvtsx_i32 : (t[@untagged]) -> (int32[@unboxed])
    = "caml_vec128_unreachable" "int16_sxi32"
    [@@noalloc]

  external cvtzx_i32 : (t[@untagged]) -> (int32[@unboxed])
    = "caml_vec128_unreachable" "int16_zxi32"
    [@@noalloc]

  external cvtsx_i64 : (t[@untagged]) -> (int64[@unboxed])
    = "caml_vec128_unreachable" "int16_sxi64"
    [@@noalloc]

  external cvtzx_i64 : (t[@untagged]) -> (int64[@unboxed])
    = "caml_vec128_unreachable" "int16_zxi64"
    [@@noalloc]

  external cvt_si8 : (t[@untagged]) -> (int[@untagged])
    = "caml_vec128_unreachable" "int16_si8"
    [@@noalloc]

  external cvt_su8 : (t[@untagged]) -> (int[@untagged])
    = "caml_vec128_unreachable" "int16_su8"
    [@@noalloc]

  let of_ints a b c d e f g h =
    let a = Int64.of_int a |> Int64.logand 0xffffL in
    let b = Int64.of_int b |> Int64.logand 0xffffL in
    let c = Int64.of_int c |> Int64.logand 0xffffL in
    let d = Int64.of_int d |> Int64.logand 0xffffL in
    let e = Int64.of_int e |> Int64.logand 0xffffL in
    let f = Int64.of_int f |> Int64.logand 0xffffL in
    let g = Int64.of_int g |> Int64.logand 0xffffL in
    let h = Int64.of_int h |> Int64.logand 0xffffL in
    let low = Int64.(logor (shift_left b 16) a) in
    let high = Int64.(logor (shift_left d 16) c) in
    let _low = Int64.(logor (shift_left high 32) low) in
    let low = Int64.(logor (shift_left f 16) e) in
    let high = Int64.(logor (shift_left h 16) g) in
    let _high = Int64.(logor (shift_left high 32) low) in
    int16x8_of_int64s _low _high

  let max_int = 0x7fff

  let min_int = 0x8000

  let check_ints f =
    Random.set_state (Random.State.make [| 1234567890 |]);
    f 0 0;
    f 0 1;
    f 1 1;
    f 0 (-1);
    f (-1) (-1);
    f 1 (-1);
    f max_int 0;
    f min_int 0;
    f max_int 1;
    f min_int 1;
    f max_int max_int;
    f min_int min_int;
    f max_int min_int;
    for _ = 0 to 100_000 do
      let i0 = Random.int 0x10000 in
      let i1 = Random.int 0x10000 in
      f
        (if Random.bool () then i0 else -i0)
        (if Random.bool () then i1 else -i1)
    done
end

module Int8 = struct
  type t = int

  external abs : t -> t = "caml_vec128_unreachable" "int8_abs"
    [@@noalloc] [@@untagged]

  external add : t -> t -> t = "caml_vec128_unreachable" "int8_add"
    [@@noalloc] [@@untagged]

  external sub : t -> t -> t = "caml_vec128_unreachable" "int8_sub"
    [@@noalloc] [@@untagged]

  external adds : t -> t -> t = "caml_vec128_unreachable" "int8_adds"
    [@@noalloc] [@@untagged]

  external subs : t -> t -> t = "caml_vec128_unreachable" "int8_subs"
    [@@noalloc] [@@untagged]

  external mulsign : t -> t -> t = "caml_vec128_unreachable" "int8_mulsign"
    [@@noalloc] [@@untagged]

  external addsu : t -> t -> t = "caml_vec128_unreachable" "int8_addsu"
    [@@noalloc] [@@untagged]

  external subsu : t -> t -> t = "caml_vec128_unreachable" "int8_subsu"
    [@@noalloc] [@@untagged]

  external min : t -> t -> t = "caml_vec128_unreachable" "int8_min"
    [@@noalloc] [@@untagged]

  external max : t -> t -> t = "caml_vec128_unreachable" "int8_max"
    [@@noalloc] [@@untagged]

  external minu : t -> t -> t = "caml_vec128_unreachable" "int8_minu"
    [@@noalloc] [@@untagged]

  external maxu : t -> t -> t = "caml_vec128_unreachable" "int8_maxu"
    [@@noalloc] [@@untagged]

  external cmpeq : t -> t -> t = "caml_vec128_unreachable" "int8_cmpeq"
    [@@noalloc] [@@untagged]

  external cmpgt : t -> t -> t = "caml_vec128_unreachable" "int8_cmpgt"
    [@@noalloc] [@@untagged]

  external mulu_i16 : t -> t -> t = "caml_vec128_unreachable" "int8_mulu_i16"
    [@@noalloc] [@@untagged]

  external avgu : t -> t -> t = "caml_vec128_unreachable" "int8_avgu"
    [@@noalloc] [@@untagged]

  external diffu : t -> t -> t = "caml_vec128_unreachable" "int8_diffu"
    [@@noalloc] [@@untagged]

  external cvtzx_i16 : (t[@untagged]) -> (Int16.t[@untagged])
    = "caml_vec128_unreachable" "int8_zxi16"
    [@@noalloc]

  external cvtsx_i16 : (t[@untagged]) -> (Int16.t[@untagged])
    = "caml_vec128_unreachable" "int8_sxi16"
    [@@noalloc]

  external cvtsx_i32 : (t[@untagged]) -> (int32[@unboxed])
    = "caml_vec128_unreachable" "int8_sxi32"
    [@@noalloc]

  external cvtzx_i32 : (t[@untagged]) -> (int32[@unboxed])
    = "caml_vec128_unreachable" "int8_zxi32"
    [@@noalloc]

  external cvtsx_i64 : (t[@untagged]) -> (int64[@unboxed])
    = "caml_vec128_unreachable" "int8_sxi64"
    [@@noalloc]

  external cvtzx_i64 : (t[@untagged]) -> (int64[@unboxed])
    = "caml_vec128_unreachable" "int8_zxi64"
    [@@noalloc]

  let of_ints a b c d e f g h =
    let a = Int64.of_int a |> Int64.logand 0xffL in
    let b = Int64.of_int b |> Int64.logand 0xffL in
    let c = Int64.of_int c |> Int64.logand 0xffL in
    let d = Int64.of_int d |> Int64.logand 0xffL in
    let e = Int64.of_int e |> Int64.logand 0xffL in
    let f = Int64.of_int f |> Int64.logand 0xffL in
    let g = Int64.of_int g |> Int64.logand 0xffL in
    let h = Int64.of_int h |> Int64.logand 0xffL in
    let ba = Int64.(logor (shift_left b 8) a) in
    let dc = Int64.(logor (shift_left d 8) c) in
    let fe = Int64.(logor (shift_left f 8) e) in
    let hg = Int64.(logor (shift_left h 8) g) in
    let dcba = Int64.(logor (shift_left dc 16) ba) in
    let hgfe = Int64.(logor (shift_left hg 16) fe) in
    let i = Int64.(logor (shift_left hgfe 32) dcba) in
    int8x16_of_int64s i i

  let check_ints f =
    Random.set_state (Random.State.make [| 1234567890 |]);
    for i = 0 to 0xff do
      for j = 0 to 0xff do
        f i j
      done
    done
end
