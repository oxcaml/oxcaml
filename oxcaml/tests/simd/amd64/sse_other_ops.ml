[@@@ocaml.warning "-unused-module"]

open Utils

(* CR gyorsh: Move these tests to the parent directory after adding arm64
   support. This file tests amd64 intrinsics that don't have an equivalent arm64
   neon intrinsic. They can be implemented using a very short sequence of arm64
   instructons. *)
let eqi lv hv l h =
  if l <> lv then Printf.printf "%016x <> %016x\n" lv l;
  if h <> hv then Printf.printf "%016x <> %016x\n" hv h;
  if l <> lv || h <> hv then !failmsg ()

module Float32x4 = struct
  include Builtins.Float32x4
  include Builtins.Sse_other_builtins.Float32x4

  let () =
    Float32.check_floats (fun f0 f1 ->
        (failmsg
           := fun () ->
                Printf.printf "dpf32 %f %f\n%!" (Int32.float_of_bits f0)
                  (Int32.float_of_bits f1));
        let fv0 = Float32.to_float32x4 f0 f1 f0 f1 in
        let fv1 = Float32.to_float32x4 f1 f0 f1 f0 in
        let result = dp 0b1111_0001 fv0 fv1 in
        let expect =
          Float32.to_float32x4
            (Float32.add
               (Float32.add (Float32.mul f0 f1) (Float32.mul f1 f0))
               (Float32.add (Float32.mul f0 f1) (Float32.mul f1 f0)))
            0l 0l 0l
        in
        (* When both are NaN, AMD returns the first argument and Intel returns
           the second argument. Hence we do not test this case. *)
        if f0 |> Int32.float_of_bits |> Float.is_nan
           && f1 |> Int32.float_of_bits |> Float.is_nan
        then ()
        else eq_float32x4 ~result ~expect)

  let () =
    Float32.check_floats (fun f0 f1 ->
        (failmsg
           := fun () ->
                Printf.printf "%f | %f\n%!" (Int32.float_of_bits f0)
                  (Int32.float_of_bits f1));
        let fv0 = Float32.to_float32x4 f0 f0 f1 f1 in
        let fv1 = Float32.to_float32x4 f1 f1 f0 f0 in
        let result = addsub fv0 fv1 in
        let expect =
          Float32.to_float32x4 (Float32.sub f0 f1) (Float32.add f0 f1)
            (Float32.sub f1 f0) (Float32.add f1 f0)
        in
        eq_float32x4 ~result ~expect);
    Float32.check_floats (fun f0 f1 ->
        (failmsg
           := fun () ->
                Printf.printf "%f | %f\n%!" (Int32.float_of_bits f0)
                  (Int32.float_of_bits f1));
        let fv0 = Float32.to_float32x4 f0 f1 f0 f1 in
        let fv1 = Float32.to_float32x4 f1 f0 f1 f0 in
        let result = hsub fv0 fv1 in
        let expect =
          Float32.to_float32x4 (Float32.sub f0 f1) (Float32.sub f0 f1)
            (Float32.sub f1 f0) (Float32.sub f1 f0)
        in
        eq_float32x4 ~result ~expect)
end

module Float64x2 = struct
  include Builtins.Float64x2
  include Builtins.Sse_other_builtins.Float64x2

  let () =
    Float64.check_floats (fun f0 f1 ->
        (failmsg := fun () -> Printf.printf "%f | %f\n%!" f0 f1);
        let fv0 = Float64.to_float64x2 f0 f0 in
        let fv1 = Float64.to_float64x2 f1 f1 in
        let result = addsub fv0 fv1 in
        let expect = Float64.to_float64x2 (f0 -. f1) (f0 +. f1) in
        eq_float64x2 ~result ~expect);
    Float64.check_floats (fun f0 f1 ->
        (failmsg := fun () -> Printf.printf "%f | %f\n%!" f0 f1);
        let fv0 = Float64.to_float64x2 f0 f1 in
        let fv1 = Float64.to_float64x2 f1 f0 in
        let result = hsub fv0 fv1 in
        let expect = Float64.to_float64x2 (f0 -. f1) (f1 -. f0) in
        eq_float64x2 ~result ~expect)

  let () =
    Float64.check_floats (fun f0 f1 ->
        (failmsg := fun () -> Printf.printf "%f dp %f\n%!" f0 f1);
        let fv0 = Float64.to_float64x2 f0 f1 in
        let fv1 = Float64.to_float64x2 f1 f0 in
        let result = dp 0b0011_0001 fv0 fv1 in
        let expect = Float64.to_float64x2 ((f0 *. f1) +. (f1 *. f0)) 0.0 in
        eq_float64x2 ~result ~expect)
end

module Int64 = struct
  include Builtins.Sse_other_builtins.Int64

  let eq' x y = if x <> y then Printf.printf "%016Lx <> %016Lx\n" x y

  let () =
    eq' (bit_deposit 3L 4L) 0x4L;
    eq' (bit_deposit 235L 522L) 0xAL;
    eq' (bit_extract 3L 4L) 0x0L;
    eq' (bit_extract 235L 522L) 0x3L
end

module Int64x2 = struct
  type t = int64x2

  external int64x2_of_int64s : int64 -> int64 -> int64x2
    = "caml_vec128_unreachable" "vec128_of_int64s"
    [@@noalloc] [@@unboxed]

  external int64x2_low_int64 : int64x2 -> int64
    = "caml_vec128_unreachable" "vec128_low_int64"
    [@@noalloc] [@@unboxed]

  external int64x2_high_int64 : int64x2 -> int64
    = "caml_vec128_unreachable" "vec128_high_int64"
    [@@noalloc] [@@unboxed]

  external clmul :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_clmul_int64x2"
    [@@noalloc] [@@builtin]

  let eq lv hv l h =
    if l <> lv then Printf.printf "%016Lx <> %016Lx\n" lv l;
    if h <> hv then Printf.printf "%016Lx <> %016Lx\n" hv h

  let () =
    let v0 = int64x2_of_int64s 3L 4L in
    let v1 = int64x2_of_int64s 5L 12L in
    let c0 = clmul 0b0000_0000 v0 v1 in
    let c1 = clmul 0b0000_0001 v0 v1 in
    let c2 = clmul 0b0001_0000 v0 v1 in
    let c3 = clmul 0b0001_0001 v0 v1 in
    eq (int64x2_low_int64 c0) (int64x2_high_int64 c0) 15L 0L;
    eq (int64x2_low_int64 c1) (int64x2_high_int64 c1) 20L 0L;
    eq (int64x2_low_int64 c2) (int64x2_high_int64 c2) 20L 0L;
    eq (int64x2_low_int64 c3) (int64x2_high_int64 c3) 48L 0L
end

module SSE_Util = struct
  let () =
    let v = Int32s.to_int32x4 0xffffffffl 0x80000000l 0x7fffffffl 0x0l in
    let i = Builtins.SSE_Util.movemask_32 v in
    eqi i 0 0b0011 0
end

module Int32x4 = struct
  include Builtins.Sse_other_builtins.Int32x4

  let () =
    Int32s.check_ints (fun l r ->
        (failmsg := fun () -> Printf.printf "%08lx|%08lx mul_even\n%!" l r);
        let v0 = Int32s.to_int32x4 l 1l r 3l in
        let v1 = Int32s.to_int32x4 r 2l l 1l in
        let result = mul_even v0 v1 in
        let expect =
          int64x2_of_int64s
            Stdlib.Int64.(mul (of_int32 l) (of_int32 r))
            Stdlib.Int64.(mul (of_int32 r) (of_int32 l))
        in
        eq (int64x2_low_int64 result)
          (int64x2_high_int64 result)
          (int64x2_low_int64 expect)
          (int64x2_high_int64 expect));
    Int32s.check_ints (fun l r ->
        (failmsg
           := fun () -> Printf.printf "%08lx|%08lx mul_even_unsigned\n%!" l r);
        let v0 = Int32s.to_int32x4 l 1l r 3l in
        let v1 = Int32s.to_int32x4 r 2l l 1l in
        let result = mul_even_unsigned v0 v1 in
        let expect =
          int64x2_of_int64s
            Stdlib.Int64.(
              mul
                (logand (of_int32 l) 0xffffffffL)
                (logand (of_int32 r) 0xffffffffL))
            Stdlib.Int64.(
              mul
                (logand (of_int32 r) 0xffffffffL)
                (logand (of_int32 l) 0xffffffffL))
        in
        eq (int64x2_low_int64 result)
          (int64x2_high_int64 result)
          (int64x2_low_int64 expect)
          (int64x2_high_int64 expect));
    Int32s.check_ints (fun l r ->
        (failmsg := fun () -> Printf.printf "%08lx|%08lx mulsign\n%!" l r);
        let v0 = Int32s.to_int32x4 l l r r in
        let v1 = Int32s.to_int32x4 l r l r in
        let result = mulsign v0 v1 in
        let mulsign x y = Int32.mul (Int32.compare y 0l |> Int32.of_int) x in
        let expect =
          Int32s.to_int32x4 (mulsign l l) (mulsign l r) (mulsign r l)
            (mulsign r r)
        in
        eq (int32x4_low_int64 result)
          (int32x4_high_int64 result)
          (int32x4_low_int64 expect)
          (int32x4_high_int64 expect));
    Int32s.check_ints (fun l r ->
        (failmsg := fun () -> Printf.printf "%08lx|%08lx hsub\n%!" l r);
        let v0 = Int32s.to_int32x4 l r r l in
        let v1 = Int32s.to_int32x4 r l l r in
        let result = hsub v0 v1 in
        let expect =
          Int32s.to_int32x4 (Int32.sub l r) (Int32.sub r l) (Int32.sub r l)
            (Int32.sub l r)
        in
        eq (int32x4_low_int64 result)
          (int32x4_high_int64 result)
          (int32x4_low_int64 expect)
          (int32x4_high_int64 expect))
end

module Int16x8 = struct
  include Builtins.Sse_other_builtins.Int16x8

  let () =
    Int16.check_ints (fun l r ->
        (failmsg := fun () -> Printf.printf "%04x|%04x mulsign\n%!" l r);
        let v0 = Int16.to_int16x8 l l r r l l r r in
        let v1 = Int16.to_int16x8 l r l r l r l r in
        let result = mulsign v0 v1 in
        let mulsign x y = Int16.mulsign x y in
        let expect =
          Int16.to_int16x8 (mulsign l l) (mulsign l r) (mulsign r l)
            (mulsign r r) (mulsign l l) (mulsign l r) (mulsign r l)
            (mulsign r r)
        in
        eq (int16x8_low_int64 result)
          (int16x8_high_int64 result)
          (int16x8_low_int64 expect)
          (int16x8_high_int64 expect));
    Int16.check_ints (fun l r ->
        (failmsg := fun () -> Printf.printf "%04x|%04x mul_round\n%!" l r);
        let v0 = Int16.to_int16x8 l l r r l l r r in
        let v1 = Int16.to_int16x8 l r l r l r l r in
        let result = mul_round v0 v1 in
        let mul_round x y = Int16.mul_round x y in
        let expect =
          Int16.to_int16x8 (mul_round l l) (mul_round l r) (mul_round r l)
            (mul_round r r) (mul_round l l) (mul_round l r) (mul_round r l)
            (mul_round r r)
        in
        eq (int16x8_low_int64 result)
          (int16x8_high_int64 result)
          (int16x8_low_int64 expect)
          (int16x8_high_int64 expect));
    Int16.check_ints (fun l r ->
        (failmsg := fun () -> Printf.printf "%04x|%04x hsubs\n%!" l r);
        let v0 = Int16.to_int16x8 l l r r l l r r in
        let v1 = Int16.to_int16x8 r r l l r r l l in
        let result = hsub_saturating v0 v1 in
        let expect =
          Int16.to_int16x8 (Int16.subs l l) (Int16.subs r r) (Int16.subs l l)
            (Int16.subs r r) (Int16.subs r r) (Int16.subs l l) (Int16.subs r r)
            (Int16.subs l l)
        in
        eq (int16x8_low_int64 result)
          (int16x8_high_int64 result)
          (int16x8_low_int64 expect)
          (int16x8_high_int64 expect));
    Int16.check_ints (fun l r ->
        (failmsg := fun () -> Printf.printf "%04x|%04x hsub\n%!" l r);
        let v0 = Int16.to_int16x8 l l r r l l r r in
        let v1 = Int16.to_int16x8 r r l l r r l l in
        let result = hsub v0 v1 in
        let expect =
          Int16.to_int16x8 (Int16.sub l l) (Int16.sub r r) (Int16.sub l l)
            (Int16.sub r r) (Int16.sub r r) (Int16.sub l l) (Int16.sub r r)
            (Int16.sub l l)
        in
        eq (int16x8_low_int64 result)
          (int16x8_high_int64 result)
          (int16x8_low_int64 expect)
          (int16x8_high_int64 expect));
    Int16.check_ints (fun l r ->
        (failmsg := fun () -> Printf.printf "%04x|%04x hadds\n%!" l r);
        let v0 = Int16.to_int16x8 l l r r l l r r in
        let v1 = Int16.to_int16x8 r r l l r r l l in
        let result = hadd_saturating v0 v1 in
        let expect =
          Int16.to_int16x8 (Int16.adds l l) (Int16.adds r r) (Int16.adds l l)
            (Int16.adds r r) (Int16.adds r r) (Int16.adds l l) (Int16.adds r r)
            (Int16.adds l l)
        in
        eq (int16x8_low_int64 result)
          (int16x8_high_int64 result)
          (int16x8_low_int64 expect)
          (int16x8_high_int64 expect));
    Int16.check_ints (fun l r ->
        (failmsg := fun () -> Printf.printf "%04x|%04x mul_hadd_i32\n%!" l r);
        let v0 = Int16.to_int16x8 l l r r l l r r in
        let v1 = Int16.to_int16x8 r r l l r r l l in
        let result = mul_hadd_i32 v0 v1 in
        let sum = Int32.add (Int16.mul_i32 l r) (Int16.mul_i32 l r) in
        let expect = Int32s.to_int32x4 sum sum sum sum in
        eq (int32x4_low_int64 result)
          (int32x4_high_int64 result)
          (int32x4_low_int64 expect)
          (int32x4_high_int64 expect));
    Int16.check_ints (fun l r ->
        (failmsg := fun () -> Printf.printf "%04x|%04x minposu\n%!" l r);
        let v0 = Int16.to_int16x8 l r l r l r l r in
        let result = minposu v0 in
        let min_v = Int16.minu l r in
        let idx = if min_v = l then 0 else 1 in
        let expect =
          Stdlib.Int64.(
            logor
              (shift_left (of_int idx |> logand 0x3L) 16)
              (of_int min_v |> logand 0xffffL))
        in
        eq (int16x8_low_int64 result) (int16x8_high_int64 result) expect 0L);
    Int16.check_ints (fun l r ->
        (failmsg := fun () -> Printf.printf "%04x|%04x avgu\n%!" l r);
        let v0 = Int16.to_int16x8 l l r r l l r r in
        let v1 = Int16.to_int16x8 l r l r l r l r in
        let result = avgu v0 v1 in
        let lr = Int16.avgu l r in
        let expect = Int16.to_int16x8 l lr lr r l lr lr r in
        eq (int16x8_low_int64 result)
          (int16x8_high_int64 result)
          (int16x8_low_int64 expect)
          (int16x8_high_int64 expect))
end

module Int8x16 = struct
  include Builtins.Sse_other_builtins.Int8x16

  let () =
    Int8.check_ints (fun l r ->
        (failmsg := fun () -> Printf.printf "%02x|%02x mulsign\n%!" l r);
        let v0 = Int8.to_int8x16 l l r r l l r r in
        let v1 = Int8.to_int8x16 l r l r l r l r in
        let result = mulsign v0 v1 in
        let mulsign x y = Int8.mulsign x y in
        let expect =
          Int8.to_int8x16 (mulsign l l) (mulsign l r) (mulsign r l)
            (mulsign r r) (mulsign l l) (mulsign l r) (mulsign r l)
            (mulsign r r)
        in
        eq (int8x16_low_int64 result)
          (int8x16_high_int64 result)
          (int8x16_low_int64 expect)
          (int8x16_high_int64 expect));
    Int16.check_ints (fun l r ->
        (failmsg
           := fun () ->
                Printf.printf "%04x|%04x mul_unsigned_hadd_saturating_i16\n%!" l
                  r);
        let v0 = Int8.to_int8x16 l l r r l l r r in
        let v1 = Int8.to_int8x16 l r l r l r l r in
        let result = mul_unsigned_hadd_saturating_i16 v0 v1 in
        let sum0 = Int16.adds (Int8.mulu_i16 l l) (Int8.mulu_i16 l r) in
        let sum1 = Int16.adds (Int8.mulu_i16 r l) (Int8.mulu_i16 r r) in
        let expect = Int16.to_int16x8 sum0 sum1 sum0 sum1 sum0 sum1 sum0 sum1 in
        eq (int16x8_low_int64 result)
          (int16x8_high_int64 result)
          (int16x8_low_int64 expect)
          (int16x8_high_int64 expect));
    Int8.check_ints (fun l r ->
        (failmsg := fun () -> Printf.printf "%02x|%02x avgu\n%!" l r);
        let v0 = Int8.to_int8x16 l l r r l l r r in
        let v1 = Int8.to_int8x16 l r l r l r l r in
        let result = avgu v0 v1 in
        let lr = Int8.avgu l r in
        let expect = Int8.to_int8x16 l lr lr r l lr lr r in
        eq (int8x16_low_int64 result)
          (int8x16_high_int64 result)
          (int8x16_low_int64 expect)
          (int8x16_high_int64 expect));
    Int8.check_ints (fun l r ->
        (failmsg := fun () -> Printf.printf "%02x|%02x sadu\n%!" l r);
        let v0 = Int8.to_int8x16 l l r r l l r r in
        let v1 = Int8.to_int8x16 l r l r l r l r in
        let result = sadu v0 v1 in
        let lr = Int8.diffu l r in
        let expect = Stdlib.Int64.of_int (4 * lr) in
        eq (int64x2_low_int64 result) (int64x2_high_int64 result) expect expect);
    Int8.check_ints (fun l r ->
        (failmsg := fun () -> Printf.printf "%02x|%02x msadu\n%!" l r);
        let v0 = Int8.to_int8x16 l l r r l l r r in
        let v1 = Int8.to_int8x16 l r l r l r l r in
        let result = msadu 0 v0 v1 in
        let lr = 2 * Int8.diffu l r in
        let expect = Int16.to_int16x8 lr lr lr lr lr lr lr lr in
        eq (int16x8_low_int64 result) (int16x8_low_int64 result)
          (int16x8_low_int64 expect)
          (int16x8_high_int64 expect))
end

module SSSE3_Util = struct
  include Builtins.Sse_other_builtins.SSSE3_Util

  let () =
    let v0 = Int8.to_int8x16 0 1 2 3 4 5 6 7 in
    let sel0 = Int8.to_int8x16 0 0 0 0 0 0 0 0 in
    let sel1 = Int8.to_int8x16 0 1 2 3 4 5 6 7 in
    let sel2 = Int8.to_int8x16 15 15 15 15 15 15 15 15 in
    let s0 = shuffle_8 v0 sel0 in
    let s1 = shuffle_8 v0 sel1 in
    let s2 = shuffle_8 v0 sel2 in
    eq (int8x16_low_int64 s0) (int8x16_high_int64 s0) 0L 0L;
    eq (int8x16_low_int64 s1) (int8x16_high_int64 s1) 0x0706050403020100L
      0x0706050403020100L;
    eq (int8x16_low_int64 s2) (int8x16_high_int64 s2) 0x0707070707070707L
      0x0707070707070707L

  let () =
    let v0 = Int8.to_int8x16 0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 in
    let v1 = Int8.to_int8x16 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf in
    let v2 = align_right_bytes 3 v1 v0 in
    eq (int8x16_low_int64 v2) (int8x16_high_int64 v2) 0x0201000706050403L
      0x0a09080706050403L
end

module SSE41_Util = struct
  include Builtins.Sse_other_builtins.SSE41_Util

  let () =
    let v0 = Int16.to_int16x8 0 1 2 3 4 5 6 7 in
    let v1 = Int16.to_int16x8 8 9 0xa 0xb 0xc 0xd 0xe 0xf in
    let b0 = blend_16 0b01010101 v0 v1 in
    eq (int16x8_low_int64 b0) (int16x8_high_int64 b0) 0x0003_000a_0001_0008L
      0x0007_000e_0005_000cL;
    let v0 = Int32s.to_int32x4 0l 1l 2l 3l in
    let v1 = Int32s.to_int32x4 4l 5l 6l 7l in
    let b0 = blend_32 0b0101 v0 v1 in
    eq (int32x4_low_int64 b0) (int32x4_high_int64 b0) 0x00000001_00000004L
      0x00000003_00000006L;
    let v0 = int64x2_of_int64s 0L 1L in
    let v1 = int64x2_of_int64s 2L 3L in
    let b0 = blend_64 0b01 v0 v1 in
    eq (int64x2_low_int64 b0) (int64x2_high_int64 b0) 2L 1L

  let () =
    let v0 = Int8.to_int8x16 0 1 2 3 4 5 6 7 in
    let v1 = Int8.to_int8x16 8 9 0xa 0xb 0xc 0xd 0xe 0xf in
    let b0 =
      blendv_8 v0 v1 (Int8.to_int8x16 0xff 0x00 0xff 0x00 0xff 0x00 0xff 0x00)
    in
    eq (int8x16_low_int64 b0) (int8x16_high_int64 b0) 0x07_0e_05_0c_03_0a_01_08L
      0x07_0e_05_0c_03_0a_01_08L;
    let v0 = Int32s.to_int32x4 0l 1l 2l 3l in
    let v1 = Int32s.to_int32x4 4l 5l 6l 7l in
    let b0 =
      blendv_32 v0 v1 (Int32s.to_int32x4 0xffffffffl 0x0l 0xffffffffl 0x0l)
    in
    eq (int32x4_low_int64 b0) (int32x4_high_int64 b0) 0x00000001_00000004L
      0x00000003_00000006L;
    let v0 = int64x2_of_int64s 0L 1L in
    let v1 = int64x2_of_int64s 2L 3L in
    let b0 = blendv_64 v0 v1 (int64x2_of_int64s 0xffffffffffffffffL 0x0L) in
    eq (int64x2_low_int64 b0) (int64x2_high_int64 b0) 2L 1L
end
