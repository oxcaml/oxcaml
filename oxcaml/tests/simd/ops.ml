[@@@ocaml.warning "-unused-module"]

open Utils

let check_binop scalar vector i0 i1 =
  (failmsg := fun () -> Printf.printf "%016Lx | %016Lx\n%!" i0 i1);
  let r0 = scalar i0 i1 in
  let r1 = scalar i1 i0 in
  let expect = int64x2_of_int64s r0 r1 in
  let v1 = int64x2_of_int64s i0 i1 in
  let v2 = int64x2_of_int64s i1 i0 in
  let result = vector v1 v2 in
  eq (int64x2_low_int64 result)
    (int64x2_high_int64 result)
    (int64x2_low_int64 expect)
    (int64x2_high_int64 expect)

module SSE_Util = struct
  include Builtins.SSE_Util

  let make a b c d =
    Float32.to_float32x4 a b c d |> Vector_casts.int32x4_of_float32x4

  let low = int32x4_low_int64

  let high = int32x4_high_int64

  let () =
    Int64s.check_ints (check_binop Int64.logand bitwise_and);
    Int64s.check_ints
      (check_binop (fun l r -> Int64.(logand (lognot l) r)) andnot);
    Int64s.check_ints (check_binop Int64.logor bitwise_or);
    Int64s.check_ints (check_binop Int64.logxor bitwise_xor)

  let () =
    let eql = eq in
    let open Float32 in
    let _0011 = make zero zero one one in
    let _1100 = make one one zero zero in
    let _0101 = make zero one zero one in
    let _1010 = make one zero one zero in
    let _1111 = make one one one one in
    let _0000 = make zero zero zero zero in
    let res = high_64_to_low_64 _1100 _1100 in
    eql (low _0000) (high _0000) (low res) (high res);
    let res = low_64_to_high_64 _1100 _1100 in
    eql (low _1111) (high _1111) (low res) (high res);
    let res = interleave_high_32 _1100 _0011 in
    eql (low _0101) (high _0101) (low res) (high res);
    let res = interleave_low_32 _1100 _0011 in
    eql (low _1010) (high _1010) (low res) (high res)

  let () =
    let eql = eq in
    let open Float32 in
    let two = add one one in
    let three = add two one in
    let _0123 = make zero one two three in
    let _0000 = make zero zero zero zero in
    let _1111 = make one one one one in
    let _2222 = make two two two two in
    let _3333 = make three three three three in
    let _r0000 = shuffle_32 0b00000000 _0123 _0123 in
    let _r1111 = shuffle_32 0b01010101 _0123 _0123 in
    let _r2222 = shuffle_32 0b10101010 _0123 _0123 in
    let _r3333 = shuffle_32 0b11111111 _0123 _0123 in
    eql (low _0000) (high _0000) (low _r0000) (high _r0000);
    eql (low _1111) (high _1111) (low _r1111) (high _r1111);
    eql (low _2222) (high _2222) (low _r2222) (high _r2222);
    eql (low _3333) (high _3333) (low _r3333) (high _r3333);
    let _1010 = make one zero one zero in
    let _0101 = make zero one zero one in
    let _r0101 = shuffle_32 0b01000100 _0123 _0123 in
    let _r1010 = shuffle_32 0b00010001 _0123 _0123 in
    let _r1111 = shuffle_32 0b11010010 _r1010 _r0101 in
    eql (low _0101) (high _0101) (low _r0101) (high _r0101);
    eql (low _1010) (high _1010) (low _r1010) (high _r1010);
    eql (low _1111) (high _1111) (low _r1111) (high _r1111)
end

module SSE2_Util = struct
  include Builtins.SSE2_Util

  let () =
    let v0 = int64x2_of_int64s 0xffffffffffffffffL 0x8000000000000000L in
    let v1 = int64x2_of_int64s 0x7fffffffffffffffL 0x0L in
    let i0 = movemask_64 v0 in
    let i1 = movemask_64 v1 in
    eqi i0 i1 0b11 0b00;
    ()

  let () =
    (failmsg := fun () -> Printf.printf "movemask_8");
    let v0 = Int8.of_ints 0xff 0x7f 0x80 0x0 0x1 0xcc 0x33 0x55 in
    let i0 = movemask_8 v0 in
    eqi i0 0 0b0010_0101_0010_0101 0

  let () =
    let v0 = Int8.of_ints 0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 in
    let v1 = shift_left_bytes 1 v0 in
    eq (int8x16_low_int64 v1) (int8x16_high_int64 v1) 0x0605040302010000L
      0x0605040302010007L;
    ()

  let () =
    let v0 = Int8.of_ints 0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 in
    let v2 = shift_right_bytes 1 v0 in
    eq (int8x16_low_int64 v2) (int8x16_high_int64 v2) 0x07060504030201L
      0x0007060504030201L

  let () =
    let _12 = int64x2_of_int64s 1L 2L in
    let _34 = int64x2_of_int64s 3L 4L in
    let v0 = shuffle_64 0b00 _12 _34 in
    let v1 = shuffle_64 0b01 _12 _34 in
    let v2 = shuffle_64 0b10 _12 _34 in
    let v3 = shuffle_64 0b11 _12 _34 in
    eq (int64x2_low_int64 v0) (int64x2_high_int64 v0) 1L 3L;
    eq (int64x2_low_int64 v1) (int64x2_high_int64 v1) 2L 3L;
    eq (int64x2_low_int64 v2) (int64x2_high_int64 v2) 1L 4L;
    eq (int64x2_low_int64 v3) (int64x2_high_int64 v3) 2L 4L

  let () =
    let v0 = Int16.of_ints 1 2 3 4 5 6 7 8 in
    let s0 = shuffle_high_16 0 v0 in
    let s1 = shuffle_high_16 0b01010101 v0 in
    let s2 = shuffle_high_16 0b10101010 v0 in
    let s3 = shuffle_high_16 0b11111111 v0 in
    eq (int16x8_low_int64 s0) (int16x8_high_int64 s0) 0x0004000300020001L
      0x0005000500050005L;
    eq (int16x8_low_int64 s1) (int16x8_high_int64 s1) 0x0004000300020001L
      0x0006000600060006L;
    eq (int16x8_low_int64 s2) (int16x8_high_int64 s2) 0x0004000300020001L
      0x0007000700070007L;
    eq (int16x8_low_int64 s3) (int16x8_high_int64 s3) 0x0004000300020001L
      0x0008000800080008L;
    let s0 = shuffle_low_16 0 v0 in
    let s1 = shuffle_low_16 0b01010101 v0 in
    let s2 = shuffle_low_16 0b10101010 v0 in
    let s3 = shuffle_low_16 0b11111111 v0 in
    eq (int16x8_low_int64 s0) (int16x8_high_int64 s0) 0x0001000100010001L
      0x0008000700060005L;
    eq (int16x8_low_int64 s1) (int16x8_high_int64 s1) 0x0002000200020002L
      0x0008000700060005L;
    eq (int16x8_low_int64 s2) (int16x8_high_int64 s2) 0x0003000300030003L
      0x0008000700060005L;
    eq (int16x8_low_int64 s3) (int16x8_high_int64 s3) 0x0004000400040004L
      0x0008000700060005L

  let () =
    (failmsg := fun () -> Printf.printf "interleave_8");
    let v0 = Int8.of_ints 0 1 2 3 4 5 6 7 in
    let v1 = Int8.of_ints 8 9 0xa 0xb 0xc 0xd 0xe 0xf in
    let i0 = interleave_high_8 v0 v1 in
    let i1 = interleave_low_8 v0 v1 in
    eq (int8x16_low_int64 i0) (int8x16_high_int64 i0) 0x0b030a0209010800L
      0x0f070e060d050c04L;
    eq (int8x16_low_int64 i1) (int8x16_high_int64 i1) 0x0b030a0209010800L
      0x0f070e060d050c04L;
    ()

  let () =
    (failmsg := fun () -> Printf.printf "interleave_16");
    let v0 = Int16.of_ints 0 1 2 3 4 5 6 7 in
    let v1 = Int16.of_ints 8 9 0xa 0xb 0xc 0xd 0xe 0xf in
    let i0 = interleave_high_16 v0 v1 in
    let i1 = interleave_low_16 v0 v1 in
    eq (int16x8_low_int64 i0) (int16x8_high_int64 i0) 0x000d_0005_000c_0004L
      0x000f_0007_000e_0006L;
    eq (int16x8_low_int64 i1) (int16x8_high_int64 i1) 0x0009_0001_0008_0000L
      0x000b_0003_000a_0002L;
    ()

  let () =
    (failmsg := fun () -> Printf.printf "interleave_64");
    let v0 = int64x2_of_int64s 0L 1L in
    let v1 = int64x2_of_int64s 2L 3L in
    let i0 = interleave_high_64 v0 v1 in
    let i1 = interleave_low_64 v0 v1 in
    eq (int64x2_low_int64 i0) (int64x2_high_int64 i0) 1L 3L;
    eq (int64x2_low_int64 i1) (int64x2_high_int64 i1) 0L 2L
end

module SSE3_Util = struct
  include Builtins.SSE3_Util

  let () =
    let v0 = int64x2_of_int64s 1L 2L in
    let d0 = dup_low_64 v0 in
    eq (int64x2_low_int64 d0) (int64x2_high_int64 d0) 1L 1L;
    let v0 = Int32s.of_int32s 1l 2l 3l 4l in
    let d0 = dup_odd_32 v0 in
    let d1 = dup_even_32 v0 in
    eq (int32x4_low_int64 d0) (int32x4_high_int64 d0) 0x0000000200000002L
      0x0000000400000004L;
    eq (int32x4_low_int64 d1) (int32x4_high_int64 d1) 0x0000000100000001L
      0x0000000300000003L
end
