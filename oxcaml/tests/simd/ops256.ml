[@@@ocaml.warning "-unused-module"]

open Utils256

let word0_32 = int32x8_first_int64

let word1_32 = int32x8_second_int64

let word2_32 = int32x8_third_int64

let word3_32 = int32x8_fourth_int64

let word0 = int64x4_first_int64

let word1 = int64x4_second_int64

let word2 = int64x4_third_int64

let word3 = int64x4_fourth_int64

let check_binop scalar vector i0 i1 =
  (failmsg := fun () -> Printf.printf "%016Lx | %016Lx\n%!" i0 i1);
  let r0 = scalar i0 i1 in
  let r1 = scalar i1 i0 in
  let r2 = scalar i0 i0 in
  let r3 = scalar i1 i1 in
  let expect = int64x4_of_int64s r0 r1 r2 r3 in
  let v1 = int64x4_of_int64s i0 i1 i0 i1 in
  let v2 = int64x4_of_int64s i1 i0 i0 i1 in
  let result = vector v1 v2 in
  eq4
    (int64x4_first_int64 result)
    (int64x4_second_int64 result)
    (int64x4_third_int64 result)
    (int64x4_fourth_int64 result)
    (int64x4_first_int64 expect)
    (int64x4_second_int64 expect)
    (int64x4_third_int64 expect)
    (int64x4_fourth_int64 expect)

module AVX_Util = struct
  include Builtins.AVX

  let () =
    Int64s.check_ints (check_binop Int64.logand bitwise_and);
    Int64s.check_ints
      (check_binop (fun l r -> Int64.(logand (lognot l) r)) andnot);
    Int64s.check_ints (check_binop Int64.logor bitwise_or);
    Int64s.check_ints (check_binop Int64.logxor bitwise_xor)

  let () =
    (failmsg := fun () -> Printf.printf "interleave_32");
    let _00000011 = Int32s.to_int32x8 0l 0l 0l 0l 0l 0l 1l 1l in
    let _00110000 = Int32s.to_int32x8 0l 0l 1l 1l 0l 0l 0l 0l in
    let _11000000 = Int32s.to_int32x8 1l 1l 0l 0l 0l 0l 0l 0l in
    let _00001100 = Int32s.to_int32x8 0l 0l 0l 0l 1l 1l 0l 0l in
    let res = interleave_high_32 _00000011 _00110000 in
    eq4 (word0_32 res) (word1_32 res) (word2_32 res) (word3_32 res)
      (Int32s.to_int64 0l 1l) (Int32s.to_int64 0l 1l) (Int32s.to_int64 1l 0l)
      (Int32s.to_int64 1l 0l);
    let res = interleave_low_32 _11000000 _00001100 in
    eq4 (word0_32 res) (word1_32 res) (word2_32 res) (word3_32 res)
      (Int32s.to_int64 1l 0l) (Int32s.to_int64 1l 0l) (Int32s.to_int64 0l 1l)
      (Int32s.to_int64 0l 1l)

  let () =
    (failmsg := fun () -> Printf.printf "shuffle_32");
    let _01234567 = Int32s.to_int32x8 0l 1l 2l 3l 4l 5l 6l 7l in
    let r0 = shuffle_32 0b00000000 _01234567 _01234567 in
    eq4 (word0_32 r0) (word1_32 r0) (word2_32 r0) (word3_32 r0) 0L 0L
      0x0000000400000004L 0x0000000400000004L;
    let r1 = shuffle_32 0b01010101 _01234567 _01234567 in
    eq4 (word0_32 r1) (word1_32 r1) (word2_32 r1) (word3_32 r1)
      0x0000000100000001L 0x0000000100000001L 0x0000000500000005L
      0x0000000500000005L;
    let r2 = shuffle_32 0b10101010 _01234567 _01234567 in
    eq4 (word0_32 r2) (word1_32 r2) (word2_32 r2) (word3_32 r2)
      0x0000000200000002L 0x0000000200000002L 0x0000000600000006L
      0x0000000600000006L;
    let r3 = shuffle_32 0b11111111 _01234567 _01234567 in
    eq4 (word0_32 r3) (word1_32 r3) (word2_32 r3) (word3_32 r3)
      0x0000000300000003L 0x0000000300000003L 0x0000000700000007L
      0x0000000700000007L

  let () =
    (failmsg := fun () -> Printf.printf "movemask_64");
    let v0 =
      int64x4_of_int64s 0x0L 0x7fffffffffffffffL 0x8000000000000000L
        0xffffffffffffffffL
    in
    let i0 = movemask_64 v0 in
    eqi i0 0 0b1100 0;
    ()

  let () =
    (failmsg := fun () -> Printf.printf "movemask_32");
    let v0 =
      Int32s.to_int32x8 (-1l) 1l 2l Int32.min_int (-1l) 1l 2l Int32.min_int
    in
    let i0 = movemask_32 v0 in
    eqi i0 0 0b10011001 0

  let () =
    (failmsg := fun () -> Printf.printf "shuffle_64");
    let _1234 = int64x4_of_int64s 1L 2L 3L 4L in
    let v0 = shuffle_64 0b0000 _1234 _1234 in
    let v1 = shuffle_64 0b0101 _1234 _1234 in
    let v2 = shuffle_64 0b1010 _1234 _1234 in
    let v3 = shuffle_64 0b1111 _1234 _1234 in
    eq4 (word0 _1234) (word1 _1234) (word2 _1234) (word3 _1234) 1L 2L 3L 4L;
    eq4 (word0 v0) (word1 v0) (word2 v0) (word3 v0) 1L 1L 3L 3L;
    eq4 (word0 v1) (word1 v1) (word2 v1) (word3 v1) 2L 1L 4L 3L;
    eq4 (word0 v2) (word1 v2) (word2 v2) (word3 v2) 1L 2L 3L 4L;
    eq4 (word0 v3) (word1 v3) (word2 v3) (word3 v3) 2L 2L 4L 4L

  let () =
    (failmsg := fun () -> Printf.printf "interleave_64");
    let v0 = int64x4_of_int64s 0L 1L 2L 3L in
    let v1 = int64x4_of_int64s 4L 5L 6L 7L in
    let i0 = interleave_high_64 v0 v1 in
    let i1 = interleave_low_64 v0 v1 in
    eq4 (word0 i0) (word1 i0) (word2 i0) (word3 i0) 1L 5L 3L 7L;
    eq4 (word0 i1) (word1 i1) (word2 i1) (word3 i1) 0L 4L 2L 6L

  let () =
    (failmsg := fun () -> Printf.printf "dup_64");
    let v0 = int64x4_of_int64s 1L 2L 3L 4L in
    let d0 = dup_even_64 v0 in
    eq4 (word0 d0) (word1 d0) (word2 d0) (word3 d0) 1L 1L 3L 3L;
    let v0 = Int32s.to_int32x8 0l 1l 2l 3l 4l 5l 6l 7l in
    let d0 = dup_odd_32 v0 in
    let d1 = dup_even_32 v0 in
    eq4 (word0_32 d0) (word1_32 d0) (word2_32 d0) (word3_32 d0)
      0x0000000100000001L 0x0000000300000003L 0x0000000500000005L
      0x0000000700000007L;
    eq4 (word0_32 d1) (word1_32 d1) (word2_32 d1) (word3_32 d1)
      0x0000000000000000L 0x0000000200000002L 0x0000000400000004L
      0x0000000600000006L

  let () =
    (failmsg := fun () -> Printf.printf "blend_64");
    let v0 = int64x4_of_int64s 0L 1L 2L 3L in
    let v1 = int64x4_of_int64s 4L 5L 6L 7L in
    let b0 = blend_64 0b0000 v0 v1 in
    let b1 = blend_64 0b0101 v0 v1 in
    let b2 = blend_64 0b1010 v0 v1 in
    let b3 = blend_64 0b1111 v0 v1 in
    eq4 (word0 b0) (word1 b0) (word2 b0) (word3 b0) 0L 1L 2L 3L;
    eq4 (word0 b1) (word1 b1) (word2 b1) (word3 b1) 4L 1L 6L 3L;
    eq4 (word0 b2) (word1 b2) (word2 b2) (word3 b2) 0L 5L 2L 7L;
    eq4 (word0 b3) (word1 b3) (word2 b3) (word3 b3) 4L 5L 6L 7L

  let () =
    (failmsg := fun () -> Printf.printf "broadcast");
    let v128 = int64x2_of_int64s 0x1122334455667788L 0x99aabbccddeeff00L in
    let v256 = broadcast_128 v128 in
    eq4 (word0 v256) (word1 v256) (word2 v256) (word3 v256) 0x1122334455667788L
      0x99aabbccddeeff00L 0x1122334455667788L 0x99aabbccddeeff00L;
    let v256_64 = broadcast_64 v128 in
    eq4 (word0 v256_64) (word1 v256_64) (word2 v256_64) (word3 v256_64)
      0x1122334455667788L 0x1122334455667788L 0x1122334455667788L
      0x1122334455667788L;
    let v128_32 =
      Int32s.to_int32x4 0x11223344l 0x55667788l 0x99aabbccl 0xddeeff00l
    in
    let v256_32 = broadcast_32 v128_32 in
    eq4 (word0_32 v256_32) (word1_32 v256_32) (word2_32 v256_32)
      (word3_32 v256_32) 0x1122334411223344L 0x1122334411223344L
      0x1122334411223344L 0x1122334411223344L;
    let v128_32_res = broadcast_32_128 v128_32 in
    Utils.eq
      (int32x4_low_int64 v128_32_res)
      (int32x4_high_int64 v128_32_res)
      0x1122334411223344L 0x1122334411223344L

  let () =
    (failmsg := fun () -> Printf.printf "extract/insert_128");
    let v256 =
      int64x4_of_int64s 0x0123456789abcdefL 0xfedcba9876543210L
        0x1111222233334444L 0x5555666677778888L
    in
    let low = extract_128 0 v256 in
    let high = extract_128 1 v256 in
    Utils.eq (int64x2_low_int64 low) (int64x2_high_int64 low)
      0x0123456789abcdefL 0xfedcba9876543210L;
    Utils.eq (int64x2_low_int64 high) (int64x2_high_int64 high)
      0x1111222233334444L 0x5555666677778888L;
    let new_low = int64x2_of_int64s 0xaaaaaaaabbbbbbbbL 0xccccccccddddddddL in
    let new_high = int64x2_of_int64s 0xeeeeeeeeffffffffL 0x0000000011111111L in
    let v1 = insert_128 0 v256 new_low in
    let v2 = insert_128 1 v256 new_high in
    eq4 (word0 v1) (word1 v1) (word2 v1) (word3 v1) 0xaaaaaaaabbbbbbbbL
      0xccccccccddddddddL 0x1111222233334444L 0x5555666677778888L;
    eq4 (word0 v2) (word1 v2) (word2 v2) (word3 v2) 0x0123456789abcdefL
      0xfedcba9876543210L 0xeeeeeeeeffffffffL 0x0000000011111111L

  let () =
    (failmsg := fun () -> Printf.printf "permute2_128");
    let v0 = int64x4_of_int64s 0L 1L 2L 3L in
    let v1 = int64x4_of_int64s 4L 5L 6L 7L in
    let p0 = permute2_128 0b00000000 v0 v1 in
    let p1 = permute2_128 0b00110001 v0 v1 in
    let p2 = permute2_128 0b00100000 v0 v1 in
    let p3 = permute2_128 0b00010011 v0 v1 in
    let p4 = permute2_128 0b10000000 v0 v1 in
    eq4 (word0 p0) (word1 p0) (word2 p0) (word3 p0) 0L 1L 0L 1L;
    eq4 (word0 p1) (word1 p1) (word2 p1) (word3 p1) 2L 3L 6L 7L;
    eq4 (word0 p2) (word1 p2) (word2 p2) (word3 p2) 0L 1L 4L 5L;
    eq4 (word0 p3) (word1 p3) (word2 p3) (word3 p3) 6L 7L 2L 3L;
    eq4 (word0 p4) (word1 p4) (word2 p4) (word3 p4) 0L 1L 0L 0L

  let () =
    (failmsg := fun () -> Printf.printf "vzeroall/vzeroupper");
    let vec256 =
      int64x4_of_int64s 0x1122334455667788L 0x99aabbccddeeff00L
        0xfedcba9876543210L 0x0123456789abcdefL
    in
    let vec128 = int64x2_of_int64s 0xaaaaaaaabbbbbbbbL 0xccccccccddddddddL in
    let f32 = 1.25s in
    let f64 = 2.5 in
    vzeroupper ();
    eq4 (word0 vec256) (word1 vec256) (word2 vec256) (word3 vec256)
      0x1122334455667788L 0x99aabbccddeeff00L 0xfedcba9876543210L
      0x0123456789abcdefL;
    Utils.eq (int64x2_low_int64 vec128)
      (int64x2_high_int64 vec128)
      0xaaaaaaaabbbbbbbbL 0xccccccccddddddddL;
    eqf32 f32 f32 1.25s 1.25s;
    eqf f64 f64 2.5 2.5;
    vzeroall ();
    eq4 (word0 vec256) (word1 vec256) (word2 vec256) (word3 vec256)
      0x1122334455667788L 0x99aabbccddeeff00L 0xfedcba9876543210L
      0x0123456789abcdefL;
    Utils.eq (int64x2_low_int64 vec128)
      (int64x2_high_int64 vec128)
      0xaaaaaaaabbbbbbbbL 0xccccccccddddddddL;
    eqf32 f32 f32 1.25s 1.25s;
    eqf f64 f64 2.5 2.5
end

module AVX2_Util = struct
  include Builtins.AVX2

  let () =
    (failmsg := fun () -> Printf.printf "movemask_8");
    let v0 =
      Int8.to_int8x32 0xff 0x7f 0x80 0x0 0x1 0xcc 0x33 0x55 0xff 0x7f 0x80 0x0
        0x1 0xcc 0x33 0x55 0xff 0x7f 0x80 0x0 0x1 0xcc 0x33 0x55 0xff 0x7f 0x80
        0x0 0x1 0xcc 0x33 0x55
    in
    let i0 = movemask_8 v0 in
    (* Read bits right-to-left *)
    eqi i0 0 0b00100101_00100101_00100101_00100101 0

  let () =
    (failmsg := fun () -> Printf.printf "shift_left_bytes");
    let v0 =
      Int8.to_int8x32 0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd
        0xe 0xf 0x10 0x11 0x12 0x13 0x14 0x15 0x16 0x17 0x18 0x19 0x1a 0x1b 0x1c
        0x1d 0x1e 0x1f
    in
    let v1 = shift_left_bytes 1 v0 in
    eq4 (int8x32_first_int64 v1) (int8x32_second_int64 v1)
      (int8x32_third_int64 v1) (int8x32_fourth_int64 v1) 0x0605040302010000L
      0x0e0d0c0b0a090807L 0x1615141312111000L 0x1e1d1c1b1a191817L;
    ()

  let () =
    (failmsg := fun () -> Printf.printf "shift_right_bytes");
    let v0 =
      Int8.to_int8x32 0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd
        0xe 0xf 0x10 0x11 0x12 0x13 0x14 0x15 0x16 0x17 0x18 0x19 0x1a 0x1b 0x1c
        0x1d 0x1e 0x1f
    in
    let v2 = shift_right_bytes 1 v0 in
    eq4 (int8x32_first_int64 v2) (int8x32_second_int64 v2)
      (int8x32_third_int64 v2) (int8x32_fourth_int64 v2) 0x0807060504030201L
      0x000f0e0d0c0b0a09L 0x1817161514131211L 0x001f1e1d1c1b1a19L

  let () =
    (failmsg := fun () -> Printf.printf "shuffle_16");
    let v0 = Int16.to_int16x16 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 in
    let s0 = shuffle_high_16 0 v0 in
    let s1 = shuffle_high_16 0b01010101 v0 in
    let s2 = shuffle_high_16 0b10101010 v0 in
    let s3 = shuffle_high_16 0b11111111 v0 in
    eq4 (int16x16_first_int64 s0) (int16x16_second_int64 s0)
      (int16x16_third_int64 s0) (int16x16_fourth_int64 s0) 0x0004000300020001L
      0x0005000500050005L 0x000c000b000a0009L 0x000d000d000d000dL;
    eq4 (int16x16_first_int64 s1) (int16x16_second_int64 s1)
      (int16x16_third_int64 s1) (int16x16_fourth_int64 s1) 0x0004000300020001L
      0x0006000600060006L 0x000c000b000a0009L 0x000e000e000e000eL;
    eq4 (int16x16_first_int64 s2) (int16x16_second_int64 s2)
      (int16x16_third_int64 s2) (int16x16_fourth_int64 s2) 0x0004000300020001L
      0x0007000700070007L 0x000c000b000a0009L 0x000f000f000f000fL;
    eq4 (int16x16_first_int64 s3) (int16x16_second_int64 s3)
      (int16x16_third_int64 s3) (int16x16_fourth_int64 s3) 0x0004000300020001L
      0x0008000800080008L 0x000c000b000a0009L 0x0010001000100010L;
    let s0 = shuffle_low_16 0 v0 in
    let s1 = shuffle_low_16 0b01010101 v0 in
    let s2 = shuffle_low_16 0b10101010 v0 in
    let s3 = shuffle_low_16 0b11111111 v0 in
    eq4 (int16x16_first_int64 s0) (int16x16_second_int64 s0)
      (int16x16_third_int64 s0) (int16x16_fourth_int64 s0) 0x0001000100010001L
      0x0008000700060005L 0x0009000900090009L 0x0010000f000e000dL;
    eq4 (int16x16_first_int64 s1) (int16x16_second_int64 s1)
      (int16x16_third_int64 s1) (int16x16_fourth_int64 s1) 0x0002000200020002L
      0x0008000700060005L 0x000a000a000a000aL 0x0010000f000e000dL;
    eq4 (int16x16_first_int64 s2) (int16x16_second_int64 s2)
      (int16x16_third_int64 s2) (int16x16_fourth_int64 s2) 0x0003000300030003L
      0x0008000700060005L 0x000b000b000b000bL 0x0010000f000e000dL;
    eq4 (int16x16_first_int64 s3) (int16x16_second_int64 s3)
      (int16x16_third_int64 s3) (int16x16_fourth_int64 s3) 0x0004000400040004L
      0x0008000700060005L 0x000c000c000c000cL 0x0010000f000e000dL

  let () =
    (failmsg := fun () -> Printf.printf "interleave_8");
    let v0 =
      Int8.to_int8x32 0 1 2 3 4 5 6 7 8 9 0xa 0xb 0xc 0xd 0xe 0xf 0x10 0x11 0x12
        0x13 0x14 0x15 0x16 0x17 0x18 0x19 0x1a 0x1b 0x1c 0x1d 0x1e 0x1f
    in
    let v1 =
      Int8.to_int8x32 0x20 0x21 0x22 0x23 0x24 0x25 0x26 0x27 0x28 0x29 0x2a
        0x2b 0x2c 0x2d 0x2e 0x2f 0x30 0x31 0x32 0x33 0x34 0x35 0x36 0x37 0x38
        0x39 0x3a 0x3b 0x3c 0x3d 0x3e 0x3f
    in
    let i0 = interleave_high_8 v0 v1 in
    let i1 = interleave_low_8 v0 v1 in
    eq4 (int8x32_first_int64 i0) (int8x32_second_int64 i0)
      (int8x32_third_int64 i0) (int8x32_fourth_int64 i0) 0x2b0b2a0a29092808L
      0x2f0f2e0e2d0d2c0cL 0x3b1b3a1a39193818L 0x3f1f3e1e3d1d3c1cL;
    eq4 (int8x32_first_int64 i1) (int8x32_second_int64 i1)
      (int8x32_third_int64 i1) (int8x32_fourth_int64 i1) 0x2303220221012000L
      0x2707260625052404L 0x3313321231113010L 0x3717361635153414L;
    ()

  let () =
    (failmsg := fun () -> Printf.printf "interleave_16");
    let v0 = Int16.to_int16x16 0 1 2 3 4 5 6 7 8 9 0xa 0xb 0xc 0xd 0xe 0xf in
    let v1 =
      Int16.to_int16x16 0x10 0x11 0x12 0x13 0x14 0x15 0x16 0x17 0x18 0x19 0x1a
        0x1b 0x1c 0x1d 0x1e 0x1f
    in
    let i0 = interleave_high_16 v0 v1 in
    let i1 = interleave_low_16 v0 v1 in
    eq4 (int16x16_first_int64 i0) (int16x16_second_int64 i0)
      (int16x16_third_int64 i0) (int16x16_fourth_int64 i0) 0x0015000500140004L
      0x0017000700160006L 0x001d000d001c000cL 0x001f000f001e000eL;
    eq4 (int16x16_first_int64 i1) (int16x16_second_int64 i1)
      (int16x16_third_int64 i1) (int16x16_fourth_int64 i1) 0x0011000100100000L
      0x0013000300120002L 0x0019000900180008L 0x001b000b001a000aL;
    ()

  let () =
    (failmsg := fun () -> Printf.printf "blend_16");
    let v0 = Int16.to_int16x16 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 in
    let v1 =
      Int16.to_int16x16 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
    in
    let b0 = blend_16 0b00000000 v0 v1 in
    let b1 = blend_16 0b01010101 v0 v1 in
    let b2 = blend_16 0b10101010 v0 v1 in
    let b3 = blend_16 0b11111111 v0 v1 in
    eq4 (int16x16_fourth_int64 b0) (int16x16_third_int64 b0)
      (int16x16_second_int64 b0) (int16x16_first_int64 b0) 0x000f000e000d000cL
      0x000b000a00090008L 0x0007000600050004L 0x0003000200010000L;
    eq4 (int16x16_fourth_int64 b1) (int16x16_third_int64 b1)
      (int16x16_second_int64 b1) (int16x16_first_int64 b1) 0x000f001e000d001cL
      0x000b001a00090018L 0x0007001600050014L 0x0003001200010010L;
    eq4 (int16x16_fourth_int64 b2) (int16x16_third_int64 b2)
      (int16x16_second_int64 b2) (int16x16_first_int64 b2) 0x001f000e001d000cL
      0x001b000a00190008L 0x0017000600150004L 0x0013000200110000L;
    eq4 (int16x16_fourth_int64 b3) (int16x16_third_int64 b3)
      (int16x16_second_int64 b3) (int16x16_first_int64 b3) 0x001f001e001d001cL
      0x001b001a00190018L 0x0017001600150014L 0x0013001200110010L

  let () =
    (failmsg := fun () -> Printf.printf "broadcast 8/16");
    let v8 = Int8.to_int8x16 0x11 0x22 0x33 0x44 0x55 0x66 0x77 0x88 in
    let v8_128 = broadcast_8_128 v8 in
    let v8_256 = broadcast_8 v8 in
    Utils.eq (int8x16_low_int64 v8_128)
      (int8x16_high_int64 v8_128)
      0x1111111111111111L 0x1111111111111111L;
    eq4
      (int8x32_first_int64 v8_256)
      (int8x32_second_int64 v8_256)
      (int8x32_third_int64 v8_256)
      (int8x32_fourth_int64 v8_256)
      0x1111111111111111L 0x1111111111111111L 0x1111111111111111L
      0x1111111111111111L;
    let v16 =
      Int16.to_int16x8 0x1122 0x3344 0x5566 0x7788 0x1122 0x3344 0x5566 0x7788
    in
    let v16_128 = broadcast_16_128 v16 in
    let v16_256 = broadcast_16 v16 in
    Utils.eq
      (int16x8_low_int64 v16_128)
      (int16x8_high_int64 v16_128)
      0x1122112211221122L 0x1122112211221122L;
    eq4
      (int16x16_first_int64 v16_256)
      (int16x16_second_int64 v16_256)
      (int16x16_third_int64 v16_256)
      (int16x16_fourth_int64 v16_256)
      0x1122112211221122L 0x1122112211221122L 0x1122112211221122L
      0x1122112211221122L

  let () =
    (failmsg := fun () -> Printf.printf "permute_64");
    let v0 = int64x4_of_int64s 0L 1L 2L 3L in
    let p0 = permute_64 0b00000000 v0 in
    let p1 = permute_64 0b01010101 v0 in
    let p2 = permute_64 0b10101010 v0 in
    let p3 = permute_64 0b11111111 v0 in
    eq4 (word0 p0) (word1 p0) (word2 p0) (word3 p0) 0L 0L 0L 0L;
    eq4 (word0 p1) (word1 p1) (word2 p1) (word3 p1) 1L 1L 1L 1L;
    eq4 (word0 p2) (word1 p2) (word2 p2) (word3 p2) 2L 2L 2L 2L;
    eq4 (word0 p3) (word1 p3) (word2 p3) (word3 p3) 3L 3L 3L 3L
end
