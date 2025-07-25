open Utils256
include Builtins.Int8x32

external low_of : (int[@untagged]) -> (t[@unboxed])
  = "caml_vec256_unreachable" "caml_int8x32_low_of_int"
  [@@noalloc] [@@builtin]

external low_to : (t[@unboxed]) -> (int[@untagged])
  = "caml_vec256_unreachable" "caml_int8x32_low_to_int"
  [@@noalloc] [@@builtin]

let () =
  let v1 = low_of 1 in
  let v2 = low_of 2 in
  let i1 = int8x32_first_int64 v1 |> Int64.logand 0xffL in
  let i2 = int8x32_first_int64 v2 |> Int64.logand 0xffL in
  eq i1 i2 1L 2L;
  let i1 = low_to v1 in
  let i2 = low_to v2 in
  eqi i1 i2 1 2

let check_binop msg scalar vector i0 i1 =
  (failmsg := fun () -> Printf.printf "%s: %02x | %02x\n%!" msg i0 i1);
  let r0 = scalar i0 i1 in
  let r1 = scalar i1 i0 in
  let expect =
    Int8.to_int8x32 r0 r1 r0 r1 r0 r1 r0 r1 r0 r1 r0 r1 r0 r1 r0 r1 r0 r1 r0 r1
      r0 r1 r0 r1 r0 r1 r0 r1 r0 r1 r0 r1
  in
  let v1 =
    Int8.to_int8x32 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1
      i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1
  in
  let v2 =
    Int8.to_int8x32 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0
      i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0
  in
  let result = vector v1 v2 in
  eq4
    (int8x32_first_int64 result)
    (int8x32_second_int64 result)
    (int8x32_third_int64 result)
    (int8x32_fourth_int64 result)
    (int8x32_first_int64 expect)
    (int8x32_second_int64 expect)
    (int8x32_third_int64 expect)
    (int8x32_fourth_int64 expect)

let () =
  Int8.check_ints (check_binop "add" Int8.add add);
  Int8.check_ints (check_binop "sub" Int8.sub sub);
  Int8.check_ints (check_binop "add_saturating" Int8.adds add_saturating);
  Int8.check_ints (check_binop "sub_saturating" Int8.subs sub_saturating);
  Int8.check_ints
    (check_binop "add_saturating_unsigned" Int8.addsu add_saturating_unsigned);
  Int8.check_ints
    (check_binop "sub_saturating_unsigned" Int8.subsu sub_saturating_unsigned);
  Int8.check_ints (check_binop "max" Int8.max max);
  Int8.check_ints (check_binop "min" Int8.min min);
  Int8.check_ints (check_binop "max_unsigned" Int8.maxu max_unsigned);
  Int8.check_ints (check_binop "min_unsigned" Int8.minu min_unsigned);
  Int8.check_ints (check_binop "cmpeq" Int8.cmpeq cmpeq);
  Int8.check_ints (check_binop "cmpgt" Int8.cmpgt cmpgt);
  Int8.check_ints (check_binop "avg_unsigned" Int8.avgu avg_unsigned);
  Int8.check_ints (check_binop "mulsign" Int8.mulsign mulsign);
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x\n   cvtsx_int64x4\n%!" l r);
      let v = Int8.to_int8x16 l r l r 0 0 0 0 in
      let result = Builtins.Int8x16.cvtsx_int64x4 v in
      let expectl = Int8.cvtsx_i64 l in
      let expectr = Int8.cvtsx_i64 r in
      eq4
        (int64x4_first_int64 result)
        (int64x4_second_int64 result)
        (int64x4_third_int64 result)
        (int64x4_fourth_int64 result)
        expectl expectr expectl expectr);
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x\n   cvtzx_int64x4\n%!" l r);
      let v = Int8.to_int8x16 l r l r 0 0 0 0 in
      let result = Builtins.Int8x16.cvtzx_int64x4 v in
      let expectl = Int8.cvtzx_i64 l in
      let expectr = Int8.cvtzx_i64 r in
      eq4
        (int64x4_first_int64 result)
        (int64x4_second_int64 result)
        (int64x4_third_int64 result)
        (int64x4_fourth_int64 result)
        expectl expectr expectl expectr);
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x\n   cvtsx_int32x8\n%!" l r);
      let v = Int8.to_int8x16 l r l r l r l r in
      let result = Builtins.Int8x16.cvtsx_int32x8 v in
      let expectl = Int8.cvtsx_i32 l in
      let expectr = Int8.cvtsx_i32 r in
      let expect =
        Int32s.to_int32x8 expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int32x8_fourth_int64 result)
        (int32x8_third_int64 result)
        (int32x8_second_int64 result)
        (int32x8_first_int64 result)
        (int32x8_fourth_int64 expect)
        (int32x8_third_int64 expect)
        (int32x8_second_int64 expect)
        (int32x8_first_int64 expect));
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x\n   cvtzx_int32x8\n%!" l r);
      let v = Int8.to_int8x16 l r l r l r l r in
      let result = Builtins.Int8x16.cvtzx_int32x8 v in
      let expectl = Int8.cvtzx_i32 l in
      let expectr = Int8.cvtzx_i32 r in
      let expect =
        Int32s.to_int32x8 expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int32x8_fourth_int64 result)
        (int32x8_third_int64 result)
        (int32x8_second_int64 result)
        (int32x8_first_int64 result)
        (int32x8_fourth_int64 expect)
        (int32x8_third_int64 expect)
        (int32x8_second_int64 expect)
        (int32x8_first_int64 expect));
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x\n   cvtsx_int16x16\n%!" l r);
      let v = Int8.to_int8x16 l r l r l r l r in
      let result = Builtins.Int8x16.cvtsx_int16x16 v in
      let expectl = Int8.cvtsx_i16 l in
      let expectr = Int8.cvtsx_i16 r in
      let expect =
        Int16.to_int16x16 expectl expectr expectl expectr expectl expectr
          expectl expectr expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int16x16_fourth_int64 result)
        (int16x16_third_int64 result)
        (int16x16_second_int64 result)
        (int16x16_first_int64 result)
        (int16x16_fourth_int64 expect)
        (int16x16_third_int64 expect)
        (int16x16_second_int64 expect)
        (int16x16_first_int64 expect));
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x\n   cvtzx_int16x16\n%!" l r);
      let v = Int8.to_int8x16 l r l r l r l r in
      let result = Builtins.Int8x16.cvtzx_int16x16 v in
      let expectl = Int8.cvtzx_i16 l in
      let expectr = Int8.cvtzx_i16 r in
      let expect =
        Int16.to_int16x16 expectl expectr expectl expectr expectl expectr
          expectl expectr expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int16x16_fourth_int64 result)
        (int16x16_third_int64 result)
        (int16x16_second_int64 result)
        (int16x16_first_int64 result)
        (int16x16_fourth_int64 expect)
        (int16x16_third_int64 expect)
        (int16x16_second_int64 expect)
        (int16x16_first_int64 expect));
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x\n   abs\n%!" l r);
      let v =
        Int8.to_int8x32 l r l r l r l r l r l r l r l r l r l r l r l r l r l r
          l r l r
      in
      let result = abs v in
      let expectl = Int8.abs l in
      let expectr = Int8.abs r in
      let expect =
        Int8.to_int8x32 expectl expectr expectl expectr expectl expectr expectl
          expectr expectl expectr expectl expectr expectl expectr expectl
          expectr expectl expectr expectl expectr expectl expectr expectl
          expectr expectl expectr expectl expectr expectl expectr expectl
          expectr
      in
      eq4
        (int8x32_first_int64 result)
        (int8x32_second_int64 result)
        (int8x32_third_int64 result)
        (int8x32_fourth_int64 result)
        (int8x32_first_int64 expect)
        (int8x32_second_int64 expect)
        (int8x32_third_int64 expect)
        (int8x32_fourth_int64 expect));
  Int8.check_ints (fun l r ->
      (failmsg
         := fun () ->
              Printf.printf
                "%02x|%02x mul_unsigned_hadd_saturating_int16x16\n%!" l r);
      let v1 =
        Int8.to_int8x32 l l r r l l r r l l r r l l r r l l r r l l r r l l r r
          l l r r
      in
      let v2 =
        Int8.to_int8x32 l r l r l r l r l r l r l r l r l r l r l r l r l r l r
          l r l r
      in
      let result = mul_unsigned_hadd_saturating_int16x16 v1 v2 in
      let sum0 = Int16.adds (Int8.mulu_i16 l l) (Int8.mulu_i16 l r) in
      let sum1 = Int16.adds (Int8.mulu_i16 r l) (Int8.mulu_i16 r r) in
      let expect = Int16.to_int16x8 sum0 sum1 sum0 sum1 sum0 sum1 sum0 sum1 in
      eq4
        (int16x16_first_int64 result)
        (int16x16_second_int64 result)
        (int16x16_third_int64 result)
        (int16x16_fourth_int64 result)
        (int16x8_low_int64 expect)
        (int16x8_high_int64 expect)
        (int16x8_low_int64 expect)
        (int16x8_high_int64 expect));
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x\n\n   sad_unsigned\n%!" l r);
      let v1 =
        Int8.to_int8x32 l l r r l l r r l l r r l l r r l l r r l l r r l l r r
          l l r r
      in
      let v2 =
        Int8.to_int8x32 l r l r l r l r l r l r l r l r l r l r l r l r l r l r
          l r l r
      in
      let result = sad_unsigned v1 v2 in
      let lr = Int8.diffu l r in
      let expect = Stdlib.Int64.of_int (4 * lr) in
      eq4
        (int64x4_first_int64 result)
        (int64x4_second_int64 result)
        (int64x4_third_int64 result)
        (int64x4_fourth_int64 result)
        expect expect expect expect);
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x msadu\n%!" l r);
      let v0 =
        Int8.to_int8x32 l l r r l l r r l l r 0 0 0 0 0 l l r r l l r r l l r 0
          0 0 0 0
      in
      let v1 =
        Int8.to_int8x32 l r l r 0 0 0 0 0 0 0 0 0 0 0 0 l r l r 0 0 0 0 0 0 0 0
          0 0 0 0
      in
      let result = multi_sad_unsigned 0 v0 v1 in
      let lr = 2 * Int8.diffu l r in
      let expect =
        Int16.to_int16x16 lr lr lr lr lr lr lr lr lr lr lr lr lr lr lr lr
      in
      eq4
        (int16x16_first_int64 result)
        (int16x16_second_int64 result)
        (int16x16_third_int64 result)
        (int16x16_fourth_int64 result)
        (int16x16_first_int64 expect)
        (int16x16_second_int64 expect)
        (int16x16_third_int64 expect)
        (int16x16_fourth_int64 expect))
