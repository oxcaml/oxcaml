open Utils256
include Builtins.Int16x16

external low_of : (int[@untagged]) -> (t[@unboxed])
  = "caml_vec256_unreachable" "caml_int16x16_low_of_int"
  [@@noalloc] [@@builtin]

external low_to : (t[@unboxed]) -> (int[@untagged])
  = "caml_vec256_unreachable" "caml_int16x16_low_to_int"
  [@@noalloc] [@@builtin]

let () =
  let v1 = low_of 1 in
  let v2 = low_of 2 in
  let i1 = int16x16_first_int64 v1 |> Int64.logand 0xffffL in
  let i2 = int16x16_first_int64 v2 |> Int64.logand 0xffffL in
  eq i1 i2 1L 2L;
  let i1 = low_to v1 in
  let i2 = low_to v2 in
  eqi i1 i2 1 2

let check_binop msg scalar vector i0 i1 =
  (failmsg := fun () -> Printf.printf "%s: %04x | %04x\n%!" msg i0 i1);
  let r0 = scalar i0 i1 in
  let r1 = scalar i1 i0 in
  let expect =
    Int16.to_int16x16 r0 r1 r0 r1 r0 r1 r0 r1 r0 r1 r0 r1 r0 r1 r0 r1
  in
  let v1 = Int16.to_int16x16 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 in
  let v2 = Int16.to_int16x16 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 i1 i0 in
  let result = vector v1 v2 in
  (* CR-someday mslater: abstract these sort of checks *)
  eq4
    (int16x16_first_int64 result)
    (int16x16_second_int64 result)
    (int16x16_third_int64 result)
    (int16x16_fourth_int64 result)
    (int16x16_first_int64 expect)
    (int16x16_second_int64 expect)
    (int16x16_third_int64 expect)
    (int16x16_fourth_int64 expect)

let () =
  Int16.check_ints (check_binop "add" Int16.add add);
  Int16.check_ints (check_binop "sub" Int16.sub sub);
  Int16.check_ints (check_binop "add_saturating" Int16.adds add_saturating);
  Int16.check_ints (check_binop "sub_saturating" Int16.subs sub_saturating);
  Int16.check_ints
    (check_binop "add_saturating_unsigned" Int16.addsu add_saturating_unsigned);
  Int16.check_ints
    (check_binop "sub_saturating_unsigned" Int16.subsu sub_saturating_unsigned);
  Int16.check_ints (check_binop "max" Int16.max max);
  Int16.check_ints (check_binop "min" Int16.min min);
  Int16.check_ints (check_binop "max_unsigned" Int16.maxu max_unsigned);
  Int16.check_ints (check_binop "min_unsigned" Int16.minu min_unsigned);
  Int16.check_ints (check_binop "cmpeq" Int16.cmpeq cmpeq);
  Int16.check_ints (check_binop "cmpgt" Int16.cmpgt cmpgt);
  Int16.check_ints (check_binop "mul_high" Int16.mul_high mul_high);
  Int16.check_ints
    (check_binop "mul_high_unsigned" Int16.mul_high_unsigned mul_high_unsigned);
  Int16.check_ints (check_binop "mul_low" Int16.mul_low mul_low);
  Int16.check_ints (check_binop "mul_round" Int16.mul_round mul_round);
  Int16.check_ints (check_binop "avg_unsigned" Int16.avgu avg_unsigned);
  Int16.check_ints (check_binop "mulsign" Int16.mulsign mulsign);
  ()

let () =
  Int16.check_ints (fun l r ->
      (failmsg
         := fun () -> Printf.printf "%04x|%04x cvt_int8x32_saturating\n%!" l r);
      let v = Int16.to_int16x16 l r l r l r l r l r l r l r l r in
      let result = cvt_int8x32_saturating v v in
      let expectl = Int16.cvt_si8 l in
      let expectr = Int16.cvt_si8 r in
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
  Int16.check_ints (fun l r ->
      (failmsg
         := fun () ->
              Printf.printf "%04x|%04x cvt_int8x32_saturating_unsigned\n%!" l r);
      let v = Int16.to_int16x16 l r l r l r l r l r l r l r l r in
      let result = cvt_int8x32_saturating_unsigned v v in
      let expectl = Int16.cvt_su8 l in
      let expectr = Int16.cvt_su8 r in
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
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x abs\n%!" l r);
      let v = Int16.to_int16x16 l r l r l r l r l r l r l r l r in
      let result = abs v in
      let expectl = Int16.abs l in
      let expectr = Int16.abs r in
      let expect =
        Int16.to_int16x16 expectl expectr expectl expectr expectl expectr
          expectl expectr expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int16x16_first_int64 result)
        (int16x16_second_int64 result)
        (int16x16_third_int64 result)
        (int16x16_fourth_int64 result)
        (int16x16_first_int64 expect)
        (int16x16_second_int64 expect)
        (int16x16_third_int64 expect)
        (int16x16_fourth_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x hadd\n%!" l r);
      let v0 = Int16.to_int16x16 l l r r l l r r l l r r l l r r in
      let v1 = Int16.to_int16x16 r r l l r r l l r r l l r r l l in
      let result = hadd v0 v1 in
      let expect =
        Int16.to_int16x16 (Int16.add l l) (Int16.add r r) (Int16.add l l)
          (Int16.add r r) (Int16.add r r) (Int16.add l l) (Int16.add r r)
          (Int16.add l l) (Int16.add l l) (Int16.add r r) (Int16.add l l)
          (Int16.add r r) (Int16.add r r) (Int16.add l l) (Int16.add r r)
          (Int16.add l l)
      in
      eq4
        (int16x16_first_int64 result)
        (int16x16_second_int64 result)
        (int16x16_third_int64 result)
        (int16x16_fourth_int64 result)
        (int16x16_first_int64 expect)
        (int16x16_second_int64 expect)
        (int16x16_third_int64 expect)
        (int16x16_fourth_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x hsub\n%!" l r);
      let v0 = Int16.to_int16x16 l r l r l r l r l r l r l r l r in
      let v1 = Int16.to_int16x16 r l r l r l r l r l r l r l r l in
      let result = hsub v0 v1 in
      let expect =
        Int16.to_int16x16 (Int16.sub l r) (Int16.sub l r) (Int16.sub l r)
          (Int16.sub l r) (Int16.sub r l) (Int16.sub r l) (Int16.sub r l)
          (Int16.sub r l) (Int16.sub l r) (Int16.sub l r) (Int16.sub l r)
          (Int16.sub l r) (Int16.sub r l) (Int16.sub r l) (Int16.sub r l)
          (Int16.sub r l)
      in
      eq4
        (int16x16_first_int64 result)
        (int16x16_second_int64 result)
        (int16x16_third_int64 result)
        (int16x16_fourth_int64 result)
        (int16x16_first_int64 expect)
        (int16x16_second_int64 expect)
        (int16x16_third_int64 expect)
        (int16x16_fourth_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x cvtsx_int64x4\n%!" l r);
      let v = Int16.to_int16x8 l r l r 0 0 0 0 in
      let result = Builtins.Int16x8.cvtsx_int64x4 v in
      let expectl = Int16.cvtsx_i64 l in
      let expectr = Int16.cvtsx_i64 r in
      eq4
        (int64x4_first_int64 result)
        (int64x4_second_int64 result)
        (int64x4_third_int64 result)
        (int64x4_fourth_int64 result)
        expectl expectr expectl expectr);
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x cvtzx_int64x4\n%!" l r);
      let v = Int16.to_int16x8 l r l r 0 0 0 0 in
      let result = Builtins.Int16x8.cvtzx_int64x4 v in
      let expectl = Int16.cvtzx_i64 l in
      let expectr = Int16.cvtzx_i64 r in
      eq4
        (int64x4_first_int64 result)
        (int64x4_second_int64 result)
        (int64x4_third_int64 result)
        (int64x4_fourth_int64 result)
        expectl expectr expectl expectr);
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x cvtsx_int32x8\n%!" l r);
      let v = Int16.to_int16x8 l r l r l r l r in
      let result = Builtins.Int16x8.cvtsx_int32x8 v in
      let expectl = Int16.cvtsx_i32 l in
      let expectr = Int16.cvtsx_i32 r in
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
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x cvtzx_int32x8\n%!" l r);
      let v = Int16.to_int16x8 l r l r l r l r in
      let result = Builtins.Int16x8.cvtzx_int32x8 v in
      let expectl = Int16.cvtzx_i32 l in
      let expectr = Int16.cvtzx_i32 r in
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
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x hadd_saturating\n%!" l r);
      let v0 = Int16.to_int16x16 l l r r l l r r l l r r l l r r in
      let v1 = Int16.to_int16x16 r r l l r r l l r r l l r r l l in
      let result = hadd_saturating v0 v1 in
      let expect =
        Int16.to_int16x16 (Int16.adds l l) (Int16.adds r r) (Int16.adds l l)
          (Int16.adds r r) (Int16.adds r r) (Int16.adds l l) (Int16.adds r r)
          (Int16.adds l l) (Int16.adds l l) (Int16.adds r r) (Int16.adds l l)
          (Int16.adds r r) (Int16.adds r r) (Int16.adds l l) (Int16.adds r r)
          (Int16.adds l l)
      in
      eq4
        (int16x16_first_int64 result)
        (int16x16_second_int64 result)
        (int16x16_third_int64 result)
        (int16x16_fourth_int64 result)
        (int16x16_first_int64 expect)
        (int16x16_second_int64 expect)
        (int16x16_third_int64 expect)
        (int16x16_fourth_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x hsub_saturating\n%!" l r);
      let v0 = Int16.to_int16x16 l r l r l r l r l r l r l r l r in
      let v1 = Int16.to_int16x16 r l r l r l r l r l r l r l r l in
      let result = hsub_saturating v0 v1 in
      let expect =
        Int16.to_int16x16 (Int16.subs l r) (Int16.subs l r) (Int16.subs l r)
          (Int16.subs l r) (Int16.subs r l) (Int16.subs r l) (Int16.subs r l)
          (Int16.subs r l) (Int16.subs l r) (Int16.subs l r) (Int16.subs l r)
          (Int16.subs l r) (Int16.subs r l) (Int16.subs r l) (Int16.subs r l)
          (Int16.subs r l)
      in
      eq4
        (int16x16_first_int64 result)
        (int16x16_second_int64 result)
        (int16x16_third_int64 result)
        (int16x16_fourth_int64 result)
        (int16x16_first_int64 expect)
        (int16x16_second_int64 expect)
        (int16x16_third_int64 expect)
        (int16x16_fourth_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x mul_hadd\n%!" l r);
      let v0 = Int16.to_int16x16 l l r r l l r r l l r r l l r r in
      let v1 = Int16.to_int16x16 r r l l r r l l r r l l r r l l in
      let result = mul_hadd v0 v1 in
      let sum1 = Int32.add (Int16.mul_i32 l r) (Int16.mul_i32 l r) in
      let sum2 = Int32.add (Int16.mul_i32 r l) (Int16.mul_i32 r l) in
      let expect = Int32s.to_int32x8 sum1 sum2 sum1 sum2 sum1 sum2 sum1 sum2 in
      eq4
        (int32x8_fourth_int64 result)
        (int32x8_third_int64 result)
        (int32x8_second_int64 result)
        (int32x8_first_int64 result)
        (int32x8_fourth_int64 expect)
        (int32x8_third_int64 expect)
        (int32x8_second_int64 expect)
        (int32x8_first_int64 expect))

(* Arbitrary test shift *)
let const_shift = 7

let () =
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08x << %08x\n%!" l r);
      let v = Int16.to_int16x16 l r l r l r l r l r l r l r l r in
      let shift = Int16.logand r 0xf in
      let result = sll v (int64x2_of_int64s (Int64.of_int shift) 0L) in
      let expectl = Int16.shift_left l shift in
      let expectr = Int16.shift_left r shift in
      let expect =
        Int16.to_int16x16 expectl expectr expectl expectr expectl expectr
          expectl expectr expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int16x16_first_int64 result)
        (int16x16_second_int64 result)
        (int16x16_third_int64 result)
        (int16x16_fourth_int64 result)
        (int16x16_first_int64 expect)
        (int16x16_second_int64 expect)
        (int16x16_third_int64 expect)
        (int16x16_fourth_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08x >> %08x\n%!" l r);
      let v = Int16.to_int16x16 l r l r l r l r l r l r l r l r in
      let shift = Int16.logand r 0xf in
      let result = srl v (int64x2_of_int64s (Int64.of_int shift) 0L) in
      let expectl = Int16.shift_right_logical l shift in
      let expectr = Int16.shift_right_logical r shift in
      let expect =
        Int16.to_int16x16 expectl expectr expectl expectr expectl expectr
          expectl expectr expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int16x16_first_int64 result)
        (int16x16_second_int64 result)
        (int16x16_third_int64 result)
        (int16x16_fourth_int64 result)
        (int16x16_first_int64 expect)
        (int16x16_second_int64 expect)
        (int16x16_third_int64 expect)
        (int16x16_fourth_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08x >>a %08x\n%!" l r);
      let v = Int16.to_int16x16 l r l r l r l r l r l r l r l r in
      let shift = Int16.logand r 0xf in
      let result = sra v (int64x2_of_int64s (Int64.of_int shift) 0L) in
      let expectl = Int16.shift_right l shift in
      let expectr = Int16.shift_right r shift in
      let expect =
        Int16.to_int16x16 expectl expectr expectl expectr expectl expectr
          expectl expectr expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int16x16_first_int64 result)
        (int16x16_second_int64 result)
        (int16x16_third_int64 result)
        (int16x16_fourth_int64 result)
        (int16x16_first_int64 expect)
        (int16x16_second_int64 expect)
        (int16x16_third_int64 expect)
        (int16x16_fourth_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08x|%08x << 7\n%!" l r);
      let v = Int16.to_int16x16 l r l r l r l r l r l r l r l r in
      let result = slli const_shift v in
      let expectl = Int16.shift_left l 7 in
      let expectr = Int16.shift_left r 7 in
      let expect =
        Int16.to_int16x16 expectl expectr expectl expectr expectl expectr
          expectl expectr expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int16x16_first_int64 result)
        (int16x16_second_int64 result)
        (int16x16_third_int64 result)
        (int16x16_fourth_int64 result)
        (int16x16_first_int64 expect)
        (int16x16_second_int64 expect)
        (int16x16_third_int64 expect)
        (int16x16_fourth_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08x|%08x >> 7\n%!" l r);
      let v = Int16.to_int16x16 l r l r l r l r l r l r l r l r in
      let result = srli const_shift v in
      let expectl = Int16.shift_right_logical l 7 in
      let expectr = Int16.shift_right_logical r 7 in
      let expect =
        Int16.to_int16x16 expectl expectr expectl expectr expectl expectr
          expectl expectr expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int16x16_first_int64 result)
        (int16x16_second_int64 result)
        (int16x16_third_int64 result)
        (int16x16_fourth_int64 result)
        (int16x16_first_int64 expect)
        (int16x16_second_int64 expect)
        (int16x16_third_int64 expect)
        (int16x16_fourth_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08x|%08x >>a 7\n%!" l r);
      let v = Int16.to_int16x16 l r l r l r l r l r l r l r l r in
      let result = srai const_shift v in
      let expectl = Int16.shift_right l 7 in
      let expectr = Int16.shift_right r 7 in
      let expect =
        Int16.to_int16x16 expectl expectr expectl expectr expectl expectr
          expectl expectr expectl expectr expectl expectr expectl expectr
          expectl expectr
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
