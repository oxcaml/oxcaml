open Utils
include Builtins.Int16x8
(* Creation / Destruction *)

external low_of : (int[@untagged]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "caml_int16x8_low_of_int"
  [@@noalloc] [@@builtin]

external low_to : (t[@unboxed]) -> (int[@untagged])
  = "caml_vec128_unreachable" "caml_int16x8_low_to_int"
  [@@noalloc] [@@builtin]

let () =
  let v1 = low_of 1 in
  let v2 = low_of 2 in
  let i1 = int16x8_low_int64 v1 |> Int64.logand 0xffffL in
  let i2 = int16x8_low_int64 v2 |> Int64.logand 0xffffL in
  eq i1 i2 1L 2L;
  let i1 = low_to v1 in
  let i2 = low_to v2 in
  eqi i1 i2 1 2

let check_binop msg scalar vector i0 i1 =
  (failmsg := fun () -> Printf.printf "%s: %04x | %04x\n%!" msg i0 i1);
  let r0 = scalar i0 i1 in
  let r1 = scalar i1 i0 in
  let expect = Int16.to_int16x8 r0 r1 r0 r1 r0 r1 r0 r1 in
  let v1 = Int16.to_int16x8 i0 i1 i0 i1 i0 i1 i0 i1 in
  let v2 = Int16.to_int16x8 i1 i0 i1 i0 i1 i0 i1 i0 in
  let result = vector v1 v2 in
  eq (int16x8_low_int64 result)
    (int16x8_high_int64 result)
    (int16x8_low_int64 expect)
    (int16x8_high_int64 expect)

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
  Int16.check_ints (check_binop "maxu" Int16.maxu maxu);
  Int16.check_ints (check_binop "minu" Int16.minu minu);
  Int16.check_ints (check_binop "cmpeq" Int16.cmpeq cmpeq);
  Int16.check_ints (check_binop "cmpgt" Int16.cmpgt cmpgt);
  Int16.check_ints (check_binop "mul_high" Int16.mul_high mul_high);
  Int16.check_ints
    (check_binop "mul_high_unsigned" Int16.mul_high_unsigned mul_high_unsigned);
  Int16.check_ints (check_binop "mul_low" Int16.mul_low mul_low);
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x cvt_sx_i64\n%!" l r);
      let v = Int16.to_int16x8 l r 0 0 0 0 0 0 in
      let result = cvtsx_i64 v in
      let expectl = Int16.cvtsx_i64 l in
      let expectr = Int16.cvtsx_i64 r in
      eq (int64x2_low_int64 result) (int64x2_high_int64 result) expectl expectr);
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x cvt_zx_i64\n%!" l r);
      let v = Int16.to_int16x8 l r 0 0 0 0 0 0 in
      let result = cvtzx_i64 v in
      let expectl = Int16.cvtzx_i64 l in
      let expectr = Int16.cvtzx_i64 r in
      eq (int64x2_low_int64 result) (int64x2_high_int64 result) expectl expectr);
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x cvt_si8\n%!" l r);
      let v = Int16.to_int16x8 l r l r l r l r in
      let result = cvt_si8 v v in
      let expectl = Int16.cvt_si8 l in
      let expectr = Int16.cvt_si8 r in
      let expect =
        Int8.to_int8x16 expectl expectr expectl expectr expectl expectr expectl
          expectr
      in
      eq (int8x16_low_int64 result)
        (int8x16_high_int64 result)
        (int8x16_low_int64 expect)
        (int8x16_high_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x cvt_su8\n%!" l r);
      let v = Int16.to_int16x8 l r l r l r l r in
      let result = cvt_su8 v v in
      let expectl = Int16.cvt_su8 l in
      let expectr = Int16.cvt_su8 r in
      let expect =
        Int8.to_int8x16 expectl expectr expectl expectr expectl expectr expectl
          expectr
      in
      eq (int8x16_low_int64 result)
        (int8x16_high_int64 result)
        (int8x16_low_int64 expect)
        (int8x16_high_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x cvt_sx_i32\n%!" l r);
      let v = Int16.to_int16x8 l r l r 0 0 0 0 in
      let result = cvtsx_i32 v in
      let expectl = Int16.cvtsx_i32 l in
      let expectr = Int16.cvtsx_i32 r in
      let expect = Int32s.to_int32x4 expectl expectr expectl expectr in
      eq (int32x4_low_int64 result)
        (int32x4_high_int64 result)
        (int32x4_low_int64 expect)
        (int32x4_high_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x cvt_zx_i32\n%!" l r);
      let v = Int16.to_int16x8 l r l r 0 0 0 0 in
      let result = cvtzx_i32 v in
      let expectl = Int16.cvtzx_i32 l in
      let expectr = Int16.cvtzx_i32 r in
      let expect = Int32s.to_int32x4 expectl expectr expectl expectr in
      eq (int32x4_low_int64 result)
        (int32x4_high_int64 result)
        (int32x4_low_int64 expect)
        (int32x4_high_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x abs\n%!" l r);
      let v = Int16.to_int16x8 l r l r l r l r in
      let result = abs v in
      let expectl = Int16.abs l in
      let expectr = Int16.abs r in
      let expect =
        Int16.to_int16x8 expectl expectr expectl expectr expectl expectr expectl
          expectr
      in
      eq (int16x8_low_int64 result)
        (int16x8_high_int64 result)
        (int16x8_low_int64 expect)
        (int16x8_high_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%04x|%04x hadd\n%!" l r);
      let v0 = Int16.to_int16x8 l l r r l l r r in
      let v1 = Int16.to_int16x8 r r l l r r l l in
      let result = hadd v0 v1 in
      let expect =
        Int16.to_int16x8 (Int16.add l l) (Int16.add r r) (Int16.add l l)
          (Int16.add r r) (Int16.add r r) (Int16.add l l) (Int16.add r r)
          (Int16.add l l)
      in
      eq (int16x8_low_int64 result)
        (int16x8_high_int64 result)
        (int16x8_low_int64 expect)
        (int16x8_high_int64 expect))

let () =
  let v0 = low_of 0 in
  let v1 = insert 0 v0 1 in
  let v2 = insert 1 v1 2 in
  let v3 = insert 2 v2 3 in
  let v4 = insert 3 v3 4 in
  let v5 = insert 4 v4 5 in
  let v6 = insert 5 v5 6 in
  let v7 = insert 6 v6 7 in
  let v8 = insert 7 v7 8 in
  let i0 = extract 0 v8 in
  let i1 = extract 1 v8 in
  let i2 = extract 2 v8 in
  let i3 = extract 3 v8 in
  let i4 = extract 4 v8 in
  let i5 = extract 5 v8 in
  let i6 = extract 6 v8 in
  let i7 = extract 7 v8 in
  eqi i0 i1 1 2;
  eqi i2 i3 3 4;
  eqi i4 i5 5 6;
  eqi i6 i7 7 8

let () =
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08x << %08x\n%!" l r);
      let v = Int16.to_int16x8 l r l r l r l r in
      let shift = Int16.logand r 0xf in
      let result = sll v (Int16.to_int16x8 shift 0 0 0 0 0 0 0) in
      let expectl = Int16.shift_left l shift in
      let expectr = Int16.shift_left r shift in
      let expect =
        Int16.to_int16x8 expectl expectr expectl expectr expectl expectr expectl
          expectr
      in
      eq (int16x8_low_int64 result)
        (int16x8_high_int64 result)
        (int16x8_low_int64 expect)
        (int16x8_high_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08x >> %08x\n%!" l r);
      let v = Int16.to_int16x8 l r l r l r l r in
      let shift = Int16.logand r 0xf in
      let result = srl v (Int16.to_int16x8 shift 0 0 0 0 0 0 0) in
      let expectl = Int16.shift_right_logical l shift in
      let expectr = Int16.shift_right_logical r shift in
      let expect =
        Int16.to_int16x8 expectl expectr expectl expectr expectl expectr expectl
          expectr
      in
      eq (int16x8_low_int64 result)
        (int16x8_high_int64 result)
        (int16x8_low_int64 expect)
        (int16x8_high_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08x >>a %08x\n%!" l r);
      let v = Int16.to_int16x8 l r l r l r l r in
      let shift = Int16.logand r 0xf in
      let result = sra v (Int16.to_int16x8 shift 0 0 0 0 0 0 0) in
      let expectl = Int16.shift_right l shift in
      let expectr = Int16.shift_right r shift in
      let expect =
        Int16.to_int16x8 expectl expectr expectl expectr expectl expectr expectl
          expectr
      in
      eq (int16x8_low_int64 result)
        (int16x8_high_int64 result)
        (int16x8_low_int64 expect)
        (int16x8_high_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08x|%08x << 7\n%!" l r);
      let v = Int16.to_int16x8 l r l r l r l r in
      let result = slli 7 v in
      let expectl = Int16.shift_left l 7 in
      let expectr = Int16.shift_left r 7 in
      let expect =
        Int16.to_int16x8 expectl expectr expectl expectr expectl expectr expectl
          expectr
      in
      eq (int16x8_low_int64 result)
        (int16x8_high_int64 result)
        (int16x8_low_int64 expect)
        (int16x8_high_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08x|%08x >> 7\n%!" l r);
      let v = Int16.to_int16x8 l r l r l r l r in
      let result = srli 7 v in
      let expectl = Int16.shift_right_logical l 7 in
      let expectr = Int16.shift_right_logical r 7 in
      let expect =
        Int16.to_int16x8 expectl expectr expectl expectr expectl expectr expectl
          expectr
      in
      eq (int16x8_low_int64 result)
        (int16x8_high_int64 result)
        (int16x8_low_int64 expect)
        (int16x8_high_int64 expect));
  Int16.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08x|%08x >>a 7\n%!" l r);
      let v = Int16.to_int16x8 l r l r l r l r in
      let result = srai 7 v in
      let expectl = Int16.shift_right l 7 in
      let expectr = Int16.shift_right r 7 in
      let expect =
        Int16.to_int16x8 expectl expectr expectl expectr expectl expectr expectl
          expectr
      in
      eq (int16x8_low_int64 result)
        (int16x8_high_int64 result)
        (int16x8_low_int64 expect)
        (int16x8_high_int64 expect));
  ()
