open Utils
include Builtins.Int8x16
(* Creation / Destruction *)

external low_of : (int[@untagged]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "caml_int8x16_low_of_int"
  [@@noalloc] [@@builtin]

external low_to : (t[@unboxed]) -> (int[@untagged])
  = "caml_vec128_unreachable" "caml_int8x16_low_to_int"
  [@@noalloc] [@@builtin]

let () =
  let v1 = low_of 1 in
  let v2 = low_of 2 in
  let i1 = int8x16_low_int64 v1 |> Int64.logand 0xffL in
  let i2 = int8x16_low_int64 v2 |> Int64.logand 0xffL in
  eq i1 i2 1L 2L;
  let i1 = low_to v1 in
  let i2 = low_to v2 in
  eqi i1 i2 1 2

let check_binop msg scalar vector i0 i1 =
  (failmsg := fun () -> Printf.printf "%s: %02x | %02x\n%!" msg i0 i1);
  let r0 = scalar i0 i1 in
  let r1 = scalar i1 i0 in
  let expect = Int8.to_int8x16 r0 r1 r0 r1 r0 r1 r0 r1 in
  let v1 = Int8.to_int8x16 i0 i1 i0 i1 i0 i1 i0 i1 in
  let v2 = Int8.to_int8x16 i1 i0 i1 i0 i1 i0 i1 i0 in
  let result = vector v1 v2 in
  eq (int8x16_low_int64 result)
    (int8x16_high_int64 result)
    (int8x16_low_int64 expect)
    (int8x16_high_int64 expect)

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
  Int8.check_ints (check_binop "maxu" Int8.maxu maxu);
  Int8.check_ints (check_binop "minu" Int8.minu minu);
  Int8.check_ints (check_binop "cmpeq" Int8.cmpeq cmpeq);
  Int8.check_ints (check_binop "cmpgt" Int8.cmpgt cmpgt);
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x cvt_sx_i64\n%!" l r);
      let v = Int8.to_int8x16 l r 0 0 0 0 0 0 in
      let result = cvtsx_i64 v in
      let expectl = Int8.cvtsx_i64 l in
      let expectr = Int8.cvtsx_i64 r in
      eq (int64x2_low_int64 result) (int64x2_high_int64 result) expectl expectr);
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x cvt_zx_i64\n%!" l r);
      let v = Int8.to_int8x16 l r 0 0 0 0 0 0 in
      let result = cvtzx_i64 v in
      let expectl = Int8.cvtzx_i64 l in
      let expectr = Int8.cvtzx_i64 r in
      eq (int64x2_low_int64 result) (int64x2_high_int64 result) expectl expectr);
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x cvt_sx_i32\n%!" l r);
      let v = Int8.to_int8x16 l r l r 0 0 0 0 in
      let result = cvtsx_i32 v in
      let expectl = Int8.cvtsx_i32 l in
      let expectr = Int8.cvtsx_i32 r in
      let expect = Int32s.to_int32x4 expectl expectr expectl expectr in
      eq (int32x4_low_int64 result)
        (int32x4_high_int64 result)
        (int32x4_low_int64 expect)
        (int32x4_high_int64 expect));
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x cvt_zx_i32\n%!" l r);
      let v = Int8.to_int8x16 l r l r 0 0 0 0 in
      let result = cvtzx_i32 v in
      let expectl = Int8.cvtzx_i32 l in
      let expectr = Int8.cvtzx_i32 r in
      let expect = Int32s.to_int32x4 expectl expectr expectl expectr in
      eq (int32x4_low_int64 result)
        (int32x4_high_int64 result)
        (int32x4_low_int64 expect)
        (int32x4_high_int64 expect));
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x cvt_sx_i16\n%!" l r);
      let v = Int8.to_int8x16 l r l r l r l r in
      let result = cvtsx_i16 v in
      let expectl = Int8.cvtsx_i16 l in
      let expectr = Int8.cvtsx_i16 r in
      let expect =
        Int16.to_int16x8 expectl expectr expectl expectr expectl expectr expectl
          expectr
      in
      eq (int16x8_low_int64 result)
        (int16x8_high_int64 result)
        (int16x8_low_int64 expect)
        (int16x8_high_int64 expect));
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x cvt_zx_i16\n%!" l r);
      let v = Int8.to_int8x16 l r l r l r l r in
      let result = cvtzx_i16 v in
      let expectl = Int8.cvtzx_i16 l in
      let expectr = Int8.cvtzx_i16 r in
      let expect =
        Int16.to_int16x8 expectl expectr expectl expectr expectl expectr expectl
          expectr
      in
      eq (int16x8_low_int64 result)
        (int16x8_high_int64 result)
        (int16x8_low_int64 expect)
        (int16x8_high_int64 expect));
  Int8.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%02x|%02x abs\n%!" l r);
      let v = Int8.to_int8x16 l r l r l r l r in
      let result = abs v in
      let expectl = Int8.abs l in
      let expectr = Int8.abs r in
      let expect =
        Int8.to_int8x16 expectl expectr expectl expectr expectl expectr expectl
          expectr
      in
      eq (int8x16_low_int64 result)
        (int8x16_high_int64 result)
        (int8x16_low_int64 expect)
        (int8x16_high_int64 expect))

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
  let v9 = insert 8 v8 9 in
  let v10 = insert 9 v9 10 in
  let v11 = insert 10 v10 11 in
  let v12 = insert 11 v11 12 in
  let v13 = insert 12 v12 13 in
  let v14 = insert 13 v13 14 in
  let v15 = insert 14 v14 15 in
  let v16 = insert 15 v15 16 in
  let i1 = extract 0 v16 in
  let i2 = extract 1 v16 in
  let i3 = extract 2 v16 in
  let i4 = extract 3 v16 in
  let i5 = extract 4 v16 in
  let i6 = extract 5 v16 in
  let i7 = extract 6 v16 in
  let i8 = extract 7 v16 in
  let i9 = extract 8 v16 in
  let i10 = extract 9 v16 in
  let i11 = extract 10 v16 in
  let i12 = extract 11 v16 in
  let i13 = extract 12 v16 in
  let i14 = extract 13 v16 in
  let i15 = extract 14 v16 in
  let i16 = extract 15 v16 in
  eqi i1 i2 1 2;
  eqi i3 i4 3 4;
  eqi i5 i6 5 6;
  eqi i7 i8 7 8;
  eqi i9 i10 9 10;
  eqi i11 i12 11 12;
  eqi i13 i14 13 14;
  eqi i15 i16 15 16
