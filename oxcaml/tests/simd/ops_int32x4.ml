open Utils
include Builtins.Int32x4
(* Creation / Destruction *)

external low_of : int32 -> t
  = "caml_vec128_unreachable" "caml_int32x4_low_of_int32"
  [@@noalloc] [@@unboxed] [@@builtin]

external low_to : t -> int32
  = "caml_vec128_unreachable" "caml_int32x4_low_to_int32"
  [@@noalloc] [@@unboxed] [@@builtin]

let () =
  let v1 = low_of 1l in
  let v2 = low_of 2l in
  let i1 = int32x4_low_int64 v1 |> Int64.logand 0xffffffffL in
  let i2 = int32x4_low_int64 v2 |> Int64.logand 0xffffffffL in
  eq i1 i2 1L 2L;
  let i1 = low_to v1 in
  let i2 = low_to v2 in
  eql i1 i2 1l 2l

(* Math *)

let check_binop scalar vector i0 i1 =
  (failmsg := fun () -> Printf.printf "%08lx | %08lx\n%!" i0 i1);
  let r0 = scalar i0 i1 in
  let r1 = scalar i1 i0 in
  let expect = Int32s.to_int32x4 r0 r1 r0 r1 in
  let v1 = Int32s.to_int32x4 i0 i1 i0 i1 in
  let v2 = Int32s.to_int32x4 i1 i0 i1 i0 in
  let result = vector v1 v2 in
  eq (int32x4_low_int64 result)
    (int32x4_high_int64 result)
    (int32x4_low_int64 expect)
    (int32x4_high_int64 expect)

let () =
  Int32s.check_ints (check_binop Int32.add add);
  Int32s.check_ints (check_binop Int32.sub sub);
  Int32s.check_ints
    (check_binop (fun l r -> if Int32.equal l r then 0xffffffffl else 0l) cmpeq);
  Int32s.check_ints
    (check_binop
       (fun l r -> if Int32.compare l r = 1 then 0xffffffffl else 0l)
       cmpgt);
  Int32s.check_ints (check_binop Int32.max max);
  Int32s.check_ints (check_binop Int32.min min);
  Int32s.check_ints (check_binop Int32s.max_unsigned max_unsigned);
  Int32s.check_ints (check_binop Int32s.min_unsigned min_unsigned);
  Int32s.check_ints (check_binop Int32s.mul_low mul_low);
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx << %08lx\n%!" l r);
      let v = Int32s.to_int32x4 l r l r in
      let shift = Int32.logand r 0x1fl in
      let result = sll v (Int32s.to_int32x4 shift 0l 0l 0l) in
      let expectl = Int32.shift_left l (Int32.to_int shift) in
      let expectr = Int32.shift_left r (Int32.to_int shift) in
      let expect = Int32s.to_int32x4 expectl expectr expectl expectr in
      eq (int32x4_low_int64 result)
        (int32x4_high_int64 result)
        (int32x4_low_int64 expect)
        (int32x4_high_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx >> %08lx\n%!" l r);
      let v = Int32s.to_int32x4 l r l r in
      let shift = Int32.logand r 0x1fl in
      let result = srl v (Int32s.to_int32x4 shift 0l 0l 0l) in
      let expectl = Int32.shift_right_logical l (Int32.to_int shift) in
      let expectr = Int32.shift_right_logical r (Int32.to_int shift) in
      let expect = Int32s.to_int32x4 expectl expectr expectl expectr in
      eq (int32x4_low_int64 result)
        (int32x4_high_int64 result)
        (int32x4_low_int64 expect)
        (int32x4_high_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx >>a %08lx\n%!" l r);
      let v = Int32s.to_int32x4 l r l r in
      let shift = Int32.logand r 0x1fl in
      let result = sra v (Int32s.to_int32x4 shift 0l 0l 0l) in
      let expectl = Int32.shift_right l (Int32.to_int shift) in
      let expectr = Int32.shift_right r (Int32.to_int shift) in
      let expect = Int32s.to_int32x4 expectl expectr expectl expectr in
      eq (int32x4_low_int64 result)
        (int32x4_high_int64 result)
        (int32x4_low_int64 expect)
        (int32x4_high_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx << 7\n%!" l r);
      let v = Int32s.to_int32x4 l r l r in
      let result = slli 7 v in
      let expectl = Int32.shift_left l 7 in
      let expectr = Int32.shift_left r 7 in
      let expect = Int32s.to_int32x4 expectl expectr expectl expectr in
      eq (int32x4_low_int64 result)
        (int32x4_high_int64 result)
        (int32x4_low_int64 expect)
        (int32x4_high_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx >> 7\n%!" l r);
      let v = Int32s.to_int32x4 l r l r in
      let result = srli 7 v in
      let expectl = Int32.shift_right_logical l 7 in
      let expectr = Int32.shift_right_logical r 7 in
      let expect = Int32s.to_int32x4 expectl expectr expectl expectr in
      eq (int32x4_low_int64 result)
        (int32x4_high_int64 result)
        (int32x4_low_int64 expect)
        (int32x4_high_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx >>a 7\n%!" l r);
      let v = Int32s.to_int32x4 l r l r in
      let result = srai 7 v in
      let expectl = Int32.shift_right l 7 in
      let expectr = Int32.shift_right r 7 in
      let expect = Int32s.to_int32x4 expectl expectr expectl expectr in
      eq (int32x4_low_int64 result)
        (int32x4_high_int64 result)
        (int32x4_low_int64 expect)
        (int32x4_high_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx cvt_f32\n%!" l r);
      let v = Int32s.to_int32x4 l r l r in
      let result = cvt_f32 v in
      let expectl = Int32.to_float l |> Int32.bits_of_float in
      let expectr = Int32.to_float r |> Int32.bits_of_float in
      let expect = Float32.to_float32x4 expectl expectr expectl expectr in
      eq
        (float32x4_low_int64 result)
        (float32x4_high_int64 result)
        (float32x4_low_int64 expect)
        (float32x4_high_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx cvt_f64\n%!" l r);
      let v = Int32s.to_int32x4 l r 0l 0l in
      let result = cvt_f64 v in
      let expectl = Int32.to_float l |> Int64.bits_of_float in
      let expectr = Int32.to_float r |> Int64.bits_of_float in
      eq
        (float64x2_low_int64 result)
        (float64x2_high_int64 result)
        expectl expectr);
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx abs\n%!" l r);
      let v = Int32s.to_int32x4 l r l r in
      let result = abs v in
      let expectl = Int32.abs l in
      let expectr = Int32.abs r in
      let expect = Int32s.to_int32x4 expectl expectr expectl expectr in
      eq (int32x4_low_int64 result)
        (int32x4_high_int64 result)
        (int32x4_low_int64 expect)
        (int32x4_high_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx hadd\n%!" l r);
      let v0 = Int32s.to_int32x4 l l r r in
      let v1 = Int32s.to_int32x4 r r l l in
      let result = hadd v0 v1 in
      let expect =
        Int32s.to_int32x4 (Int32.add l l) (Int32.add r r) (Int32.add r r)
          (Int32.add l l)
      in
      eq (int32x4_low_int64 result)
        (int32x4_high_int64 result)
        (int32x4_low_int64 expect)
        (int32x4_high_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx cvt_sx_i64\n%!" l r);
      let v = Int32s.to_int32x4 l r 0l 0l in
      let result = cvtsx_i64 v in
      let expectl = Int64.of_int32 l in
      let expectr = Int64.of_int32 r in
      eq (int64x2_low_int64 result) (int64x2_high_int64 result) expectl expectr);
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx cvt_zx_i64\n%!" l r);
      let v = Int32s.to_int32x4 l r 0l 0l in
      let result = cvtzx_i64 v in
      let expectl = Int64.of_int32 l |> Int64.logand 0xffffffffL in
      let expectr = Int64.of_int32 r |> Int64.logand 0xffffffffL in
      eq (int64x2_low_int64 result) (int64x2_high_int64 result) expectl expectr);
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx cvt_si16\n%!" l r);
      let v = Int32s.to_int32x4 l r l r in
      let result = cvt_si16 v v in
      let expectl = Int32s.cvt_si16 l in
      let expectr = Int32s.cvt_si16 r in
      let expect =
        Int16.to_int16x8 expectl expectr expectl expectr expectl expectr expectl
          expectr
      in
      eq (int16x8_low_int64 result)
        (int16x8_high_int64 result)
        (int16x8_low_int64 expect)
        (int16x8_high_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx cvt_su16\n%!" l r);
      let v = Int32s.to_int32x4 l r l r in
      let result = cvt_su16 v v in
      let expectl = Int32s.cvt_su16 l in
      let expectr = Int32s.cvt_su16 r in
      let expect =
        Int16.to_int16x8 expectl expectr expectl expectr expectl expectr expectl
          expectr
      in
      eq (int16x8_low_int64 result)
        (int16x8_high_int64 result)
        (int16x8_low_int64 expect)
        (int16x8_high_int64 expect));
  ()

let () =
  let v0 = low_of 0l in
  let v1 = insert 0 v0 1l in
  let v2 = insert 1 v1 2l in
  let v3 = insert 2 v2 3l in
  let v4 = insert 3 v3 4l in
  let i0 = extract 0 v0 in
  let i1 = extract 0 v1 in
  let i2 = extract 0 v2 in
  let i3 = extract 1 v2 in
  let i4 = extract 0 v3 in
  let i5 = extract 1 v3 in
  let i6 = extract 2 v3 in
  let i7 = extract 0 v4 in
  let i8 = extract 1 v4 in
  let i9 = extract 2 v4 in
  let i10 = extract 3 v4 in
  eql i0 i1 0l 1l;
  eql i2 i3 1l 2l;
  eql i4 i5 1l 2l;
  eql i6 i7 3l 1l;
  eql i8 i9 2l 3l;
  eql i10 0l 4l 0l
