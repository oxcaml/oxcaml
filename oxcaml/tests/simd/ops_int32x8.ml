open Utils256
include Builtins.Int32x8

external low_of : int32 -> t
  = "caml_vec256_unreachable" "caml_int32x8_low_of_int32"
  [@@noalloc] [@@unboxed] [@@builtin]

external low_to : t -> int32
  = "caml_vec256_unreachable" "caml_int32x8_low_to_int32"
  [@@noalloc] [@@unboxed] [@@builtin]

let () =
  let v1 = low_of 1l in
  let v2 = low_of 2l in
  let i1 = int32x8_first_int64 v1 |> Int64.logand 0xffffffffL in
  let i2 = int32x8_first_int64 v2 |> Int64.logand 0xffffffffL in
  eq i1 i2 1L 2L;
  let i1 = low_to v1 in
  let i2 = low_to v2 in
  eql i1 i2 1l 2l

let check_binop msg scalar vector i0 i1 =
  (failmsg := fun () -> Printf.printf "%s: %08lx | %08lx\n%!" msg i0 i1);
  let r0 = scalar i0 i1 in
  let r1 = scalar i1 i0 in
  let expect = Int32s.to_int32x8 r0 r1 r0 r1 r0 r1 r0 r1 in
  let v1 = Int32s.to_int32x8 i0 i1 i0 i1 i0 i1 i0 i1 in
  let v2 = Int32s.to_int32x8 i1 i0 i1 i0 i1 i0 i1 i0 in
  let result = vector v1 v2 in
  eq4
    (int32x8_first_int64 result)
    (int32x8_second_int64 result)
    (int32x8_third_int64 result)
    (int32x8_fourth_int64 result)
    (int32x8_first_int64 expect)
    (int32x8_second_int64 expect)
    (int32x8_third_int64 expect)
    (int32x8_fourth_int64 expect)

let () =
  Int32s.check_ints (check_binop "add" Int32.add add);
  Int32s.check_ints (check_binop "sub" Int32.sub sub);
  Int32s.check_ints
    (check_binop "cmpeq"
       (fun l r -> if Int32.equal l r then 0xffffffffl else 0l)
       cmpeq);
  Int32s.check_ints
    (check_binop "cmpgt"
       (fun l r -> if Int32.compare l r = 1 then 0xffffffffl else 0l)
       cmpgt);
  Int32s.check_ints (check_binop "max" Int32.max max);
  Int32s.check_ints (check_binop "min" Int32.min min);
  Int32s.check_ints
    (check_binop "max_unsigned" Int32s.max_unsigned max_unsigned);
  Int32s.check_ints
    (check_binop "min_unsigned" Int32s.min_unsigned min_unsigned);
  Int32s.check_ints (check_binop "mul_low" Int32s.mul_low mul_low);
  Int32s.check_ints
    (check_binop "mulsign"
       (fun l r ->
         let sign_r = Int32.compare r 0l in
         Int32.mul l (Int32.of_int sign_r))
       mulsign);
  ()

let () =
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx abs\n%!" l r);
      let v = Int32s.to_int32x8 l r l r l r l r in
      let result = abs v in
      let expectl = Int32.abs l in
      let expectr = Int32.abs r in
      let expect =
        Int32s.to_int32x8 expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int32x8_first_int64 result)
        (int32x8_second_int64 result)
        (int32x8_third_int64 result)
        (int32x8_fourth_int64 result)
        (int32x8_first_int64 expect)
        (int32x8_second_int64 expect)
        (int32x8_third_int64 expect)
        (int32x8_fourth_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx hadd\n%!" l r);
      let v0 = Int32s.to_int32x8 l l r r l l r r in
      let v1 = Int32s.to_int32x8 r r l l r r l l in
      let result = hadd v0 v1 in
      let expect =
        Int32s.to_int32x8 (Int32.add l l) (Int32.add r r) (Int32.add r r)
          (Int32.add l l) (Int32.add l l) (Int32.add r r) (Int32.add r r)
          (Int32.add l l)
      in
      eq4
        (int32x8_first_int64 result)
        (int32x8_second_int64 result)
        (int32x8_third_int64 result)
        (int32x8_fourth_int64 result)
        (int32x8_first_int64 expect)
        (int32x8_second_int64 expect)
        (int32x8_third_int64 expect)
        (int32x8_fourth_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx hsub\n%!" l r);
      let v0 = Int32s.to_int32x8 l r l r l r l r in
      let v1 = Int32s.to_int32x8 r l r l r l r l in
      let result = hsub v0 v1 in
      let expect =
        Int32s.to_int32x8 (Int32.sub l r) (Int32.sub l r) (Int32.sub r l)
          (Int32.sub r l) (Int32.sub l r) (Int32.sub l r) (Int32.sub r l)
          (Int32.sub r l)
      in
      eq4
        (int32x8_first_int64 result)
        (int32x8_second_int64 result)
        (int32x8_third_int64 result)
        (int32x8_fourth_int64 result)
        (int32x8_first_int64 expect)
        (int32x8_second_int64 expect)
        (int32x8_third_int64 expect)
        (int32x8_fourth_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx mul_even\n%!" l r);
      let v1 = Int32s.to_int32x8 l 0l r 0l l 0l r 0l in
      let v2 = Int32s.to_int32x8 r 0l l 0l r 0l l 0l in
      let result = mul_even v1 v2 in
      let expect = Int64.mul (Int64.of_int32 l) (Int64.of_int32 r) in
      eq4
        (int64x4_first_int64 result)
        (int64x4_second_int64 result)
        (int64x4_third_int64 result)
        (int64x4_fourth_int64 result)
        expect expect expect expect);
  Int32s.check_ints (fun l r ->
      (failmsg
         := fun () -> Printf.printf "%08lx|%08lx mul_even_unsigned\n%!" l r);
      let v1 = Int32s.to_int32x8 l r l r l r l r in
      let v2 = Int32s.to_int32x8 r l r l r l r l in
      let result = mul_even_unsigned v1 v2 in
      let ul = Int64.logand (Int64.of_int32 l) 0xffffffffL in
      let ur = Int64.logand (Int64.of_int32 r) 0xffffffffL in
      let expect = Int64.mul ul ur in
      eq4
        (int64x4_first_int64 result)
        (int64x4_second_int64 result)
        (int64x4_third_int64 result)
        (int64x4_fourth_int64 result)
        expect expect expect expect);
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx cvtsx_int64x4\n%!" l r);
      let v = Int32s.to_int32x4 l r l r in
      let result = Builtins.Int32x4.cvtsx_int64x4 v in
      let expectl = Int64.of_int32 l in
      let expectr = Int64.of_int32 r in
      eq4
        (int64x4_first_int64 result)
        (int64x4_second_int64 result)
        (int64x4_third_int64 result)
        (int64x4_fourth_int64 result)
        expectl expectr expectl expectr);
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx cvtzx_int64x4\n%!" l r);
      let v = Int32s.to_int32x4 l r l r in
      let result = Builtins.Int32x4.cvtzx_int64x4 v in
      let expectl = Int64.logand (Int64.of_int32 l) 0xffffffffL in
      let expectr = Int64.logand (Int64.of_int32 r) 0xffffffffL in
      eq4
        (int64x4_first_int64 result)
        (int64x4_second_int64 result)
        (int64x4_third_int64 result)
        (int64x4_fourth_int64 result)
        expectl expectr expectl expectr);
  Int32s.check_ints (fun l r ->
      (failmsg
         := fun () ->
              Printf.printf "%08lx|%08lx cvt_int16x16_saturating\n%!" l r);
      let v = Int32s.to_int32x8 l r l r l r l r in
      let result = cvt_int16x16_saturating v v in
      let expectl = Int32s.cvt_si16 l in
      let expectr = Int32s.cvt_si16 r in
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
  Int32s.check_ints (fun l r ->
      (failmsg
         := fun () ->
              Printf.printf "%08lx|%08lx cvt_int16x16_saturating_unsigned\n%!" l
                r);
      let v = Int32s.to_int32x8 l r l r l r l r in
      let result = cvt_int16x16_saturating_unsigned v v in
      let expectl = Int32s.cvt_su16 l in
      let expectr = Int32s.cvt_su16 r in
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

(* Arbitrary test shift *)
let const_shift = 7

let to_shift x = Int32.logand x 31l

let () =
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx << %08lx\n%!" l r);
      let v = Int32s.to_int32x8 l r l r l r l r in
      let shift = to_shift r in
      let result = sll v (int64x2_of_int64s (Int64.of_int32 shift) 0L) in
      let expectl = Int32.shift_left l (Int32.to_int shift) in
      let expectr = Int32.shift_left r (Int32.to_int shift) in
      let expect =
        Int32s.to_int32x8 expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int32x8_first_int64 result)
        (int32x8_second_int64 result)
        (int32x8_third_int64 result)
        (int32x8_fourth_int64 result)
        (int32x8_first_int64 expect)
        (int32x8_second_int64 expect)
        (int32x8_third_int64 expect)
        (int32x8_fourth_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx >> %08lx\n%!" l r);
      let v = Int32s.to_int32x8 l r l r l r l r in
      let shift = to_shift r in
      let result = srl v (int64x2_of_int64s (Int64.of_int32 shift) 0L) in
      let expectl = Int32.shift_right_logical l (Int32.to_int shift) in
      let expectr = Int32.shift_right_logical r (Int32.to_int shift) in
      let expect =
        Int32s.to_int32x8 expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int32x8_first_int64 result)
        (int32x8_second_int64 result)
        (int32x8_third_int64 result)
        (int32x8_fourth_int64 result)
        (int32x8_first_int64 expect)
        (int32x8_second_int64 expect)
        (int32x8_third_int64 expect)
        (int32x8_fourth_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx >>a %08lx\n%!" l r);
      let v = Int32s.to_int32x8 l r l r l r l r in
      let shift = to_shift r in
      let result = sra v (int64x2_of_int64s (Int64.of_int32 shift) 0L) in
      let expectl = Int32.shift_right l (Int32.to_int shift) in
      let expectr = Int32.shift_right r (Int32.to_int shift) in
      let expect =
        Int32s.to_int32x8 expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int32x8_first_int64 result)
        (int32x8_second_int64 result)
        (int32x8_third_int64 result)
        (int32x8_fourth_int64 result)
        (int32x8_first_int64 expect)
        (int32x8_second_int64 expect)
        (int32x8_third_int64 expect)
        (int32x8_fourth_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx << 7\n%!" l r);
      let v = Int32s.to_int32x8 l r l r l r l r in
      let result = slli const_shift v in
      let expectl = Int32.shift_left l 7 in
      let expectr = Int32.shift_left r 7 in
      let expect =
        Int32s.to_int32x8 expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int32x8_first_int64 result)
        (int32x8_second_int64 result)
        (int32x8_third_int64 result)
        (int32x8_fourth_int64 result)
        (int32x8_first_int64 expect)
        (int32x8_second_int64 expect)
        (int32x8_third_int64 expect)
        (int32x8_fourth_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx >> 7\n%!" l r);
      let v = Int32s.to_int32x8 l r l r l r l r in
      let result = srli const_shift v in
      let expectl = Int32.shift_right_logical l 7 in
      let expectr = Int32.shift_right_logical r 7 in
      let expect =
        Int32s.to_int32x8 expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int32x8_first_int64 result)
        (int32x8_second_int64 result)
        (int32x8_third_int64 result)
        (int32x8_fourth_int64 result)
        (int32x8_first_int64 expect)
        (int32x8_second_int64 expect)
        (int32x8_third_int64 expect)
        (int32x8_fourth_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx >>a 7\n%!" l r);
      let v = Int32s.to_int32x8 l r l r l r l r in
      let result = srai const_shift v in
      let expectl = Int32.shift_right l 7 in
      let expectr = Int32.shift_right r 7 in
      let expect =
        Int32s.to_int32x8 expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int32x8_first_int64 result)
        (int32x8_second_int64 result)
        (int32x8_third_int64 result)
        (int32x8_fourth_int64 result)
        (int32x8_first_int64 expect)
        (int32x8_second_int64 expect)
        (int32x8_third_int64 expect)
        (int32x8_fourth_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx sllv\n%!" l r);
      let v = Int32s.to_int32x8 l r l r l r l r in
      let shifts =
        Int32s.to_int32x8 (to_shift r) (to_shift l) (to_shift r) (to_shift l)
          (to_shift r) (to_shift l) (to_shift r) (to_shift l)
      in
      let result = sllv v shifts in
      let expectl = Int32.shift_left l (Int32.to_int (to_shift r)) in
      let expectr = Int32.shift_left r (Int32.to_int (to_shift l)) in
      let expect =
        Int32s.to_int32x8 expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int32x8_first_int64 result)
        (int32x8_second_int64 result)
        (int32x8_third_int64 result)
        (int32x8_fourth_int64 result)
        (int32x8_first_int64 expect)
        (int32x8_second_int64 expect)
        (int32x8_third_int64 expect)
        (int32x8_fourth_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx srav\n%!" l r);
      let v = Int32s.to_int32x8 l r l r l r l r in
      let shifts =
        Int32s.to_int32x8 (to_shift r) (to_shift l) (to_shift r) (to_shift l)
          (to_shift r) (to_shift l) (to_shift r) (to_shift l)
      in
      let result = srav v shifts in
      let expectl = Int32.shift_right l (Int32.to_int (to_shift r)) in
      let expectr = Int32.shift_right r (Int32.to_int (to_shift l)) in
      let expect =
        Int32s.to_int32x8 expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int32x8_first_int64 result)
        (int32x8_second_int64 result)
        (int32x8_third_int64 result)
        (int32x8_fourth_int64 result)
        (int32x8_first_int64 expect)
        (int32x8_second_int64 expect)
        (int32x8_third_int64 expect)
        (int32x8_fourth_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx srlv\n%!" l r);
      let v = Int32s.to_int32x8 l r l r l r l r in
      let shifts =
        Int32s.to_int32x8 (to_shift r) (to_shift l) (to_shift r) (to_shift l)
          (to_shift r) (to_shift l) (to_shift r) (to_shift l)
      in
      let result = srlv v shifts in
      let expectl = Int32.shift_right_logical l (Int32.to_int (to_shift r)) in
      let expectr = Int32.shift_right_logical r (Int32.to_int (to_shift l)) in
      let expect =
        Int32s.to_int32x8 expectl expectr expectl expectr expectl expectr
          expectl expectr
      in
      eq4
        (int32x8_first_int64 result)
        (int32x8_second_int64 result)
        (int32x8_third_int64 result)
        (int32x8_fourth_int64 result)
        (int32x8_first_int64 expect)
        (int32x8_second_int64 expect)
        (int32x8_third_int64 expect)
        (int32x8_fourth_int64 expect))

(* Tests for AVX operations on int32x4 *)
let () =
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx sllv_128\n%!" l r);
      let v = Int32s.to_int32x4 l r l r in
      let shifts =
        Int32s.to_int32x4 (to_shift r) (to_shift l) (to_shift r) (to_shift l)
      in
      let result = Builtins.Int32x4.sllv v shifts in
      let expectl = Int32.shift_left l (Int32.to_int (to_shift r)) in
      let expectr = Int32.shift_left r (Int32.to_int (to_shift l)) in
      let expect = Int32s.to_int32x4 expectl expectr expectl expectr in
      eq (int32x4_low_int64 result)
        (int32x4_high_int64 result)
        (int32x4_low_int64 expect)
        (int32x4_high_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx srav_128\n%!" l r);
      let v = Int32s.to_int32x4 l r l r in
      let shifts =
        Int32s.to_int32x4 (to_shift r) (to_shift l) (to_shift r) (to_shift l)
      in
      let result = Builtins.Int32x4.srav v shifts in
      let expectl = Int32.shift_right l (Int32.to_int (to_shift r)) in
      let expectr = Int32.shift_right r (Int32.to_int (to_shift l)) in
      let expect = Int32s.to_int32x4 expectl expectr expectl expectr in
      eq (int32x4_low_int64 result)
        (int32x4_high_int64 result)
        (int32x4_low_int64 expect)
        (int32x4_high_int64 expect));
  Int32s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%08lx|%08lx srlv_128\n%!" l r);
      let v = Int32s.to_int32x4 l r l r in
      let shifts =
        Int32s.to_int32x4 (to_shift r) (to_shift l) (to_shift r) (to_shift l)
      in
      let result = Builtins.Int32x4.srlv v shifts in
      let expectl = Int32.shift_right_logical l (Int32.to_int (to_shift r)) in
      let expectr = Int32.shift_right_logical r (Int32.to_int (to_shift l)) in
      let expect = Int32s.to_int32x4 expectl expectr expectl expectr in
      eq (int32x4_low_int64 result)
        (int32x4_high_int64 result)
        (int32x4_low_int64 expect)
        (int32x4_high_int64 expect))
