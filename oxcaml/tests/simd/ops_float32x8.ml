open Utils256
include Builtins.Float32x8

(* Creation / Destruction *)
external low_of : float32 -> t
  = "caml_vec256_unreachable" "caml_float32x8_low_of_float32"
  [@@noalloc] [@@unboxed] [@@builtin]

external low_to : t -> float32
  = "caml_vec256_unreachable" "caml_float32x8_low_to_float32"
  [@@noalloc] [@@unboxed] [@@builtin]

let () =
  let v1 = low_of 1.s in
  let v2 = low_of 2.s in
  let i1 = Int64.logand (float32x8_first_int64 v1) 0xffffffffL in
  let i2 = Int64.logand (float32x8_first_int64 v2) 0xffffffffL in
  eq i1 i2 0x3f800000L 0x40000000L;
  let f1 = low_to v1 in
  let f2 = low_to v2 in
  eqf32 f1 f2 1.s 2.s

(* Math *)

let check_cmp msg scalar vector f0 f1 =
  (failmsg := fun () -> Printf.printf "check_cmp %s\n" msg);
  let r0, m0 = if scalar f0 f1 then 0xffffffffl, 1 else 0l, 0 in
  let r1, m1 = if scalar f1 f0 then 0xffffffffl, 1 else 0l, 0 in
  let expect = Float32.to_float32x8 r0 r1 r0 r1 r0 r1 r0 r1 in
  let expect_mask =
    m0 lor (m1 lsl 1) lor (m0 lsl 2) lor (m1 lsl 3) lor (m0 lsl 4)
    lor (m1 lsl 5) lor (m0 lsl 6) lor (m1 lsl 7)
  in
  let v1 = Float32.to_float32x8 f0 f1 f0 f1 f0 f1 f0 f1 in
  let v2 = Float32.to_float32x8 f1 f0 f1 f0 f1 f0 f1 f0 in
  let result = vector v1 v2 in
  let result_i = Vector256_casts.int32x8_of_float32x8 result in
  let mask = Builtins.AVX.movemask_32 result_i in
  let expect_i = Vector256_casts.int32x8_of_float32x8 expect in
  eqi mask mask expect_mask (Builtins.AVX.movemask_32 expect_i);
  eq_float32x8 ~result ~expect

let () =
  Float32.check_floats (check_cmp "eq" Float32.eq (fun l r -> cmp 0 l r));
  Float32.check_floats (check_cmp "lt" Float32.lt (fun l r -> cmp 1 l r));
  Float32.check_floats (check_cmp "le" Float32.le (fun l r -> cmp 2 l r));
  Float32.check_floats (check_cmp "uord" Float32.uord (fun l r -> cmp 3 l r));
  Float32.check_floats (check_cmp "neq" Float32.neq (fun l r -> cmp 4 l r));
  Float32.check_floats (check_cmp "nlt" Float32.nlt (fun l r -> cmp 5 l r));
  Float32.check_floats (check_cmp "nle" Float32.nle (fun l r -> cmp 6 l r));
  Float32.check_floats (check_cmp "ord" Float32.ord (fun l r -> cmp 7 l r))

let check_binop msg scalar vector f0 f1 =
  let r0 = scalar f0 f1 in
  let r1 = scalar f1 f0 in
  let expect = Float32.to_float32x8 r0 r1 r0 r1 r0 r1 r0 r1 in
  let v1 = Float32.to_float32x8 f0 f1 f0 f1 f0 f1 f0 f1 in
  let v2 = Float32.to_float32x8 f1 f0 f1 f0 f1 f0 f1 f0 in
  let result = vector v1 v2 in
  (failmsg := fun () -> Printf.printf "check_binop32 %s %lx %lx\n%!" msg f0 f1);
  eq_float32x8 ~result ~expect

let () =
  Float32.check_floats (check_binop "add" Float32.add add);
  Float32.check_floats (check_binop "sub" Float32.sub sub);
  Float32.check_floats (check_binop "mul" Float32.mul mul);
  Float32.check_floats (check_binop "div" Float32.div div);
  Float32.check_floats (check_binop "max" Float32.c_max max);
  Float32.check_floats (check_binop "min" Float32.c_min min)

let check_unop msg scalar vector f =
  (failmsg := fun () -> Printf.printf "check_unop %s  %lx\n%!" msg f);
  let r = scalar f in
  let expect = Float32.to_float32x8 r r r r r r r r in
  let v = Float32.to_float32x8 f f f f f f f f in
  let result = vector v in
  eq_float32x8 ~result ~expect

let () =
  Float32.check_floats (fun f _ -> check_unop "rcp" Float32.rcp rcp f);
  Float32.check_floats (fun f _ -> check_unop "sqrt" Float32.sqrt sqrt f);
  Float32.check_floats (fun f _ -> check_unop "rsqrt" Float32.rsqrt rsqrt f)

let () =
  Float32.check_floats (fun f0 f1 ->
      (failmsg
         := fun () ->
              Printf.printf "%f | %f\n%!" (Int32.float_of_bits f0)
                (Int32.float_of_bits f1));
      let i0 =
        Float32.cvt_i32 f0 |> Int64.of_int32 |> Int64.logand 0xffffffffL
      in
      let i1 =
        Float32.cvt_i32 f1 |> Int64.of_int32 |> Int64.logand 0xffffffffL
      in
      let v = Float32.to_float32x8 f0 f1 f0 f1 f0 f1 f0 f1 in
      let i = cvt_float32x8_int32x8 v in
      eq4 (int32x8_first_int64 i) (int32x8_second_int64 i)
        (int32x8_third_int64 i) (int32x8_fourth_int64 i)
        (Int64.logor (Int64.shift_left i1 32) i0)
        (Int64.logor (Int64.shift_left i1 32) i0)
        (Int64.logor (Int64.shift_left i1 32) i0)
        (Int64.logor (Int64.shift_left i1 32) i0))

let () =
  Float32.check_floats (fun f0 f1 ->
      (failmsg
         := fun () ->
              Printf.printf "%f | %f\n%!" (Int32.float_of_bits f0)
                (Int32.float_of_bits f1));
      let i0 =
        Float32.cvtt_i32 f0 |> Int64.of_int32 |> Int64.logand 0xffffffffL
      in
      let i1 =
        Float32.cvtt_i32 f1 |> Int64.of_int32 |> Int64.logand 0xffffffffL
      in
      let v = Float32.to_float32x8 f0 f1 f0 f1 f0 f1 f0 f1 in
      let i = cvtt_float32x8_int32x8 v in
      eq4 (int32x8_first_int64 i) (int32x8_second_int64 i)
        (int32x8_third_int64 i) (int32x8_fourth_int64 i)
        (Int64.logor (Int64.shift_left i1 32) i0)
        (Int64.logor (Int64.shift_left i1 32) i0)
        (Int64.logor (Int64.shift_left i1 32) i0)
        (Int64.logor (Int64.shift_left i1 32) i0))

let () =
  Int32s.check_ints (fun i0 i1 ->
      let f0 = Int32.to_float i0 |> Int32.bits_of_float in
      let f1 = Int32.to_float i1 |> Int32.bits_of_float in
      let v = Int32s.to_int32x8 i0 i1 i0 i1 i0 i1 i0 i1 in
      let f = cvt_int32x8_float32x8 v in
      let expect = Float32.to_float32x8 f0 f1 f0 f1 f0 f1 f0 f1 in
      eq_float32x8 ~result:f ~expect)

let () =
  Float32.check_floats (fun f0 f1 ->
      (failmsg := fun () -> Printf.printf "addsub\n");
      let fv0 = Float32.to_float32x8 f0 f1 f0 f1 f0 f1 f0 f1 in
      let fv1 = Float32.to_float32x8 f1 f0 f1 f0 f1 f0 f1 f0 in
      let result = addsub fv0 fv1 in
      let expect =
        Float32.to_float32x8 (Float32.sub f0 f1) (Float32.add f1 f0)
          (Float32.sub f0 f1) (Float32.add f1 f0) (Float32.sub f0 f1)
          (Float32.add f1 f0) (Float32.sub f0 f1) (Float32.add f1 f0)
      in
      eq_float32x8 ~result ~expect)

let () =
  Float32.check_floats (fun f0 f1 ->
      (failmsg := fun () -> Printf.printf "hadd\n");
      let fv0 = Float32.to_float32x8 f0 f0 f1 f1 f0 f0 f1 f1 in
      let fv1 = Float32.to_float32x8 f1 f1 f0 f0 f1 f1 f0 f0 in
      let result = hadd fv0 fv1 in
      let expect =
        Float32.to_float32x8 (Float32.add f0 f0) (Float32.add f1 f1)
          (Float32.add f1 f1) (Float32.add f0 f0) (Float32.add f0 f0)
          (Float32.add f1 f1) (Float32.add f1 f1) (Float32.add f0 f0)
      in
      eq_float32x8 ~result ~expect)

let () =
  Float32.check_floats (fun f0 f1 ->
      (failmsg := fun () -> Printf.printf "hsub\n");
      let fv0 = Float32.to_float32x8 f0 f0 f1 f1 f0 f0 f1 f1 in
      let fv1 = Float32.to_float32x8 f1 f1 f0 f0 f1 f1 f0 f0 in
      let result = hsub fv0 fv1 in
      let expect =
        Float32.to_float32x8 (Float32.sub f0 f0) (Float32.sub f1 f1)
          (Float32.sub f1 f1) (Float32.sub f0 f0) (Float32.sub f0 f0)
          (Float32.sub f1 f1) (Float32.sub f1 f1) (Float32.sub f0 f0)
      in
      eq_float32x8 ~result ~expect)

let () =
  Float32.check_floats (fun f0 f1 ->
      (failmsg := fun () -> Printf.printf "round\n");
      let fv = Float32.to_float32x8 f0 f1 f0 f1 f0 f1 f0 f1 in
      let result = round_near fv in
      let expect =
        Float32.to_float32x8 (Float32.round f0) (Float32.round f1)
          (Float32.round f0) (Float32.round f1) (Float32.round f0)
          (Float32.round f1) (Float32.round f0) (Float32.round f1)
      in
      eq_float32x8 ~result ~expect)

let () =
  Float32.check_floats (fun f0 f1 ->
      (failmsg
         := fun () ->
              Printf.printf "dpf32 %f %f\n%!" (Int32.float_of_bits f0)
                (Int32.float_of_bits f1));
      let fv0 = Float32.to_float32x8 f0 f1 f0 f1 f0 f1 f0 f1 in
      let fv1 = Float32.to_float32x8 f1 f0 f1 f0 f1 f0 f1 f0 in
      let result = dp 0b1111_0001 fv0 fv1 in
      let dp =
        Float32.add
          (Float32.add (Float32.mul f0 f1) (Float32.mul f1 f0))
          (Float32.add (Float32.mul f0 f1) (Float32.mul f1 f0))
      in
      let expect = Float32.to_float32x8 dp 0l 0l 0l dp 0l 0l 0l in
      (* When both are NaN, AMD returns the first argument and Intel returns the
         second argument. Hence we do not test this case. *)
      if f0 |> Int32.float_of_bits |> Float.is_nan
         && f1 |> Int32.float_of_bits |> Float.is_nan
      then ()
      else eq_float32x8 ~result ~expect)
