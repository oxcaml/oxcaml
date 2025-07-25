open Utils256
include Builtins.Float64x4

(* Creation / Destruction *)

external low_of : float -> t
  = "caml_vec256_unreachable" "caml_float64x4_low_of_float"
  [@@noalloc] [@@unboxed] [@@builtin]

external low_to : t -> float
  = "caml_vec256_unreachable" "caml_float64x4_low_to_float"
  [@@noalloc] [@@unboxed] [@@builtin]

let () =
  let v1 = low_of 1. in
  let v2 = low_of 2. in
  let i1 = float64x4_first_int64 v1 in
  let i2 = float64x4_first_int64 v2 in
  eq64 i1 0x3ff0000000000000L;
  eq64 i2 0x4000000000000000L;
  let f1 = low_to v1 in
  let f2 = low_to v2 in
  eqf f1 f2 1. 2.

(* Math *)

let check_cmp msg scalar vector f0 f1 =
  (failmsg := fun () -> Printf.printf "check_cmp %s\n" msg);
  let r0, m0 =
    if scalar f0 f1
    then Int64.float_of_bits 0xFFFFFFFFFFFFFFFFL, 1
    else Int64.float_of_bits 0L, 0
  in
  let r1, m1 =
    if scalar f1 f0
    then Int64.float_of_bits 0xFFFFFFFFFFFFFFFFL, 1
    else Int64.float_of_bits 0L, 0
  in
  let expect = Float64.to_float64x4 r0 r1 r0 r1 in
  let expect_mask = m0 lor (m1 lsl 1) lor (m0 lsl 2) lor (m1 lsl 3) in
  let v1 = Float64.to_float64x4 f0 f1 f0 f1 in
  let v2 = Float64.to_float64x4 f1 f0 f1 f0 in
  let result = vector v1 v2 in
  let result_i = Vector256_casts.int64x4_of_float64x4 result in
  let mask = Builtins.AVX.movemask_64 result_i in
  let expect_i = Vector256_casts.int64x4_of_float64x4 expect in
  eqi mask mask expect_mask (Builtins.AVX.movemask_64 expect_i);
  eq_float64x4 ~result ~expect

let () =
  let remove_nan p l r = p l r && not (Float.is_nan l || Float.is_nan r) in
  let add_nan p l r = p l r || Float.is_nan l || Float.is_nan r in
  Float64.check_floats
    (check_cmp "eq" (remove_nan Float.equal) (fun l r -> cmp 0 l r));
  Float64.check_floats
    (check_cmp "lt"
       (remove_nan (fun l r -> Float.compare l r = -1))
       (fun l r -> cmp 1 l r));
  Float64.check_floats
    (check_cmp "le"
       (remove_nan (fun l r -> Float.compare l r <= 0))
       (fun l r -> cmp 2 l r));
  Float64.check_floats
    (check_cmp "uord"
       (fun l r -> Float.is_nan l || Float.is_nan r)
       (fun l r -> cmp 3 l r));
  Float64.check_floats
    (check_cmp "neq"
       (fun l r ->
         (not (Float.equal l r)) || (Float.is_nan l && Float.is_nan r))
       (fun l r -> cmp 4 l r));
  Float64.check_floats
    (check_cmp "nlt"
       (add_nan (fun l r -> Float.compare l r >= 0))
       (fun l r -> cmp 5 l r));
  Float64.check_floats
    (check_cmp "nle"
       (add_nan (fun l r -> Float.compare l r = 1))
       (fun l r -> cmp 6 l r));
  Float64.check_floats
    (check_cmp "ord"
       (fun l r -> (not (Float.is_nan l)) && not (Float.is_nan r))
       (fun l r -> cmp 7 l r))

let check_binop msg scalar vector f0 f1 =
  (failmsg := fun () -> Printf.printf "check_binop64 %s %f %f\n%!" msg f0 f1);
  let r0 = scalar f0 f1 in
  let r1 = scalar f1 f0 in
  let expect = Float64.to_float64x4 r0 r1 r0 r1 in
  let v1 = Float64.to_float64x4 f0 f1 f0 f1 in
  let v2 = Float64.to_float64x4 f1 f0 f1 f0 in
  let result = vector v1 v2 in
  eq_float64x4 ~result ~expect

let () =
  Float64.check_floats (check_binop "add" Float.add add);
  Float64.check_floats (check_binop "sub" Float.sub sub);
  Float64.check_floats (check_binop "mul" Float.mul mul);
  Float64.check_floats (check_binop "div" Float.div div);
  Float64.check_floats (check_binop "max" Float64.c_max max);
  Float64.check_floats (check_binop "min" Float64.c_min min)

let check_unop msg scalar vector f =
  (failmsg := fun () -> Printf.printf "check_unop %s  %f\n%!" msg f);
  let r = scalar f in
  let expect = Float64.to_float64x4 r r r r in
  let v = Float64.to_float64x4 f f f f in
  let result = vector v in
  eq_float64x4 ~result ~expect

let () = Float64.check_floats (fun f _ -> check_unop "sqrt" Float.sqrt sqrt f)

let () =
  Float64.check_floats (fun f0 f1 ->
      (failmsg := fun () -> Printf.printf "cvti32 %g | %g\n%!" f0 f1);
      let i0 =
        Int32.of_float (Float64.c_round f0)
        |> Int64.of_int32 |> Int64.logand 0xffffffffL
      in
      let i1 =
        Int32.of_float (Float.round f1)
        |> Int64.of_int32 |> Int64.logand 0xffffffffL
      in
      let ii = Int64.(logor (shift_left i1 32) i0) in
      let iv = int32x4_of_int64s ii ii in
      let fv = Float64.to_float64x4 f0 f1 f0 f1 in
      let res = cvt_int32x4 fv in
      eq (int32x4_low_int64 res) (int32x4_high_int64 res) (int32x4_low_int64 iv)
        (int32x4_high_int64 iv))

let () =
  Float64.check_floats (fun f0 f1 ->
      (failmsg := fun () -> Printf.printf "cvtti32 %g | %g\n%!" f0 f1);
      let i0 =
        Float64.cvtt_i32 f0 |> Int64.of_int32 |> Int64.logand 0xffffffffL
      in
      let i1 =
        Float64.cvtt_i32 f1 |> Int64.of_int32 |> Int64.logand 0xffffffffL
      in
      let ii = Int64.(logor (shift_left i1 32) i0) in
      let iv = int32x4_of_int64s ii ii in
      let fv = Float64.to_float64x4 f0 f1 f0 f1 in
      let res = cvtt_int32x4 fv in
      eq (int32x4_low_int64 res) (int32x4_high_int64 res) (int32x4_low_int64 iv)
        (int32x4_high_int64 iv))

let () =
  Int32s.check_ints (fun i0 i1 ->
      let f0 = Int32.to_float i0 in
      let f1 = Int32.to_float i1 in
      let v = Int32s.to_int32x4 i0 i1 i0 i1 in
      let f = cvt_int32x4_float64x4 v in
      let expect = Float64.to_float64x4 f0 f1 f0 f1 in
      eq_float64x4 ~result:f ~expect)

let () =
  Float64.check_floats (fun f0 f1 ->
      (failmsg := fun () -> Printf.printf "addsub\n");
      let fv0 = Float64.to_float64x4 f0 f1 f0 f1 in
      let fv1 = Float64.to_float64x4 f1 f0 f1 f0 in
      let result = addsub fv0 fv1 in
      let expect =
        Float64.to_float64x4 (Float.sub f0 f1) (Float.add f1 f0)
          (Float.sub f0 f1) (Float.add f1 f0)
      in
      eq_float64x4 ~result ~expect)

let () =
  Float64.check_floats (fun f0 f1 ->
      (failmsg := fun () -> Printf.printf "hadd\n");
      let fv0 = Float64.to_float64x4 f0 f0 f1 f1 in
      let fv1 = Float64.to_float64x4 f1 f1 f0 f0 in
      let result = hadd fv0 fv1 in
      let expect =
        Float64.to_float64x4 (Float.add f0 f0) (Float.add f1 f1)
          (Float.add f1 f1) (Float.add f0 f0)
      in
      eq_float64x4 ~result ~expect)

let () =
  Float64.check_floats (fun f0 f1 ->
      (failmsg := fun () -> Printf.printf "hsub\n");
      let fv0 = Float64.to_float64x4 f0 f0 f1 f1 in
      let fv1 = Float64.to_float64x4 f1 f1 f0 f0 in
      let result = hsub fv0 fv1 in
      let expect =
        Float64.to_float64x4 (Float.sub f0 f0) (Float.sub f1 f1)
          (Float.sub f1 f1) (Float.sub f0 f0)
      in
      eq_float64x4 ~result ~expect)

let () =
  Float64.check_floats (fun f0 f1 ->
      (failmsg := fun () -> Printf.printf "round\n");
      let fv = Float64.to_float64x4 f0 f1 f0 f1 in
      let result = round_near fv in
      let expect =
        Float64.to_float64x4 (Float64.c_round f0) (Float64.c_round f1)
          (Float64.c_round f0) (Float64.c_round f1)
      in
      eq_float64x4 ~result ~expect)

let () =
  Float64.check_floats (fun f0 f1 ->
      (failmsg
         := fun () -> Printf.printf "float64x4 -> float32x4: %f | %f\n%!" f0 f1);
      let v = Float64.to_float64x4 f0 f1 f0 f1 in
      let r = cvt_float32x4 v in
      let r0 = Int32.bits_of_float f0 in
      let r1 = Int32.bits_of_float f1 in
      let expect = Float32.to_float32x4 r0 r1 r0 r1 in
      eq_float32x4 ~result:r ~expect)

let () =
  Float32.check_floats (fun f0 f1 ->
      (failmsg
         := fun () ->
              Printf.printf "float32x4 -> float64x4: %lx | %lx\n%!" f0 f1);
      let v = Float32.to_float32x4 f0 f1 f0 f1 in
      let r = cvt_float32x4_float64x4 v in
      let r0 = Float32.to_float f0 in
      let r1 = Float32.to_float f1 in
      let expect = Float64.to_float64x4 r0 r1 r0 r1 in
      eq_float64x4 ~result:r ~expect)
