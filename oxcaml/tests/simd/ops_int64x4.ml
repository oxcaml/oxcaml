open Utils256
include Builtins.Int64x4

external low_of : int64 -> t
  = "caml_vec256_unreachable" "caml_int64x4_low_of_int64"
  [@@noalloc] [@@unboxed] [@@builtin]

external low_to : t -> int64
  = "caml_vec256_unreachable" "caml_int64x4_low_to_int64"
  [@@noalloc] [@@unboxed] [@@builtin]

let () =
  let v1 = low_of 1L in
  let v2 = low_of 2L in
  let i1 = int64x4_first_int64 v1 in
  let i2 = int64x4_first_int64 v2 in
  eq i1 i2 1L 2L;
  let i1 = low_to v1 in
  let i2 = low_to v2 in
  eq i1 i2 1L 2L

let check_binop msg scalar vector i0 i1 =
  (failmsg := fun () -> Printf.printf "%s: %016Lx | %016Lx\n%!" msg i0 i1);
  let r0 = scalar i0 i1 in
  let r1 = scalar i1 i0 in
  let expect = Utils256.int64x4_of_int64s r0 r1 r0 r1 in
  let v1 = Utils256.int64x4_of_int64s i0 i1 i0 i1 in
  let v2 = Utils256.int64x4_of_int64s i1 i0 i1 i0 in
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

let () =
  Int64s.check_ints (check_binop "add" Int64.add add);
  Int64s.check_ints (check_binop "sub" Int64.sub sub);
  Int64s.check_ints
    (check_binop "cmpeq"
       (fun l r -> if Int64.equal l r then 0xffffffffffffffffL else 0L)
       cmpeq);
  Int64s.check_ints
    (check_binop "cmpgt"
       (fun l r -> if Int64.compare l r = 1 then 0xffffffffffffffffL else 0L)
       cmpgt);
  ()

(* Arbitrary test shift *)
let const_shift = 7

let to_shift x = Int64.logand x 63L

let () =
  Int64s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%016Lx << %016Lx\n%!" l r);
      let v = Utils256.int64x4_of_int64s l r l r in
      let shift = to_shift r in
      let result = sll v (int64x2_of_int64s shift 0L) in
      let expectl = Int64.shift_left l (Int64.to_int shift) in
      let expectr = Int64.shift_left r (Int64.to_int shift) in
      let expect = Utils256.int64x4_of_int64s expectl expectr expectl expectr in
      eq4
        (int64x4_first_int64 result)
        (int64x4_second_int64 result)
        (int64x4_third_int64 result)
        (int64x4_fourth_int64 result)
        (int64x4_first_int64 expect)
        (int64x4_second_int64 expect)
        (int64x4_third_int64 expect)
        (int64x4_fourth_int64 expect));
  Int64s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%016Lx >> %016Lx\n%!" l r);
      let v = Utils256.int64x4_of_int64s l r l r in
      let shift = to_shift r in
      let result = srl v (int64x2_of_int64s shift 0L) in
      let expectl = Int64.shift_right_logical l (Int64.to_int shift) in
      let expectr = Int64.shift_right_logical r (Int64.to_int shift) in
      let expect = Utils256.int64x4_of_int64s expectl expectr expectl expectr in
      eq4
        (int64x4_first_int64 result)
        (int64x4_second_int64 result)
        (int64x4_third_int64 result)
        (int64x4_fourth_int64 result)
        (int64x4_first_int64 expect)
        (int64x4_second_int64 expect)
        (int64x4_third_int64 expect)
        (int64x4_fourth_int64 expect));
  Int64s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%016Lx|%016Lx << 7\n%!" l r);
      let v = Utils256.int64x4_of_int64s l r l r in
      let result = slli const_shift v in
      let expectl = Int64.shift_left l 7 in
      let expectr = Int64.shift_left r 7 in
      let expect = Utils256.int64x4_of_int64s expectl expectr expectl expectr in
      eq4
        (int64x4_first_int64 result)
        (int64x4_second_int64 result)
        (int64x4_third_int64 result)
        (int64x4_fourth_int64 result)
        (int64x4_first_int64 expect)
        (int64x4_second_int64 expect)
        (int64x4_third_int64 expect)
        (int64x4_fourth_int64 expect));
  Int64s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%016Lx|%016Lx >> 7\n%!" l r);
      let v = Utils256.int64x4_of_int64s l r l r in
      let result = srli const_shift v in
      let expectl = Int64.shift_right_logical l 7 in
      let expectr = Int64.shift_right_logical r 7 in
      let expect = Utils256.int64x4_of_int64s expectl expectr expectl expectr in
      eq4
        (int64x4_first_int64 result)
        (int64x4_second_int64 result)
        (int64x4_third_int64 result)
        (int64x4_fourth_int64 result)
        (int64x4_first_int64 expect)
        (int64x4_second_int64 expect)
        (int64x4_third_int64 expect)
        (int64x4_fourth_int64 expect));
  Int64s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%016Lx|%016Lx sllv\n%!" l r);
      let v = Utils256.int64x4_of_int64s l r l r in
      let shifts =
        Utils256.int64x4_of_int64s (to_shift r) (Int64.logand l 63L)
          (to_shift r) (Int64.logand l 63L)
      in
      let result = sllv v shifts in
      let expectl = Int64.shift_left l (Int64.to_int (to_shift r)) in
      let expectr = Int64.shift_left r (Int64.to_int (Int64.logand l 63L)) in
      let expect = Utils256.int64x4_of_int64s expectl expectr expectl expectr in
      eq4
        (int64x4_first_int64 result)
        (int64x4_second_int64 result)
        (int64x4_third_int64 result)
        (int64x4_fourth_int64 result)
        (int64x4_first_int64 expect)
        (int64x4_second_int64 expect)
        (int64x4_third_int64 expect)
        (int64x4_fourth_int64 expect));
  Int64s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%016Lx|%016Lx srlv\n%!" l r);
      let v = Utils256.int64x4_of_int64s l r l r in
      let shifts =
        Utils256.int64x4_of_int64s (to_shift r) (Int64.logand l 63L)
          (to_shift r) (Int64.logand l 63L)
      in
      let result = srlv v shifts in
      let expectl = Int64.shift_right_logical l (Int64.to_int (to_shift r)) in
      let expectr =
        Int64.shift_right_logical r (Int64.to_int (Int64.logand l 63L))
      in
      let expect = Utils256.int64x4_of_int64s expectl expectr expectl expectr in
      eq4
        (int64x4_first_int64 result)
        (int64x4_second_int64 result)
        (int64x4_third_int64 result)
        (int64x4_fourth_int64 result)
        (int64x4_first_int64 expect)
        (int64x4_second_int64 expect)
        (int64x4_third_int64 expect)
        (int64x4_fourth_int64 expect))

(* Tests for AVX operations on int64x2 *)
let () =
  Int64s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%016Lx|%016Lx sllv_128\n%!" l r);
      let v = int64x2_of_int64s l r in
      let shifts = int64x2_of_int64s (to_shift r) (Int64.logand l 63L) in
      let result = Builtins.Int64x2.sllv v shifts in
      let expectl = Int64.shift_left l (Int64.to_int (to_shift r)) in
      let expectr = Int64.shift_left r (Int64.to_int (Int64.logand l 63L)) in
      let expect = int64x2_of_int64s expectl expectr in
      eq (int64x2_low_int64 result)
        (int64x2_high_int64 result)
        (int64x2_low_int64 expect)
        (int64x2_high_int64 expect));
  Int64s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "%016Lx|%016Lx srlv_128\n%!" l r);
      let v = int64x2_of_int64s l r in
      let shifts = int64x2_of_int64s (to_shift r) (Int64.logand l 63L) in
      let result = Builtins.Int64x2.srlv v shifts in
      let expectl = Int64.shift_right_logical l (Int64.to_int (to_shift r)) in
      let expectr =
        Int64.shift_right_logical r (Int64.to_int (Int64.logand l 63L))
      in
      let expect = int64x2_of_int64s expectl expectr in
      eq (int64x2_low_int64 result)
        (int64x2_high_int64 result)
        (int64x2_low_int64 expect)
        (int64x2_high_int64 expect))
