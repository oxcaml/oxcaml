open Utils
include Builtins.Int64x2

(* Creation / Destruction *)

external low_of : int64 -> t
  = "caml_vec128_unreachable" "caml_int64x2_low_of_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external low_to : t -> int64
  = "caml_vec128_unreachable" "caml_int64x2_low_to_int64"
[@@noalloc] [@@unboxed] [@@builtin]

let () =
  let v1 = low_of 1L in
  let v2 = low_of 2L in
  let i1 = int64x2_low_int64 v1 in
  let i2 = int64x2_low_int64 v2 in
  eq i1 i2 1L 2L;
  let i1 = low_to v1 in
  let i2 = low_to v2 in
  eq i1 i2 1L 2L

(* Math *)

let check_binop scalar vector i0 i1 =
  (failmsg := fun () -> Printf.printf "%016Lx | %016Lx\n%!" i0 i1);
  let r0 = scalar i0 i1 in
  let r1 = scalar i1 i0 in
  let expect = int64x2_of_int64s r0 r1 in
  let v1 = int64x2_of_int64s i0 i1 in
  let v2 = int64x2_of_int64s i1 i0 in
  let result = vector v1 v2 in
  eq (int64x2_low_int64 result)
    (int64x2_high_int64 result)
    (int64x2_low_int64 expect)
    (int64x2_high_int64 expect)

let () =
  Int64s.check_ints (check_binop Int64.add add);
  Int64s.check_ints (check_binop Int64.sub sub);
  Int64s.check_ints
    (check_binop
       (fun l r -> if Int64.equal l r then 0xffffffffffffffffL else 0L)
       cmpeq);
  Int64s.check_ints
    (check_binop
       (fun l r -> if Int64.compare l r = 1 then 0xffffffffffffffffL else 0L)
       cmpgt);
  Int64s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "sll: %016Lx << %016Lx\n%!" l r);
      let v = int64x2_of_int64s l r in
      let shift = Int64.logand r 0x3fL in
      let result = sll v (int64x2_of_int64s shift 0L) in
      let expectl = Int64.shift_left l (Int64.to_int shift) in
      let expectr = Int64.shift_left r (Int64.to_int shift) in
      eq (int64x2_low_int64 result) (int64x2_high_int64 result) expectl expectr);
  Int64s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "srl: %016Lx >> %016Lx\n%!" l r);
      let v = int64x2_of_int64s l r in
      let shift = Int64.logand r 0x3fL in
      let result = srl v (int64x2_of_int64s shift 0L) in
      let expectl = Int64.shift_right_logical l (Int64.to_int shift) in
      let expectr = Int64.shift_right_logical r (Int64.to_int shift) in
      eq (int64x2_low_int64 result) (int64x2_high_int64 result) expectl expectr);
  Int64s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "slli: %016Lx|%016Lx << 7\n%!" l r);
      let v = int64x2_of_int64s l r in
      let result = slli 7 v in
      let expectl = Int64.shift_left l 7 in
      let expectr = Int64.shift_left r 7 in
      eq (int64x2_low_int64 result) (int64x2_high_int64 result) expectl expectr);
  Int64s.check_ints (fun l r ->
      (failmsg := fun () -> Printf.printf "srli: %016Lx|%016Lx >> 7\n%!" l r);
      let v = int64x2_of_int64s l r in
      let result = srli 7 v in
      let expectl = Int64.shift_right_logical l 7 in
      let expectr = Int64.shift_right_logical r 7 in
      eq (int64x2_low_int64 result) (int64x2_high_int64 result) expectl expectr)

let () =
  let v0 = low_of 0L in
  let v1 = insert 0 v0 1L in
  let v2 = insert 1 v1 2L in
  let i0 = extract 0 v0 in
  let i1 = extract 0 v1 in
  let i2 = extract 0 v2 in
  let i3 = extract 1 v2 in
  eq i0 i1 0L 1L;
  eq i2 i3 1L 2L
