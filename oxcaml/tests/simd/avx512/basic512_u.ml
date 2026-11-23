open Stdlib

(* !!!

Should be kept in sync with basic512.ml.
CR-someday mslater: with layout polymorphism, the tests could be functorized.

!!! *)

[@@@ocaml.warning "-unused-type-declaration"]

external box_float : float# -> float = "%box_float"

external box_int64x8 : int64x8# -> int64x8 = "%box_vec512"
external unbox_int64x8 : int64x8 -> int64x8# = "%unbox_vec512"

external int64x8_of_int64s :
  int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64x8
  = "" "vec512_of_int64s"
[@@noalloc] [@@unboxed]

external int64x8_w0 : int64x8 -> int64 = "" "vec512_w0" [@@noalloc] [@@unboxed]
external int64x8_w1 : int64x8 -> int64 = "" "vec512_w1" [@@noalloc] [@@unboxed]
external int64x8_w2 : int64x8 -> int64 = "" "vec512_w2" [@@noalloc] [@@unboxed]
external int64x8_w3 : int64x8 -> int64 = "" "vec512_w3" [@@noalloc] [@@unboxed]
external int64x8_w4 : int64x8 -> int64 = "" "vec512_w4" [@@noalloc] [@@unboxed]
external int64x8_w5 : int64x8 -> int64 = "" "vec512_w5" [@@noalloc] [@@unboxed]
external int64x8_w6 : int64x8 -> int64 = "" "vec512_w6" [@@noalloc] [@@unboxed]
external int64x8_w7 : int64x8 -> int64 = "" "vec512_w7" [@@noalloc] [@@unboxed]

let eq l r = if l <> r then Printf.printf "%Ld <> %Ld\n" l r

let[@inline never] check v a b c d e f g h =
  eq (int64x8_w0 v) a;
  eq (int64x8_w1 v) b;
  eq (int64x8_w2 v) c;
  eq (int64x8_w3 v) d;
  eq (int64x8_w4 v) e;
  eq (int64x8_w5 v) f;
  eq (int64x8_w6 v) g;
  eq (int64x8_w7 v) h

(* Unbox/Box *)
let () =
  let[@inline never] opaque_identity v = v in
  let v = unbox_int64x8 (int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L) in
  let v = opaque_identity v in
  let v = box_int64x8 v in
  check v 1L 2L 3L 4L 5L 6L 7L 8L

(* Unboxed *)

type nonrec int8x64 = int8x64#
type nonrec int16x32 = int16x32#
type nonrec int32x16 = int32x16#
type nonrec int64x8 = int64x8#
type nonrec float32x16 = float32x16#
type nonrec float64x8 = float64x8#

external int64x8_of_int64s :
  int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64x8
  = "" "vec512_of_int64s"
[@@noalloc] [@@unboxed]

external int64x8_w0 : int64x8 -> int64 = "" "vec512_w0" [@@noalloc] [@@unboxed]
external int64x8_w1 : int64x8 -> int64 = "" "vec512_w1" [@@noalloc] [@@unboxed]
external int64x8_w2 : int64x8 -> int64 = "" "vec512_w2" [@@noalloc] [@@unboxed]
external int64x8_w3 : int64x8 -> int64 = "" "vec512_w3" [@@noalloc] [@@unboxed]
external int64x8_w4 : int64x8 -> int64 = "" "vec512_w4" [@@noalloc] [@@unboxed]
external int64x8_w5 : int64x8 -> int64 = "" "vec512_w5" [@@noalloc] [@@unboxed]
external int64x8_w6 : int64x8 -> int64 = "" "vec512_w6" [@@noalloc] [@@unboxed]
external int64x8_w7 : int64x8 -> int64 = "" "vec512_w7" [@@noalloc] [@@unboxed]

let eq l r = if l <> r then Printf.printf "%Ld <> %Ld\n" l r

let[@inline never] check v a b c d e f g h =
  eq (int64x8_w0 v) a;
  eq (int64x8_w1 v) b;
  eq (int64x8_w2 v) c;
  eq (int64x8_w3 v) d;
  eq (int64x8_w4 v) e;
  eq (int64x8_w5 v) f;
  eq (int64x8_w6 v) g;
  eq (int64x8_w7 v) h

(* Box/Unbox *)
let () =
  let v = box_int64x8 (int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L) in
  let v = Sys.opaque_identity v in
  let v = unbox_int64x8 v in
  check v 1L 2L 3L 4L 5L 6L 7L 8L

let[@inline never] combine v0 v1 =
  let add f = Int64.add (f v0) (f v1) in
  int64x8_of_int64s (add int64x8_w0) (add int64x8_w1) (add int64x8_w2)
    (add int64x8_w3) (add int64x8_w4) (add int64x8_w5) (add int64x8_w6)
    (add int64x8_w7)

let[@inline never] combine_with_floats v0 f0 v1 f1 =
  let add f = Int64.add (f v0) (f v1) in
  let w0 = Int64.add (Int64.of_float f0) (add int64x8_w0) in
  let w1 = Int64.add (Int64.of_float f1) (add int64x8_w1) in
  int64x8_of_int64s w0 w1 (add int64x8_w2) (add int64x8_w3) (add int64x8_w4)
    (add int64x8_w5) (add int64x8_w6) (add int64x8_w7)

let mk n =
  int64x8_of_int64s n (Int64.add n 1L) (Int64.add n 2L) (Int64.add n 3L)
    (Int64.add n 4L) (Int64.add n 5L) (Int64.add n 6L) (Int64.add n 7L)

(* Identity *)
let () =
  let v = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let v = Sys.opaque_identity v in
  check v 1L 2L 3L 4L 5L 6L 7L 8L

(* Identity fn *)
let () =
  let v = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let[@inline never] id v = v in
  let v = id v in
  check v 1L 2L 3L 4L 5L 6L 7L 8L

(* Pass to function *)
let () =
  let v0 = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let v1 = int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L in
  let v = combine v0 v1 in
  check v 10L 12L 14L 16L 18L 20L 22L 24L

(* Pass to function (inlined) *)
let () =
  let v0 = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let v1 = int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L in
  let v = (combine [@inlined hint]) v0 v1 in
  check v 10L 12L 14L 16L 18L 20L 22L 24L

(* Pass to function with floats *)
let () =
  let v0 = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let v1 = int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L in
  let f0 = Sys.opaque_identity 9. in
  let v = combine_with_floats v0 f0 v1 10. in
  check v 19L 22L 14L 16L 18L 20L 22L 24L

(* Pass to function with floats (inlined) *)
let () =
  let v0 = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let v1 = int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L in
  let v = (combine_with_floats [@inlined hint]) v0 9. v1 10. in
  check v 19L 22L 14L 16L 18L 20L 22L 24L

(* Capture in closure *)
let () =
  let v0 = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let v1 = int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L in
  let f = combine v0 in
  let f = Sys.opaque_identity f in
  let v = f v1 in
  check v 10L 12L 14L 16L 18L 20L 22L 24L

(* Capture vectors and floats in a closure *)
let () =
  let[@inline never] f v0 v1 f0 v2 f1 v3 =
    combine (combine_with_floats v0 f0 v1 f1) (combine v2 v3)
  in
  let v0 = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let v1 = int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L in
  let v2 = int64x8_of_int64s 17L 18L 19L 20L 21L 22L 23L 24L in
  let v3 = int64x8_of_int64s 25L 26L 27L 28L 29L 30L 31L 32L in
  let f = f v0 v1 17. v2 in
  let f = Sys.opaque_identity f in
  let v = f 18. v3 in
  check v 69L 74L 60L 64L 68L 72L 76L 80L

(* Capture vectors and floats in a closure (inlined) *)
let () =
  let[@inline always] f v0 v1 f0 v2 f1 v3 =
    (combine [@inlined hint])
      ((combine_with_floats [@inlined hint]) v0 f0 v1 f1)
      ((combine [@inlined hint]) v2 v3)
  in
  let v0 = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let v1 = int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L in
  let v2 = int64x8_of_int64s 17L 18L 19L 20L 21L 22L 23L 24L in
  let v3 = int64x8_of_int64s 25L 26L 27L 28L 29L 30L 31L 32L in
  let f = f v0 v1 17. v2 in
  let v = f 18. v3 in
  check v 69L 74L 60L 64L 68L 72L 76L 80L

(* Store in module *)
module type M = sig
  val prefix : string
  val a : int64x8
  val b : float#
  val c : int64x8
end

module M : M = struct
  let prefix = "prefix"
  let a = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L
  let b = #9.
  let c = int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L
end

let () =
  check M.a 1L 2L 3L 4L 5L 6L 7L 8L;
  check M.c 9L 10L 11L 12L 13L 14L 15L 16L;
  assert (M.prefix = "prefix");
  let (module M) = Sys.opaque_identity (module M : M) in
  check M.a 1L 2L 3L 4L 5L 6L 7L 8L;
  check M.c 9L 10L 11L 12L 13L 14L 15L 16L;
  assert (M.prefix = "prefix");
  let v = combine_with_floats M.a (box_float M.b) M.c 14. in
  check v 19L 26L 14L 16L 18L 20L 22L 24L

(* Store in record *)
type record =
  { prefix : string;
    a : int64x8;
    mutable b : int64x8;
    c : float#
  }

let () =
  let record =
    { prefix = "prefix";
      a = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L;
      b = int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L;
      c = #9.
    }
  in
  check record.a 1L 2L 3L 4L 5L 6L 7L 8L;
  check record.b 9L 10L 11L 12L 13L 14L 15L 16L;
  assert (record.prefix = "prefix");
  let record = Sys.opaque_identity record in
  record.b <- int64x8_of_int64s 17L 18L 19L 20L 21L 22L 23L 24L;
  check record.a 1L 2L 3L 4L 5L 6L 7L 8L;
  check record.b 17L 18L 19L 20L 21L 22L 23L 24L;
  assert (record.prefix = "prefix");
  let v = combine_with_floats record.a (box_float record.c) record.b 14. in
  check v 27L 34L 22L 24L 26L 28L 30L 32L

type record2 =
  { imm0 : int;
    str1 : string;
    a : int64x8
  }

let copy_via_weak x =
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some x);
  Weak.get_copy weak 0 |> Option.get

let copy_via_tag x =
  let obj = Obj.repr x in
  Obj.with_tag (Obj.tag obj) obj |> Obj.obj

let () =
  let record =
    { imm0 = 0; str1 = ""; a = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L }
  in
  check record.a 1L 2L 3L 4L 5L 6L 7L 8L;
  assert (record.str1 = "" && record.imm0 = 0);
  let record = { record with imm0 = 5 } in
  check record.a 1L 2L 3L 4L 5L 6L 7L 8L;
  assert (record.str1 = "" && record.imm0 = 5);
  let record = copy_via_weak record in
  check record.a 1L 2L 3L 4L 5L 6L 7L 8L;
  assert (record.str1 = "" && record.imm0 = 5);
  let record = copy_via_tag record in
  check record.a 1L 2L 3L 4L 5L 6L 7L 8L;
  assert (record.str1 = "" && record.imm0 = 5)

(* Store in variant *)
type variant =
  | A of int64x8
  | B of int64x8
  | C of float

let () =
  let variant = A (int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L) in
  let variant = Sys.opaque_identity variant in
  (match variant with
  | A v -> check v 1L 2L 3L 4L 5L 6L 7L 8L
  | _ -> print_endline "fail");
  let variant = ref (C 5.) in
  let variant = Sys.opaque_identity variant in
  variant := B (int64x8_of_int64s 9L 10L 11L 12L 13L 14L 15L 16L);
  match !variant with
  | B v -> check v 9L 10L 11L 12L 13L 14L 15L 16L
  | _ -> print_endline "fail"

(* Pass lots of vectors to an external *)
external lots_of_vectors :
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 = "" "lots_of_vectors512"
[@@noalloc] [@@unboxed]

let () =
  let v0 = mk 1L in
  let v1 = mk 9L in
  let v2 = mk 17L in
  let v3 = mk 25L in
  let v4 = mk 33L in
  let v5 = mk 41L in
  let v6 = mk 49L in
  let v7 = mk 57L in
  let v8 = mk 65L in
  let v9 = mk 73L in
  let v10 = mk 81L in
  let v11 = mk 89L in
  let v12 = mk 97L in
  let v13 = mk 105L in
  let v14 = mk 113L in
  let v15 = mk 121L in
  let v =
    lots_of_vectors v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
  in
  check v 976L 992L 1008L 1024L 1040L 1056L 1072L 1088L

(* Pass mixed floats/vectors to an external *)
external vectors_and_floats :
  int64x8 ->
  float ->
  int64x8 ->
  float ->
  int64x8 ->
  float ->
  int64x8 ->
  float ->
  float ->
  int64x8 ->
  int64x8 ->
  float ->
  float ->
  int64x8 ->
  int64x8 ->
  float ->
  float ->
  float ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  float ->
  float ->
  float ->
  int64x8 = "" "vectors_and_floats512"
[@@noalloc] [@@unboxed]

let () =
  let v0 = mk 1L in
  let v1 = mk 9L in
  let v2 = mk 17L in
  let v3 = mk 25L in
  let v4 = mk 33L in
  let v5 = mk 41L in
  let v6 = mk 49L in
  let v7 = mk 57L in
  let v8 = mk 65L in
  let v9 = mk 73L in
  let v10 = mk 81L in
  let v =
    vectors_and_floats v0 23. v1 24. v2 25. v3 26. 27. v4 v5 28. 29. v6 v7 30.
      31. 32. v8 v9 v10 33. 34. 35.
  in
  check v 377L 913L 473L 484L 495L 506L 517L 528L

(* Pass mixed ints/floats/vectors to an external *)
external vectors_and_floats_and_ints :
  int64x8 ->
  float ->
  int64x8 ->
  int64 ->
  int64x8 ->
  float ->
  int64x8 ->
  int64 ->
  int64 ->
  int64x8 ->
  int64x8 ->
  float ->
  float ->
  int64x8 ->
  int64x8 ->
  int64 ->
  int64 ->
  float ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64 ->
  int64 ->
  float ->
  int64x8 = "" "vectors_and_floats_and_ints512"
[@@noalloc] [@@unboxed]

let () =
  let v0 = mk 1L in
  let v1 = mk 9L in
  let v2 = mk 17L in
  let v3 = mk 25L in
  let v4 = mk 33L in
  let v5 = mk 41L in
  let v6 = mk 49L in
  let v7 = mk 57L in
  let v8 = mk 65L in
  let v9 = mk 73L in
  let v10 = mk 81L in
  let v =
    vectors_and_floats_and_ints v0 23. v1 24L v2 25. v3 26L 27L v4 v5 28. 29. v6
      v7 30L 31L 32. v8 v9 v10 33L 34L 35.
  in
  check v 377L 913L 473L 484L 495L 506L 517L 528L

(* Pass vector reg and then stack floats *)
external vector_and_then_stack_floats :
  int64x8 ->
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  float = "" "vector_and_then_stack_floats512"
[@@noalloc] [@@unboxed]

let () =
  let v0 = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L in
  let v = vector_and_then_stack_floats v0 1. 2. 3. 4. 5. 6. 7. 8. in
  assert (v = 36.)

(* Masks *)

external box_mask : mask# -> mask = "%box_mask"
external unbox_mask : mask -> mask# = "%unbox_mask"

external mask_of_int64 : int64 -> mask = "" "mask_ret" [@@noalloc] [@@unboxed]
external int64_of_mask : mask -> int64 = "" "mask_bits" [@@noalloc] [@@unboxed]
external bytes_get_mask : bytes -> int -> mask = "%caml_bytes_getmask"
external bytes_set_mask : bytes -> int -> mask -> unit = "%caml_bytes_setmask"

(* Unbox/Box *)
let () =
  let[@inline never] opaque_identity m = m in
  let m = unbox_mask (mask_of_int64 0xAAL) in
  let m = opaque_identity m in
  let m = box_mask m in
  eq (int64_of_mask m) 0xAAL

(* Load from and store to bytes *)
let () =
  let bytes = Bytes.make 16 '\x00' in
  bytes_set_mask bytes 3 (mask_of_int64 0x0102030405060708L);
  eq (Bytes.get_int64_le bytes 3) 0x0102030405060708L;
  eq (int64_of_mask (bytes_get_mask bytes 3)) 0x0102030405060708L;
  eq (int64_of_mask (bytes_get_mask bytes 8)) 0x0000000000010203L

(* Unboxed *)

type nonrec mask = mask#

external mask_of_int64 : int64 -> mask = "" "mask_ret" [@@noalloc] [@@unboxed]
external int64_of_mask : mask -> int64 = "" "mask_bits" [@@noalloc] [@@unboxed]
external bytes_get_mask : bytes -> int -> mask = "%caml_bytes_getmask#"
external bytes_set_mask : bytes -> int -> mask -> unit = "%caml_bytes_setmask#"

let[@inline never] check_mask m bits = eq (int64_of_mask m) bits

(* Box/Unbox *)
let () =
  let m = box_mask (mask_of_int64 0x1234L) in
  let m = Sys.opaque_identity m in
  let m = unbox_mask m in
  check_mask m 0x1234L

(* Identity *)
let () =
  let m = mask_of_int64 0xAAL in
  let m = Sys.opaque_identity m in
  check_mask m 0xAAL

(* Identity fn *)
let () =
  let[@inline never] id (m : mask) = m in
  let m = id (mask_of_int64 0xFEDCBA9876543210L) in
  check_mask m 0xFEDCBA9876543210L

(* Pass to function *)
let[@inline never] pick c (m0 : mask) (m1 : mask) = if c then m0 else m1

let () =
  let m0 = mask_of_int64 0xF0L in
  let m1 = mask_of_int64 0x0FL in
  check_mask (pick (Sys.opaque_identity true) m0 m1) 0xF0L;
  check_mask (pick (Sys.opaque_identity false) m0 m1) 0x0FL

(* Capture in closure *)
let () =
  let m = mask_of_int64 0x55L in
  let f () = int64_of_mask m in
  let f = Sys.opaque_identity f in
  eq (f ()) 0x55L

(* Store in module *)
module type Mask_m = sig
  val prefix : string
  val m : mask
  val v : int64x8
end

module Mask_m : Mask_m = struct
  let prefix = "prefix"
  let m = mask_of_int64 0xABCDL
  let v = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L
end

let () =
  check_mask Mask_m.m 0xABCDL;
  let (module M) = Sys.opaque_identity (module Mask_m : Mask_m) in
  check_mask M.m 0xABCDL;
  check M.v 1L 2L 3L 4L 5L 6L 7L 8L;
  assert (M.prefix = "prefix")

(* Store in record *)
type mask_record =
  { prefix : string;
    mutable m : mask;
    v : int64x8
  }

let () =
  let record =
    { prefix = "prefix";
      m = mask_of_int64 0x0102L;
      v = int64x8_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L
    }
  in
  check_mask record.m 0x0102L;
  let record = Sys.opaque_identity record in
  record.m <- mask_of_int64 0x0304L;
  check_mask record.m 0x0304L;
  check record.v 1L 2L 3L 4L 5L 6L 7L 8L;
  assert (record.prefix = "prefix");
  let record = copy_via_weak record in
  check_mask record.m 0x0304L;
  check record.v 1L 2L 3L 4L 5L 6L 7L 8L;
  let record = copy_via_tag record in
  check_mask record.m 0x0304L;
  check record.v 1L 2L 3L 4L 5L 6L 7L 8L;
  assert (record.prefix = "prefix")

(* Store in variant *)
type mask_variant =
  | M of mask
  | I of int64

let () =
  (match Sys.opaque_identity (M (mask_of_int64 0xABCDL)) with
  | M m -> check_mask m 0xABCDL
  | I _ -> print_endline "fail");
  match Sys.opaque_identity (I 0x1234L) with
  | I i -> eq i 0x1234L
  | M _ -> print_endline "fail"

(* Store in array *)
external array_get : mask array -> int -> mask = "%array_safe_get"
external array_set : mask array -> int -> mask -> unit = "%array_safe_set"

let () =
  let[@inline never] make bits =
    [| mask_of_int64 bits; mask_of_int64 0x55L; mask_of_int64 0xF0L |]
  in
  let array = make 0xAAL in
  check_mask (array_get array 0) 0xAAL;
  check_mask (array_get array 1) 0x55L;
  check_mask (array_get array 2) 0xF0L;
  array_set array 1 (mask_of_int64 0x1234L);
  check_mask (array_get array 0) 0xAAL;
  check_mask (array_get array 1) 0x1234L;
  check_mask (array_get array 2) 0xF0L

(* Load from and store to bytes *)
let () =
  let bytes = Bytes.make 16 '\x00' in
  bytes_set_mask bytes 3 (mask_of_int64 0x0102030405060708L);
  eq (Bytes.get_int64_le bytes 3) 0x0102030405060708L;
  check_mask (bytes_get_mask bytes 3) 0x0102030405060708L;
  check_mask (bytes_get_mask bytes 8) 0x0000000000010203L

(* Pass a mask on the stack to an external: the eight preceding arguments
   exhaust the C integer argument registers. *)
external mask_after_ints :
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  mask ->
  int64 = "" "mask_after_ints"
[@@noalloc] [@@unboxed]

let () =
  let m = Sys.opaque_identity (mask_of_int64 0x5AL) in
  eq (mask_after_ints 1L 2L 3L 4L 5L 6L 7L 8L m) 0x7EL

(* Pass and return unboxed products containing masks *)
external box_int64 : int64_u -> int64 = "%box_int64"

external ret_mask_i64 : int64_u -> int64_u -> #(mask * int64_u)
  = "" "extcall512_ret_mask_i64"
[@@noalloc]

external sum_i64_mask : #(int64_u * mask) -> int64_u
  = "" "extcall512_sum_i64_mask"
[@@noalloc]

let () =
  let #(m, i) = ret_mask_i64 #0x5aL #77L in
  check_mask m 0x5aL;
  eq (box_int64 i) 77L;
  (* 10 + 2 * 0x5a *)
  eq (box_int64 (sum_i64_mask #(#10L, m))) 190L
