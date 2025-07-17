[@@@ocaml.warning "-unused-module"]

open Utils

module Vec128 = struct
  external testz : int64x2 -> int64x2 -> int
    = "caml_vec128_unreachable" "caml_sse41_vec128_testz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external testc : int64x2 -> int64x2 -> int
    = "caml_vec128_unreachable" "caml_sse41_vec128_testc"
    [@@noalloc] [@@unboxed] [@@builtin]

  external testnzc : int64x2 -> int64x2 -> int
    = "caml_vec128_unreachable" "caml_sse41_vec128_testnzc"
    [@@noalloc] [@@unboxed] [@@builtin]
end

let () =
  (* Test testz - returns 1 if (a & b) == 0 *)
  let v1 = Int64x2.of_int64s 0xFF00FF00FF00FF00L 0x00FF00FF00FF00FFL in
  let v2 = Int64x2.of_int64s 0x00FF00FF00FF00FFL 0xFF00FF00FF00FF00L in
  let result = Vec128.testz v1 v2 in
  if result <> 1 then Printf.printf "testz failed: expected 1, got %d\n" result

let () =
  (* Test testz - returns 0 if (a & b) != 0 *)
  let v1 = Int64x2.of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
  let v2 = Int64x2.of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
  let result = Vec128.testz v1 v2 in
  if result <> 0 then Printf.printf "testz failed: expected 0, got %d\n" result

let () =
  (* Test testc - returns 1 if (~a & b) == 0 *)
  let v1 = Int64x2.of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
  let v2 = Int64x2.of_int64s 0x0000000000000000L 0x0000000000000000L in
  let result = Vec128.testc v1 v2 in
  if result <> 1 then Printf.printf "testc failed: expected 1, got %d\n" result

let () =
  (* Test testc - returns 0 if (~a & b) != 0 *)
  let v1 = Int64x2.of_int64s 0x0000000000000000L 0x0000000000000000L in
  let v2 = Int64x2.of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
  let result = Vec128.testc v1 v2 in
  if result <> 0 then Printf.printf "testc failed: expected 0, got %d\n" result

let () =
  (* Test testnzc - returns 1 if (a & b) != 0 AND (~a & b) != 0 *)
  let v1 = Int64x2.of_int64s 0xFF00FF00FF00FF00L 0xFF00FF00FF00FF00L in
  let v2 = Int64x2.of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
  let result = Vec128.testnzc v1 v2 in
  if result <> 1
  then Printf.printf "testnzc failed: expected 1, got %d\n" result

let () =
  (* Test testnzc - returns 0 otherwise *)
  let v1 = Int64x2.of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
  let v2 = Int64x2.of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
  let result = Vec128.testnzc v1 v2 in
  if result <> 0
  then Printf.printf "testnzc failed: expected 0, got %d\n" result

let () =
  (* Another testnzc test - returns 0 when all zeros *)
  let v1 = Int64x2.of_int64s 0x0000000000000000L 0x0000000000000000L in
  let v2 = Int64x2.of_int64s 0x0000000000000000L 0x0000000000000000L in
  let result = Vec128.testnzc v1 v2 in
  if result <> 0
  then Printf.printf "testnzc failed: expected 0, got %d\n" result

let () =
  (* Test with different vector types *)
  let f1 = Float32x4.of_float32s 1.0s 2.0s 3.0s 4.0s in
  let f2 = Float32x4.of_float32s 0.0s 0.0s 0.0s 0.0s in
  let i1 = (Obj.magic f1 : int64x2) in
  let i2 = (Obj.magic f2 : int64x2) in
  let result = Vec128.testz i1 i2 in
  if result <> 1
  then
    Printf.printf "testz with float vectors failed: expected 1, got %d\n" result
