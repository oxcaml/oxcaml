[@@@ocaml.warning "-unused-module"]

open Utils256

module Vec256 = struct
  external testz :
    (int64x4[@unboxed]) -> (int64x4[@unboxed]) -> (int[@untagged])
    = "caml_vec256_unreachable" "caml_avx_vec256_testz"
    [@@noalloc] [@@builtin]

  external testc :
    (int64x4[@unboxed]) -> (int64x4[@unboxed]) -> (int[@untagged])
    = "caml_vec256_unreachable" "caml_avx_vec256_testc"
    [@@noalloc] [@@builtin]

  external testnzc :
    (int64x4[@unboxed]) -> (int64x4[@unboxed]) -> (int[@untagged])
    = "caml_vec256_unreachable" "caml_avx_vec256_testnzc"
    [@@noalloc] [@@builtin]
end

let () =
  (* Test testz - returns 1 if (a & b) == 0 *)
  let v1 =
    int64x4_of_int64s 0xFF00FF00FF00FF00L 0x00FF00FF00FF00FFL
      0xFF00FF00FF00FF00L 0x00FF00FF00FF00FFL
  in
  let v2 =
    int64x4_of_int64s 0x00FF00FF00FF00FFL 0xFF00FF00FF00FF00L
      0x00FF00FF00FF00FFL 0xFF00FF00FF00FF00L
  in
  let result = Vec256.testz v1 v2 in
  if result <> 1 then Printf.printf "testz failed: expected 1, got %d\n" result

let () =
  (* Test testz - returns 0 if (a & b) != 0 *)
  let v1 =
    int64x4_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL
      0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL
  in
  let v2 =
    int64x4_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL
      0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL
  in
  let result = Vec256.testz v1 v2 in
  if result <> 0 then Printf.printf "testz failed: expected 0, got %d\n" result

let () =
  (* Test testc - returns 1 if (~a & b) == 0 *)
  let v1 =
    int64x4_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL
      0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL
  in
  let v2 =
    int64x4_of_int64s 0x0000000000000000L 0x0000000000000000L
      0x0000000000000000L 0x0000000000000000L
  in
  let result = Vec256.testc v1 v2 in
  if result <> 1 then Printf.printf "testc failed: expected 1, got %d\n" result

let () =
  (* Test testc - returns 0 if (~a & b) != 0 *)
  let v1 =
    int64x4_of_int64s 0x0000000000000000L 0x0000000000000000L
      0x0000000000000000L 0x0000000000000000L
  in
  let v2 =
    int64x4_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL
      0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL
  in
  let result = Vec256.testc v1 v2 in
  if result <> 0 then Printf.printf "testc failed: expected 0, got %d\n" result

let () =
  (* Test testnzc - returns 1 if (a & b) != 0 AND (~a & b) != 0 *)
  let v1 =
    int64x4_of_int64s 0xFF00FF00FF00FF00L 0xFF00FF00FF00FF00L
      0xFF00FF00FF00FF00L 0xFF00FF00FF00FF00L
  in
  let v2 =
    int64x4_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL
      0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL
  in
  let result = Vec256.testnzc v1 v2 in
  if result <> 1
  then Printf.printf "testnzc failed: expected 1, got %d\n" result

let () =
  (* Test testnzc - returns 0 otherwise *)
  let v1 =
    int64x4_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL
      0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL
  in
  let v2 =
    int64x4_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL
      0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL
  in
  let result = Vec256.testnzc v1 v2 in
  if result <> 0
  then Printf.printf "testnzc failed: expected 0, got %d\n" result

let () =
  (* Another testnzc test - returns 0 when all zeros *)
  let v1 =
    int64x4_of_int64s 0x0000000000000000L 0x0000000000000000L
      0x0000000000000000L 0x0000000000000000L
  in
  let v2 =
    int64x4_of_int64s 0x0000000000000000L 0x0000000000000000L
      0x0000000000000000L 0x0000000000000000L
  in
  let result = Vec256.testnzc v1 v2 in
  if result <> 0
  then Printf.printf "testnzc failed: expected 0, got %d\n" result
