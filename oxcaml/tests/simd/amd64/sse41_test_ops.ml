[@@@ocaml.warning "-unused-module"]

open Utils
open Builtins

let () =
  (* Test testz - returns 1 if (a & b) == 0 *)
  let v1 = int64x2_of_int64s 0xFF00FF00FF00FF00L 0x00FF00FF00FF00FFL in
  let v2 = int64x2_of_int64s 0x00FF00FF00FF00FFL 0xFF00FF00FF00FF00L in
  let result = Sse_other_builtins.testz v1 v2 in
  if result <> 1 then Printf.printf "testz failed: expected 1, got %d\n" result

let () =
  (* Test testz - returns 0 if (a & b) != 0 *)
  let v1 = int64x2_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
  let v2 = int64x2_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
  let result = Sse_other_builtins.testz v1 v2 in
  if result <> 0 then Printf.printf "testz failed: expected 0, got %d\n" result

let () =
  (* Test testc - returns 1 if (~a & b) == 0 *)
  let v1 = int64x2_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
  let v2 = int64x2_of_int64s 0x0000000000000000L 0x0000000000000000L in
  let result = Sse_other_builtins.testc v1 v2 in
  if result <> 1 then Printf.printf "testc failed: expected 1, got %d\n" result

let () =
  (* Test testc - returns 0 if (~a & b) != 0 *)
  let v1 = int64x2_of_int64s 0x0000000000000000L 0x0000000000000000L in
  let v2 = int64x2_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
  let result = Sse_other_builtins.testc v1 v2 in
  if result <> 0 then Printf.printf "testc failed: expected 0, got %d\n" result

let () =
  (* Test testnzc - returns 1 if (a & b) != 0 AND (~a & b) != 0 *)
  let v1 = int64x2_of_int64s 0xFF00FF00FF00FF00L 0xFF00FF00FF00FF00L in
  let v2 = int64x2_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
  let result = Sse_other_builtins.testnzc v1 v2 in
  if result <> 1
  then Printf.printf "testnzc failed: expected 1, got %d\n" result

let () =
  (* Test testnzc - returns 0 otherwise *)
  let v1 = int64x2_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
  let v2 = int64x2_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
  let result = Sse_other_builtins.testnzc v1 v2 in
  if result <> 0
  then Printf.printf "testnzc failed: expected 0, got %d\n" result

let () =
  (* Another testnzc test - returns 0 when all zeros *)
  let v1 = int64x2_of_int64s 0x0000000000000000L 0x0000000000000000L in
  let v2 = int64x2_of_int64s 0x0000000000000000L 0x0000000000000000L in
  let result = Sse_other_builtins.testnzc v1 v2 in
  if result <> 0
  then Printf.printf "testnzc failed: expected 0, got %d\n" result
