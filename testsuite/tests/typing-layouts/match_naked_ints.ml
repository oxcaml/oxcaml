(* TEST
 flags = "-extension layouts_beta -extension small_numbers_beta";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* Tests for matches over naked (untagged or unboxed) integer constants,
   which are dispatched through the tagged-integer switch machinery
   ([Matching.call_switcher_naked_integral]).  Wide scrutinees (int64#,
   nativeint#) are cast to a tagged integer behind a range test: values
   that the truncating cast could alias onto a matched constant (e.g.
   [c + 2^63] for a matched constant [c] on 64-bit targets) must reach the
   default action instead.  This file is not an expect test so that it also
   gets exercised as native code. *)

external unbox_nativeint : nativeint -> nativeint# = "%unbox_nativeint"
external unbox_int64 : int64 -> int64# = "%unbox_int64"
external unbox_int32 : int32 -> int32# = "%unbox_int32"
external untag_int8 : int8 -> int8# = "%untag_int8"
external untag_int16 : int16 -> int16# = "%untag_int16"
external char_u_of_int8_u : int8# -> char# = "%identity"

(* Dense nativeint# constants: the default must catch out-of-range values,
   including values that only differ from a matched constant by a multiple
   of 2^63. *)
let f_nativeint (x : nativeint#) =
  match x with
  | #0n -> 0
  | #1n -> 1
  | #2n -> 2
  | #3n -> 3
  | _ -> -1

(* int64# constants including negatives; four constants so that the
   range-tested switch path is used rather than the comparison tree. *)
let f_int64 (x : int64#) =
  match x with
  | -#5L -> 0
  | #0L -> 1
  | #5L -> 2
  | #10L -> 3
  | _ -> -1

(* Fewer than four int64# constants: stays on the comparison tree. *)
let f_int64_small (x : int64#) =
  match x with
  | #0L -> 0
  | #1L -> 1
  | _ -> -1

(* int64# constants not representable as tagged integers: falls back to a
   comparison tree. *)
let f_int64_wide (x : int64#) =
  match x with
  | #0x4000_0000_0000_0000L -> 0 (* 2^62 *)
  | #0L -> 1
  | _ -> -1

(* int32# extremes. *)
let f_int32 (x : int32#) =
  match x with
  | -#2147483648l -> 0
  | #0l -> 1
  | #2147483647l -> 2
  | _ -> -1

(* Sparse nativeint# constants: interval clustering must stay correct, and
   values falling in the gaps must reach the default. *)
let f_sparse (x : nativeint#) =
  match x with
  | #0n -> 0
  | #7n -> 1
  | #1000n -> 2
  | #1000000n -> 3
  | _ -> -1

let f_char (x : char#) =
  match x with
  | #'a' -> 0
  | #'b' -> 1
  | #'z' -> 2
  | #'\255' -> 3
  | _ -> -1

let f_int8 (x : int8#) =
  match x with
  | #0s -> 0
  | #1s -> 1
  | -#128s -> 2
  | #127s -> 3
  | _ -> -1

let f_int16 (x : int16#) =
  match x with
  | #0S -> 0
  | -#32768S -> 1
  | #32767S -> 2
  | _ -> -1

let () =
  assert (f_nativeint #0n = 0);
  assert (f_nativeint #3n = 3);
  assert (f_nativeint #4n = -1);
  assert (f_nativeint (-#1n) = -1);
  assert (f_nativeint (unbox_nativeint Nativeint.min_int) = -1);
  assert (f_nativeint (unbox_nativeint Nativeint.max_int) = -1);
  (* On 64-bit targets, [min_int + 2] truncates to [2] modulo 2^63: it must
     not reach the [#2n] case. *)
  assert (f_nativeint (unbox_nativeint (Nativeint.add Nativeint.min_int 2n))
          = -1);
  assert (f_int64 (-#5L) = 0);
  assert (f_int64 #0L = 1);
  assert (f_int64 #5L = 2);
  assert (f_int64 #10L = 3);
  assert (f_int64 #6L = -1);
  assert (f_int64 (unbox_int64 (Int64.add Int64.min_int 5L)) = -1);
  assert (f_int64 (unbox_int64 Int64.min_int) = -1);
  assert (f_int64 (unbox_int64 Int64.max_int) = -1);
  assert (f_int64_small #0L = 0);
  assert (f_int64_small #1L = 1);
  assert (f_int64_small #2L = -1);
  assert (f_int64_small (unbox_int64 (Int64.add Int64.min_int 1L)) = -1);
  assert (f_int64_wide #0x4000_0000_0000_0000L = 0);
  assert (f_int64_wide #0L = 1);
  assert (f_int64_wide #1L = -1);
  assert (f_int32 (-#2147483648l) = 0);
  assert (f_int32 #0l = 1);
  assert (f_int32 #2147483647l = 2);
  assert (f_int32 #7l = -1);
  assert (f_int32 (unbox_int32 Int32.min_int) = 0);
  assert (f_sparse #0n = 0);
  assert (f_sparse #7n = 1);
  assert (f_sparse #1000n = 2);
  assert (f_sparse #1000000n = 3);
  assert (f_sparse #8n = -1);
  assert (f_sparse #999999n = -1);
  assert (f_sparse (-#1000n) = -1);
  assert (f_char (char_u_of_int8_u (untag_int8 97s)) = 0);
  assert (f_char (char_u_of_int8_u (untag_int8 122s)) = 2);
  assert (f_char (char_u_of_int8_u (untag_int8 (-1s))) = 3);
  assert (f_char (char_u_of_int8_u (untag_int8 113s)) = -1);
  assert (f_int8 (untag_int8 0s) = 0);
  assert (f_int8 (untag_int8 127s) = 3);
  assert (f_int8 (untag_int8 (-128s)) = 2);
  assert (f_int8 (untag_int8 5s) = -1);
  assert (f_int16 (untag_int16 0S) = 0);
  assert (f_int16 (untag_int16 (-32768S)) = 1);
  assert (f_int16 (untag_int16 32767S) = 2);
  assert (f_int16 (untag_int16 5S) = -1)
