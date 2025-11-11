(* Test various logical immediate encoding patterns for ARM64 binary emitter.

   ARM64 logical immediates use a complex bitmask encoding where the immediate
   must be a repeating pattern of 2, 4, 8, 16, 32, or 64 bit elements.
   Each element has consecutive 1s that may be rotated.

   The encoding uses three fields:
   - N: 1 for 64-bit element size, 0 otherwise
   - immr: right rotation amount
   - imms: encodes element size and number of 1s

   Key patterns to test:
   1. Simple consecutive 1s from LSB (no rotation needed)
   2. Rotated patterns (consecutive 1s not at LSB)
   3. Wrapping patterns (1s at both ends, 0s in middle) - these are
      consecutive 1s that have been rotated to wrap around
   4. Repeated element patterns (same pattern in each element)
   5. Near-boundary patterns (testing edge cases)
*)

(* Unboxed int64 operations - copied from Int64_u to avoid module dependency *)
external box_int64 : int64# -> (int64[@local_opt]) = "%box_int64"
external unbox_int64 : (int64[@local_opt]) -> int64# = "%unbox_int64"
let[@inline always] logor x y =
  unbox_int64 (Int64.logor (box_int64 x) (box_int64 y))
let[@inline always] logand x y =
  unbox_int64 (Int64.logand (box_int64 x) (box_int64 y))
let[@inline always] logxor x y =
  unbox_int64 (Int64.logxor (box_int64 x) (box_int64 y))

(* === Simple consecutive 1s from LSB ===
   These are the basic patterns: N consecutive 1s starting at bit 0.
   immr=0, imms encodes the count of 1s minus 1.
   Example: 0x7 = 0b111 has 3 consecutive 1s, imms would encode 2 *)
let[@inline never] test_or_1 x = logor x #1L              (* 1 bit *)
let[@inline never] test_or_3 x = logor x #3L              (* 2 bits *)
let[@inline never] test_or_7 x = logor x #7L              (* 3 bits *)
let[@inline never] test_or_f x = logor x #0xfL            (* 4 bits *)
let[@inline never] test_or_1f x = logor x #0x1fL          (* 5 bits *)
let[@inline never] test_or_3f x = logor x #0x3fL          (* 6 bits *)
let[@inline never] test_or_7f x = logor x #0x7fL          (* 7 bits *)
let[@inline never] test_or_ff x = logor x #0xffL          (* 8 bits *)
let[@inline never] test_or_1ff x = logor x #0x1ffL        (* 9 bits *)
let[@inline never] test_or_3ff x = logor x #0x3ffL        (* 10 bits *)
let[@inline never] test_or_fff x = logor x #0xfffL        (* 12 bits *)
let[@inline never] test_or_ffff x = logor x #0xffffL      (* 16 bits *)
let[@inline never] test_or_fffff x = logor x #0xfffffL    (* 20 bits *)
let[@inline never] test_or_ffffff x = logor x #0xffffffL  (* 24 bits *)
let[@inline never] test_or_fffffff x = logor x #0xfffffffL  (* 28 bits *)
let[@inline never] test_or_ffffffff x = logor x #0xffffffffL  (* 32 bits *)

(* === Single bit patterns (power of 2) ===
   A single 1 bit at various positions. These are 1-bit patterns rotated.
   immr encodes the rotation, imms=0 (for 1 bit in 64-bit element) *)
let[@inline never] test_or_2 x = logor x #0x2L            (* bit 1 *)
let[@inline never] test_or_4 x = logor x #0x4L            (* bit 2 *)
let[@inline never] test_or_8 x = logor x #0x8L            (* bit 3 *)
let[@inline never] test_or_10 x = logor x #0x10L          (* bit 4 *)
let[@inline never] test_or_100 x = logor x #0x100L        (* bit 8 *)
let[@inline never] test_or_1000 x = logor x #0x1000L      (* bit 12 *)
let[@inline never] test_or_10000 x = logor x #0x10000L    (* bit 16 *)
let[@inline never] test_or_100000000 x = logor x #0x100000000L  (* bit 32 *)

(* === Rotated consecutive bit patterns ===
   Multiple consecutive 1s that don't start at bit 0.
   These require non-zero immr to encode the rotation. *)
let[@inline never] test_or_6 x = logor x #0x6L           (* bits 1-2: 0b110 *)
let[@inline never] test_or_1e x = logor x #0x1eL         (* bits 1-4: 0b11110 *)
let[@inline never] test_or_7e0 x = logor x #0x7e0L       (* bits 5-10 *)
let[@inline never] test_or_ff00 x = logor x #0xff00L     (* bits 8-15 *)
let[@inline never] test_or_ff0000 x = logor x #0xff0000L (* bits 16-23 *)
let[@inline never] test_or_ff000000 x = logor x #0xff000000L  (* bits 24-31 *)

(* === Wrapping patterns (1+0+1* type) ===
   These have 1s at both ends with 0s in the middle. They're actually
   consecutive 1s that have been rotated to wrap around the word.
   Example: -255 = 0xffffffffffffff01 has 1 at bit 0, then 0s at bits 1-7,
   then 1s at bits 8-63. This is 57 consecutive 1s rotated right by 8.
   These patterns caused bugs in early implementations! *)
let[@inline never] test_or_neg3 x = logor x (-#3L)
  (* 0xfffffffffffffffd: 0 at bit 1 only - 63 ones rotated *)
let[@inline never] test_or_neg5 x = logor x (-#5L)
  (* 0xfffffffffffffffb: 0 at bit 2 only *)
let[@inline never] test_or_neg9 x = logor x (-#9L)
  (* 0xfffffffffffffff7: 0 at bit 3 only *)
let[@inline never] test_or_neg17 x = logor x (-#17L)
  (* 0xffffffffffffffef: 0 at bit 4 only *)
let[@inline never] test_or_neg255 x = logor x (-#255L)
  (* 0xffffffffffffff01: 0s at bits 1-7, critical test case! *)
let[@inline never] test_or_neg257 x = logor x (-#257L)
  (* 0xfffffffffffffeff: 0 at bit 8 only *)
let[@inline never] test_or_neg65535 x = logor x (-#65535L)
  (* 0xffffffffffff0001: 0s at bits 1-15 *)

(* === Repeated element patterns ===
   The same bit pattern repeated in each element (2, 4, 8, 16, 32 bits).
   These use smaller element sizes in the encoding. *)
let[@inline never] test_or_5s x = logor x #0x5555555555555555L
  (* Alternating 01: element size 2, pattern 01 *)
let[@inline never] test_or_as x = logor x #0xaaaaaaaaaaaaaaaaL
  (* Alternating 10: element size 2, pattern 10 *)
let[@inline never] test_or_1s x = logor x #0x1111111111111111L
  (* One bit per nibble: element size 4, pattern 0001 *)
let[@inline never] test_or_0f0f x = logor x #0x0f0f0f0f0f0f0f0fL
  (* 4 ones, 4 zeros: element size 8, pattern 00001111 *)
let[@inline never] test_or_f0f0 x = logor x #0xf0f0f0f0f0f0f0f0L
  (* 4 zeros, 4 ones: element size 8, pattern 11110000 *)
let[@inline never] test_or_00ff x = logor x #0x00ff00ff00ff00ffL
  (* 8 ones, 8 zeros: element size 16 *)
let[@inline never] test_or_ff00_ x = logor x #0xff00ff00ff00ff00L
  (* 8 zeros, 8 ones: element size 16 *)
let[@inline never] test_or_0000ffff x = logor x #0x0000ffff0000ffffL
  (* 16 ones, 16 zeros: element size 32 *)
let[@inline never] test_or_ffff0000 x = logor x #0xffff0000ffff0000L
  (* 16 zeros, 16 ones: element size 32 *)

(* === Boundary patterns ===
   Patterns near the edges of the encoding space *)
let[@inline never] test_or_7f63 x = logor x #0x7fffffffffffffffL
  (* All ones except MSB: 63 consecutive 1s *)
let[@inline never] test_or_fe x = logor x #0xfffffffffffffffeL
  (* All ones except LSB: 63 ones rotated by 1 *)
let[@inline never] test_or_upper x = logor x #0xffffffff00000000L
  (* Upper 32 bits: 32 ones rotated *)
let[@inline never] test_or_lower x = logor x #0x00000000ffffffffL
  (* Lower 32 bits: 32 consecutive ones *)

(* === Powers of 2 plus 1 patterns ===
   Two 1 bits separated by zeros. These are 2-bit patterns rotated. *)
let[@inline never] test_or_11 x = logor x #0x11L
  (* bits 0 and 4: can use 4-bit element with pattern 0001 repeated *)
let[@inline never] test_or_101 x = logor x #0x101L
  (* bits 0 and 8 *)
let[@inline never] test_or_10001 x = logor x #0x10001L
  (* bits 0 and 16 *)
let[@inline never] test_or_100000001 x = logor x #0x100000001L
  (* bits 0 and 32 *)

(* === AND operations ===
   Same patterns but with AND to test that instruction too *)
let[@inline never] test_and_1 x = logand x #1L
let[@inline never] test_and_ff x = logand x #0xffL
let[@inline never] test_and_ffff x = logand x #0xffffL
let[@inline never] test_and_5s x = logand x #0x5555555555555555L
let[@inline never] test_and_neg255 x = logand x (-#255L)  (* wrapping pattern *)

(* === XOR operations ===
   Same patterns but with XOR to test that instruction too *)
let[@inline never] test_xor_1 x = logxor x #1L
let[@inline never] test_xor_ff x = logxor x #0xffL
let[@inline never] test_xor_ffff x = logxor x #0xffffL
let[@inline never] test_xor_as x = logxor x #0xaaaaaaaaaaaaaaaaL
let[@inline never] test_xor_neg255 x = logxor x (-#255L)  (* wrapping pattern *)

(* Entry point that exercises all patterns *)
let () =
  let x = #0L in
  (* Simple consecutive 1s *)
  let _ = test_or_1 x in
  let _ = test_or_3 x in
  let _ = test_or_7 x in
  let _ = test_or_f x in
  let _ = test_or_1f x in
  let _ = test_or_3f x in
  let _ = test_or_7f x in
  let _ = test_or_ff x in
  let _ = test_or_1ff x in
  let _ = test_or_3ff x in
  let _ = test_or_fff x in
  let _ = test_or_ffff x in
  let _ = test_or_fffff x in
  let _ = test_or_ffffff x in
  let _ = test_or_fffffff x in
  let _ = test_or_ffffffff x in
  (* Single bits *)
  let _ = test_or_2 x in
  let _ = test_or_4 x in
  let _ = test_or_8 x in
  let _ = test_or_10 x in
  let _ = test_or_100 x in
  let _ = test_or_1000 x in
  let _ = test_or_10000 x in
  let _ = test_or_100000000 x in
  (* Rotated consecutive bits *)
  let _ = test_or_6 x in
  let _ = test_or_1e x in
  let _ = test_or_7e0 x in
  let _ = test_or_ff00 x in
  let _ = test_or_ff0000 x in
  let _ = test_or_ff000000 x in
  (* Wrapping patterns - critical! *)
  let _ = test_or_neg3 x in
  let _ = test_or_neg5 x in
  let _ = test_or_neg9 x in
  let _ = test_or_neg17 x in
  let _ = test_or_neg255 x in
  let _ = test_or_neg257 x in
  let _ = test_or_neg65535 x in
  (* Repeated elements *)
  let _ = test_or_5s x in
  let _ = test_or_as x in
  let _ = test_or_1s x in
  let _ = test_or_0f0f x in
  let _ = test_or_f0f0 x in
  let _ = test_or_00ff x in
  let _ = test_or_ff00_ x in
  let _ = test_or_0000ffff x in
  let _ = test_or_ffff0000 x in
  (* Boundary patterns *)
  let _ = test_or_7f63 x in
  let _ = test_or_fe x in
  let _ = test_or_upper x in
  let _ = test_or_lower x in
  (* Powers of 2 plus 1 *)
  let _ = test_or_11 x in
  let _ = test_or_101 x in
  let _ = test_or_10001 x in
  let _ = test_or_100000001 x in
  (* AND operations *)
  let _ = test_and_1 x in
  let _ = test_and_ff x in
  let _ = test_and_ffff x in
  let _ = test_and_5s x in
  let _ = test_and_neg255 x in
  (* XOR operations *)
  let _ = test_xor_1 x in
  let _ = test_xor_ff x in
  let _ = test_xor_ffff x in
  let _ = test_xor_as x in
  let _ = test_xor_neg255 x in
  ()
