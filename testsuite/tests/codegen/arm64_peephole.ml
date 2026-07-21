(* TEST
 flags += " -O3";
 flags += " -arm64-peephole-optimize";
 only-default-codegen;
 expect.opt;
*)

(* Tests for the arm64 assembly-level peephole optimizer
   (backend/arm64_peephole). The expected assembly blocks below are only
   checked on arm64 hosts; promote them there with
   [make promote-one TEST=codegen/arm64_peephole.ml]. *)

(* Field initialization of the allocated block should use [stp] rather than
   one [str] per field (rule: fuse_memory_pairs). *)
type t =
  { a : int;
    b : int;
    c : int;
    d : int
  }

let make_record a b c d = { a; b; c; d }
[%%expect_asm ARM64 {|
|}]

(* Comballoc gives the second allocation its address as
   [add rd, x27, #8; add rd, rd, #k]; the two adds should be merged
   (rule: merge_add_immediates), and the field initializations should use
   [stp] (rule: fuse_memory_pairs). *)
let make_two_pairs x y = (x, y), (y, x)
[%%expect_asm ARM64 {|
|}]

(* The string length computation is [lsl #8; lsr #17] on the header (for
   byte-sized elements, followed by a subtraction involving the last byte);
   the two shifts should be composed into a single [ubfm]
   (rule: compose_shift_pairs). *)
let string_length s = String.length s
[%%expect_asm ARM64 {|
|}]

(* The second comparison of [x] against the same constant is redundant: the
   store in between does not affect the flags
   (rule: remove_redundant_cmp). *)
let redundant_compare x r =
  if x = 1 then r := 3;
  if x = 1 then 5 else 7
[%%expect_asm ARM64 {|
|}]
