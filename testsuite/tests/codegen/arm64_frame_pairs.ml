(* TEST
 flags += " -O3";
 flags += " -arm64-fuse-frame-ops";
 only-default-codegen;
 expect.opt;
*)

(* Tests for fused arm64 frame allocation (-arm64-fuse-frame-ops). The
   expected assembly blocks below are only checked on arm64 hosts; promote
   them there with [make promote-one TEST=codegen/arm64_frame_pairs.ml]. *)

(* A function that makes a non-tail call with no values live across it needs
   a 16-byte frame (just the saved return address). The prologue should be a
   single [stp xzr, x30, [sp, #-16]!] and the epilogue a single
   [ldp xzr, x30, [sp], #16]. *)
let[@inline never] callee x = x + 1

let small_frame x = callee x + 1
[%%expect_asm ARM64 {|
|}]

(* The fused epilogue must also appear before a tail call ([b], not [ret]). *)
let epilogue_before_tail_call x =
  let y = callee x in
  callee y
[%%expect_asm ARM64 {|
|}]

(* Values live across the call force spill slots, growing the frame beyond
   16 bytes; the prologue/epilogue must then keep the two-instruction
   sequences. *)
let big_frame a b c =
  let x = callee a in
  x + b + c
[%%expect_asm ARM64 {|
|}]
