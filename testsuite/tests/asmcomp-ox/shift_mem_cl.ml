(* TEST
 arch_amd64;
 flags = "-internal-assembler";
 native;
*)

(* Regression test for x86 binary emitter assertion failure.

   Bug: SHL/SHR/SAR instructions with memory destination and CL register count
   were not supported, causing an assertion failure in x86_binary_emitter.ml.

   Triggers when:
   1. Variable shift count (not compile-time constant) -> uses CL register
   2. High register pressure -> shift result spills to stack (memory)
   3. Internal assembler enabled (-internal-assembler flag)

   The fix was adding "Mem _" to the pattern in emit_shift for the Reg8L RCX
   case. *)

let[@inline always] shift_right x shift_amount = x lsr shift_amount
let[@inline always] shift_left x shift_amount = x lsl shift_amount

let compute a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8 ~shift =
  let r1 = shift_right a1 shift in
  let r2 = shift_right a2 shift in
  let r3 = shift_right a3 shift in
  let r4 = shift_right a4 shift in
  let r5 = shift_right a5 shift in
  let r6 = shift_right a6 shift in
  let r7 = shift_right a7 shift in
  let r8 = shift_right a8 shift in
  let s1 = shift_left b1 shift in
  let s2 = shift_left b2 shift in
  let s3 = shift_left b3 shift in
  let s4 = shift_left b4 shift in
  let s5 = shift_left b5 shift in
  let s6 = shift_left b6 shift in
  let s7 = shift_left b7 shift in
  let s8 = shift_left b8 shift in
  r1 + r2 + r3 + r4 + r5 + r6 + r7 + r8 + s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8
;;

let () =
  let result = compute 1 2 3 4 5 6 7 8 10 20 30 40 50 60 70 80 ~shift:3 in
  Printf.printf "Result: %d\n" result
;;
