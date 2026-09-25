(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -x86-peephole-optimize";
 expect.opt;
*)

open Intrinsics

(* Codegen tests for bit tests, i.e. [x land mask <> 0] and related idioms.

   The translation to Cmm drops the tag bit from both the mask and the constant
   when comparing tagged integers, avoids redundant sign extensions for 32-bit
   integers, and rewrites [x land bit = bit] into [x land bit <> 0]. The x86
   peephole optimizer then fuses the [and] into the following [test] when the
   result of the [and] is dead, and otherwise only drops the [test].

   Each phrase is compiled separately, so the constants are written inline
   rather than bound at toplevel. *)

let test_int32 (t : Int32_u.t) =
  let flag = #1L in
  Int32_u.to_int32
    (Int32_u.of_int32
       (Stdlib.Int32.logand (Int32_u.to_int32 t)
          (Int32_u.to_int32 (Int64_u.to_int32_u flag))))
  <> 0l
[%%expect_asm X86_64{|
test_int32:
  testl $1, %eax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let test_int64 (t : Int64_u.t) =
  let flag = #1L in
  Int64_u.to_int64 (Int64_u.logand t flag) <> 0L
[%%expect_asm X86_64{|
test_int64:
  testl $1, %eax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let test_int (t : int) =
  let flag = #1L in
  t land Int64_u.to_int flag <> 0
[%%expect_asm X86_64{|
test_int:
  testl $2, %eax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let test_int_bit (t : int) = t land 8 <> 0
[%%expect_asm X86_64{|
test_int_bit:
  testl $16, %eax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let test_int64_negative_mask (t : Int64_u.t) =
  Int64_u.to_int64 (Int64_u.logand t (-#2L)) <> 0L
[%%expect_asm X86_64{|
test_int64_negative_mask:
  testq $-2, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* The mask does not fit in an immediate, so the [and] takes a register
   operand and only the [test] is removed. *)
let test_int64_large_mask (t : Int64_u.t) =
  Int64_u.to_int64 (Int64_u.logand t #0x1_0000_0000L) <> 0L
[%%expect_asm X86_64{|
test_int64_large_mask:
  movabsq $4294967296, %rbx
  andq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let has_bit_int (t : int) = t land 4 = 4
[%%expect_asm X86_64{|
has_bit_int:
  testl $8, %eax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let has_bit_int64 (t : Int64_u.t) =
  Int64_u.to_int64 (Int64_u.logand t #4L) = 4L
[%%expect_asm X86_64{|
has_bit_int64:
  testl $4, %eax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* In the branching forms, the peephole optimizer cannot see past the
   conditional jump that the result of the [and] is dead, so the [and] is kept
   and only the [test] is removed. *)
let branch_int (t : int) = if t land 4 <> 0 then 10 else 20
[%%expect_asm X86_64{|
branch_int:
  andl  $8, %eax
  je    .L0
  movl  $21, %eax
  ret
.L0:
  movl  $41, %eax
  ret
|}]

let branch_int64 (t : Int64_u.t) =
  if Int64_u.to_int64 (Int64_u.logand t #4L) <> 0L then 10 else 20
[%%expect_asm X86_64{|
branch_int64:
  andl  $4, %eax
  je    .L0
  movl  $21, %eax
  ret
.L0:
  movl  $41, %eax
  ret
|}]

(* The result of the [and] is used afterwards, so the [and] must stay: only the
   [test] is removed. *)
let keep_and_result (t : Int64_u.t) =
  let m = Int64_u.logand t #6L in
  if Int64_u.to_int64 m <> 0L then m else #0L
[%%expect_asm X86_64{|
keep_and_result:
  andl  $6, %eax
  je    .L0
  ret
.L0:
  xorl  %eax, %eax
  ret
|}]
