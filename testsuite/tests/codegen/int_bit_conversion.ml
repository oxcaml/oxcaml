(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -experimental-optimizations";
 expect.opt;
*)

open Intrinsics

(* Codegen tests for conversions between tagged ints and untagged
   [nativeint#] values that only manipulate individual bits. *)

(* The following three functions are equivalent and compile to the same
   code. *)

let land_max_int_after_tag n = Nativeint_u.to_int n land max_int
[%%expect_asm X86_64{|
land_max_int_after_tag:
  salq  $2, %rax
  shrq  $1, %rax
  incq  %rax
  ret
|}]

let lsl_lsr_after_tag n = (Nativeint_u.to_int n lsl 1) lsr 1
[%%expect_asm X86_64{|
lsl_lsr_after_tag:
  salq  $2, %rax
  shrq  $1, %rax
  incq  %rax
  ret
|}]

let clear_top_bits_before_tag n =
  Nativeint_u.(to_int (shift_right_logical (shift_left n 2) 2))
[%%expect_asm X86_64{|
clear_top_bits_before_tag:
  salq  $2, %rax
  shrq  $1, %rax
  incq  %rax
  ret
|}]

(* This reconstructs the tagged representation of [i], so it is a no-op:
   untagging asserts that the low bit of [i] is one. *)
let untag_then_retag i =
  Nativeint_u.(logor (shift_left (of_int i) 1) (of_nativeint 1n))
[%%expect_asm X86_64{|
untag_then_retag:
  ret
|}]

(* Since the low bit of [i] is known to be one, this is [i - 1]. *)
let untag_then_lsl_1 i = Nativeint_u.(shift_left (of_int i) 1)
[%%expect_asm X86_64{|
untag_then_lsl_1:
  decq  %rax
  ret
|}]

(* Likewise, this is [(i lsl 2) - 4]. *)
let untag_then_lsl_3 i = Nativeint_u.(shift_left (of_int i) 3)
[%%expect_asm X86_64{|
untag_then_lsl_3:
  leaq  -4(,%rax,4), %rax
  ret
|}]

(* CR ttebbi: Could be [ror rax, 1]: the low bit of [n] is known to be one,
   but the backend has no rotate instruction selection yet. *)
let untag_then_set_top_bit n =
  Nativeint_u.(logor (of_nativeint 0x8000000000000000n) (of_int n))
[%%expect_asm X86_64{|
untag_then_set_top_bit:
  movabsq $-9223372036854775808, %rbx
  sarq  $1, %rax
  orq   %rbx, %rax
  ret
|}]

let low_bit_is_zero n =
  Nativeint_u.(to_int (logand n (of_nativeint 1n))) = 0
[%%expect_asm X86_64{|
low_bit_is_zero:
  andl  $1, %eax
  xorq  $1, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* CR ttebbi: The [and] and [test] could be combined into
   [testb $1, %al]. *)
let branch_on_low_bit n ~then_ ~else_ =
  if Nativeint_u.(to_int (logand n (of_nativeint 1n))) = 0
  then then_ ()
  else else_ ()
[%%expect_asm X86_64{|
branch_on_low_bit:
  andl  $1, %eax
  testq %rax, %rax
  jne   .L0
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
.L0:
  movl  $1, %eax
  movq  (%rdi), %rsi
  movq  %rdi, %rbx
  jmp   *%rsi
|}]
