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

(* Codegen tests for the Cmm peephole rules of Cmm_peephole_rules, on
   untagged integers so that [lognot] is [xor -1] and [neg] is [0 - x]. *)

(* and *)

let and_self x = Int64_u.logand x x
[%%expect_asm X86_64{|
and_self:
  ret
|}]

let and_not_self x = Int64_u.logand x (Int64_u.lognot x)
[%%expect_asm X86_64{|
and_not_self:
  xorl  %eax, %eax
  ret
|}]

let and_or_absorb x y = Int64_u.logand (Int64_u.logor x y) x
[%%expect_asm X86_64{|
and_or_absorb:
  ret
|}]

let and_not_xor a b = Int64_u.logand a (Int64_u.lognot (Int64_u.logxor a b))
[%%expect_asm X86_64{|
and_not_xor:
  andq  %rbx, %rax
  ret
|}]

let and_or_not_and a b =
  Int64_u.logand (Int64_u.logor a b) (Int64_u.lognot (Int64_u.logand a b))
[%%expect_asm X86_64{|
and_or_not_and:
  xorq  %rbx, %rax
  ret
|}]

let and_de_morgan x y = Int64_u.logand (Int64_u.lognot x) (Int64_u.lognot y)
[%%expect_asm X86_64{|
and_de_morgan:
  orq   %rbx, %rax
  xorq  $-1, %rax
  ret
|}]

let and_xor_const x = Int64_u.logand (Int64_u.logxor x #0xF0L) #0x0FL
[%%expect_asm X86_64{|
and_xor_const:
  andl  $15, %eax
  ret
|}]

let and_or_const x = Int64_u.logand (Int64_u.logor x #0xF0L) #0x0FL
[%%expect_asm X86_64{|
and_or_const:
  andl  $15, %eax
  ret
|}]

let and_lsl_mask x = Int64_u.logand (Int64_u.shift_left x 8) #0xFFFFFFFFFFFFFF00L
[%%expect_asm X86_64{|
and_lsl_mask:
  salq  $8, %rax
  ret
|}]

let and_lsr_mask x =
  Int64_u.logand (Int64_u.shift_right_logical x 8) #0x00FFFFFFFFFFFFFFL
[%%expect_asm X86_64{|
and_lsr_mask:
  shrq  $8, %rax
  ret
|}]

(* or *)

let or_self x = Int64_u.logor x x
[%%expect_asm X86_64{|
or_self:
  ret
|}]

let or_not_self x = Int64_u.logor x (Int64_u.lognot x)
[%%expect_asm X86_64{|
or_not_self:
  movq  $-1, %rax
  ret
|}]

let or_and_absorb x y = Int64_u.logor (Int64_u.logand x y) x
[%%expect_asm X86_64{|
or_and_absorb:
  ret
|}]

let or_xor x y = Int64_u.logor (Int64_u.logxor x y) y
[%%expect_asm X86_64{|
or_xor:
  orq   %rbx, %rax
  ret
|}]

let or_and_xor a b = Int64_u.logor (Int64_u.logand a b) (Int64_u.logxor a b)
[%%expect_asm X86_64{|
or_and_xor:
  orq   %rbx, %rax
  ret
|}]

let or_and_not_and_not a b =
  Int64_u.logor
    (Int64_u.logand a (Int64_u.lognot b))
    (Int64_u.logand (Int64_u.lognot a) b)
[%%expect_asm X86_64{|
or_and_not_and_not:
  xorq  %rbx, %rax
  ret
|}]

let or_de_morgan x y = Int64_u.logor (Int64_u.lognot x) (Int64_u.lognot y)
[%%expect_asm X86_64{|
or_de_morgan:
  andq  %rbx, %rax
  xorq  $-1, %rax
  ret
|}]

let or_not_xor a b = Int64_u.logor (Int64_u.lognot a) (Int64_u.logxor a b)
[%%expect_asm X86_64{|
or_not_xor:
  andq  %rbx, %rax
  xorq  $-1, %rax
  ret
|}]

(* xor *)

let xor_self x = Int64_u.logxor x x
[%%expect_asm X86_64{|
xor_self:
  xorl  %eax, %eax
  ret
|}]

let xor_not_self x = Int64_u.logxor x (Int64_u.lognot x)
[%%expect_asm X86_64{|
xor_not_self:
  movq  $-1, %rax
  ret
|}]

let xor_not_and a b = Int64_u.logxor (Int64_u.logand (Int64_u.lognot a) b) a
[%%expect_asm X86_64{|
xor_not_and:
  orq   %rbx, %rax
  ret
|}]

let not_add_const x = Int64_u.lognot (Int64_u.add x #10L)
[%%expect_asm X86_64{|
not_add_const:
  addq  $10, %rax
  xorq  $-1, %rax
  ret
|}]

let not_not_add x y = Int64_u.lognot (Int64_u.add (Int64_u.lognot x) y)
[%%expect_asm X86_64{|
not_not_add:
  subq  %rbx, %rax
  ret
|}]

let not_not_sub x y = Int64_u.lognot (Int64_u.sub (Int64_u.lognot x) y)
[%%expect_asm X86_64{|
not_not_sub:
  addq  %rbx, %rax
  ret
|}]

let not_neg x = Int64_u.lognot (Int64_u.neg x)
[%%expect_asm X86_64{|
not_neg:
  decq  %rax
  ret
|}]

let not_not_xor x y = Int64_u.lognot (Int64_u.logxor (Int64_u.lognot x) y)
[%%expect_asm X86_64{|
not_not_xor:
  xorq  %rbx, %rax
  ret
|}]

let not_not_and x y = Int64_u.lognot (Int64_u.logand (Int64_u.lognot x) y)
[%%expect_asm X86_64{|
not_not_and:
  xorq  $-1, %rbx
  orq   %rbx, %rax
  ret
|}]

let not_not_or x y = Int64_u.lognot (Int64_u.logor (Int64_u.lognot x) y)
[%%expect_asm X86_64{|
not_not_or:
  xorq  $-1, %rbx
  andq  %rbx, %rax
  ret
|}]

(* add *)

let add_neg x y = Int64_u.add x (Int64_u.neg y)
[%%expect_asm X86_64{|
add_neg:
  subq  %rbx, %rax
  ret
|}]

let add_sub_cancel a b = Int64_u.add (Int64_u.sub a b) b
[%%expect_asm X86_64{|
add_sub_cancel:
  ret
|}]

let add_and_or x y = Int64_u.add (Int64_u.logand x y) (Int64_u.logor x y)
[%%expect_asm X86_64{|
add_and_or:
  addq  %rbx, %rax
  ret
|}]

let add_and_xor x y = Int64_u.add (Int64_u.logand x y) (Int64_u.logxor x y)
[%%expect_asm X86_64{|
add_and_xor:
  orq   %rbx, %rax
  ret
|}]

let add_not_const x = Int64_u.add (Int64_u.lognot x) #10L
[%%expect_asm X86_64{|
add_not_const:
  xorq  $-1, %rax
  addq  $10, %rax
  ret
|}]

let add_not_not x y = Int64_u.add (Int64_u.lognot x) (Int64_u.lognot y)
[%%expect_asm X86_64{|
add_not_not:
  addq  %rax, %rbx
  movq  $-2, %rax
  subq  %rbx, %rax
  ret
|}]

let add_mul_const_self x = Int64_u.add (Int64_u.mul x #10L) x
[%%expect_asm X86_64{|
add_mul_const_self:
  imulq $11, %rax
  ret
|}]

let add_mul_distribute x y z =
  Int64_u.add (Int64_u.mul x z) (Int64_u.mul y z)
[%%expect_asm X86_64{|
add_mul_distribute:
  addq  %rbx, %rax
  imulq %rdi, %rax
  ret
|}]

(* sub *)

let sub_add_cancel_left x y = Int64_u.sub (Int64_u.add x y) x
[%%expect_asm X86_64{|
sub_add_cancel_left:
  movq  %rbx, %rax
  ret
|}]

let sub_add_cancel_right x y = Int64_u.sub x (Int64_u.add x y)
[%%expect_asm X86_64{|
sub_add_cancel_right:
  movq  %rbx, %rax
  neg   %rax
  ret
|}]

let sub_sub_cancel x y = Int64_u.sub x (Int64_u.sub x y)
[%%expect_asm X86_64{|
sub_sub_cancel:
  movq  %rbx, %rax
  ret
|}]

let sub_sub_cancel_left x y = Int64_u.sub (Int64_u.sub x y) x
[%%expect_asm X86_64{|
sub_sub_cancel_left:
  movq  %rbx, %rax
  neg   %rax
  ret
|}]

let sub_neg x y = Int64_u.sub x (Int64_u.neg y)
[%%expect_asm X86_64{|
sub_neg:
  addq  %rbx, %rax
  ret
|}]

let neg_sub x y = Int64_u.neg (Int64_u.sub x y)
[%%expect_asm X86_64{|
neg_sub:
  movq  %rax, %rdi
  movq  %rbx, %rax
  subq  %rdi, %rax
  ret
|}]

let neg_neg x = Int64_u.neg (Int64_u.neg x)
[%%expect_asm X86_64{|
neg_neg:
  ret
|}]

let neg_not x = Int64_u.neg (Int64_u.lognot x)
[%%expect_asm X86_64{|
neg_not:
  incq  %rax
  ret
|}]

let sub_or_and x y = Int64_u.sub (Int64_u.logor x y) (Int64_u.logand x y)
[%%expect_asm X86_64{|
sub_or_and:
  xorq  %rbx, %rax
  ret
|}]

let sub_or_xor x y = Int64_u.sub (Int64_u.logor x y) (Int64_u.logxor x y)
[%%expect_asm X86_64{|
sub_or_xor:
  andq  %rbx, %rax
  ret
|}]

let sub_add_and x y = Int64_u.sub (Int64_u.add x y) (Int64_u.logand x y)
[%%expect_asm X86_64{|
sub_add_and:
  orq   %rbx, %rax
  ret
|}]

let sub_add_or x y = Int64_u.sub (Int64_u.add x y) (Int64_u.logor x y)
[%%expect_asm X86_64{|
sub_add_or:
  andq  %rbx, %rax
  ret
|}]

let sub_not_not x y = Int64_u.sub (Int64_u.lognot x) (Int64_u.lognot y)
[%%expect_asm X86_64{|
sub_not_not:
  movq  %rax, %rdi
  movq  %rbx, %rax
  subq  %rdi, %rax
  ret
|}]

let sub_mul_const_self x = Int64_u.sub (Int64_u.mul x #10L) x
[%%expect_asm X86_64{|
sub_mul_const_self:
  leaq  (%rax,%rax,8), %rax
  ret
|}]

let sub_self_mul_const x = Int64_u.sub x (Int64_u.mul x #10L)
[%%expect_asm X86_64{|
sub_self_mul_const:
  imulq $-9, %rax
  ret
|}]

let sub_mul_distribute x y z =
  Int64_u.sub (Int64_u.mul x z) (Int64_u.mul y z)
[%%expect_asm X86_64{|
sub_mul_distribute:
  subq  %rbx, %rax
  imulq %rdi, %rax
  ret
|}]

(* shifts *)

let lsl_and_mask x = Int64_u.shift_left (Int64_u.logand x #0x00FFFFFFFFFFFFFFL) 8
[%%expect_asm X86_64{|
lsl_and_mask:
  salq  $8, %rax
  ret
|}]

let lsl_mul_const x = Int64_u.shift_left (Int64_u.mul x #10L) 3
[%%expect_asm X86_64{|
lsl_mul_const:
  imulq $80, %rax
  ret
|}]

let lsr_lsl x = Int64_u.shift_right_logical (Int64_u.shift_left x 8) 8
[%%expect_asm X86_64{|
lsr_lsl:
  movabsq $72057594037927935, %rbx
  andq  %rbx, %rax
  ret
|}]

(* mul *)

let mul_const_const x = Int64_u.mul (Int64_u.mul x #3L) #5L
[%%expect_asm X86_64{|
mul_const_const:
  imulq $15, %rax
  ret
|}]

let mul_lsl_const x = Int64_u.mul (Int64_u.shift_left x 3) #5L
[%%expect_asm X86_64{|
mul_lsl_const:
  imulq $40, %rax
  ret
|}]

let mul_neg_neg x y = Int64_u.mul (Int64_u.neg x) (Int64_u.neg y)
[%%expect_asm X86_64{|
mul_neg_neg:
  imulq %rbx, %rax
  ret
|}]

let mul_neg_const x = Int64_u.mul (Int64_u.neg x) #10L
[%%expect_asm X86_64{|
mul_neg_const:
  imulq $-10, %rax
  ret
|}]
