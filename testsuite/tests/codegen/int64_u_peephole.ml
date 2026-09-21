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
  andq  %rax, %rax
  ret
|}]

let and_not_self x = Int64_u.logand x (Int64_u.lognot x)
[%%expect_asm X86_64{|
and_not_self:
  movq  %rax, %rbx
  xorq  $-1, %rbx
  andq  %rbx, %rax
  ret
|}]

let and_or_absorb x y = Int64_u.logand (Int64_u.logor x y) x
[%%expect_asm X86_64{|
and_or_absorb:
  movq  %rax, %rdi
  orq   %rbx, %rax
  andq  %rdi, %rax
  ret
|}]

let and_not_xor a b = Int64_u.logand a (Int64_u.lognot (Int64_u.logxor a b))
[%%expect_asm X86_64{|
and_not_xor:
  movq  %rax, %rdi
  xorq  %rbx, %rdi
  xorq  $-1, %rdi
  andq  %rdi, %rax
  ret
|}]

let and_or_not_and a b =
  Int64_u.logand (Int64_u.logor a b) (Int64_u.lognot (Int64_u.logand a b))
[%%expect_asm X86_64{|
and_or_not_and:
  movq  %rax, %rdi
  andq  %rbx, %rdi
  xorq  $-1, %rdi
  orq   %rbx, %rax
  andq  %rdi, %rax
  ret
|}]

let and_de_morgan x y = Int64_u.logand (Int64_u.lognot x) (Int64_u.lognot y)
[%%expect_asm X86_64{|
and_de_morgan:
  xorq  $-1, %rbx
  xorq  $-1, %rax
  andq  %rbx, %rax
  ret
|}]

let and_xor_const x = Int64_u.logand (Int64_u.logxor x #0xF0L) #0x0FL
[%%expect_asm X86_64{|
and_xor_const:
  xorq  $240, %rax
  andl  $15, %eax
  ret
|}]

let and_or_const x = Int64_u.logand (Int64_u.logor x #0xF0L) #0x0FL
[%%expect_asm X86_64{|
and_or_const:
  orq   $240, %rax
  andl  $15, %eax
  ret
|}]

let and_lsl_mask x = Int64_u.logand (Int64_u.shift_left x 8) #0xFFFFFFFFFFFFFF00L
[%%expect_asm X86_64{|
and_lsl_mask:
  salq  $8, %rax
  andq  $-256, %rax
  ret
|}]

let and_lsr_mask x =
  Int64_u.logand (Int64_u.shift_right_logical x 8) #0x00FFFFFFFFFFFFFFL
[%%expect_asm X86_64{|
and_lsr_mask:
  movabsq $72057594037927935, %rbx
  shrq  $8, %rax
  andq  %rbx, %rax
  ret
|}]

(* or *)

let or_self x = Int64_u.logor x x
[%%expect_asm X86_64{|
or_self:
  orq   %rax, %rax
  ret
|}]

let or_not_self x = Int64_u.logor x (Int64_u.lognot x)
[%%expect_asm X86_64{|
or_not_self:
  movq  %rax, %rbx
  xorq  $-1, %rbx
  orq   %rbx, %rax
  ret
|}]

let or_and_absorb x y = Int64_u.logor (Int64_u.logand x y) x
[%%expect_asm X86_64{|
or_and_absorb:
  movq  %rax, %rdi
  andq  %rbx, %rax
  orq   %rdi, %rax
  ret
|}]

let or_xor x y = Int64_u.logor (Int64_u.logxor x y) y
[%%expect_asm X86_64{|
or_xor:
  xorq  %rbx, %rax
  orq   %rbx, %rax
  ret
|}]

let or_and_xor a b = Int64_u.logor (Int64_u.logand a b) (Int64_u.logxor a b)
[%%expect_asm X86_64{|
or_and_xor:
  movq  %rax, %rdi
  xorq  %rbx, %rdi
  andq  %rbx, %rax
  orq   %rdi, %rax
  ret
|}]

let or_and_not_and_not a b =
  Int64_u.logor
    (Int64_u.logand a (Int64_u.lognot b))
    (Int64_u.logand (Int64_u.lognot a) b)
[%%expect_asm X86_64{|
or_and_not_and_not:
  movq  %rax, %rdi
  xorq  $-1, %rdi
  andq  %rbx, %rdi
  xorq  $-1, %rbx
  andq  %rbx, %rax
  orq   %rdi, %rax
  ret
|}]

let or_de_morgan x y = Int64_u.logor (Int64_u.lognot x) (Int64_u.lognot y)
[%%expect_asm X86_64{|
or_de_morgan:
  xorq  $-1, %rbx
  xorq  $-1, %rax
  orq   %rbx, %rax
  ret
|}]

let or_not_xor a b = Int64_u.logor (Int64_u.lognot a) (Int64_u.logxor a b)
[%%expect_asm X86_64{|
or_not_xor:
  movq  %rax, %rdi
  xorq  %rbx, %rdi
  xorq  $-1, %rax
  orq   %rdi, %rax
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
  movq  %rax, %rbx
  xorq  $-1, %rbx
  xorq  %rbx, %rax
  ret
|}]

let xor_not_and a b = Int64_u.logxor (Int64_u.logand (Int64_u.lognot a) b) a
[%%expect_asm X86_64{|
xor_not_and:
  movq  %rax, %rdi
  xorq  $-1, %rax
  andq  %rbx, %rax
  xorq  %rdi, %rax
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
  xorq  $-1, %rax
  addq  %rbx, %rax
  xorq  $-1, %rax
  ret
|}]

let not_not_sub x y = Int64_u.lognot (Int64_u.sub (Int64_u.lognot x) y)
[%%expect_asm X86_64{|
not_not_sub:
  xorq  $-1, %rax
  subq  %rbx, %rax
  xorq  $-1, %rax
  ret
|}]

let not_neg x = Int64_u.lognot (Int64_u.neg x)
[%%expect_asm X86_64{|
not_neg:
  neg   %rax
  xorq  $-1, %rax
  ret
|}]

let not_not_xor x y = Int64_u.lognot (Int64_u.logxor (Int64_u.lognot x) y)
[%%expect_asm X86_64{|
not_not_xor:
  xorq  $-1, %rax
  xorq  %rbx, %rax
  xorq  $-1, %rax
  ret
|}]

let not_not_and x y = Int64_u.lognot (Int64_u.logand (Int64_u.lognot x) y)
[%%expect_asm X86_64{|
not_not_and:
  xorq  $-1, %rax
  andq  %rbx, %rax
  xorq  $-1, %rax
  ret
|}]

let not_not_or x y = Int64_u.lognot (Int64_u.logor (Int64_u.lognot x) y)
[%%expect_asm X86_64{|
not_not_or:
  xorq  $-1, %rax
  orq   %rbx, %rax
  xorq  $-1, %rax
  ret
|}]

(* add *)

let add_neg x y = Int64_u.add x (Int64_u.neg y)
[%%expect_asm X86_64{|
add_neg:
  neg   %rbx
  addq  %rbx, %rax
  ret
|}]

let add_sub_cancel a b = Int64_u.add (Int64_u.sub a b) b
[%%expect_asm X86_64{|
add_sub_cancel:
  subq  %rbx, %rax
  addq  %rbx, %rax
  ret
|}]

let add_and_or x y = Int64_u.add (Int64_u.logand x y) (Int64_u.logor x y)
[%%expect_asm X86_64{|
add_and_or:
  movq  %rax, %rdi
  orq   %rbx, %rdi
  andq  %rbx, %rax
  addq  %rdi, %rax
  ret
|}]

let add_and_xor x y = Int64_u.add (Int64_u.logand x y) (Int64_u.logxor x y)
[%%expect_asm X86_64{|
add_and_xor:
  movq  %rax, %rdi
  xorq  %rbx, %rdi
  andq  %rbx, %rax
  addq  %rdi, %rax
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
  xorq  $-1, %rbx
  xorq  $-1, %rax
  addq  %rbx, %rax
  ret
|}]

let add_mul_const_self x = Int64_u.add (Int64_u.mul x #10L) x
[%%expect_asm X86_64{|
add_mul_const_self:
  movq  %rax, %rbx
  imulq $10, %rbx
  addq  %rbx, %rax
  ret
|}]

let add_mul_distribute x y z =
  Int64_u.add (Int64_u.mul x z) (Int64_u.mul y z)
[%%expect_asm X86_64{|
add_mul_distribute:
  imulq %rdi, %rbx
  imulq %rdi, %rax
  addq  %rbx, %rax
  ret
|}]

(* sub *)

let sub_add_cancel_left x y = Int64_u.sub (Int64_u.add x y) x
[%%expect_asm X86_64{|
sub_add_cancel_left:
  movq  %rax, %rdi
  leaq  (%rdi,%rbx), %rax
  subq  %rdi, %rax
  ret
|}]

let sub_add_cancel_right x y = Int64_u.sub x (Int64_u.add x y)
[%%expect_asm X86_64{|
sub_add_cancel_right:
  addq  %rax, %rbx
  subq  %rbx, %rax
  ret
|}]

let sub_sub_cancel x y = Int64_u.sub x (Int64_u.sub x y)
[%%expect_asm X86_64{|
sub_sub_cancel:
  movq  %rax, %rdi
  subq  %rbx, %rdi
  subq  %rdi, %rax
  ret
|}]

let sub_sub_cancel_left x y = Int64_u.sub (Int64_u.sub x y) x
[%%expect_asm X86_64{|
sub_sub_cancel_left:
  movq  %rax, %rdi
  subq  %rbx, %rax
  subq  %rdi, %rax
  ret
|}]

let sub_neg x y = Int64_u.sub x (Int64_u.neg y)
[%%expect_asm X86_64{|
sub_neg:
  neg   %rbx
  subq  %rbx, %rax
  ret
|}]

let neg_sub x y = Int64_u.neg (Int64_u.sub x y)
[%%expect_asm X86_64{|
neg_sub:
  subq  %rbx, %rax
  neg   %rax
  ret
|}]

let neg_neg x = Int64_u.neg (Int64_u.neg x)
[%%expect_asm X86_64{|
neg_neg:
  neg   %rax
  neg   %rax
  ret
|}]

let neg_not x = Int64_u.neg (Int64_u.lognot x)
[%%expect_asm X86_64{|
neg_not:
  xorq  $-1, %rax
  neg   %rax
  ret
|}]

let sub_or_and x y = Int64_u.sub (Int64_u.logor x y) (Int64_u.logand x y)
[%%expect_asm X86_64{|
sub_or_and:
  movq  %rax, %rdi
  andq  %rbx, %rdi
  orq   %rbx, %rax
  subq  %rdi, %rax
  ret
|}]

let sub_or_xor x y = Int64_u.sub (Int64_u.logor x y) (Int64_u.logxor x y)
[%%expect_asm X86_64{|
sub_or_xor:
  movq  %rax, %rdi
  xorq  %rbx, %rdi
  orq   %rbx, %rax
  subq  %rdi, %rax
  ret
|}]

let sub_add_and x y = Int64_u.sub (Int64_u.add x y) (Int64_u.logand x y)
[%%expect_asm X86_64{|
sub_add_and:
  movq  %rax, %rdi
  andq  %rbx, %rdi
  addq  %rbx, %rax
  subq  %rdi, %rax
  ret
|}]

let sub_add_or x y = Int64_u.sub (Int64_u.add x y) (Int64_u.logor x y)
[%%expect_asm X86_64{|
sub_add_or:
  movq  %rax, %rdi
  orq   %rbx, %rdi
  addq  %rbx, %rax
  subq  %rdi, %rax
  ret
|}]

let sub_not_not x y = Int64_u.sub (Int64_u.lognot x) (Int64_u.lognot y)
[%%expect_asm X86_64{|
sub_not_not:
  xorq  $-1, %rbx
  xorq  $-1, %rax
  subq  %rbx, %rax
  ret
|}]

let sub_mul_const_self x = Int64_u.sub (Int64_u.mul x #10L) x
[%%expect_asm X86_64{|
sub_mul_const_self:
  movq  %rax, %rbx
  imulq $10, %rax
  subq  %rbx, %rax
  ret
|}]

let sub_self_mul_const x = Int64_u.sub x (Int64_u.mul x #10L)
[%%expect_asm X86_64{|
sub_self_mul_const:
  movq  %rax, %rbx
  imulq $10, %rbx
  subq  %rbx, %rax
  ret
|}]

let sub_mul_distribute x y z =
  Int64_u.sub (Int64_u.mul x z) (Int64_u.mul y z)
[%%expect_asm X86_64{|
sub_mul_distribute:
  imulq %rdi, %rbx
  imulq %rdi, %rax
  subq  %rbx, %rax
  ret
|}]

(* shifts *)

let lsl_and_mask x = Int64_u.shift_left (Int64_u.logand x #0x00FFFFFFFFFFFFFFL) 8
[%%expect_asm X86_64{|
lsl_and_mask:
  movabsq $72057594037927935, %rbx
  andq  %rbx, %rax
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
  salq  $8, %rax
  shrq  $8, %rax
  ret
|}]

(* mul *)

let mul_const_const x = Int64_u.mul (Int64_u.mul x #3L) #5L
[%%expect_asm X86_64{|
mul_const_const:
  leaq  (%rax,%rax,2), %rax
  imulq $5, %rax
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
  neg   %rbx
  neg   %rax
  imulq %rbx, %rax
  ret
|}]

let mul_neg_const x = Int64_u.mul (Int64_u.neg x) #10L
[%%expect_asm X86_64{|
mul_neg_const:
  neg   %rax
  imulq $10, %rax
  ret
|}]
