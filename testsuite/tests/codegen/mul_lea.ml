(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 only-default-codegen;
 expect.opt;
*)

(* Multiplication by 3, 5 and 9 should be strength-reduced to a single [lea],
   even when the operand is a computed expression (not just a variable) and for
   [int32] / [int64], where it previously emitted [imul]. *)

let mul5_var (x : int) = x * 5
[%%expect_asm X86_64{|
mul5_var:
  leaq  -4(%rax,%rax,4), %rax
  ret
|}]

let mul5_expr (x : int) = (x lxor 3) * 5
[%%expect_asm X86_64{|
mul5_expr:
  xorq  $7, %rax
  orq   $1, %rax
  leaq  (%rax,%rax,4), %rax
  addq  $-4, %rax
  ret
|}]

let mul3_expr (x : int) (y : int) = (x + y) * 3
[%%expect_asm X86_64{|
mul3_expr:
  addq  %rbx, %rax
  leaq  (%rax,%rax,2), %rax
  addq  $-5, %rax
  ret
|}]

let mul9_expr (x : int) (y : int) = (x + y) * 9
[%%expect_asm X86_64{|
mul9_expr:
  addq  %rbx, %rax
  leaq  (%rax,%rax,8), %rax
  addq  $-17, %rax
  ret
|}]

let mul5_int64 (x : int64) = Int64.mul x 5L
[%%expect_asm X86_64{|
mul5_int64:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rbx
  movq  $2303, -8(%rbx)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rbx)
  movq  8(%rax), %rax
  leaq  (%rax,%rax,4), %rax
  movq  %rax, 8(%rbx)
  movq  %rbx, %rax
  addq  $8, %rsp
  ret
|}]

let mul5_int32 (x : int32) = Int32.mul x 5l
[%%expect_asm X86_64{|
mul5_int32:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rbx
  movq  $2303, -8(%rbx)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rbx)
  movslq 8(%rax), %rax
  leaq  (%rax,%rax,4), %rax
  movslq %eax, %rax
  movq  %rax, 8(%rbx)
  movq  %rbx, %rax
  addq  $8, %rsp
  ret
|}]

let mul7_expr (x : int) (y : int) = (x + y) * 7
[%%expect_asm X86_64{|
mul7_expr:
  addq  %rbx, %rax
  imulq $7, %rax
  addq  $-13, %rax
  ret
|}]
