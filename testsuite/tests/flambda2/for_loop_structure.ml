(* TEST
 flags += " -O3";
 only-default-codegen;
 expect.opt;
*)

let f bound r t =
  for x = 0 to bound do
    t := !t + x
  done
[%%expect_asm X86_64{|
f:
  cmpq  $1, %rax
  jl    .L111
  sarq  $1, %rax
  xorl  %ebx, %ebx
.L108:
  movq  (%rdi), %rsi
  leaq  (%rsi,%rbx,2), %rsi
  movq  %rsi, (%rdi)
  incq  %rbx
  cmpq  %rax, %rbx
  jle   .L108
  movl  $1, %eax
  ret
.L111:
  movl  $1, %eax
  ret
|}]
